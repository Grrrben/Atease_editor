/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** defines ***/

#define CTRL_KEY(k) ((k) & 0x1f)
#define ABUF_INIT {NULL, 0}
#define EDITOR_DEBUG 1
#define EDITOR_VERSION "0.0.1"
#define EDITOR_TAB_STOP 4
#define EDITOR_QUIT_TIMES 3
#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)


#define MESSAGE_START_UP_TEXT "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find"
#define MESSAGE_SAVED "%d bytes written to disk"
#define MESSAGE_SAVE_ABORTED "Save aborted"
#define MESSAGE_SAVE_AS "Save as: %s (ESC to cancel)"
#define MESSAGE_SAVE_ERROR "Can't save! I/O error: %s"
#define MESSAGE_SEARCH "Search: %s (Use ESC/Arrows/Enter)"
#define MESSAGE_QUIT_WARNING "WARNING!!! File has unsaved changes. Press Ctrl-Q %d more times to quit."

/*** data ***/

struct editorSyntax {
    char *filetype;
    char **filematch;
    char **keywords;
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
    int flags;
};

/* editor row */
typedef struct erow {
    int idx;
    int size;
    int rsize;
    char *chars;
    char *render;
    unsigned char *hl; /* highlight */
    int hl_open_comment;
} erow;

struct editorConfig {
    int cx, cy;
    int rx;
    int coloff;
    int rowoff;
    int screencols;
    int screenrows;
    int numrows;
    erow *row;
    int dirty; /* modified since opening or saving the file.  */
    char *filename;
    char statusmsg[80];
    char debugmsg[80];
    time_t statusmsg_time;
    struct editorSyntax *syntax;
    struct termios orig_termios;
};

struct editorConfig E;

/*
 * By setting the first constant in the enum to 1000,
 * the rest of the constants get incrementing values
 * of 1001, 1002, 1003, and so on.
 */
enum editorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

struct abuf {
    char *b;
    int len;
};
/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };

char *PHP_HL_extensions[] = { ".php", NULL };

/* we’ll highlight common C types as secondary keywords, so we end each one with a | character. */
char *C_HL_keywords[] = {
        "#define", "#include", "switch", "if", "while", "for", "break", "continue", "return", "else",
        "struct", "union", "typedef", "static", "enum", "class", "case",
        "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", "void|", NULL
};

char *PHP_HL_keywords[] = {
        "use", "include", "switch", "if", "while", "for", "break", "continue", "return", "else",
        "struct", "static", "class", "case",
        "int|", "float|",  "unsigned|", "signed|", "string|", "void|", "enum|", NULL
};

/* HighLightDataBase*/
struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//",
        "/*",
        "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    {
        "php",
        PHP_HL_extensions,
        PHP_HL_keywords,
        "//",
        "/*",
        "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/

/*
 * When we call a function in C, the compiler needs to know the arguments and return value
 * of that function. We can tell the compiler this information about that function
 * by declaring a function prototype for it near the top of the file.
 * This allows us to call the function before it is defined.
 */
void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void editorFreeRow(erow *row);

/*** terminal ***/
#include "terminal.h"

/*** syntax highlighting ***/
#include "syntax.h"

/*** row operations ***/

/**
 * chars to rendered text (used for tabs)
 * todo special multibyte chars give a wrong cursor position
 */
int editorRowCxToRx(erow *row, int cx) {
    int rx = 0;
    int j;
    for (j = 0; j < cx; j++) {
        if (row->chars[j] == '\t')
            rx += (EDITOR_TAB_STOP - 1) - (rx % EDITOR_TAB_STOP);
        rx++;
    }
    return rx;
}

/**
 * Rendered text to chars
 */
int editorRowRxToCx(erow *row, int rx) {
    int cur_rx = 0;
    int cx;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t')
            cur_rx += (EDITOR_TAB_STOP - 1) - (cur_rx % EDITOR_TAB_STOP);
        cur_rx++;
        if (cur_rx > rx) return cx;
    }
    return cx;
}

void editorUpdateRow(erow *row) {
    int tabs = 0;
    int j;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t')
            tabs++;
    }

    free(row->render);

    /*
     * The maximum number of characters needed for each tab is 8.
     * row->size already counts 1 for each tab, so we multiply the
     * number of tabs by 7 and add that to row->size to get the
     * maximum amount of memory we’ll need for the rendered row.
     */
    row->render = malloc(row->size + tabs*(EDITOR_TAB_STOP - 1) + 1);

    int idx = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            /*
             * we append one space (because each tab must advance the cursor forward at least one column),
             * and then append spaces until we get to a tab stop, which is a column that is divisible by 8.
             */
            row->render[idx++] = ' ';
            while (idx % EDITOR_TAB_STOP != 0) row->render[idx++] = ' ';
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
    if (at < 0 || at > E.numrows)
        return;

    /*
     * Allocate space for a new erow
     * We have to tell realloc() how many bytes we want to allocate,
     * so we multiply the number of bytes each erow takes (sizeof(erow))
     * and multiply that by the number of rows we want.
     */
    E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
    for (int j = at + 1; j <= E.numrows; j++) {
        E.row[j].idx++;
    }

    E.row[at].idx = at;

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].rsize = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    editorUpdateRow(&E.row[at]);

    E.numrows++;
    E.dirty++;
}

void editorDelRow(int at) {
    if (at < 0 || at >= E.numrows) return;
    editorFreeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
    for (int j = at + 1; j <= E.numrows; j++) {
        E.row[j].idx--;
    }
    E.numrows--;
    E.dirty++;
}

void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

void editorRowInsertChar(erow *row, int at, int c) {
    /** validate int at */
    if (at < 0 || at > row->size)
        at = row->size;
    row->chars = realloc(row->chars, row->size + 2);
    /* memmove copies count bytes from src to dst */
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
}

void editorRowDelChar(erow *row, int at) {
    if (at < 0 || at >= row->size)
        return;
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editorUpdateRow(row);
    E.dirty++;
}

/*** editor operations ***/

void editorInsertChar(int c) {
    if (E.cy == E.numrows) {
        /* append row */
        editorInsertRow(E.numrows, "", 0);
    }
    editorRowInsertChar(&E.row[E.cy], E.cx, c);
    E.cx++;
    E.dirty++;
}

void editorInsertNewline() {
    if (E.cx == 0) {
        editorInsertRow(E.cy, "", 0);
    } else {
        erow *row = &E.row[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editorUpdateRow(row);
    }
    E.cy++;
    E.cx = 0;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}

void editorDelChar() {
    if (E.cy == E.numrows) return;
    if (E.cx == 0 && E.cy == 0) return;
    erow *row = &E.row[E.cy];
    if (E.cx > 0) {
        editorRowDelChar(row, E.cx - 1);
        E.cx--;
    } else {
        E.cx = E.row[E.cy - 1].size;
        editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
        editorDelRow(E.cy);
        E.cy--;
    }
}

/*** file i/o ***/

void editorOpen(char *filename) {
    free(E.filename);
    E.filename = strdup(filename);

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp)
        die("fopen");

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    /* looping the input file */
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        while (linelen > 0 && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r'))
            linelen--;
        editorInsertRow(E.numrows, line, linelen);
    }
    free(line);
    fclose(fp);
    E.dirty = 0;
}

char *editorRowsToString(int *buflen) {
    int totlen = 0;
    int j;
    for (j = 0; j < E.numrows; j++)
        totlen += E.row[j].size + 1;
    *buflen = totlen;
    char *buf = malloc(totlen);
    char *p = buf;
    for (j = 0; j < E.numrows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }
    return buf;
}

void editorSave() {
    if (E.filename == NULL) {
        E.filename = editorPrompt(MESSAGE_SAVE_AS, NULL);
        if (E.filename == NULL) {
            editorSetStatusMessage(MESSAGE_SAVE_ABORTED);
            return;
        }
        /* we know a filetype :) */
        editorSelectSyntaxHighlight();
    }

    int len;
    char *buf = editorRowsToString(&len);
    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            if (write(fd, buf, len) == len) {
                close(fd);
                free(buf);
                E.dirty = 0;
                editorSetStatusMessage(MESSAGE_SAVED, len);
                return;
            }
        }
        close(fd);
    }
    free(buf);
    /* should not be here */
    editorSetStatusMessage(MESSAGE_SAVE_ERROR, strerror(errno));
}

/*** find ***/

void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;

    static int saved_hl_line;
    static char *saved_hl = NULL;

    if (saved_hl) {
        memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == '\x1b') {
        /* enter or esc */
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
        direction = -1;
    } else {
        /* reset last_match to -1 unless an arrow key was pressed */
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1) direction = 1;
    int current = last_match;
    int i;
    for (i = 0; i < E.numrows; i++) {
        current += direction;
        if (current == -1) current = E.numrows - 1;
        else if (current == E.numrows) current = 0;
        erow *row = &E.row[current];
        char *match = strstr(row->render, query);
        if (match) {
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render);
            E.rowoff = E.numrows;


            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            /*
             * void *memcpy(void *str1, const void *str2, size_t n)
             * copies n characters from memory area str2 to memory area str1.
             */
            memcpy(saved_hl, row->hl, row->rsize);

            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

/**
 * Search logic in editorFindCallback()
 */
void editorFind() {

    /* cursor position */
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char *query = editorPrompt(MESSAGE_SEARCH, editorFindCallback);

    if (query) {
        free(query);
    } else {
        /* end of search, return cursor to old pos. */
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}

/*** buffer ***/

void abAppend(struct abuf *ab, const char *s, int len) {
    char *newCharAb = realloc(ab->b, ab->len + len);
    if (newCharAb == NULL)
        return;
    memcpy(&newCharAb[ab->len], s, len);
    ab->b = newCharAb;
    ab->len += len;
}

void abFree(struct abuf *ab) {
    free(ab->b);
}

/*** output ***/

void editorScroll() {
    E.rx = 0;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
    }

    /* vertical */
    if (E.cy < E.rowoff) {
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        E.rowoff = E.cy - E.screenrows + 1;
    }

    /* horizontal */
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}

void editorDrawRows(struct abuf *ab) {
    int y;
    for (y = 0; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;
        if (filerow >= E.numrows) {
            if (E.numrows == 0 && y == E.screenrows / 3) {
                char welcome[80];
                int welcomelen = snprintf(welcome, sizeof(welcome),
                                          "Atease. version %s", EDITOR_VERSION);

                if (welcomelen > E.screencols)
                    welcomelen = E.screencols;

                int padding = (E.screencols - welcomelen) / 2;
                if (padding) {
                    abAppend(ab, "~", 1);
                    padding--;
                }
                while (padding--)
                    abAppend(ab, " ", 1);

                abAppend(ab, welcome, welcomelen);

            } else {
                abAppend(ab, "~", 1);
            }
        } else {
            /*
             * To display each row at the column offset,
             * we’ll use E.coloff as an index into the chars
             * of each erow we display, and subtract the
             * number of characters that are to the left of
             * the offset from the length of the row.
             */
            int len = E.row[filerow].rsize - E.coloff;
            if (len < 0) {
                /* nothing to display on this line. */
                len = 0;
            }
            if (len > E.screencols)
                len = E.screencols;

            /* highlighting numbers */
            /* see https://en.wikipedia.org/wiki/ANSI_escape_code#Colors */
            char *c = &E.row[filerow].render[E.coloff];
            unsigned char *hl = &E.row[filerow].hl[E.coloff];
            int current_color = -1;

            int j;
            for (j = 0; j < len; j++) {

                if (iscntrl(c[j])) {
                    /*
                     * Non printable character
                     * Translate it into a printable character by adding its value to '@'
                     * (in ASCII, the capital letters of the alphabet come after the @ character),
                     * or using the '?' character if it’s not in the alphabetic range.
                     */
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    abAppend(ab, "\x1b[7m", 4); /* inverted colors */
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);

                    if (current_color != -1) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }

                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color) {
                        current_color = color;
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
        }

        abAppend(ab, "\x1b[K", 3); /* clear rest of line; http://vt100.net/docs/vt100-ug/chapter3.html#EL */
        abAppend(ab, "\r\n", 2);
    }
}

void editorDrawStatusBar(struct abuf *ab) {
    /*
     * The m command (Select Graphic Rendition) causes the text printed after it to be printed with various possible
     * attributes including bold (1), underscore (4), blink (5), and inverted colors (7).
     * http://vt100.net/docs/vt100-ug/chapter3.html#SGR
     */
    abAppend(ab, "\x1b[7m", 4);

    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
                       E.filename ? E.filename : "[No Name]", E.numrows,
                       E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
                        E.syntax ? E.syntax->filetype : "no filetype", E.cy + 1, E.numrows);
    if (len > E.screencols) len = E.screencols;
    abAppend(ab, status, len);
    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        } else {
            abAppend(ab, " ", 1);
            len++;
        }
    }
    /* An argument of 0 (default) clears all attributes */
    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3);
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) msglen = E.screencols;
    if (msglen && time(NULL) - E.statusmsg_time < 5)
        abAppend(ab, E.statusmsg, msglen);
    if (EDITOR_DEBUG == 1)
		abAppend(ab, "\r\n", 2);
}

void editorDrawDebugBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3);
    int msglen = strlen(E.debugmsg);
    if (msglen > E.screencols) 
		msglen = E.screencols;
    abAppend(ab, E.debugmsg, msglen);
}

void editorRefreshScreen() {
    editorScroll();

    struct abuf ab = ABUF_INIT;

    /* \x1b is the escape character */
    abAppend(&ab, "\x1b[?25l", 6); /* hide cursor */
    abAppend(&ab, "\x1b[H", 3); /* reposition cursor */

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);
    editorDrawDebugBar(&ab);

    /* 
     * reposition cursor
     * changed the old H command into an H command with arguments, 
     * specifying the exact position we want the cursor to move to. 
     */
    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 1);
    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6); /* show cursor */

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

/* The ... argument makes a variadic function, meaning it can take any number of arguments. */
void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL); /* Passing NULL gets the current time */
}


/* The ... argument makes a variadic function, meaning it can take any number of arguments. */
void editorSetDebugMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.debugmsg, sizeof(E.debugmsg), fmt, ap);
    va_end(ap);
}
/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);
    size_t buflen = 0;
    buf[0] = '\0';
    while (1) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();
        int c = editorReadKey();

        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0) buf[--buflen] = '\0';
        } else if (c == '\x1b') {
            /* escape */
            editorSetStatusMessage("");
            if (callback)
                callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen != 0) {
                editorSetStatusMessage("");
                if (callback)
                    callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                buf = realloc(buf, bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }

        if (callback)
            callback(buf, c);
    }
}

void editorMoveCursor(int key) {
    erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];

    switch (key) {
        case ARROW_LEFT:
            if (E.cx != 0) {
                E.cx--;
            } else if (E.cy > 0) {
                /* negative number, move up a line */
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            break;
        case ARROW_RIGHT:
            if (row && E.cx < row->size) {
                E.cx++;
            } else if (row && E.cx == row->size) {
                /* go to next line */
                E.cy++;
                E.cx = 0;
            }
            break;
        case ARROW_UP:
            if (E.cy != 0) {
                E.cy--;
            }
            break;
        case ARROW_DOWN:
            if (E.cy < E.numrows) {
                E.cy++;
            }
            break;
    }

    /*  corrects E.cx if it ends up past the end of the line it’s on. */
    row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    int rowlen = row ? row->size : 0;
    if (E.cx > rowlen) {
        E.cx = rowlen;
    }
}

/*** react on keypresses ***/
#include "keypress.h"

/*** init ***/

void initEditor() {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.coloff = 0;
    E.rowoff = 0;
    E.numrows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.debugmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;

    if (getWindowSize(&E.screenrows, &E.screencols) == -1)
        die("getWindowSize");

    /* for status bar and message on the last 2 lines
     * if DEBUG is on, one more line 
     */
    int add_lines = 2;
    if (EDITOR_DEBUG == 1)
		add_lines = 3;
    E.screenrows -= add_lines;
}

int main(int argc, char *argv[]) {
    enableRawMode();
    initEditor();
    if (argc >= 2) {
        editorOpen(argv[1]);
    }

    editorSetStatusMessage(MESSAGE_START_UP_TEXT);

    while (1) {
        editorRefreshScreen();
        editorProcessKeypress();
    }
    return 0;
}
