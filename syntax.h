#ifndef TEXT_EDITOR_SYNTAX_H
#define TEXT_EDITOR_SYNTAX_H

enum editorHighlight {
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH
};

enum colors {
    COLOR_BLACK = 37,
    COLOR_RED = 31,
    COLOR_GREEN = 32,
    COLOR_YELLOW = 33,
    COLOR_BLUE = 34,
    COLOR_MAGENTA = 35,
    COLOR_CYAN = 36
};


int is_separator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) return;

    char **keywords = E.syntax->keywords;

    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;


    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row->rsize) {
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        /* single line comment */
        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }

        /* multi line comment */
        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        /* strings */
        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;

                if (c == '\\' && i + 1 < row->rsize) {
                    /* If we’re in a string and the current character is a backslash (\), and there’s at least
                     * one more character in that line that comes after the backslash, then we highlight the
                     * character that comes after the backslash with HL_STRING and consume it.
                     * We increment i by 2 to consume both characters at once.
                     */
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }

                if (c == in_string)
                    in_string = 0;

                i++;
                prev_sep = 1;
                continue;
            } else {
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        /* numbers */
        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }

        /* keywords */
        if (prev_sep) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;
                if (!strncmp(&row->render[i], keywords[j], klen) &&
                    is_separator(row->render[i + klen])) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }

    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.numrows)
        editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColor(int hl) {
    switch (hl) {
        case HL_COMMENT:
        case HL_MLCOMMENT:
            return COLOR_CYAN;
        case HL_NUMBER:
            return COLOR_RED;
        case HL_MATCH:
            return COLOR_BLUE;
        case HL_STRING:
            return COLOR_MAGENTA;
        case HL_KEYWORD1:
            return COLOR_YELLOW;
        case HL_KEYWORD2:
            return COLOR_GREEN;
        default:
            return COLOR_BLACK;
    }
}

void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) return;
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            char *p = strstr(E.filename, s->filematch[i]);
            if (p != NULL) {
                int patlen = strlen(s->filematch[i]);
                if (s->filematch[i][0] != '.' || p[patlen] == '\0') {
                    E.syntax = s;

                    int filerow;
                    for (filerow = 0; filerow < E.numrows; filerow++) {
                        editorUpdateSyntax(&E.row[filerow]);
                    }
                    return;
                }
            }
            i++;
        }
    }
}

#endif //TEXT_EDITOR_SYNTAX_H
