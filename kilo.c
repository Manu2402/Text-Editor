#pragma region Includes

// Feature test macros to enable "getline()"" and other POSIX functions.
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <unistd.h>    // Standard POSIX APIs (read, write, etc.).
#include <termios.h>   // Terminal I/O interfaces for "raw mode".
#include <ctype.h>     // Character classification functions.
#include <fcntl.h>     // File control options for file operations.
#include <errno.h>     // Error handling definitions.
#include <stdlib.h>    // Standard library functions (malloc, free, etc.).
#include <stdarg.h>    // Variable argument lists for "printf-like functions".
#include <stdio.h>     // Standard I/O functions.
#include <string.h>    // String manipulation functions.
#include <sys/ioctl.h> // I/O control operations for terminal size.
#include <sys/types.h> // System data types (ssize_t).
#include <time.h>      // Time functions for status message timing.

#pragma endregion // Includes

// -----------------------------------------------------------------------------------------------------------------

#pragma region Defines

#define KILO_VERSION "0.0.1"   // Editor version number.
#define KILO_TAB_STOP 8        // Number of spaces per "tab" character.
#define KILO_QUIT_TIMES 3      // Number of Ctrl-Q presses required to quit with unsaved changes.

// Macro to convert regular key to "control key" combination.
// Example: 'C' = 67 (01000011); 'CTRL+C' = 3 (01000011 & 00011111 = 00000011).
#define CTRL_KEY(k) ((k) & 0x1F)

// Enumeration for special keys that don't have ASCII values.
enum editor_key
{
    BACKSPACE = 127,    // Backspace key code.

    // Arrow keys - start at 1000 to avoid conflicts with regular ASCII.
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,

    DEL_KEY,            // Delete key.

    HOME_KEY,           // Home key (beginning of line).
    END_KEY,            // End key (end of line).

    PAGE_UP,            // Page up key.
    PAGE_DOWN           // Page down key.
};

// Enumeration for syntax highlighting types.
enum editor_highlight
{
    HL_NORMAL = 0,      // Normal text (no highlighting).
    HL_COMMENT,         // Single-line comments.
    HL_MLCOMMENT,       // Multi-line comments.
    HL_KEYWORD1,        // Primary keywords (if, while, etc.).
    HL_KEYWORD2,        // Secondary keywords (data types).
    HL_STRING,          // String literals.
    HL_NUMBER,          // Numeric literals.
    HL_MATCH            // Search match highlighting.
};

// Bit flags for syntax highlighting features.
#define HL_HIGHLIGHT_NUMBERS (1 << 0) // Enable number highlighting (???????1).
#define HL_HIGHLIGHT_STRINGS (1 << 1) // Enable string highlighting (??????1?).

#pragma endregion // Defines

// -----------------------------------------------------------------------------------------------------------------

#pragma region Data

// Structure to define syntax highlighting rules for different file types.
struct editor_syntax
{
    char* file_type;                    // Name of the file type (e.g., "c").
    char** file_match;                  // Array of file extensions to match.
    char** keywords;                    // Array of keywords to highlight.
    char* single_line_comment_start;    // Single-line comment delimiter.
    char* multi_line_comment_start;     // Multi-line comment start delimiter.
    char* multi_line_comment_end;       // Multi-line comment end delimiter.
    int flags;                          // Feature flags (numbers, strings, etc.).
};

// Structure representing a row of text in the editor.
typedef struct erow
{
    int idx;                // Index of the row in the file.
    int size;               // Size of the actual text content.
    int r_size;             // Size of the rendered text (with tabs expanded).
    char* chars;            // Raw text content of the row.
    char* render;           // Rendered text with tabs converted to spaces.
    unsigned char* hl;      // Syntax highlighting information for each character.
    int hl_open_comment;    // Flag indicating if multi-line comment continues from this row.
} erow;

// Main editor configuration structure.
struct editor_config
{
    int cx, cy;         // Cursor position (x, y coordinates).
    int rx;             // Render "x" position (accounting for tabs).
    int row_off;        // Row offset for vertical scrolling.
    int col_off;        // Column offset for horizontal scrolling.
    int screen_rows;    // Number of rows in terminal.
    int screen_cols;    // Number of columns in terminal.

    int num_rows;       // Number of rows in the current file.
    erow* row;          // Array of text rows.

    int dirty;          // Flag indicating if file has unsaved changes.

    char* file_name;    // Name of the currently opened file.

    char status_msg[120];       // Status message buffer.
    time_t status_msg_time;     // Timestamp of last status message.

    struct editor_syntax* syntax;  // Current syntax highlighting rules.

    // Original terminal attributes (saved to restore on exit).
    struct termios orig_termios;
};

// Global editor state.
struct editor_config E;

// -----------------------------------------------------------------------------------------------------------------

#pragma region File Types

// Array of file extensions for C/C++ files.
char* C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };

// Array of C/C++ keywords for syntax highlighting.
// Keywords ending with '|' are treated as secondary keywords (types).
char* C_HL_keywords[] = 
{ 
    // Control flow and structure keywords.
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case",

    // Data type keywords (marked with '|' for different coloring).
    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
};

// Syntax highlighting database --> array of supported file types.
struct editor_syntax HLDB[] = 
{
    { 
        "c",                                                    // File type name.
        C_HL_extensions,                                        // Supported extensions.
        C_HL_keywords,                                          // Keywords array.
        "//",                                                   // Single-line comment start.
        "/*",                                                   // Multi-line comment start.
        "*/",                                                   // Multi-line comment end.
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS             // Enable number and string highlighting.
    },
};

#pragma endregion // File Types

// Calculate number of entries in syntax database.
#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

// -----------------------------------------------------------------------------------------------------------------

#pragma region Prototypes

// Function prototypes for functions used before their definitions.
void editor_set_status_message(const char* fmt, ...);
void editor_refresh_screen();
char* editor_prompt(char* prompt, void (*callback)(char*, int));

#pragma endregion // Prototypes

// -----------------------------------------------------------------------------------------------------------------

#pragma endregion // Data

// -----------------------------------------------------------------------------------------------------------------

#pragma region Terminal

/**
 * Error handling function --> clears screen and exits with error message.
 * @param s Error message to display.
 */
void die(const char* s)
{
    // Clear entire screen.
    write(STDOUT_FILENO, "\x1b[2J", 4);
    // Move cursor to top-left corner.
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(s); // Print error message with system error description.

    exit(1); // Exit with error code.
}

/**
 * Restore terminal to original state (canonical mode).
 */
void disable_raw_mode()
{
    // Restore original terminal attributes.
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    {
        die("tcsetattr");
    }
}

/**
 * Enable raw mode for direct key input handling.
 */
void enable_raw_mode() 
{
    // Save current terminal attributes for restoration.
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
    {
        die("tcgetattr");
    }

    // Register cleanup function to restore terminal on exit.
    atexit(disable_raw_mode);

    // Copy original attributes to modify.
    struct termios raw = E.orig_termios;

    // Disable various terminal flags:
    // • ECHO: Don't echo typed characters
    // • ICANON: Disable canonical (line-buffered) input
    // • ISIG: Disable Ctrl-C and Ctrl-Z signals
    // • IXON: Disable Ctrl-S and Ctrl-Q flow control
    // • IEXTEN: Disable Ctrl-V and Ctrl-O (on macOS)
    // • ICRNL: Don't convert carriage return to newline
    // • OPOST: Disable output processing (don't convert "\n" to "\r\n")
    // • BRKINT, INPCK, ISTRIP: Disable legacy features
    // • CS8: Set character size to 8 bits per byte

    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);           // Local flags.
    raw.c_iflag &= ~(ICRNL | IXON | BRKINT | INPCK | ISTRIP);  // Input flags.
    raw.c_oflag &= ~(OPOST);                                   // Output flags.
    raw.c_cflag |= (CS8);                                      // Character flags.
    
    // Set "read()"" to return immediately with any input.
    raw.c_cc[VMIN] = 0;   // Minimum bytes needed before "read()" returns.
    raw.c_cc[VTIME] = 1;  // Timeout in tenths of seconds (100ms).

    // Apply new terminal settings.
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
    {
        die("tcsetattr");
    }
}

/**
 * Read a key press and handle escape sequences for special keys.
 * @return Key code (ASCII for regular keys, enum values for special keys).
 */
int editor_read_key()
{
    int n_read;
    char c;
    
    // Read one byte at a time until we get input.
    while ((n_read = read(STDIN_FILENO, &c, 1)) != 1)
    {
        // Handle read errors (except EAGAIN which is normal).
        if (n_read == -1 && errno != EAGAIN)
        {
            die("read");
        }
    }

    // Handle escape sequences for special keys.
    if (c == '\x1b')
    {
        char seq[3];

        // Try to read the next two characters of the escape sequence.
        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

        // Handle sequences starting with '['.
        if (seq[0] == '[')
        {
            // Handle sequences like \x1b[1~ (numeric sequences).
            if (seq[1] >= '0' && seq[1] <= '9')
            {
                if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if (seq[2] == '~')
                {
                    // Map numeric escape sequences to key codes.
                    switch (seq[1])
                    {
                        case '1': return HOME_KEY;  // \x1b[1~
                        case '3': return DEL_KEY;   // \x1b[3~
                        case '4': return END_KEY;   // \x1b[4~
                        case '5': return PAGE_UP;   // \x1b[5~
                        case '6': return PAGE_DOWN; // \x1b[6~
                        case '7': return HOME_KEY;  // \x1b[7~
                        case '8': return END_KEY;   // \x1b[8~
                    }
                }
            }
            else
            {
                // Handle arrow keys and other single-character sequences.
                switch (seq[1])
                {
                    case 'A': return ARROW_UP;    // \x1b[A
                    case 'B': return ARROW_DOWN;  // \x1b[B
                    case 'C': return ARROW_RIGHT; // \x1b[C
                    case 'D': return ARROW_LEFT;  // \x1b[D
                    case 'H': return HOME_KEY;    // \x1b[H
                    case 'F': return END_KEY;     // \x1b[F
                }
            }
        }
        // Handle sequences starting with 'O' (alternative format).
        else if (seq[0] == 'O')
        {
            switch (seq[1])
            {
                case 'H': return HOME_KEY; // \x1bOH
                case 'F': return END_KEY;  // \x1bOF
            }
        }

        return '\x1b'; // Return escape if sequence not recognized.
    }

    return c; // Return regular character.
}

/**
 * Get current cursor position by querying the terminal.
 * @param rows Pointer to store row position.
 * @param cols Pointer to store column position.
 * @return 0 on success, -1 on failure.
 */
int get_cursor_position(int* rows, int* cols)
{
    char buf[32];
    unsigned int i = 0;

    // Send "Device Status Report" query to get cursor position.
    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4)
    {
        return -1;
    }

    // Read response until we get 'R' (end of response).
    while (i < sizeof(buf) - 1)
    {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }

    buf[i] = '\0';

    // Parse response format: "\x1b[rows;colsR"
    if (buf[0] != '\x1b' || buf[1] != '[') return -1;
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
    
    return 0;
}

/**
 * Get terminal window size using "ioctl", fallback to cursor positioning.
 * @param rows Pointer to store number of rows.
 * @param cols Pointer to store number of columns.
 * @return 0 on success, -1 on failure.
 */
int get_window_size(int* rows, int* cols)
{
    struct winsize ws;

    // Try to get window size using "ioctl" system call.
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
    {
        // Fallback: move cursor to bottom-right corner and query position.
        // \x1b[999C moves cursor 999 positions right.
        // \x1b[999B moves cursor 999 positions down.
        // Terminal automatically stops at screen edges.
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12)
        {
            return -1;
        }

        return get_cursor_position(rows, cols);
    }

    // Extract dimensions from "winsize" structure.
    *cols = ws.ws_col;
    *rows = ws.ws_row;

    return 0;
}

#pragma region AppendBuffer

/**
 * Dynamic string buffer for building output efficiently.
 */
struct abuf
{
    char* b;    // Pointer to buffer.
    int len;    // Current length.
};

// Initialize empty buffer.
#define ABUF_INIT { NULL, 0 }

/**
 * Append string to buffer, reallocating as needed.
 * @param ab Buffer to append to.
 * @param s String to append.
 * @param len Length of string to append.
 */
void ab_append(struct abuf* ab, const char* s, int len)
{
    // Reallocate buffer to accommodate new data.
    char *new = realloc(ab->b, ab->len + len);

    if (new == NULL) return;
    
    // Copy new data to the end of the buffer.
    memcpy(&new[ab->len], s, len);

    ab->b = new;
    ab->len += len;
}

/**
 * Free buffer memory.
 * @param ab Buffer to free.
 */
void ab_free(struct abuf* ab)
{
    free(ab->b);
}

#pragma endregion // AppendBuffer

#pragma endregion // Terminal

// -----------------------------------------------------------------------------------------------------------------

#pragma region Syntax Highlighting

/**
 * Check if character is a word separator for keyword detection.
 * @param c Character to check.
 * @return 1 if separator, 0 if not.
 */
int is_separator(int c)
{
    // A character is a separator if it's whitespace, null, or a punctuation mark.
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL; 
}

/**
 * Update syntax highlighting for a row of text.
 * @param row Row to update highlighting for.
 */
void editor_update_syntax(erow* row)
{
    // Allocate highlighting array (one byte per character).
    row->hl = realloc(row->hl, row->r_size);

    // Initialize to normal highlighting.
    memset(row->hl, HL_NORMAL, row->r_size); 

    // Skip if no syntax rules defined.
    if (E.syntax == NULL) return;

    // Get syntax configuration.
    char** keywords = E.syntax->keywords;
    char* scs = E.syntax->single_line_comment_start;  // Single-line comment start
    char* mcs = E.syntax->multi_line_comment_start;   // Multi-line comment start  
    char* mce = E.syntax->multi_line_comment_end;     // Multi-line comment end

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    int prev_sep = 1;       // Previous character was a separator.
    int in_string = 0;      // Currently inside a string (stores quote character).
    int in_comment = (row->idx > 0) && E.row[row->idx - 1].hl_open_comment; // Multi-line comment from previous row.

    int i = 0;
    while (i < row->r_size)
    {
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        // Handle single-line comments.
        if (scs_len && !in_string && !in_comment)
        {
            if (!strncmp(&row->render[i], scs, scs_len))
            {
                // Highlight rest of line as comment.
                memset(&row->hl[i], HL_COMMENT, row->r_size - i);
                break;
            }
        }

        // Handle multi-line comments.
        if (mcs_len && mce_len && !in_string)
        {
            if (in_comment)
            {
                row->hl[i] = HL_MLCOMMENT;

                // Check for comment end.
                if (!strncmp(&row->render[i], mce, mce_len))
                {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                }
                else
                {
                    i++;
                    continue;
                }
            }
            // Check for comment start.
            else if (!strncmp(&row->render[i], mcs, mcs_len))
            {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        // Handle string literals.
        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS)
        {
            if (in_string)
            {
                row->hl[i] = HL_STRING;

                // Handle escaped characters in strings.
                if (c == '\\' && (i + 1) < row->r_size)
                {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }

                // Check for string end (matching quote).
                if (c == in_string)
                {
                    in_string = 0;
                }

                i++;
                prev_sep = 1;
                continue;
            }

            // Check for string start (quote/double quotes character).
            if (c == '"' || c == '\'')
            {
                in_string = c;
                row->hl[i] = HL_STRING;
                i++;
                continue;
            }
        }

        // Handle number highlighting.
        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS)
        {
            // Highlight digits and decimal points in numbers.
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) || 
                (c == '.' && prev_hl == HL_NUMBER))
            {
                row->hl[i] = HL_NUMBER;
                prev_sep = 0;
                i++;
                continue;
            }
        }

        // Handle keyword highlighting.
        if (prev_sep)
        {
            int j;
            for (j = 0; keywords[j]; j++)
            {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|'; // Secondary keyword (type).
                if (kw2)
                {
                    klen--; // Don't include the '|' in comparison.
                }

                // Check if keyword matches and is followed by separator.
                if (!strncmp(&row->render[i], keywords[j], klen) && is_separator(row->render[i + klen]))
                {
                    // Highlight keyword with appropriate type.
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }

            if (keywords[j] != NULL)
            {
                prev_sep = 0;
                continue;
            }
        }   

        prev_sep = is_separator(c);
        i++;
    }

    // Check if multi-line comment state changed.
    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;

    // If state changed, update next row's highlighting.
    if (changed && ((row->idx + 1) < E.num_rows))
    {
        editor_update_syntax(&E.row[row->idx + 1]);
    }
}

/**
 * Convert highlight type to ANSI color code.
 * @param hl Highlight type.
 * @return ANSI color code.
 */
int editor_syntax_to_color(int hl)
{
    switch (hl)
    {
        case HL_COMMENT:
        case HL_MLCOMMENT: return 36; // Cyan
        case HL_KEYWORD1: return 33;  // Yellow
        case HL_KEYWORD2: return 32;  // Green
        case HL_NUMBER: return 31;    // Red
        case HL_STRING: return 35;    // Magenta
        case HL_MATCH: return 34;     // Blue (search matches)
        default: return 37;           // White (normal text)
    }
}

/**
 * Select appropriate syntax highlighting rules based on filename.
 */
void editor_select_syntax_highlighting()
{
    E.syntax = NULL;
    if (E.file_name == NULL) return;

    // Find file extension.
    char* ext = strrchr(E.file_name, '.');

    // Search through syntax database.
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++)
    {
        struct editor_syntax* s = &HLDB[j];
        
        unsigned int i = 0;
        while (s->file_match[i])
        {
            int is_ext = (s->file_match[i][0] == '.'); // Check if it's an extension.
            
            // Match by extension or filename substring.
            if ((is_ext && ext && !strcmp(ext, s->file_match[i])) || (!is_ext && strstr(E.file_name, s->file_match[i])))
            {
                E.syntax = s;

                // Update highlighting for all existing rows.
                for (int file_row = 0; file_row < E.num_rows; file_row++)
                {
                    editor_update_syntax(&E.row[file_row]);
                }

                return;
            }

            i++;
        }
    }
}

#pragma endregion // Syntax Highlighting

// -----------------------------------------------------------------------------------------------------------------

#pragma region Row Operations

/**
 * Convert cursor "x" position to render x position (accounting for tabs).
 * @param row Row to calculate for.
 * @param cx Cursor "x" position.
 * @return Render "x" position.
 */
int editor_row_cx_to_rx(erow* row, int cx)
{
    int rx = 0;

    // Calculate render position by expanding tabs.
    for (int j = 0; j < cx; j++)
    {
        if (row->chars[j] == '\t')
        {
            // Move to next tab stop.
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        }

        rx++;
    }

    return rx;
}

/**
 * Convert render "x" position back to cursor "x" position.
 * @param row Row to calculate for.
 * @param rx Render "x" position.
 * @return Cursor "x" position.
 */
int editor_row_rx_to_cx(erow* row, int rx)
{
    int cur_rx = 0;
    int cx;

    // Find cursor position that corresponds to render position.
    for (cx = 0; cx < row->size; cx++)
    {
        if (row->chars[cx] == '\t')
        {
            cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
        }

        cur_rx++;

        if (cur_rx > rx)
        {
            return cx;
        }
    }

    return cx;
}

/**
 * Update the render representation of a row (expand tabs to spaces).
 * @param row Row to update.
 */
void editor_update_row(erow* row)
{
    int tabs = 0;

    // Count tabs to calculate render buffer size.
    for (int j = 0; j < row->size; j++)
    {
        if (row->chars[j] == '\t') tabs++;
    }

    // Allocate render buffer (each tab becomes up to "KILO_TAB_STOP" spaces).
    free(row->render);
    row->render = malloc(row->size + tabs * (KILO_TAB_STOP - 1) + 1);

    int idx = 0;
    for (int j = 0; j < row->size; j++)
    {
        if (row->chars[j] == '\t')
        {
            // Convert tab to appropriate number of spaces.
            row->render[idx++] = ' ';
            while (idx % KILO_TAB_STOP != 0)
            {
                row->render[idx++] = ' ';
            }
        }
        else
        {
            row->render[idx++] = row->chars[j];
        }
    }

    row->render[idx] = '\0';
    row->r_size = idx;

    // Update syntax highlighting for the rendered text.
    editor_update_syntax(row);
}

/**
 * Insert a new row at the specified position.
 * @param at Position to insert at.
 * @param s String content for the row.
 * @param len Length of the string.
 */
void editor_insert_row(int at, char* s, size_t len)
{
    if (at < 0 || at > E.num_rows) return;

    // Reallocate row array to accommodate new row.
    E.row = realloc(E.row, sizeof(erow) * (E.num_rows + 1));
    
    // Move existing rows down to make space.
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.num_rows - at));

    // Update row indices for moved rows.
    for (int j = at + 1; j <= E.num_rows; j++)
    {
        E.row[j].idx++;
    }

    // Initialize new row.
    E.row[at].idx = at;
    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].r_size = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    
    editor_update_row(&E.row[at]);

    E.num_rows++;
    E.dirty++; // Mark file as modified.
}

/**
 * Free memory allocated for a row.
 * @param row Row to free.
 */
void editor_free_row(erow* row)
{
    free(row->render);
    free(row->chars);
    free(row->hl);
}

/**
 * Delete a row at the specified position.
 * @param at Position of row to delete.
 */
void editor_del_row(int at)
{
    if (at < 0 || at >= E.num_rows) return;

    // Free row memory.
    editor_free_row(&E.row[at]);

    // Move rows up to fill the gap.
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.num_rows - at - 1));

    // Update row indices for moved rows.
    for (int j = at; j < E.num_rows - 1; j++)
    {
        E.row[j].idx--;
    }

    E.num_rows--;
    E.dirty++; // Mark file as modified.
}

/**
 * Insert a character into a row at the specified position.
 * @param row Row to insert into.
 * @param at Position to insert at.
 * @param c Character to insert.
 */
void editor_row_insert_char(erow* row, int at, int c)
{
    // Clamp position to valid range.
    if (at < 0 || at > row->size) at = row->size;

    // Reallocate to accommodate new character.
    row->chars = realloc(row->chars, row->size + 2);
    
    // Move characters after insertion point to make room.
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;

    // Update rendered version and syntax highlighting.
    editor_update_row(row);

    E.dirty++; // Mark file as modified.
}

/**
 * Insert a newline at the current cursor position.
 */
void editor_insert_newline()
{
    if (E.cx == 0)
    {
        // Insert empty row at current position.
        editor_insert_row(E.cy, "", 0);
    }
    else
    {
        // Split current row at cursor position.
        erow* row = &E.row[E.cy];
        editor_insert_row(E.cy + 1, &row->chars[E.cx], row->size - E.cx);

        // Truncate current row at cursor position.
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';

        editor_update_row(row);
    }

    // Move cursor to beginning of next line.
    E.cy++;
    E.cx = 0;
}

/**
 * Append a string to the end of a row.
 * @param row Row to append to.
 * @param s String to append.
 * @param len Length of string to append.
 */
void editor_row_append_string(erow* row, char* s, size_t len)
{
    // Reallocate to accommodate additional text.
    row->chars = realloc(row->chars, row->size + len + 1);

    // Copy new text to end of row.
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';

    // Update rendered version and syntax highlighting.
    editor_update_row(row);

    E.dirty++; // Mark file as modified.
}

/**
 * Delete a character from a row at the specified position.
 * @param row Row to delete from.
 * @param at Position to delete at.
 */
void editor_row_del_char(erow* row, int at)
{
    if (at < 0 || at > row->size) return;
    
    // Move characters after deletion point to close the gap.
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;

    // Update rendered version and syntax highlighting.
    editor_update_row(row);

    E.dirty++; // Mark file as modified.
}

#pragma endregion // Row Operations

// -----------------------------------------------------------------------------------------------------------------

#pragma region Editor Operations

/**
 * Insert a character at the current cursor position.
 * @param c Character to insert.
 */
void editor_insert_char(int c)
{
    // If cursor is past the end of file, create a new row.
    if (E.cy == E.num_rows)
    {
        editor_insert_row(E.num_rows, "", 0);
    }

    // Insert character at cursor position.
    editor_row_insert_char(&E.row[E.cy], E.cx, c);
    E.cx++; // Move cursor forward.
}

/**
 * Delete character at or before the cursor position (backspace/delete).
 */
void editor_del_char()
{
    // Can't delete if at end of file.
    if (E.cy == E.num_rows) return;
    // Can't delete if at beginning of first line.
    if (E.cx == 0 && E.cy == 0) return;

    erow* row = &E.row[E.cy];

    if (E.cx > 0)
    {
        // Delete character before cursor (backspace).
        editor_row_del_char(row, E.cx - 1);
        E.cx--;
    }
    else
    {
        // Delete newline --> join current row with previous row.
        E.cx = E.row[E.cy - 1].size; // Move cursor to end of previous row.
        editor_row_append_string(&E.row[E.cy - 1], row->chars, row->size);
        editor_del_row(E.cy);
        E.cy--;
    }
}

#pragma endregion // Editor Operations

// -----------------------------------------------------------------------------------------------------------------

#pragma region File I/O

/**
 * Convert all rows to a single string for saving to disk.
 * @param buf_len Pointer to store total buffer length.
 * @return Allocated string containing all file content.
 */
char* editor_rows_to_string(int* buf_len)
{
    int tot_len = 0;
    
    // Calculate total length needed (including newlines).
    for (int j = 0; j < E.num_rows; j++)
    {
        tot_len += E.row[j].size + 1; // +1 for newline.
    }

    *buf_len = tot_len;

    char* buf = malloc(tot_len);
    char* p = buf;

    // Copy each row to buffer with newlines.
    for (int j = 0; j < E.num_rows; j++)
    {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }

    return buf;
}

/**
 * Open and load a file into the editor.
 * @param file_name Name of file to open.
 */
void editor_open(char* file_name)
{
    free(E.file_name);
    
    // Store a copy of the filename.
    E.file_name = strdup(file_name);

    // Set syntax highlighting based on file extension.
    editor_select_syntax_highlighting();

    FILE* fp = fopen(file_name, "r");
    if (!fp) die ("fopen");

    char* line = NULL;
    size_t line_cap = 0;
    ssize_t line_len;

    // Read file line by line using "getline()"" which handles memory allocation.
    while ((line_len = getline(&line, &line_cap, fp)) != -1)
    {
        // Remove trailing newline and carriage return characters.
        while (line_len > 0 && (line[line_len - 1] == '\n' || line[line_len - 1] == '\r'))
        {
            line_len--;
        }

        // Add row to editor.
        editor_insert_row(E.num_rows, line, line_len);
    }

    free(line);
    fclose(fp);

    E.dirty = 0; // File is clean after loading.
}

/**
 * Save the current file to disk.
 */
void editor_save()
{
    if (E.file_name == NULL)
    {
        // Prompt for filename if none exists.
        E.file_name = editor_prompt("Save as: %s (ESC to cancel)", NULL);
        if (E.file_name == NULL)
        {
            editor_set_status_message("Save aborted");
            return;
        }

        // Update syntax highlighting for new filename.
        editor_select_syntax_highlighting();
    }

    int len;
    char* buf = editor_rows_to_string(&len);

    // Open file for writing.
    // O_RDWR: Read/write mode.
    // O_CREAT: Create file if it doesn't exist.
    // 0644: Standard permissions (owner can read/write, others can only read).
    int fd = open(E.file_name, O_RDWR | O_CREAT, 0644);

    if (fd != -1)
    {
        // Set file size to match our content.
        if (ftruncate(fd, len) != -1)
        {
            // Write all content to file.
            if (write(fd, buf, len) == len)
            {
                close(fd);
                free(buf);

                E.dirty = 0; // File is now clean.

                editor_set_status_message("%d bytes written to disk", len);

                return;
            }
        }

        close(fd);
    }

    free(buf);

    // Show error message if save failed
    editor_set_status_message("Can't save! I/O error: %s", strerror(errno));
}

#pragma endregion // File I/O

// -----------------------------------------------------------------------------------------------------------------

#pragma region Find

/**
 * Callback function for incremental search.
 * @param query Current search query.
 * @param key Last key pressed.
 */
void editor_find_callback(char* query, int key)
{
    static int last_match = -1;    // Row of last search match.
    static int direction = 1;      // Search direction (1 = forward, -1 = backward).

    static int saved_hl_line;      // Row with saved highlighting.
    static char* saved_hl = NULL;  // Saved highlighting data.

    // Restore previously highlighted match.
    if (saved_hl)
    {
        memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].r_size);
        free(saved_hl);
        saved_hl = NULL;
    }

    // Handle special keys.
    if (key == '\r' || key == '\x1b')
    {
        // Enter or Escape --> end search.
        last_match = -1;
        direction = 1;
        return;
    }
    else if (key == ARROW_RIGHT || key == ARROW_DOWN)
    {
        // Search forward.
        direction = 1;
    }
    else if (key == ARROW_LEFT || key == ARROW_UP)
    {
        // Search backward.
        direction = -1;
    }
    else
    {
        // New search query --> reset state.
        last_match = -1;
        direction = 1;
    }

    // Start fresh search if no previous match.
    if (last_match == -1) direction = 1;
    int current = last_match;

    // Search through all rows.
    for (int i = 0; i < E.num_rows; i++)
    {
        current += direction;
        
        // Wrap around at file boundaries.
        if (current == -1) current = E.num_rows - 1;
        else if (current == E.num_rows) current = 0;

        erow* row = &E.row[current];
        
        // Search for query string in current row.
        char* match = strstr(row->render, query);
        if (match)
        {
            // Found match --> update editor state.
            last_match = current;
            E.cy = current;
            E.cx = editor_row_rx_to_cx(row, match - row->render);
            E.row_off = E.num_rows; // Scroll to match.

            // Save current highlighting and highlight match.
            saved_hl_line = current;
            saved_hl = malloc(row->r_size);
            memcpy(saved_hl, row->hl, row->r_size);

            // Highlight the matched text.
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

/**
 * Interactive search function.
 */
void editor_find()
{
    // Save current cursor position.
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_col_off = E.col_off;
    int saved_row_off = E.row_off;

    // Get search query from user with incremental search.
    char* query = editor_prompt("Search: %s (Use ESC/Arrows/Enter)", editor_find_callback);

    if (query)
    {
        free(query);
    }
    else
    {
        // Restore cursor position if search was cancelled.
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.col_off = saved_col_off;
        E.row_off = saved_row_off;
    }
}

#pragma endregion // Find

// -----------------------------------------------------------------------------------------------------------------

#pragma region Input

/**
 * Prompt user for input with optional callback for real-time processing.
 * @param prompt Format string for prompt message.
 * @param callback Function called for each keypress (can be NULL).
 * @return User input string, or NULL if cancelled.
 */
char* editor_prompt(char* prompt, void (*callback)(char*, int))
{
    size_t buf_size = 128;
    char* buf = malloc(buf_size);

    size_t buf_len = 0;
    buf[0] = '\0';

    while (1)
    {
        // Show prompt with current input.
        editor_set_status_message(prompt, buf);
        editor_refresh_screen();

        int c = editor_read_key();

        // Handle special keys.
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE)
        {
            // Backspace --> delete last character.
            if (buf_len != 0)
            {
                buf[--buf_len] = '\0';
            }
        }
        else if (c == '\x1b')
        {
            // Escape --> cancel input.
            editor_set_status_message("");
            if (callback) callback(buf, c);
            free(buf);
            return NULL;
        }
        else if (c == '\r')
        {
            // Enter --> confirm input.
            if (buf_len != 0)
            {
                editor_set_status_message("");
                if (callback) callback(buf, c);
                return buf;
            }
        }
        else if (!iscntrl(c) && c < 128)
        {
            // Regular character --> add to buffer.
            if (buf_len == buf_size - 1)
            {
                // Grow buffer if needed.
                buf_size *= 2;
                buf = realloc(buf, buf_size);
            }

            buf[buf_len++] = c;
            buf[buf_len] = '\0';
        }

        // Call callback for real-time processing (like incremental search).
        if (callback) callback(buf, c);
    }
}

/**
 * Move cursor based on arrow key input.
 * @param key Arrow key pressed.
 */
void editor_move_cursor(int key)
{
    // Get current row (NULL if cursor is past end of file).
    erow* row = (E.cy >= E.num_rows) ? NULL : &E.row[E.cy];

    switch (key)
    {
        case ARROW_LEFT:
            if (E.cx != 0)
            {
                // Move left within current line.
                E.cx--;
            }
            else if (E.cy > 0)
            {
                // Move to end of previous line.
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            break;
            
        case ARROW_RIGHT:
            if (row && E.cx < row->size) 
            {
                // Move right within current line.
                E.cx++; 
            }
            else if (row && E.cx == row->size)
            {
                // Move to beginning of next line.
                E.cy++;
                E.cx = 0;
            }
            break;
            
        case ARROW_UP:
            if (E.cy != 0) E.cy--; // Move up one line.
            break;
            
        case ARROW_DOWN:
            if (E.cy < E.num_rows) E.cy++; // Move down one line.
            break;
    }

    // Snap cursor to end of line if it's past the line length.
    row = (E.cy >= E.num_rows) ? NULL : &E.row[E.cy];
    int row_len = row ? row->size : 0;
    if (E.cx > row_len) E.cx = row_len;
}

/**
 * Process a keypress and perform the corresponding action.
 */
void editor_process_keypress()
{
    static int quit_times = KILO_QUIT_TIMES; // Counter for quit confirmation.

    int c = editor_read_key();

    switch (c)
    {
        case '\r': // Enter key.
            editor_insert_newline();
            break;

        case CTRL_KEY('q'): // Quit.
            if (E.dirty && quit_times > 0)
            {
                // Warn about unsaved changes.
                editor_set_status_message("WARNING!!! File has unsaved changes. Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }

            // Clear screen and exit.
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;

        case CTRL_KEY('s'): // Save.
            editor_save();
            break;

        case HOME_KEY: // Home --> go to beginning of line.
            E.cx = 0;
            break;

        case END_KEY: // End --> go to end of line.
            if (E.cy < E.num_rows)
            {
                E.cx = E.row[E.cy].size;
            }
            break;

        case CTRL_KEY('f'): // Find.
            editor_find();
            break;

        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if (c == DEL_KEY)
            {
                // Delete key --> move cursor right then delete.
                editor_move_cursor(ARROW_RIGHT);
            }
            editor_del_char();
            break;

        case PAGE_UP:
        case PAGE_DOWN:
        {
            // Page up/down --> move cursor and scroll by screen height.
            if (c == PAGE_UP)
            {
                E.cy = E.row_off;
            }
            else if (c == PAGE_DOWN)
            {
                E.cy = E.row_off + E.screen_rows - 1;
                if (E.cy > E.num_rows)
                {
                    E.cy = E.num_rows;
                }
            }

            // Simulate arrow key presses for smooth scrolling.
            int times = E.screen_rows;
            while (times--)
            {
                editor_move_cursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }

            break;
        }

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editor_move_cursor(c);
            break;
            
        case CTRL_KEY('l'): // Refresh screen (traditional).
        case '\x1b':        // Escape key (do nothing).
            break;

        default:
            // Insert regular character.
            editor_insert_char(c);
            break;
    }

    // Reset quit counter after any other action.
    quit_times = KILO_QUIT_TIMES;
}

#pragma endregion // Input

// -----------------------------------------------------------------------------------------------------------------

#pragma region Output

/**
 * Handle scrolling when cursor moves outside visible area.
 */
void editor_scroll()
{
    // Calculate render "x" position (accounting for tabs).
    E.rx = 0;
    if (E.cy < E.num_rows)
    {
        E.rx = editor_row_cx_to_rx(&E.row[E.cy], E.cx);
    }

    // Vertical scrolling.
    if (E.cy < E.row_off)
    {
        E.row_off = E.cy; // Scroll up.
    }
    if (E.cy >= E.row_off + E.screen_rows)
    {
        E.row_off = E.cy - E.screen_rows + 1; // Scroll down.
    }

    // Horizontal scrolling.
    if (E.rx < E.col_off)
    {
        E.col_off = E.rx; // Scroll left.
    }
    if (E.rx >= E.col_off + E.screen_cols)
    {
        E.col_off = E.rx - E.screen_cols + 1; // Scroll right.
    }
}

/**
 * Draw text rows to the append buffer.
 * @param ab Append buffer to draw to.
 */
void editor_draw_rows(struct abuf* ab)
{
    // Draw each row of the screen.
    for (int y = 0; y < E.screen_rows; y++)
    {
        int file_row = y + E.row_off; // Actual file row (accounting for scrolling).
        
        if (file_row >= E.num_rows)
        {
            // Past end of file --> show tilde or welcome message.
            if (E.num_rows == 0 && y == E.screen_rows / 3)
            {
                // Show welcome message in center of empty screen.
                char welcome[120];
                int welcome_len = snprintf(welcome, sizeof(welcome), "Kilo editor -- version %s", KILO_VERSION);

                if (welcome_len > E.screen_cols)
                {
                    welcome_len = E.screen_cols;
                }

                // Center the welcome message.
                int padding = (E.screen_cols - welcome_len) / 2;
                if (padding)
                {
                    ab_append(ab, "~", 1);
                    padding--;
                }

                while (padding--)
                {
                    ab_append(ab, " ", 1);
                }

                ab_append(ab, welcome, welcome_len);
            }
            else
            {
                // Show tilde for empty lines.
                ab_append(ab, "~", 1);
            }
        }
        else
        {
            // Draw actual file content with syntax highlighting.
            int len = E.row[file_row].r_size - E.col_off;

            if (len < 0) len = 0; // Nothing visible if scrolled past line end.
            if (len > E.screen_cols) len = E.screen_cols; // Truncate if too long.

            char* c = &E.row[file_row].render[E.col_off];
            unsigned char* hl = &E.row[file_row].hl[E.col_off];
            int current_color = -1;

            // Render each character with appropriate coloring.
            for (int j = 0; j < len; j++)
            {
                if (iscntrl(c[j]))
                {
                    // Display control characters as visible symbols.
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    ab_append(ab, "\x1b[7m", 4); // Invert colors.
                    ab_append(ab, &sym, 1);
                    ab_append(ab, "\x1b[m", 3);  // Reset formatting.

                    // Restore color if we had one.
                    if (current_color != -1)
                    {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        ab_append(ab, buf, clen);
                    }
                }
                else if (hl[j] == HL_NORMAL)
                {
                    // Normal text --> reset color if needed.
                    if (current_color != -1)
                    {
                        ab_append(ab, "\x1b[39m", 5); // Reset to default color.
                        current_color = -1;
                    }
                    ab_append(ab, &c[j], 1);
                }
                else
                {
                    // Syntax highlighted text --> set color if different.
                    int color = editor_syntax_to_color(hl[j]);
                    if (color != current_color)
                    {
                        current_color = color;
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        ab_append(ab, buf, clen);
                    }
                    ab_append(ab, &c[j], 1);
                }
            }

            // Reset color at end of line.
            ab_append(ab, "\x1b[39m", 5);
        }     

        // Clear rest of line and add newline.
        ab_append(ab, "\x1b[K", 3); // Erase to end of line.
        ab_append(ab, "\r\n", 2);
    }
}

/**
 * Draw status bar at bottom of screen.
 * @param ab Append buffer to draw to.
 */
void editor_draw_status_bar(struct abuf* ab)
{
    // Invert colors for status bar.
    ab_append(ab, "\x1b[7m", 4); 

    // Left side of status bar --> filename, line count, modification status.
    char status[120], rstatus[120];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s", 
                       E.file_name ? E.file_name : "[No Name]", 
                       E.num_rows, 
                       E.dirty ? "(modified)" : "");
    
    // Right side of status bar --> file type and cursor position.
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d", 
                        E.syntax ? E.syntax->file_type : "no ft", 
                        E.cy + 1, 
                        E.num_rows);

    if (len > E.screen_cols)
    {
        len = E.screen_cols;
    }

    ab_append(ab, status, len);
    
    // Fill space between left and right status with spaces.
    while (len < E.screen_cols)
    {
        if (E.screen_cols - len == rlen)
        {
            ab_append(ab, rstatus, rlen);
            break;
        }

        ab_append(ab, " ", 1);
        len++;
    }

    // Reset colors.
    ab_append(ab, "\x1b[m", 4);
    ab_append(ab, "\r\n", 2);
}

/**
 * Draw message bar below status bar.
 * @param ab Append buffer to draw to.
 */
void editor_draw_message_bar(struct abuf* ab)
{
    // Clear the message bar.
    ab_append(ab, "\x1b[K", 3);

    int msg_len = strlen(E.status_msg);
    if (msg_len > E.screen_cols)
    {
        msg_len = E.screen_cols;
    }

    // Show message if it's recent (within 5 seconds).
    if (msg_len && (time(NULL) - E.status_msg_time) < 5)
    {
        ab_append(ab, E.status_msg, msg_len);
    }
}

/**
 * Refresh the entire screen.
 */
void editor_refresh_screen()
{
    editor_scroll(); // Handle scrolling first.

    struct abuf ab = ABUF_INIT;

    // Hide cursor during refresh to prevent flickering.
    ab_append(&ab, "\x1b[?25l", 6);
    
    // Move cursor to top-left corner.
    ab_append(&ab, "\x1b[H", 3);

    // Draw all screen components.
    editor_draw_rows(&ab);
    editor_draw_status_bar(&ab);
    editor_draw_message_bar(&ab);

    // Position cursor at current location.
    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.row_off) + 1 /* Screen row (1-based) */, (E.rx - E.col_off) + 1) /* Screen column (1-based) */ ; 
    ab_append(&ab, buf, strlen(buf));

    // Show cursor.
    ab_append(&ab, "\x1b[?25h", 6);

    // Write everything to terminal at once.
    write(STDOUT_FILENO, ab.b, ab.len);
    ab_free(&ab);
}

/**
 * Set status message with printf-style formatting.
 * @param fmt Format string.
 * @param ... Variable arguments.
 */
void editor_set_status_message(const char* fmt, ... /* Variadic args */)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.status_msg, sizeof(E.status_msg), fmt, ap);
    va_end(ap);
    E.status_msg_time = time(NULL); // Record when message was set.
}

#pragma endregion // Output

// -----------------------------------------------------------------------------------------------------------------

#pragma region Init

/**
 * Initialize editor state.
 */
void init_editor()
{
    // Initialize cursor position.
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    
    // Initialize scrolling offsets.
    E.row_off = 0;
    E.col_off = 0;

    // Initialize file state.
    E.num_rows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.file_name = NULL;

    // Initialize status message.
    E.status_msg[0] = '\0';
    E.status_msg_time = 0;

    // No syntax highlighting initially.
    E.syntax = NULL;

    // Get terminal size.
    if (get_window_size(&E.screen_rows, &E.screen_cols) == -1)
    {
        die("get_window_size");
    }

    // Reserve space for status bar and message bar.
    E.screen_rows -= 2;
}

#pragma endregion // Init

// -----------------------------------------------------------------------------------------------------------------

/**
 * Main function --> entry point of the program.
 * @param argc Number of command line arguments.
 * @param argv Array of command line arguments.
 * @return Exit code.
 */
int main(int argc, char* argv[])
{
    // Initialize editor and terminal.
    enable_raw_mode();
    init_editor();

    // Open file if provided as command line argument.
    if (argc >= 2)
    {
        editor_open(argv[1]);
    }

    // Show help message.
    editor_set_status_message("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    // Main editor loop --> continuously refresh screen and process input.
    while (1)
    {
        editor_refresh_screen();
        editor_process_keypress();
    }

    return 0;
}