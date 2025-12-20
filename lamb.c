// ,---@>
//  W-W'
// cc -o lamb lamb.c
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>

#ifdef _WIN32
#    define WIN32_LEAN_AND_MEAN
#    define _WINUSER_
#    define _WINGDI_
#    define _IMM_
#    define _WINCON_
#    include <windows.h>
#else
#    include <unistd.h>
#    include <sys/wait.h>
#endif // _WIN32

#if defined(__GNUC__) || defined(__clang__)
//   https://gcc.gnu.org/onlinedocs/gcc-4.7.2/gcc/Function-Attributes.html
#    ifdef __MINGW_PRINTF_FORMAT
#        define PRINTF_FORMAT(STRING_INDEX, FIRST_TO_CHECK) __attribute__ ((format (__MINGW_PRINTF_FORMAT, STRING_INDEX, FIRST_TO_CHECK)))
#    else
#        define PRINTF_FORMAT(STRING_INDEX, FIRST_TO_CHECK) __attribute__ ((format (printf, STRING_INDEX, FIRST_TO_CHECK)))
#    endif // __MINGW_PRINTF_FORMAT
#else
//   TODO: implement PRINTF_FORMAT for MSVC
#    define PRINTF_FORMAT(STRING_INDEX, FIRST_TO_CHECK)
#endif

#define UNUSED(value) (void)(value)
#define TODO(message) do { fprintf(stderr, "%s:%d: TODO: %s\n", __FILE__, __LINE__, message); abort(); } while(0)
#define UNREACHABLE(message) do { fprintf(stderr, "%s:%d: UNREACHABLE: %s\n", __FILE__, __LINE__, message); abort(); } while(0)

#define DA_INIT_CAP 256
#define da_reserve(da, expected_capacity)                                                  \
    do {                                                                                   \
        if ((expected_capacity) > (da)->capacity) {                                        \
            if ((da)->capacity == 0) {                                                     \
                (da)->capacity = DA_INIT_CAP;                                              \
            }                                                                              \
            while ((expected_capacity) > (da)->capacity) {                                 \
                (da)->capacity *= 2;                                                       \
            }                                                                              \
            (da)->items = realloc((da)->items, (da)->capacity * sizeof(*(da)->items));     \
            assert((da)->items != NULL && "Buy more RAM lol");                             \
        }                                                                                  \
    } while (0)

#define da_append(da, item)                  \
    do {                                     \
        da_reserve((da), (da)->count + 1);   \
        (da)->items[(da)->count++] = (item); \
    } while (0)

#define da_delete_at(da, i) \
    do { \
       size_t index = (i); \
       assert(index < (da)->count); \
       memmove(&(da)->items[index], &(da)->items[index + 1], ((da)->count - index - 1)*sizeof(*(da)->items)); \
       (da)->count -= 1; \
    } while(0)

#define sb_append_null(sb) da_append(sb, 0)

typedef struct {
    const char **items;
    size_t count;
    size_t capacity;
} Cmd;

bool cmd_run(Cmd *cmd)
{
    if (cmd->count < 1) {
        fprintf(stderr, "ERROR: Could not run empty command");
        return false;
    }

#ifdef _WIN32
    TODO("cmd_run is not implemented for windows");
#else
    pid_t cpid = fork();
    if (cpid < 0) {
        fprintf(stderr, "ERROR: Could not fork child process: %s", strerror(errno));
        return false;
    }

    if (cpid == 0) {
        // NOTE: This leaks a bit of memory in the child process.
        // But do we actually care? It's a one off leak anyway...
        da_append(cmd, NULL);

        if (execvp(cmd->items[0], (char * const*) cmd->items) < 0) {
            fprintf(stderr, "ERROR: Could not exec child process for %s: %s", cmd->items[0], strerror(errno));
            exit(1);
        }
        UNREACHABLE("cmd_run");
    }

    for (;;) {
        int wstatus = 0;
        if (waitpid(cpid, &wstatus, 0) < 0) {
            fprintf(stderr, "ERROR: Could not wait on command (pid %d): %s", cpid, strerror(errno));
            return false;
        }

        if (WIFEXITED(wstatus)) {
            int exit_status = WEXITSTATUS(wstatus);
            if (exit_status != 0) {
                fprintf(stderr, "ERROR: Command exited with exit code %d", exit_status);
                return false;
            }

            break;
        }

        if (WIFSIGNALED(wstatus)) {
            fprintf(stderr, "ERROR: Command process was terminated by signal %d", WTERMSIG(wstatus));
            return false;
        }
    }

    return cpid;
#endif
}

char *copy_string(const char *s)
{
    int n = strlen(s);
    char *ds = malloc(n + 1);
    assert(ds);
    memcpy(ds, s, n);
    ds[n] = '\0';
    return ds;
}

typedef struct {
    char *items;
    size_t count;
    size_t capacity;
} String_Builder;

int sb_appendf(String_Builder *sb, const char *fmt, ...) PRINTF_FORMAT(2, 3);
int sb_appendf(String_Builder *sb, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    int n = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    // NOTE: the new_capacity needs to be +1 because of the null terminator.
    // However, further below we increase sb->count by n, not n + 1.
    // This is because we don't want the sb to include the null terminator. The user can always sb_append_null() if they want it
    da_reserve(sb, sb->count + n + 1);
    char *dest = sb->items + sb->count;
    va_start(args, fmt);
    vsnprintf(dest, n+1, fmt, args);
    va_end(args);

    sb->count += n;

    return n;
}

bool read_entire_file(const char *path, String_Builder *sb)
{
    FILE *f = fopen(path, "rb");
    size_t new_count = 0;
    long long m = 0;
    if (f == NULL)                 goto fail;
    if (fseek(f, 0, SEEK_END) < 0) goto fail;
#ifndef _WIN32
    m = ftell(f);
#else
    m = _ftelli64(f);
#endif
    if (m < 0)                     goto fail;
    if (fseek(f, 0, SEEK_SET) < 0) goto fail;

    new_count = sb->count + m;
    if (new_count > sb->capacity) {
        sb->items = realloc(sb->items, new_count);
        assert(sb->items != NULL && "Buy more RAM lool!!");
        sb->capacity = new_count;
    }

    fread(sb->items + sb->count, m, 1, f);
    if (ferror(f)) {
        // TODO: Afaik, ferror does not set errno. So the error reporting in fail is not correct in this case.
        goto fail;
    }
    sb->count = new_count;

    fclose(f);
    return true;
fail:
    fprintf(stderr, "ERROR: Could not read file %s: %s\n", path, strerror(errno));
    if (f) fclose(f);
    return false;
}

bool write_entire_file(const char *path, const void *data, size_t size)
{
    const char *buf = NULL;
    FILE *f = fopen(path, "wb");
    if (f == NULL) {
        fprintf(stderr, "ERROR: Could not open file %s for writing: %s\n", path, strerror(errno));
        goto fail;
    }

    //           len
    //           v
    // aaaaaaaaaa
    //     ^
    //     data

    buf = (const char*)data;
    while (size > 0) {
        size_t n = fwrite(buf, 1, size, f);
        if (ferror(f)) {
            fprintf(stderr, "ERROR: Could not write into file %s: %s\n", path, strerror(errno));
            goto fail;
        }
        size -= n;
        buf  += n;
    }

    fclose(f);
    return true;
fail:
    if (f) fclose(f);
    return false;
}

struct {
    const char **items;
    size_t count;
    size_t capacity;
} labels = {0};

const char *intern_label(const char *label)
{
    for (size_t i = 0; i < labels.count; ++i) {
        if (strcmp(labels.items[i], label) == 0) {
            return labels.items[i];
        }
    }
    char *result = copy_string(label);
    da_append(&labels, result);
    return result;
}

typedef struct {
    // Displayed name of the symbol.
    const char *label;
    // Internal tag that makes two symbols with the same label different if needed.
    // Usually used to obtain a fresh symbol for capture avoiding substitution.
    size_t tag;
} Symbol;

bool symbol_eq(Symbol a, Symbol b)
{
    // NOTE: We compare addresses of the labels because they are expected to be interned with intern_label()
    return a.label == b.label && a.tag == b.tag;
}

Symbol symbol(const char *label)
{
    Symbol s = { .label = intern_label(label) };
    return s;
}

Symbol symbol_fresh(Symbol s)
{
    static size_t global_counter = 0;
    s.tag = ++global_counter;
    return s;
}

typedef enum {
    EXPR_VAR,
    EXPR_FUN,
    EXPR_APP,
    EXPR_MAG,
} Expr_Kind;

typedef struct {
    size_t unwrap;
} Expr_Index;

typedef struct {
    Expr_Kind kind;
    bool visited;
    bool live;
    union {
        Symbol var;
        const char *mag;
        struct {
            Symbol param;
            Expr_Index body;
        } fun;
        struct {
            Expr_Index lhs;
            Expr_Index rhs;
        } app;
    } as;
} Expr;

struct {
    Expr *items;
    size_t count;
    size_t capacity;
} expr_pool = {0};

#define expr_slot(index) (                             \
    expr_pool.items[                                   \
        (assert((index).unwrap < expr_pool.count),     \
         assert(expr_pool.items[(index).unwrap].live), \
         (index).unwrap)])

#define expr_slot_unsafe(index) expr_pool.items[(index).unwrap]

struct {
    Expr_Index *items;
    size_t count;
    size_t capacity;
} expr_dead_pool = {0};

Expr_Index alloc_expr(void)
{
    Expr_Index result;
    if (expr_dead_pool.count > 0) {
        result = expr_dead_pool.items[--expr_dead_pool.count];
    } else {
        result.unwrap = expr_pool.count;
        Expr expr = {0};
        da_append(&expr_pool, expr);
    }
    assert(!expr_slot_unsafe(result).live);
    expr_slot_unsafe(result).live = true;
    return result;
}

void free_expr(Expr_Index expr)
{
    expr_slot(expr).live = false;
    da_append(&expr_dead_pool, expr);
}

Expr_Index var(Symbol name)
{
    Expr_Index expr = alloc_expr();
    expr_slot(expr).kind = EXPR_VAR;
    expr_slot(expr).as.var = name;
    return expr;
}

Expr_Index magic(const char *label)
{
    Expr_Index expr = alloc_expr();
    expr_slot(expr).kind = EXPR_MAG;
    expr_slot(expr).as.mag = intern_label(label);
    return expr;
}

Expr_Index fun(Symbol param, Expr_Index body)
{
    Expr_Index expr = alloc_expr();
    expr_slot(expr).kind = EXPR_FUN;
    expr_slot(expr).as.fun.param = param;
    expr_slot(expr).as.fun.body = body;
    return expr;
}

Expr_Index app(Expr_Index lhs, Expr_Index rhs)
{
    Expr_Index expr = alloc_expr();
    expr_slot(expr).kind = EXPR_APP;
    expr_slot(expr).as.app.lhs = lhs;
    expr_slot(expr).as.app.rhs = rhs;
    return expr;
}

void expr_display(Expr_Index expr, String_Builder *sb)
{
    switch (expr_slot(expr).kind) {
    case EXPR_VAR:
        sb_appendf(sb, "%s", expr_slot(expr).as.var.label);
        if (expr_slot(expr).as.var.tag) {
            sb_appendf(sb, "@%zu", expr_slot(expr).as.var.tag);
        }
        break;
    case EXPR_FUN:
        if (expr_slot(expr).as.fun.param.tag) {
            sb_appendf(sb, "\\%s@%zu.", expr_slot(expr).as.fun.param.label, expr_slot(expr).as.fun.param.tag);
        } else {
            sb_appendf(sb, "\\%s.", expr_slot(expr).as.fun.param.label);
        }
        expr_display(expr_slot(expr).as.fun.body, sb);
        break;
    case EXPR_APP: {
        Expr_Index lhs = expr_slot(expr).as.app.lhs;
        if (expr_slot(lhs).kind != EXPR_VAR) sb_appendf(sb, "(");
        expr_display(lhs, sb);
        if (expr_slot(lhs).kind != EXPR_VAR) sb_appendf(sb, ")");

        sb_appendf(sb, " ");

        Expr_Index rhs = expr_slot(expr).as.app.rhs;
        if (expr_slot(rhs).kind != EXPR_VAR) sb_appendf(sb, "(");
        expr_display(rhs, sb);
        if (expr_slot(rhs).kind != EXPR_VAR) sb_appendf(sb, ")");
    } break;
    case EXPR_MAG: {
        sb_appendf(sb, "#%s", expr_slot(expr).as.mag);
    } break;
    default: UNREACHABLE("Expr_Kind");
    }
}

void trace_expr(Expr_Index expr)
{
    static String_Builder sb = {0};
    sb.count = 0;
    expr_display(expr, &sb);
    sb_append_null(&sb);
    printf("%s", sb.items);
}

bool is_var_free_there(Symbol name, Expr_Index there)
{
    switch (expr_slot(there).kind) {
    case EXPR_VAR:
        return symbol_eq(expr_slot(there).as.var, name);
    case EXPR_FUN:
        if (symbol_eq(expr_slot(there).as.fun.param, name)) return false;
        return is_var_free_there(name, expr_slot(there).as.fun.body);
    case EXPR_APP:
        if (is_var_free_there(name, expr_slot(there).as.app.lhs)) return true;
        if (is_var_free_there(name, expr_slot(there).as.app.rhs)) return true;
        return false;
    case EXPR_MAG:
        return false;
    default: UNREACHABLE("Expr_Kind");
    }
}

Expr_Index replace(Symbol param, Expr_Index body, Expr_Index arg)
{
    switch (expr_slot(body).kind) {
    case EXPR_MAG:
        return body;
    case EXPR_VAR:
        if (symbol_eq(expr_slot(body).as.var, param)) {
            return arg;
        } else {
            return body;
        }
    case EXPR_FUN:
        if (symbol_eq(expr_slot(body).as.fun.param, param)) return body;
        if (!is_var_free_there(expr_slot(body).as.fun.param, arg)) {
            return fun(expr_slot(body).as.fun.param, replace(param, expr_slot(body).as.fun.body, arg));
        }
        Symbol fresh_param_name = symbol_fresh(expr_slot(body).as.fun.param);
        Expr_Index fresh_param = var(fresh_param_name);
        return fun(
            fresh_param_name,
            replace(param,
                replace(
                    expr_slot(body).as.fun.param,
                    expr_slot(body).as.fun.body,
                    fresh_param),
                arg));
    case EXPR_APP:
        return app(
            replace(param, expr_slot(body).as.app.lhs, arg),
            replace(param, expr_slot(body).as.app.rhs, arg));
    default: UNREACHABLE("Expr_Kind");
    }
}

bool eval1(Expr_Index expr, Expr_Index *expr1)
{
    switch (expr_slot(expr).kind) {
    case EXPR_VAR:
        *expr1 = expr;
        return true;
    case EXPR_FUN: {
        Expr_Index body;
        if (!eval1(expr_slot(expr).as.fun.body, &body)) return false;
        if (body.unwrap != expr_slot(expr).as.fun.body.unwrap) {
            *expr1 = fun(expr_slot(expr).as.fun.param, body);
        } else {
            *expr1 = expr;
        }
        return true;
    }
    case EXPR_APP: {
        Expr_Index lhs = expr_slot(expr).as.app.lhs;
        Expr_Index rhs = expr_slot(expr).as.app.rhs;

        if (expr_slot(lhs).kind == EXPR_FUN) {
            *expr1 = replace(
                expr_slot(lhs).as.fun.param,
                expr_slot(lhs).as.fun.body,
                rhs);
            return true;
        } else if (expr_slot(lhs).kind == EXPR_MAG) {
            if (expr_slot(lhs).as.mag == intern_label("trace")) {
                Expr_Index new_rhs;
                if (!eval1(rhs, &new_rhs)) return false;
                if (new_rhs.unwrap == rhs.unwrap) {
                    printf("TRACE: ");
                    trace_expr(rhs);
                    printf("\n");
                    *expr1 = rhs;
                } else {
                    *expr1 = app(lhs, new_rhs);
                }
                return true;
            } else {
                printf("ERROR: unknown magic #%s\n", expr_slot(lhs).as.mag);
                return false;
            }
        }

        Expr_Index new_lhs;
        if (!eval1(lhs, &new_lhs)) return false;
        if (lhs.unwrap != new_lhs.unwrap) {
            *expr1 = app(new_lhs, rhs);
            return true;
        }

        Expr_Index new_rhs;
        if (!eval1(rhs, &new_rhs)) return false;
        if (rhs.unwrap != new_rhs.unwrap) {
            *expr1 = app(lhs, new_rhs);
            return true;
        }

        *expr1 = expr;
        return true;
    }
    case EXPR_MAG:
        *expr1 = expr;
        return true;
    default: UNREACHABLE("Expr_Kind");
    }
}

typedef enum {
    TOKEN_INVALID,
    TOKEN_END,
    TOKEN_OPAREN,
    TOKEN_CPAREN,
    TOKEN_LAMBDA,
    TOKEN_DOT,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_EQUALS,
    TOKEN_NAME,
    TOKEN_MAGIC,
    TOKEN_STRING,
} Token_Kind;

const char *token_kind_display(Token_Kind kind)
{
    switch (kind) {
    case TOKEN_INVALID:   return "TOKEN_INVALID";
    case TOKEN_END:       return "TOKEN_END";
    case TOKEN_OPAREN:    return "TOKEN_OPAREN";
    case TOKEN_CPAREN:    return "TOKEN_CPAREN";
    case TOKEN_LAMBDA:    return "TOKEN_LAMBDA";
    case TOKEN_DOT:       return "TOKEN_DOT";
    case TOKEN_COLON:     return "TOKEN_COLON";
    case TOKEN_SEMICOLON: return "TOKEN_SEMICOLON";
    case TOKEN_EQUALS:    return "TOKEN_EQUALS";
    case TOKEN_NAME:      return "TOKEN_NAME";
    case TOKEN_MAGIC:     return "TOKEN_MAGIC";
    case TOKEN_STRING:    return "TOKEN_STRING";
    default: UNREACHABLE("Token_Kind");
    }
}

typedef struct {
    size_t pos, bol, row;
} Cur;

typedef struct {
    const char *content;
    size_t count;
    const char *file_path;

    Cur cur;

    Token_Kind token;
    String_Builder string;
    size_t row, col;
} Lexer;

void lexer_init(Lexer *l, const char *content, size_t count, const char *file_path)
{
    l->content = content;
    l->count = count;
    l->file_path = file_path;
    memset(&l->cur, 0, sizeof(l->cur));
}

void lexer_print_loc(Lexer *l, FILE *stream)
{
    if (l->file_path) fprintf(stream, "%s:", l->file_path);
    fprintf(stream, "%zu:%zu: ", l->row, l->col);
}

char lexer_curr_char(Lexer *l)
{
    if (l->cur.pos >= l->count) return 0;
    return l->content[l->cur.pos];
}

char lexer_next_char(Lexer *l)
{
    if (l->cur.pos >= l->count) return 0;
    char x = l->content[l->cur.pos++];
    if (x == '\n') {
        l->cur.row += 1;
        l->cur.bol = l->cur.pos;
    }
    return x;
}

void lexer_trim_left(Lexer *l)
{
    while (isspace(lexer_curr_char(l))) {
        lexer_next_char(l);
    }
}

bool lexer_starts_with(Lexer *l, const char *prefix)
{
    size_t pos = l->cur.pos;
    while (pos < l->count && *prefix != '\0' && *prefix == l->content[pos]) {
        pos++;
        prefix++;
    }
    return *prefix == '\0';
}

void lexer_drop_line(Lexer *l)
{
    while (l->cur.pos < l->count && lexer_next_char(l) != '\n') {}
}

bool lexer_next(Lexer *l)
{
    for (;;) {
        lexer_trim_left(l);
        if (lexer_starts_with(l, "//")) lexer_drop_line(l);
        else break;
    }

    l->row = l->cur.row + 1;
    l->col = l->cur.pos - l->cur.bol + 1;

    char x = lexer_next_char(l);
    if (x == '\0') {
        l->token = TOKEN_END;
        return true;
    }

    switch (x) {
    case '(':  l->token = TOKEN_OPAREN;    return true;
    case ')':  l->token = TOKEN_CPAREN;    return true;
    case '\\': l->token = TOKEN_LAMBDA;    return true;
    case '.':  l->token = TOKEN_DOT;       return true;
    case ':':  l->token = TOKEN_COLON;     return true;
    case ';':  l->token = TOKEN_SEMICOLON; return true;
    case '=':  l->token = TOKEN_EQUALS;    return true;
    }

    if (x == '#') {
        l->token = TOKEN_MAGIC;
        l->string.count = 0;
        while (isalnum(lexer_curr_char(l))) {
            x = lexer_next_char(l);
            da_append(&l->string, x);
        }
        sb_append_null(&l->string);
        return true;
    }

    if (x == '"') {
        l->token = TOKEN_STRING;
        l->string.count = 0;
        for (;;) {
            if (l->cur.pos >= l->count) {
                l->token = TOKEN_INVALID;
                lexer_print_loc(l, stderr);
                fprintf(stderr, "ERROR: unfinished string\n");
                return false;
            }
            x = lexer_next_char(l);
            switch (x) {
            case '"':
                sb_append_null(&l->string);
                return true;
            case '\\':
                if (l->cur.pos >= l->count) {
                    l->token = TOKEN_INVALID;
                    lexer_print_loc(l, stderr);
                    fprintf(stderr, "ERROR: unfinished escape sequence\n");
                    return false;
                }
                x = lexer_next_char(l);
                switch (x) {
                case '"':
                case '\\':
                    da_append(&l->string, x);
                    break;
                default:
                    l->token = TOKEN_INVALID;
                    lexer_print_loc(l, stderr);
                    fprintf(stderr, "ERROR: unknown escape sequence starting with `%c`\n", x);
                    return false;
                }
                break;
            default:
                da_append(&l->string, x);
            }
        }
        UNREACHABLE("string literal");
    }

    if (isalnum(x)) {
        l->token = TOKEN_NAME;
        l->string.count = 0;
        da_append(&l->string, x);
        while (isalnum(lexer_curr_char(l))) {
            x = lexer_next_char(l);
            da_append(&l->string, x);
        }
        sb_append_null(&l->string);
        return true;
    }

    l->token = TOKEN_INVALID;
    lexer_print_loc(l, stderr);
    fprintf(stderr, "ERROR: Unknown token starts with `%c`\n", x);
    return false;
}

bool lexer_peek(Lexer *l)
{
    Cur cur = l->cur;
    bool result = lexer_next(l);
    l->cur = cur;
    return result;
}

bool lexer_expect(Lexer *l, Token_Kind expected)
{
    if (!lexer_next(l)) return false;
    if (l->token != expected) {
        lexer_print_loc(l, stderr);
        fprintf(stderr, "ERROR: Unexpected token %s. Expected %s instead.\n", token_kind_display(l->token), token_kind_display(expected));
        return false;
    }
    return true;
}

bool parse_expr(Lexer *l, Expr_Index *expr);

bool parse_fun(Lexer *l, Expr_Index *expr)
{
    if (!lexer_expect(l, TOKEN_NAME)) return false;
    Symbol arg = symbol(l->string.items);
    if (!lexer_expect(l, TOKEN_DOT)) return false;

    Token_Kind a, b;
    Cur cur = l->cur; {
        if (!lexer_next(l)) return false;
        a = l->token;
        if (!lexer_next(l)) return false;
        b = l->token;
    } l->cur = cur;

    Expr_Index body;
    if (a == TOKEN_NAME && b == TOKEN_DOT) {
        if (!parse_fun(l, &body)) return false;
    } else {
        if (!parse_expr(l, &body)) return false;
    }
    *expr = fun(arg, body);
    return true;
}

bool parse_primary(Lexer *l, Expr_Index *expr)
{
    if (!lexer_next(l)) return NULL;
    switch ((int)l->token) {
    case TOKEN_OPAREN: {
        if (!parse_expr(l, expr)) return false;
        if (!lexer_expect(l, TOKEN_CPAREN)) return false;
        return true;
    }
    case TOKEN_LAMBDA: return parse_fun(l, expr);
    case TOKEN_MAGIC:
        *expr = magic(l->string.items);
        return true;
    case TOKEN_NAME:
        *expr = var(symbol(l->string.items));
        return true;
    default:
        lexer_print_loc(l, stderr);
        fprintf(stderr, "ERROR: Unexpected token %s. Expected a primary expression instead.\n", token_kind_display(l->token));
        return false;
    }
}

bool parse_expr(Lexer *l, Expr_Index *expr)
{
    if (!parse_primary(l, expr)) return false;

    if (!lexer_peek(l)) return false;
    while (
        l->token != TOKEN_CPAREN &&
        l->token != TOKEN_END    &&
        l->token != TOKEN_SEMICOLON
    ) {
        Expr_Index rhs;
        if (!parse_primary(l, &rhs)) return false;
        *expr = app(*expr, rhs);
        if (!lexer_peek(l)) return false;
    }
    return true;
}

typedef struct {
    const char *name;
    const char *signature;
    const char *description;
} Command;

typedef struct {
    Command *items;
    size_t count;
    size_t capacity;
} Commands;

bool command(Commands *commands, const char *input, const char *name, const char *signature, const char *description)
{
    Command command = {
        .name        = name,
        .signature   = signature,
        .description = description,
    };
    da_append(commands, command);
    while (*input && *name && *input == *name) {
        input++;
        name++;
    }
    return *input == '\0';
}

void print_available_commands(Commands *commands)
{
    printf("Available commands:\n");
    int max_name_width = 0;
    int max_sig_width = 0;
    for (size_t i = 0; i < commands->count; ++i) {
        Command command = commands->items[i];
        int name_width  = strlen(command.name);
        int sig_width   = strlen(command.signature);
        if (name_width > max_name_width) max_name_width = name_width;
        if (sig_width  > max_sig_width)  max_sig_width  = sig_width;
    }
    for (size_t i = 0; i < commands->count; ++i) {
        Command command = commands->items[i];
        printf("  :%-*s %-*s - %s\n",
               max_name_width, command.name,
               max_sig_width,  command.signature,
               command.description);
    }
}

void gc_mark(Expr_Index root)
{
    if (expr_slot(root).visited) return;
    expr_slot(root).visited = true;
    switch (expr_slot(root).kind) {
    case EXPR_MAG:
    case EXPR_VAR: break;
    case EXPR_FUN:
        gc_mark(expr_slot(root).as.fun.body);
        break;
    case EXPR_APP:
        gc_mark(expr_slot(root).as.app.lhs);
        gc_mark(expr_slot(root).as.app.rhs);
        break;
    }
}

typedef struct {
    Symbol name;
    Expr_Index body;
} Binding;

typedef struct {
    Binding *items;
    size_t count;
    size_t capacity;
} Bindings;

void create_binding(Bindings *bindings, Symbol name, Expr_Index body)
{
    for (size_t i = 0; i < bindings->count; ++i) {
        if (symbol_eq(bindings->items[i].name, name)) {
            bindings->items[i].body = body;
            if (name.tag == 0) {
                printf("Updated binding %s\n", name.label);
            } else {
                printf("Updated binding %s@%zu\n", name.label, name.tag);
            }
            return;
        }
    }
    Binding binding = {
        .name = name,
        .body = body,
    };
    da_append(bindings, binding);
    if (name.tag == 0) {
        printf("Created binding %s\n", name.label);
    } else {
        printf("Created binding %s@%zu\n", name.label, name.tag);
    }
}

bool create_bindings_from_file(const char *file_path, Bindings *bindings)
{
    static String_Builder sb = {0};
    static Lexer l = {0};

    sb.count = 0;
    if (!read_entire_file(file_path, &sb)) return false;

    lexer_init(&l, sb.items, sb.count, file_path);

    if (!lexer_peek(&l)) return false;
    while (l.token != TOKEN_END) {
        if (!lexer_expect(&l, TOKEN_NAME)) return false;
        Symbol name = symbol(l.string.items);
        if (!lexer_expect(&l, TOKEN_EQUALS)) return false;
        Expr_Index body;
        if (!parse_expr(&l, &body)) return false;
        if (!lexer_expect(&l, TOKEN_SEMICOLON)) return false;
        create_binding(bindings, name, body);
        if (!lexer_peek(&l)) return false;
    }
    return true;
}

void gc(Expr_Index root, Bindings bindings)
{
    for (size_t i = 0; i < expr_pool.count; ++i) {
        if (expr_pool.items[i].live) {
            expr_pool.items[i].visited = false;
        }
    }

    gc_mark(root);
    for (size_t i = 0; i < bindings.count; ++i) {
        gc_mark(bindings.items[i].body);
    }

    for (size_t i = 0; i < expr_pool.count; ++i) {
        if (expr_pool.items[i].live && !expr_pool.items[i].visited) {
            free_expr((Expr_Index){i});
        }
    }
}

static volatile sig_atomic_t ctrl_c = 0;
void ctrl_c_handler(int signum)
{
    UNUSED(signum);
    ctrl_c = 1;
}

// TODO(20251219-231632): it's a bit annoying that lexer forces us to write :edit "file.lamb" instead of just :edit file.lamb
//   Maybe as soon as we parsed TOKEN_COLON and TOKEN_NAME we should just treat the rest of characters as the path without
//   using the Lexer. Maybe trim whitespaces from both ends and we are good to go. This also means we don't need TOKEN_STRING.
// TODO: Bug in all the commands that accept active file (:load, :save, :edit, etc) when you provide token that is not TOKEN_STRING.
//   It treats this as you didn't provide anything at asll. Very like will be invalidated by 20251219-231632.
// TODO: something to check alpha-equivalence of two terms
// TODO: consider changing expr_display so it displays shortened up version of exprs so on :save it all looks nice
// TODO: :save is pretty dangerous since it override all the formatting. Maybe it should do YorN?
int main(int argc, char **argv)
{
    static char buffer[1024];
    static Commands commands = {0};
    static Bindings bindings = {0};
    static Lexer l = {0};

#ifndef _WIN32
    // TODO: Handle ctrl+c on Windows
    struct sigaction act = {0};
    act.sa_handler = ctrl_c_handler;
    sigaction(SIGINT, &act, NULL);
#endif // _WIN32

    const char *editor  = getenv("LAMB_EDITOR");
    if (!editor) editor = getenv("EDITOR");
    if (!editor) editor = "vi";

    // `active_file_path` is always located on the heap. If you need to replace it, first free() it
    // and then copy_string() it.
    char *active_file_path = NULL;

    if (argc == 2) {
        active_file_path = copy_string(argv[1]);
    } else if (argc > 2) {
        fprintf(stderr, "ERROR: only a active file is support right now\n");
        return 1;
    }

    if (active_file_path) {
        create_bindings_from_file(active_file_path, &bindings);
    }

    printf(",---@>\n");
    printf(" W-W'\n");
    printf("Enter :help for more info\n");
    for (;;) {
again:
        printf("@> ");
        fflush(stdout);
        if (!fgets(buffer, sizeof(buffer), stdin)) {
            if (feof(stdin)) goto quit;
            printf("\n");
            goto again;
        }
        const char *source = buffer;

        lexer_init(&l, source, strlen(source), NULL);

        if (!lexer_peek(&l)) goto again;
        if (l.token == TOKEN_END) goto again;
        if (l.token == TOKEN_COLON) {
            if (!lexer_next(&l)) goto again;
            if (!lexer_expect(&l, TOKEN_NAME)) goto again;
            commands.count = 0;
            if (command(&commands, l.string.items, "load", "[path]", "Load/reload bindings from a file.")) {
                if (!lexer_next(&l)) goto again;

                if (l.token == TOKEN_STRING) {
                    free(active_file_path);
                    active_file_path = copy_string(l.string.items);
                }

                if (active_file_path == NULL) {
                    fprintf(stderr, "ERROR: no active file to load from\n");
                    goto again;
                }

                bindings.count = 0;
                create_bindings_from_file(active_file_path, &bindings);
                goto again;
            }
            if (command(&commands, l.string.items, "save", "[path]", "Save current bindings to a file.")) {
                if (!lexer_next(&l)) goto again;

                if (l.token == TOKEN_STRING) {
                    free(active_file_path);
                    active_file_path = copy_string(l.string.items);
                }

                if (active_file_path == NULL) {
                    fprintf(stderr, "ERROR: no active file to save to\n");
                    goto again;
                }

                static String_Builder sb = {0};
                sb.count = 0;
                for (size_t i = 0; i < bindings.count; ++i) {
                    assert(bindings.items[i].name.tag == 0);
                    sb_appendf(&sb, "%s = ", bindings.items[i].name.label);
                    expr_display(bindings.items[i].body, &sb);
                    sb_appendf(&sb, ";\n");
                }

                if (!write_entire_file(active_file_path, sb.items, sb.count)) goto again;
                printf("Saved all the bindings to %s\n", active_file_path);
                goto again;
            }
            if (command(&commands, l.string.items, "edit", "[path]", "Edit current active file. Reload it on exit.")) {
#ifdef _WIN32
                fprintf(stderr, "TODO: editing files is not implemented on Windows yet! Sorry!\n");
#else
                if (!lexer_next(&l)) goto again;

                if (l.token == TOKEN_STRING) {
                    free(active_file_path);
                    active_file_path = copy_string(l.string.items);
                }

                if (active_file_path == NULL) {
                    fprintf(stderr, "ERROR: no active file to edit\n");
                    goto again;
                }

                static Cmd cmd = {0};
                cmd.count = 0;
                da_append(&cmd, editor);
                da_append(&cmd, active_file_path);
                if (cmd_run(&cmd)) {
                    bindings.count = 0;
                    create_bindings_from_file(active_file_path, &bindings);
                }
#endif // _WIN32
                goto again;
            }
            if (command(&commands, l.string.items, "list", "", "list all the current bindings")) {
                static String_Builder sb = {0};
                for (size_t i = 0; i < bindings.count; ++i) {
                    assert(bindings.items[i].name.tag == 0);
                    sb.count = 0;
                    sb_appendf(&sb, "%s = ", bindings.items[i].name.label);
                    expr_display(bindings.items[i].body, &sb);
                    sb_appendf(&sb, ";");
                    sb_append_null(&sb);
                    printf("%s\n", sb.items);
                }
                goto again;
            }
            if (command(&commands, l.string.items, "delete", "<name>", "delete a binding by name")) {
                if (!lexer_expect(&l, TOKEN_NAME)) goto again;
                Symbol name = symbol(l.string.items);
                for (size_t i = 0; i < bindings.count; ++i) {
                    if (symbol_eq(bindings.items[i].name, name)) {
                        da_delete_at(&bindings, i);
                        printf("Deleted binding %s\n", name.label);
                        goto again;
                    }
                }
                printf("ERROR: binding %s was not found\n", name.label);
                goto again;
            }
            if (command(&commands, l.string.items, "debug", "<expr>", "Step debug the evaluation of an expression")) {
                Expr_Index expr;
                if (!parse_expr(&l, &expr)) goto again;
                for (size_t i = bindings.count; i > 0; --i) {
                    expr = replace(bindings.items[i-1].name, expr, bindings.items[i-1].body);
                }

                ctrl_c = 0;
                for (;;) {
                    if (ctrl_c) goto again; // TODO(20251220-002405)

                    printf("DEBUG: ");
                    trace_expr(expr);
                    printf("\n");

                    printf("-> ");
                    fflush(stdin);

                    if (!fgets(buffer, sizeof(buffer), stdin)) {
                        if (feof(stdin)) goto quit;
                        printf("\n");
                        goto again;
                    }

                    lexer_init(&l, buffer, strlen(buffer), NULL);
                    if (!lexer_next(&l)) goto again;
                    if (l.token == TOKEN_NAME) {
                        // TODO: Better debug mode REPL
                        // Should be on par in quality with the main REPL
                        if (strcmp(l.string.items, "quit") == 0) goto again;
                    }

                    gc(expr, bindings);

                    Expr_Index expr1;
                    if (!eval1(expr, &expr1)) goto again;
                    if (expr.unwrap == expr1.unwrap) break;
                    expr = expr1;
                }

                goto again;
            }
            if (command(&commands, l.string.items, "quit", "", "quit the REPL")) goto quit;
            if (command(&commands, l.string.items, "help", "", "print this help message")) {
                print_available_commands(&commands);
                goto again;
            }
            print_available_commands(&commands);
            printf("ERROR: unknown command `%s`\n", l.string.items);
            goto again;
        }

        Token_Kind a, b;
        Cur cur = l.cur; {
            if (!lexer_next(&l)) goto again;
            a = l.token;
            if (!lexer_next(&l)) goto again;
            b = l.token;
        } l.cur = cur;

        if (a == TOKEN_NAME && b == TOKEN_EQUALS) {
            if (!lexer_expect(&l, TOKEN_NAME)) goto again;
            Symbol name = symbol(l.string.items);
            if (!lexer_expect(&l, TOKEN_EQUALS)) goto again;
            Expr_Index body;
            if (!parse_expr(&l, &body)) goto again;
            create_binding(&bindings, name, body);
            goto again;
        }

        Expr_Index expr;
        if (!parse_expr(&l, &expr)) goto again;
        for (size_t i = bindings.count; i > 0; --i) {
            expr = replace(bindings.items[i-1].name, expr, bindings.items[i-1].body);
        }

        ctrl_c = 0;
        for (;;) {
            if (ctrl_c) {
                // TODO(20251220-002405): Is there perhaps a better way to cancel evaluation by utilizing long jumps from signal handlers?
                // Is that even legal?
                // https://www.gnu.org/savannah-checkouts/gnu/libc/manual/html_node/Longjmp-in-Handler.html
                printf("Evaluation canceled by user.\n");
                goto again;
            }

            gc(expr, bindings);

            Expr_Index expr1;
            if (!eval1(expr, &expr1)) goto again;
            if (expr.unwrap == expr1.unwrap) break;
            expr = expr1;
        }

        printf("RESULT: ");
        trace_expr(expr);
        printf("\n");
    }
quit:

    return 0;
}
// Copyright 2025 Alexey Kutepov <reximkut@gmail.com>
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
