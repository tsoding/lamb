// ,---@>
//  W-W'
// cc -pedantic -std=c99 -o lamb lamb.c
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

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

#define sb_append_null(sb) da_append(sb, 0)

typedef struct {
    size_t unwrap;
} Expr_Index;

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

typedef enum {
    EXPR_VAR,
    EXPR_FUN,
    EXPR_APP,
} Expr_Kind;

typedef struct Expr Expr;

typedef struct {
    const char *name; // TODO: I don't like how easy it is to forget to intern a name. Let's do a similar trick to Expr_Index
    size_t id;
} Var_Name;

bool var_name_eq(Var_Name a, Var_Name b)
{
    return a.name == b.name && a.id == b.id;
}

Var_Name var_name(const char *name)
{
    Var_Name var = { .name = name };
    return var;
}

typedef struct {
    Var_Name arg;
    Expr_Index body;
} Expr_Fun;

struct Expr {
    Expr_Kind kind;
    bool visited;
    bool live;
    union {
        Var_Name var;
        Expr_Fun fun;
        struct {
            Expr_Index lhs;
            Expr_Index rhs;
        } app;
    } as;
};

typedef struct {
    const char *name;
    Expr_Index body;
} Binding;

typedef struct {
    Binding *items;
    size_t count;
    size_t capacity;
} Bindings;

struct {
    Expr *items;
    size_t count;
    size_t capacity;
} expr_pool = {0};

#define expr_slot(index) (                           \
    expr_pool.items[                                 \
        (assert(index.unwrap < expr_pool.count),     \
         assert(expr_pool.items[index.unwrap].live), \
         index.unwrap)])

#define expr_slot_unsafe(index) (expr_pool.items[index.unwrap])

struct {
    Expr_Index *items;
    size_t count;
    size_t capacity;
} expr_dead_pool = {0};

struct {
    const char **items;
    size_t count;
    size_t capacity;
} strings = {0};

const char *intern(const char *s)
{
    for (size_t i = 0; i < strings.count; ++i) {
        if (strcmp(strings.items[i], s) == 0) {
            return strings.items[i];
        }
    }
    char *result = copy_string(s);
    da_append(&strings, result);
    return result;
}

Expr_Index make_expr(void)
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

void kill_expr(Expr_Index expr)
{
    expr_slot(expr).live = false;
    da_append(&expr_dead_pool, expr);
}

Expr_Index var(const char *name)
{
    Expr_Index expr = make_expr();
    expr_slot(expr).kind = EXPR_VAR;
    expr_slot(expr).as.var = var_name(intern(name));
    return expr;
}

Expr_Index var_var_name(Var_Name name)
{
    Expr_Index expr = make_expr();
    expr_slot(expr).kind = EXPR_VAR;
    expr_slot(expr).as.var = name;
    return expr;
}

Expr_Index fun(const char *arg, Expr_Index body)
{
    Expr_Index expr = make_expr();
    expr_slot(expr).kind = EXPR_FUN;
    expr_slot(expr).as.fun.arg = var_name(intern(arg));
    expr_slot(expr).as.fun.body = body;
    return expr;
}

Expr_Index fun_var_name(Var_Name arg, Expr_Index body)
{
    Expr_Index expr = make_expr();
    expr_slot(expr).kind = EXPR_FUN;
    expr_slot(expr).as.fun.arg = arg;
    expr_slot(expr).as.fun.body = body;
    return expr;
}

Expr_Index app(Expr_Index lhs, Expr_Index rhs)
{
    Expr_Index expr = make_expr();
    expr_slot(expr).kind = EXPR_APP;
    expr_slot(expr).as.app.lhs = lhs;
    expr_slot(expr).as.app.rhs = rhs;
    return expr;
}

void expr_display(Expr_Index expr, String_Builder *sb)
{
    switch (expr_slot(expr).kind) {
    case EXPR_VAR:
        sb_appendf(sb, "%s", expr_slot(expr).as.var.name);
        if (expr_slot(expr).as.var.id) {
            sb_appendf(sb, "@%zu", expr_slot(expr).as.var.id);
        }
        break;
    case EXPR_FUN:
        if (expr_slot(expr).as.fun.arg.id) {
            sb_appendf(sb, "\\%s@%zu.", expr_slot(expr).as.fun.arg.name, expr_slot(expr).as.fun.arg.id);
        } else {
            sb_appendf(sb, "\\%s.", expr_slot(expr).as.fun.arg.name);
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
    default: UNREACHABLE("Expr_Kind");
    }
}

bool is_var_free_there(Var_Name name, Expr_Index there)
{
    switch (expr_slot(there).kind) {
    case EXPR_VAR:
        return var_name_eq(expr_slot(there).as.var, name);
    case EXPR_FUN:
        if (var_name_eq(expr_slot(there).as.fun.arg, name)) return false;
        return is_var_free_there(name, expr_slot(there).as.fun.body);
    case EXPR_APP:
        if (is_var_free_there(name, expr_slot(there).as.app.lhs)) return true;
        if (is_var_free_there(name, expr_slot(there).as.app.rhs)) return true;
        return false;
    default: UNREACHABLE("Expr_Kind");
    }
}

Expr_Index replace(Var_Name arg, Expr_Index body, Expr_Index val)
{
    switch (expr_slot(body).kind) {
    case EXPR_VAR:
        if (var_name_eq(expr_slot(body).as.var, arg)) {
            return val;
        } else {
            return body;
        }
    case EXPR_FUN:
        if (var_name_eq(expr_slot(body).as.fun.arg, arg)) return body;
        if (!is_var_free_there(expr_slot(body).as.fun.arg, val)) {
            return fun_var_name(expr_slot(body).as.fun.arg, replace(arg, expr_slot(body).as.fun.body, val));
        }
        Var_Name fresh_name = expr_slot(body).as.fun.arg;
        static size_t id_counter = 0;
        fresh_name.id = ++id_counter;
        Expr_Index fresh_arg = var_var_name(fresh_name);
        return fun_var_name(
            fresh_name,
            replace(arg,
                replace(
                    expr_slot(body).as.fun.arg,
                    expr_slot(body).as.fun.body,
                    fresh_arg),
                val));
    case EXPR_APP:
        return app(
            replace(arg, expr_slot(body).as.app.lhs, val),
            replace(arg, expr_slot(body).as.app.rhs, val));
    default: UNREACHABLE("Expr_Kind");
    }
}

Expr_Index apply(Expr_Fun fun, Expr_Index val)
{
    return replace(fun.arg, fun.body, val);
}

Expr_Index eval1(Expr_Index expr)
{
    switch (expr_slot(expr).kind) {
    case EXPR_VAR:
        return expr;
    case EXPR_FUN: {
        Expr_Index body = eval1(expr_slot(expr).as.fun.body);
        if (body.unwrap != expr_slot(expr).as.fun.body.unwrap) {
            return fun_var_name(expr_slot(expr).as.fun.arg, body);
        }
        return expr;
    }
    case EXPR_APP: {
        Expr_Index lhs = expr_slot(expr).as.app.lhs;
        Expr_Index rhs = expr_slot(expr).as.app.rhs;

        if (expr_slot(lhs).kind == EXPR_FUN) {
            return apply(expr_slot(lhs).as.fun, rhs);
        }

        Expr_Index new_lhs = eval1(lhs);
        if (lhs.unwrap != new_lhs.unwrap) {
            return app(new_lhs, rhs);
        }

        Expr_Index new_rhs = eval1(rhs);
        if (rhs.unwrap != new_rhs.unwrap) {
            return app(lhs, new_rhs);
        }

        return expr;
    }
    default: UNREACHABLE("Expr_Kind");
    }
}

void trace_expr(Expr_Index expr, String_Builder *sb)
{
    sb->count = 0;
    expr_display(expr, sb);
    sb_append_null(sb);
    printf("%s\n", sb->items);
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
    case TOKEN_NAME:      return "TOKEN_NAME";
    case TOKEN_COLON:     return "TOKEN_COLON";
    case TOKEN_SEMICOLON: return "TOKEN_SEMICOLON";
    case TOKEN_EQUALS:    return "TOKEN_EQUALS";
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
    String_Builder name;
    size_t row, col;
} Lexer;

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

bool lexer_next(Lexer *l)
{
    while (isspace(lexer_curr_char(l))) {
        lexer_next_char(l);
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

    if (isalnum(x)) {
        l->token = TOKEN_NAME;
        l->name.count = 0;
        da_append(&l->name, x);
        while (isalnum(lexer_curr_char(l))) {
            x = lexer_next_char(l);
            da_append(&l->name, x);
        }
        sb_append_null(&l->name);
        return true;
    }

    l->token = TOKEN_INVALID;
    lexer_print_loc(l, stderr);
    fprintf(stderr, "ERROR: Unknown token starts with `%c`\n", x);
    return false;
}

bool lexer_peek(Lexer *l)
{
    Cur saved = l->cur;
    bool result = lexer_next(l);
    l->cur = saved;
    return result;
}

bool lexer_expect(Lexer *l, Token_Kind expected)
{
    if (!lexer_next(l)) return false;
    if (l->token != expected) {
        lexer_print_loc(l, stderr);
        fprintf(stderr, "ERROR: Unexpected token %s\n", token_kind_display(l->token));
        return false;
    }
    return true;
}

bool parse_expr(Lexer *l, Expr_Index *expr);

bool parse_fun(Lexer *l, Expr_Index *expr)
{
    if (!lexer_expect(l, TOKEN_NAME)) return false;
    const char *arg = intern(l->name.items);
    if (!lexer_expect(l, TOKEN_DOT)) return false;

    Token_Kind a, b;
    Cur saved = l->cur; {
        if (!lexer_next(l)) return false;
        a = l->token;
        if (!lexer_next(l)) return false;
        b = l->token;
    } l->cur = saved;

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
    switch (l->token) {
    case TOKEN_OPAREN: {
        if (!parse_expr(l, expr)) return false;
        if (!lexer_expect(l, TOKEN_CPAREN)) return false;
        return true;
    }
    case TOKEN_LAMBDA: return parse_fun(l, expr);
    case TOKEN_NAME:
        *expr = var(l->name.items);
        return true;
    default:
        lexer_print_loc(l, stderr);
        fprintf(stderr, "ERROR: Unexpected token %s\n", token_kind_display(l->token));
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
        l->token != TOKEN_SEMICOLON // TODO: Can we get rid of the semicolon? Why looking ahead for `name =` doesn't work?
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
        int name_width = strlen(command.name);
        int sig_width  = strlen(command.signature);
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
            kill_expr((Expr_Index){i});
        }
    }
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

void create_binding(Bindings *bindings, const char *name, Expr_Index body)
{
    for (size_t i = 0; i < bindings->count; ++i) {
        if (bindings->items[i].name == name) {
            bindings->items[i].body = body;
            printf("Updated binding %s\n", name);
            return;
        }
    }
    Binding binding = {
        .name = name,
        .body = body,
    };
    da_append(bindings, binding);
    printf("Created binding %s\n", name);
}

bool create_bindings_from_file(const char *file_path, String_Builder *sb, Bindings *bindings)
{
    sb->count = 0;
    if (!read_entire_file(file_path, sb)) return false;
    Lexer l = {
        .content = sb->items,
        .count = sb->count,
        .file_path = file_path,
    };
    if (!lexer_peek(&l)) return false;
    while (l.token != TOKEN_END) {
        if (!lexer_expect(&l, TOKEN_NAME)) return false;
        const char *name = intern(l.name.items);
        if (!lexer_expect(&l, TOKEN_EQUALS)) return false;
        Expr_Index body;
        if (!parse_expr(&l, &body)) return false;
        if (!lexer_expect(&l, TOKEN_SEMICOLON)) return false;
        create_binding(bindings, name, body);
        if (!lexer_peek(&l)) return false;
    }
    return true;
}

int main(int argc, char **argv)
{
    static char buffer[1024];
    static String_Builder sb = {0};
    static Commands commands = {0};
    static Bindings bindings = {0};

    for (int i = 1; i < argc; ++i) {
        create_bindings_from_file(argv[i], &sb, &bindings);
    }
    sb.count = 0;

    size_t limit = 0;
    bool trace = false;
    printf(",---@>\n");
    printf(" W-W'\n");
    printf("Enter :help for more info\n");
    for (;;) {
again:
        printf("@> ");
        fflush(stdout);
        if (!fgets(buffer, sizeof(buffer), stdin)) goto quit;
        const char *source = buffer;
        Lexer l = {
            .content = source,
            .count = strlen(source),
        };

        if (!lexer_peek(&l)) goto again;
        if (l.token == TOKEN_COLON) {
            if (!lexer_next(&l)) goto again;
            if (!lexer_expect(&l, TOKEN_NAME)) goto again;
            commands.count = 0;
            if (command(&commands, l.name.items, "trace", "", "toggle tracing")) {
                trace = !trace;
                if (trace) {
                    printf("Tracing ENABLED\n");
                } else {
                    printf("Tracing DISABLED\n");
                }
                goto again;
            }
            if (command(&commands, l.name.items, "reload", "", "reload all the loaded files")) {
                bindings.count = 0;
                for (int i = 1; i < argc; ++i) {
                    create_bindings_from_file(argv[i], &sb, &bindings);
                }
                sb.count = 0;
                goto again;
            }
            if (command(&commands, l.name.items, "mem", "", "print memory related stats")) {
                printf("Interned strings: %zu\n", strings.count);
                printf("Allocated exprs:  %zu\n", expr_pool.count);
                printf("Dead exprs:       %zu\n", expr_dead_pool.count);
                goto again;
            }
            if (command(&commands, l.name.items, "limit", "[number]", "change evaluation limit (0 for no limit)")) {
                if (!lexer_peek(&l)) goto again;
                switch (l.token) {
                case TOKEN_NAME:
                    if (!lexer_expect(&l, TOKEN_NAME)) goto again;
                    limit = strtoul(l.name.items, NULL, 10);
                    if (limit) {
                        printf("Setting evaluation limit to %zu\n", limit);
                    } else {
                        printf("Evaluation limit is disabled\n");
                    }
                    goto again;
                case TOKEN_END:
                    if (limit) {
                        printf("Evaluation limit is %zu\n", limit);
                    } else {
                        printf("Evaluation limit is disabled\n");
                    }
                    goto again;
                default:
                    lexer_print_loc(&l, stderr);
                    fprintf(stderr, "ERROR: Unexpected token %s\n", token_kind_display(l.token));
                    goto again;
                }
            }
            if (command(&commands, l.name.items, "quit", "", "quit the REPL")) goto quit;
            if (command(&commands, l.name.items, "help", "", "print this help message")) {
                print_available_commands(&commands);
                goto again;
            }
            print_available_commands(&commands);
            printf("ERROR: unknown command `%s`\n", l.name.items);
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
            const char *name = intern(l.name.items);
            if (!lexer_expect(&l, TOKEN_EQUALS)) goto again;
            Expr_Index body;
            if (!parse_expr(&l, &body)) goto again;
            create_binding(&bindings, name, body);
            goto again;
        }

        Expr_Index expr;
        if (!parse_expr(&l, &expr)) goto again;
        for (size_t i = bindings.count; i > 0; --i) {
            expr = app(fun(bindings.items[i-1].name, expr), bindings.items[i-1].body);
        }

        if (trace) trace_expr(expr, &sb);
        Expr_Index expr1 = eval1(expr);
        for (size_t i = 1; (limit == 0 || i < limit) && expr1.unwrap != expr.unwrap; ++i) {
            expr = expr1;
            gc(expr, bindings);
            if (trace) trace_expr(expr, &sb);
            expr1 = eval1(expr);
        }
        if (expr1.unwrap != expr.unwrap) {
            printf("Evaluation limit exceeded\n");
        } else {
            trace_expr(expr, &sb);
        }
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
