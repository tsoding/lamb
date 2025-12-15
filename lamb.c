// cc -o lamb lamb.c
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "nob.h"

typedef enum {
    EXPR_VAR,
    EXPR_FUN,
    EXPR_APP,
} Expr_Kind;

typedef struct Expr Expr;

typedef struct {
    const char *arg;
    Expr *body;
} Expr_Fun;

struct Expr {
    Expr_Kind kind;
    union {
        const char *var;
        Expr_Fun fun;
        struct {
            Expr *lhs;
            Expr *rhs;
        } app;
    } as;
};

Expr *var(const char *name)
{
    Expr *expr = malloc(sizeof(*expr));
    assert(expr != NULL);
    expr->kind = EXPR_VAR;
    expr->as.var = strdup(name);
    return expr;
}

Expr *fun(const char *arg, Expr *body)
{
    Expr *expr = malloc(sizeof(*expr));
    assert(expr != NULL);
    expr->kind = EXPR_FUN;
    expr->as.fun.arg = strdup(arg);
    expr->as.fun.body = body;
    return expr;
}

Expr *app(Expr *lhs, Expr *rhs)
{
    Expr *expr = malloc(sizeof(*expr));
    assert(expr != NULL);
    expr->kind = EXPR_APP;
    expr->as.app.lhs = lhs;
    expr->as.app.rhs = rhs;
    return expr;
}

void expr_display(Expr *expr, String_Builder *sb)
{
    switch (expr->kind) {
    case EXPR_VAR:
        sb_appendf(sb, "%s", expr->as.var);
        break;
    case EXPR_FUN:
        sb_appendf(sb, "(\\%s.", expr->as.fun.arg);
        expr_display(expr->as.fun.body, sb);
        sb_appendf(sb, ")");
        break;
    case EXPR_APP:
        sb_appendf(sb, "(");
        expr_display(expr->as.app.lhs, sb);
        sb_appendf(sb, " ");
        expr_display(expr->as.app.rhs, sb);
        sb_appendf(sb, ")");
        break;
    default: UNREACHABLE("Expr_Kind");
    }
}

Expr *replace(const char *arg, Expr *body, Expr *val)
{
    switch (body->kind) {
    case EXPR_VAR:
        if (strcmp(body->as.var, arg) == 0) {
            return val;
        } else {
            return body;
        }
    case EXPR_FUN:
        return fun(
            body->as.fun.arg,
            replace(arg, body->as.fun.body, val));
    case EXPR_APP:
        return app(
            replace(arg, body->as.app.lhs, val),
            replace(arg, body->as.app.rhs, val));
    default: UNREACHABLE("Expr_Kind");
    }
}

Expr *apply(Expr_Fun fun, Expr *val)
{
    return replace(fun.arg, fun.body, val);
}

Expr *eval1(Expr *expr)
{
    switch (expr->kind) {
    case EXPR_VAR:
    case EXPR_FUN:
        return expr;
    case EXPR_APP:
        Expr *lhs = eval1(expr->as.app.lhs);
        if (lhs != expr->as.app.lhs) {
            return app(lhs, expr->as.app.rhs);
        }

        if (lhs->kind == EXPR_FUN) {
            return apply(lhs->as.fun, expr->as.app.rhs);
        }

        Expr *rhs = eval1(expr->as.app.rhs);
        if (rhs != expr->as.app.rhs) {
            return app(lhs, rhs);
        }

        return expr;
    default: UNREACHABLE("Expr_Kind");
    }
}

void trace_expr(Expr *expr, String_Builder *sb)
{
    sb->count = 0;
    expr_display(expr, sb);
    sb_append_null(sb);
    printf("%s\n", sb->items);
}

int main()
{
    String_Builder sb = {0};

    // (\y.(\x.y)) x => (\x.x)
    //
    // > (\y.(\x.y)) then else
    // > (\x.then) else
    // > then
    //
    // > (\y.(\x.y)) x else
    // > (\x.x) else
    // > else

    // Expr *expr = app(app(fun("y", fun("x", var("y"))), var("x")), var("else"));
    Expr *expr = app(app(fun("y_420", fun("x_69", var("y_420"))), var("x")), var("else"));

    // Expr *expr = app(
    //     fun("x", app(var("x"), var("x"))),
    //     fun("x", app(var("x"), var("x"))));

    trace_expr(expr, &sb);
    Expr *expr1 = eval1(expr);
    while (expr1 != expr) {
        expr = expr1;
        trace_expr(expr, &sb);
        expr1 = eval1(expr);
    }

    // asm("int3");
    return 0;
}
