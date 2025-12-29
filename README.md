# Lamb

Tiny Pure Functional Programming Language in C. Based on [Untyped Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) with Normal Order reduction.

## Quick Start

```console
$ cc -o lamb lamb.c
$ ./lamb ./std.lamb
...
,---@>
 W-W'
Enter :help for more info
@> pair 69 (pair 420 1337)
RESULT: \f.f 69 (\f.f 420 1337)
@> xs = pair 69 (pair 420 1337)
Created binding xs
@> first xs
RESULT: 69
@> second xs
RESULT: \f.f 420 1337
@> first (second xs)
RESULT: 420
@>
```

It's recommended to use Lamb with [rlwrap](https://github.com/hanslub42/rlwrap). Just do

```console
$ rlwrap ./lamb
```

and you get Bash-style history and command line navigation.

## Syntax

The syntax is based on the [Notation of Untyped Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus_definition#Notation) and provides few improvements over it.

### Variables

Variables are any alphanumeric names:

```
@> :ast x
[VAR] x
@> :ast hello69
[VAR] hello69
@> :ast 69420
[VAR] 69420
@>
```

Yes, fully numeric sequences of characters are also considered names, because they are alphanumeric. This may change in the future.

### Functions

To denote functions instead of small greek lambda `λ` we use backslash `\` (this may change in the future, we are considering allowing `λ` as an alternative):

```
@> :ast \x.x
[FUN] \x
+--[VAR] x
@> :ast \x.\y.x
[FUN] \x
+--[FUN] \y
   +--[VAR] x
@>
```

The body of the function extends as far right as possible. Use parenthesis to denote the boundaries of the function:

```
@> :ast \x.x x
[FUN] \x
+--[APP]
   +--[VAR] x
   +--[VAR] x
@> :ast (\x.x) x
[APP]
+--[FUN] \x
|  +--[VAR] x
+--[VAR] x
@>
```

Since the variable names can be longer than 1 character we can't use that silly mathematician trick of stitching their names together by saying that `\xy.x` means `\x.\y.x`, because in Lamb it just means it's a single function with a parameter `xy`:

```
@> :ast \xy.x
[FUN] \xy
+--[VAR] x
@>
```

Instead we allow you to drop consequent backslashes turning the dot `.` into a parameter separator:

```
@> :ast \x.y.x
[FUN] \x
+--[FUN] \y
   +--[VAR] x
@>
```

## Applications

Just separate two lambda expressions with a space:

```
@> :ast (\x.x) x
[APP]
+--[FUN] \x
|  +--[VAR] x
+--[VAR] x
@> (\x.x) x
RESULT: x
@>
```

You can drop parenthesis if the expression is unambiguous:

```
@> :ast f \x.x
[APP]
+--[VAR] f
+--[FUN] \x
   +--[VAR] x
@>
```

Applications are left-associative:

```
@> :ast f a b c d
[APP]
+--[APP]
|  +--[APP]
|  |  +--[APP]
|  |  |  +--[VAR] f
|  |  |  +--[VAR] a
|  |  +--[VAR] b
|  +--[VAR] c
+--[VAR] d
@>
```

### Magics

Magics are special names that start with `#` and cannot be used as parameters of functions (that is they are always free). They perform various useful side effects.

`#trace` - when applied to a lambda expression it becomes a potential redex. When reduced it forces the reduction of its argument, prints the reduced argument to the console, and then returns it. Useful for debugging.

```
@> (#trace f) (#trace a) (#trace b) (#trace c)
TRACE: f
TRACE: a
TRACE: b
TRACE: c
RESULT: ((f a) b) c
@>
```

`#void` - when applied to a lambda expression it becomes a potential redex. When reduced it forces the reduction of its argument and returns itself. Useful when you only care about the side effects of the evaluation but not the result which might be long and useless anyway:

```console
$ ./lamb ./std.lamb
@> xs = cons 69 (cons 420 (cons 1337 (cons foo (cons bar (cons bar nil)))))
Created binding xs
@> #void (trace_list xs)
TRACE: 69
TRACE: 420
TRACE: 1337
TRACE: foo
TRACE: bar
TRACE: bar
RESULT: #void
```

## Bindings

You can take any expression and assign it to a name:

```
@> id = \x.x
Created binding id
@>
```

Then you can use that name instead of the expression:

```
@> id 69
RESULT: 69
@>
```

You can reassign the existing bindings at any moment

```
@> id = \y.y
Updated binding id
@>
```

To list all available bindings use `:list` command:

```
@> a = 69
Created binding a
@> b = 420
Created binding b
@> c = 1337
Created binding c
@> :list
id = \y.y;
a = 69;
b = 420;
c = 1337;
@>
```

### Semantics of Bindings

The bindings are not evaluated until you use them in an expression.

When you enter an expression which contains the names of the bindings into the REPL to evaluate, first things that happens is the binding names are substituted with their corresponding values before starting the evaluation. The bindings are substituted in a reversed order, so be careful with the order in which you create the bindings. Reassigning already existing bindings does not change their order. You have to `:delete` the binding first and create it again to affect its order.

Since each binding is applied only once you CANNOT use them for recursion

```
@> loop = loop
Created binding loop
@> loop
RESULT: loop
@>
```

Use [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator) to organize the recursion:

```
@> Y = \f.(\x.f (x x)) (\x.f (x x))
Created binding Y
@> :debug  Y g
DEBUG: (\f.(\x.f (x x)) (\x.f (x x))) g
DEBUG: (\x.g (x x)) (\x.g (x x))
DEBUG: g ((\x.g (x x)) (\x.g (x x)))
DEBUG: g (g ((\x.g (x x)) (\x.g (x x))))
DEBUG: g (g (g ((\x.g (x x)) (\x.g (x x)))))
DEBUG: g (g (g (g ((\x.g (x x)) (\x.g (x x))))))
DEBUG: g (g (g (g (g ((\x.g (x x)) (\x.g (x x)))))))
DEBUG: g (g (g (g (g (g ((\x.g (x x)) (\x.g (x x))))))))
DEBUG: g (g (g (g (g (g (g ((\x.g (x x)) (\x.g (x x)))))))))
DEBUG: g (g (g (g (g (g (g (g ((\x.g (x x)) (\x.g (x x))))))))))
...
```

### Managing the Bindings Files

You can save current bindings to a file with `:save` command:

```
@> id = \x.x
Created binding id
@> const = \x.y.x
Created binding const
@> true = const
Created binding true
@> false = \x.y.y
Created binding false
@> :save main.lamb
Saved all the bindings to main.lamb
@> :q
$ cat main.lamb
id = \x.x;
const = \x.y.x;
true = const;
false = \x.y.y;
$
```

You can load the bindings with the `:load` command:

```
@> :load main.lamb
Created binding id
Created binding const
Created binding true
Created binding false
@> :list
id = \x.x;
const = \x.y.x;
true = const;
false = \x.y.y;
@>
```

Loading the bindings clears out all the previously defined bindings.

IMPORTANT! The bindings in the bindings file are separate with a semicolon `;`.

Repeating `:load` or `:save` without an argument applies it the last saved or loaded file (a.k.a. the active file).

Use `:edit` command to open and edit the bindings file in an external editor. The default editor is `vi` but you can set it with `$EDITOR` and `$LAMB_EDITOR` environment variable.

We provided a bunch of useful bindings in [std.lamb][./std.lamb].

Passing a file as a command line argument to the interpreter acts as if you instantly `:load`-ed it.

```console
$ ./lamb ./main.lamb
Created binding id
Created binding const
Created binding true
Created binding false
,---@>
 W-W'
Enter :help for more info
@> :list
id = \x.x;
const = \x.y.x;
true = const;
false = \x.y.y;
@>
```

The file also automatically becames the active file. So `:load`, `:save`, and `:edit` will operate on it by default.

The interpreter can only work with one active file at a time right now.
