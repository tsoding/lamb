# Lamb

Smallest Pure Functional Programming Language in C. Based on [Untype Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) with Normal Order reduction.

## Quick Start

```console
$ cc -o lamb lamb.c
$ ./lamb ./std.lamb
...
,---@>
 W-W'
Enter :help for more info
@> pair 69 (pair 420 1337)
RESULT: \f.(f 69) (\f.(f 420) 1337)
@> xs = pair 69 (pair 420 1337)
Created binding xs
@> first xs
RESULT: 69
@> second xs
RESULT: \f.(f 420) 1337
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

The syntax is based on the [Notation of Untype Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus_definition#Notation) and provides few improvements over it.

### Variables

Variables are any alphanumeric names:

```
@> x
RESULT: x
@> hello69
RESULT: hello69
@> 69420
RESULT: 69420
@>
```

Yes, fully numeric sequences of characters are also considered names, because they are alphanumeric. This may change in the future.

### Functions

To denote functions instead of small greek lambda `λ` we use backslash `\` (this may change in the future, we are considering allowing `λ` as an alternative):

```
@> \x.x
RESULT: \x.x
@> \x.\y.x
RESULT: \x.\y.x
@>
```

The body of the function extends as far right as possible. Use parenthesis to denote the boundaries of the function:

```
@> \x.x x
RESULT: \x.x x
@> (\x.x) x
RESULT: x
@>
```

Since the variable names can be longer than 1 character we can't use that silly mathematician trick of stitching their names together by saying that `\xy.x` means `\x.\y.x`, because in Lamb it just means it's a single function with a parameter `xy`:

```
@> \xy.x
RESULT: \xy.x
@> (\xy.x) z
RESULT: x
@>
```

Instead we allow you to drop consequent backslashes turning the dot `.` into a parameter separator:

```
@> \x.y.x
RESULT: \x.\y.x
@> (\x.y.x) z
RESULT: \y.z
@>
```

## Applications

Just separate two lambda expressions with a space:

```
@> (\x.x) x
RESULT: x
@>
```

You can drop parenthesis if the expression is unambiguous:

```
@> f \x.x
RESULT: f (\x.x)
@>
```

Applications are left-associative:

```
@> f a b c d
RESULT: (((f a) b) c) d
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
