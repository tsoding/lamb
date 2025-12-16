# Lamb

Smallest Pure Functional Programming Language in C. Based on [Untype Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) with Normal Order reduction.

> [!WARNING]
> The language currently does not have any memory management and leaks a lot of memory on each reduction. Garbage Collection will be implemented later.

## Quick Start

```console
$ cc -std=c99 -o lamb lamb.c
$ ./lamb
λ> (\f.(\x.f (x x)) (\x.f (x x))) g
...
```
