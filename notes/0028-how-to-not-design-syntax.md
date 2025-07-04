---
title: How to not design syntax?
date: 2022-03-12
---

First of all, Why not to design syntax?

Because syntax design can be an endless tormenting circle,
in which no real progress can be made.

How to not design syntax?

1. Use [S-exp](https://en.wikipedia.org/wiki/S-expression), keep lexer and parser simple.
2. Use prefix notation wholeheartedly, and be consistent.
3. Add more parentheses if needed, they are lovely.

# Examples

## Introducing variable into scope

When want to introduce a variable into the scope,
use the syntax like `(lambda)`

```scheme
(lambda (x) x)
```

## Introducing typed variable

When want to introduce a typed variable binding,
use the syntax like `(Pi)`

```scheme
(Pi ((x A)
     (y (B x)))
  (C x y))
```

For example, if we want to make typed lambda calculus a la Church,
we can write:

```scheme
(lambda ((x A)) x)
```

## Key-Value binding

Use syntax of [Association list](https://en.wikipedia.org/wiki/Association_list)
to denote Key-Value binding.

```scheme
(object
  (x 1)
  (y 2))
```
