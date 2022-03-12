---
title: How to not design syntax?
date: 2022-03-12
---

First of all, Why not to design syntax?

Because syntax design can be an endless tormenting circle,
in which no real progress can be made.

How to not design syntax?

1. Use Sexp, keep lexer and parser simple.
2. Use prefix notation wholeheartedly, and be consistent.
3. Add more parentheses if needed, they are lovely.

Examples:

```javascript
claim f: (x: A, y: B(x)) -> C(x, y)

claim f: (
  x: A,
  y: B(x),
) -> C(x, y)
```

```scheme
(claim f (Pi ([x A] [y (B x)]) (C x y)))

(claim f
  (Pi ([x A]
       [y (B x)])
    (C x y)))
```
