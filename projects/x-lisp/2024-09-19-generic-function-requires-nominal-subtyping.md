---
title: Generic function requires nominal subtyping
date: 2024-09-19
---

generic function 要求我们用 nominal subtyping，
而不能用 structural subtyping。

因为我们需 nominal 信息来在运行时做 dispatching。

```scheme
(define (display (self bordered-figure))
  (display (as-figure self))
  (display-border self))
```
