---
title: How to not design syntax
---

```cicada
claim f: (x: A, y: B(x)) -> C(x, y)
```

```scheme
(claim f (Pi ([x A] [y (B x)]) (C x y)))

(claim f
  (Pi ([x A]
       [y (B x)])
    (C x y)))
```
