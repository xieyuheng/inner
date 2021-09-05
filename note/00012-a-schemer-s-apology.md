---
title: A schemer's apology
date: 2021-09-05
---

> What is good software library API design?

I asked this question,
maybe because of I am not satisfied with shallow embedding,
maybe I want some syntax like:

``` scheme
(define-space torus
  (0 origin)
  (1 (toro origin origin)
     (polo origin origin))
  (2 (spoke (a toro)
            (b polo)
            (c (inverse toro))
            (d (inverse polo)))))
```

But I can not afford to go back to scheme or lisp.
