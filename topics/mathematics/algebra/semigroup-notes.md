---
title: 半群笔记
title: semigroup notes
author: 谢宇恒
year: 2025
---

# 形式定义

```scheme
(define-class semigroup-t ()
  :element-t type-t
  :mul (-> @element-t @element-t @element-t)
  :mul-associative
  (forall ((x @element-t)
           (y @element-t)
           (z @element-t))
    (equal-t @element-t
      (@mul x (@mul y z))
      (@mul (@mul x y) z))))
```

# 典型例子

TODO
