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
  :compose (-> @element-t @element-t @element-t)
  :compose-associative
  (forall ((x @element-t)
           (y @element-t)
           (z @element-t))
    (equal-t @element-t
      (@compose x (@compose y z))
      (@compose (@compose x y) z))))
```

# 典型例子

TODO
