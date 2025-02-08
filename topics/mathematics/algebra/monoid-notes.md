---
title: 幺半群笔记
title: monoid notes
author: 谢宇恒
year: 2025
---

# 形式定义

```scheme
(import semigroup-t semigroup)

(define-class monoid-t (semigroup-t)
  :id @element-t
  :id-left (forall ((x @element-t)) (equal-t @element-t (@mul @id x) x))
  :id-right (forall ((x @element-t)) (equal-t @element-t (@mul x @id) x)))
```

# 典型例子

TODO

# 幺半群作用

TODO

# 参考资料

- [Monoid / Wikipedia](https://en.wikipedia.org/wiki/Monoid)
