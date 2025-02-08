---
title: 幺半群笔记
title: monoid notes
author: 谢宇恒
year: 2025
---

# 形式定义

半群是某个类型上，满足结合律的复合运算。

```scheme
(import semigroup-t semigroup)

(define-class monoid-t (semigroup-t)
  :id @element-t
  :id-left (forall ((x @element-t)) (equal-t @element-t (@mul @id x) x))
  :id-right (forall ((x @element-t)) (equal-t @element-t (@mul x @id) x)))
```

# 典型例子

## nat-monoid

自然数、加法、零，形成一个幺半群。

```scheme
(define-object nat-monoid (monoid-t)
  :element-t nat-t :mul add :id zero
  ...)
```

显然不能构成群，因为需要负数才能构成群。

## path-monoid

linux 的 relative path 的集合，构成一个幺半群。

- `/` 是复合。
- `.` 是单位元。
- `..` 可以从右边消除任意一个元素。

显然不能构成群，
因为虽然 `..` 可以从右边消除任意一个元素，
但是一般的 relative path 没有逆元。

# 幺半群作用

任意一个集合 `A` -- `(the type-t A)`，
`(-> A A)` 在函数作用下是一个幺半群。

这也是幺半群的群作用理论。
看到一个幺半群的时候，
首先就应该想想它能作用于哪个集合。
比如 relative path 能作用于 absolute path 的集合，
对某个 absolute path 求作用，
能计算出两个 relative path 是否相等。

TODO

# 参考资料

- [Monoid / Wikipedia](https://en.wikipedia.org/wiki/Monoid)
