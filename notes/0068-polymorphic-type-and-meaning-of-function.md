---
title: polymorphic type and meaning of function
date: 2026-03-13
---

# polymorphic type 违背集合论直觉

考虑 untyped lisp，
人们自然会去考虑数据组成的集合，
函数也在集合论的意义下，被理解为集合之间的函数。

这样就会自然产生一种起源于集合论直觉的类型系统。

但是 parametric polymorphic type 会完全破这种直觉：

```scheme
(claim identity (polymorphic (A) (-> A A)))
(define identity (lambda (x) x))
```

`identity` 既是 `(-> int-t int-t)` 的元素，
也是 `(-> string-t string-t)` 的元素。
但是从集合论的角度看，这两个函数的集合是没有交集的，
因为集合论意义上的函数是特殊的关系，
而关系是笛卡尔积的子集，
`int-t` 和 `string-t` 既然没有交集，
那么 `int-t x int-t` 和 `string-t x string-t` 也没有交集。

这个悖论会引发很多问题。
比如，如果我们的类型系统中有类型的 inter（intersection） 操作，
那么 `(-> int-t int-t)` 和 `(-> string-t string-t)` 的 inter 是否空？

# 用 church 风格的 expression 来解决悖论

我们说 `(lambda (x) x)` 不算 expression，
只有带有类型的 `(the (-> int-t int-t) (lambda (x) x))` 才算 expression。

这就是 curry 和 church 对类型系统的理解方式的差异。

但是这样没法理解下面三个类型之间的关系：

```scheme
(polymorphic (A) (-> A A))
(-> string-t string-t)
(-> int-t int-t)
```

# 用类型参数来解决悖论

可以让 `identity` 在作用时必须带上类型参数。

不能写：

```scheme
(identity 1)
(identity "a")
```

必须写：

```scheme
(identity int-t 1)
(identity string-t "a")
```

也就是在 `lambda` 之外又类似 `type-lambda` 的东西。

```scheme
(claim identity (polymorphic (A) (-> A A)))
(define identity (type-lambda (A) (lambda (x) x)))
```

但是这是不合理的，因为这要求了类型参数的顺序。

考虑下面的三个函数，可以发现类型变量应该是自动引入的变量，
而不应该作为参数显式传递：

```scheme
(claim swap
  (polymorphic (X Y Z)
    (-> (-> Y X Z) (-> X Y Z))))

(define (swap f x y) (f y x))

(claim drop
  (polymorphic (X Y Z)
    (-> (-> X Z) (-> Y X Z))))

(define (drop f)
  (lambda (dropped)
    (lambda (x)
      (f x))))

(claim dup
  (polymorphic (X Z)
    (-> (-> X X Z) (-> X Z))))

(define (dup f)
  (lambda (x)
    (f x x)))
```

# 通过完全抛弃集合论对函数的定义来解决悖论

在集合论中，人们把函数视为笛卡尔积的子集，
是因为人们所关注的是函数的输入输出行为。

而在程序语言中，函数作为对参数的操作（算法），
除了输入输出行为之外，其核心信息在于其所描述的算法。

expression 或 value 与 type 之间的关系，是二元关系。
因此通过固定 type，总能引出一个 expression 或 value 的集合。
这个集合才是 type 所自然对应的集合。

在这种考量下，`identity` 即属于 `(-> string-t string-t)`，
又属于 `(-> int-t int-t)`，
这两个集合的交集并不为空。

`(polymorphic (A) (-> A A))` 本身，也代表一个集合。

人们所关心的，函数的输入输出行为所构成的笛卡尔积，
应该被称为函数的 "behavioral record"，
不应该被视为函数本身所唯一对应的集合。

# 关于数学基础

这就回到了 0055-foundation-of-mathematics.md 中的讨论。

数学基础对集合论的要求应该是：

- （1）有高效的算法来判断任意元素是否为某集合中的元素。
- （2）有高效的算法来某集合中的任意两个元素是否相等。
