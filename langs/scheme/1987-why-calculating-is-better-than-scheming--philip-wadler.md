---
title: Why calculating is better than scheming
subtitle: A critique of Abelson and Sussman
author: Philip Wadler
year: 1987
---

可以设想一个类似 Haskell 的语言，
但是以 `f(x)` 而不是 `f x` 为函数作用的语法。

```cicada
datatype List(A) {
  Null: List(A)
  Cons(head: A, tail: List(A)): List(A)
}

sum: (implicit A: Type, List(A)) -> Number
sum(List::Null) = 0
sum(List::Cons(head, tail)) = add(head, sum(tail))
```
