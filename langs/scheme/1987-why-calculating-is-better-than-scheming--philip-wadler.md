---
title: Why calculating is better than scheming
subtitle: A critique of Abelson and Sussman
author: Philip Wadler
year: 1987
---

# 笔记

可以设想一个类似 Haskell 的语言，
但是以 `f(x)` 而不是 `f x` 为函数作用的语法。

```cicada
sum: (implicit A: Type, List(A)) -> Number
sum(List::Null) = 0
sum(List::Cons(head, tail)) = add(head, sum(tail))
```

其中 `List` 的定义如下：

```cicada
datatype List(A) {
  Null: List(A)
  Cons(head: A, tail: List(A)): List(A)
}
```

对比类似 JS 的语法，可以说类似 Haskell 的语法是为 match 而优化的。

```cicada
function sum(implicit A: Type, list: List(A)): Number {
  match (list) {
    case List::Null => 0
    case List::Cons(head, tail) => add(head, sum(tail))
  }
}
```

# 1. Data types

说类 Haskell 的语法比 lisp 容易理解，完全是偏见。

- 首先纯前缀表达式比前缀+中缀表达式简单，
- 其次纯 sexp 比一般的具体语法简单。

Haskell 把 Pattern match 以 implicit 的方式内置在语言里，
而不用 explicit 的处理，确实是优点。
但是只需要给 lisp 加 `match` 关键词就可以了。
