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

## 1.4. Data representation and abetract data types

强调了 pattern match 只能用于具体的 data type，
比如 algebraic data type（free data type），
而不能用于抽象的 data type。

非 free data type 的例子是集合。

## 1.5. A last word on the mobile exercise

Lisp 没有让用户自定义 data type 的机制。
只能用 sexp + selector functions 来模拟自定义 data type 的过程。

这正是违反了 Sussman 说的某些机制应该在语言层面 implicit 处理。

但是这却遵循了 R5RS 开篇说的：

> Programming languages should be designed not by piling feature on
> top of feature, but by removing the weaknesses and restrictions that
> make additional features appear necessary. Scheme demonstrates that
> a very small number of rules for forming expressions, with no
> restrictions on how they are composed, suffice to form a practical
> and efficient programming language that is flexible enough to
> support most of the major programming paradigms in use today.

两个原则确实相互矛盾了，重点在于 trade-off。

# 2. Confusion between program and data

## 2.1. Lisp list are not self-quoting

这里的对这个 Lisp feature 的批评并不成立。

只是揭露了作者自己的教学水平不行。

> In this case, perhaps one can afford the extra effort. But the
> problem is greatly compounded when one must explain this sort of
> thing in the middle of some other derivation. I have encountered
> this sort of problem many times.

应该先打好基础，再介绍进一步的概念，在教学任何东西的时候都一样。

## 2.5. Syntax

对这一章，我的理解是，作者假装自己喜欢 Lisp，然后编故事来抹黑 Lisp。

整个 2. 章 的批评都站不住脚。

# 3. Programs that manipulate programs

Lisp 让人们可以不用写 parser 就能直接处理用 sexp 表示的具体语法，
但是作者故意在使用 Lisp 写解释器的时候还用 ADT 而没有用具体语法。

# 4. Lazy evaluation

Lazy evaluation 确实让语言更靠近数学。

但是 Lazy evaluation 的代价是，
语言必须是没有副作用的纯函数式。

确实某些需要被定义为语法关键词的东西，
比如 if，在 lazy evaluation 下可以被定义为函数。

macro 很多时候都是在处理 lazy evaluation。

除了 lazy evaluation 之外，macro 经常处理的是 bindings，
也许 nominal logic 可以让我们把 bindings 也变成语言内部的机制，
而避免用 macro 才能处理。

# 结论

最后作者还找 Abelson 和 Sussman 来点评自己的批评了，
可见 Abelson 和 Sussman 的格局是很高的。
