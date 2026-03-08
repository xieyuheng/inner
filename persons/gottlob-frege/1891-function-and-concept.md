---
title: function and concept
author: gottlob frege
year: 1891
---

# My Motive

[2026-03-09] 在 1987-the-implementation-of-functional-programming-languages.md 中，
在 chapter 8 polymorphic type-checking，
peter hancock 在介绍程序语言的中的类型与数学中的类型的差异时，
引用了这篇文章，说是描述数学中的「类型」概念的。

# function and concept

数学中有「表达式」与「值」之分，
表达式是能指，值是所指，
不同的表达式经过求值，可能指向同一个值。

函数是带有空缺的表达式，
和值拼起来就形成新的值。

在写解释器时，人们对 Exp Value 和 Closure 的理解正是如此。

从写程序的角度看，表达式看来可以认为是对值的操作，也就是算法。
而从数学角度看，不同的函数尽管代表了不同的操作，
但是所得到的函数曲线可能是完全一样的。
这相当于是给程序意义上的函数之间，
定义了一种更粗的一种等价关系。

也就是说，两个函数在所有可能的输入下都产生相同的输出，
称作 behavioral equivalence，或者说 extensional equivalence。
相对应的，函数之间作为算法之间的相等，
称作 operational equivalence，或者说 intensional equivalence。

不同的表达式可能代表相同的值，而值才是数学对象。
因此，frege 认为，从数学角度看，
如果想把函数视为值，就必须采取后一种等价关系。

为了清晰表达函数之间的等价关系，
并且把函数当成值，就需要设计类似 lambda 的语法，
并且引入 bound variable 的概念。
这篇文章就是 lambda 表达式的起点。

研究函数的 behavioral equivalence 的同时，
也要研究函数的 operational equivalence，
因为不同的 operation 代表了不同的算法。
正如研究 value 的结构的同时，
也要研究 expression 的结构，
因为不同的 expression 代表了不同的 thought。

这篇文章所表达的知识，在现在看来已经是常识了。
重要的是学习 frege 探究知识的方式。

标题中所说的 concept，
就是作为 boolean valued function 的 predicate 所代表的集合。

type 作为集合与 predicate 作为集合的差异是什么？
也许在于，我们可以判断 type 之间是否等价（或者形成子类型关系），
而不能判断 predicate 所代表的集合之间是否等价。
所谓「可以判断」在于我们可以实现一个算法来判断。

对于 dependent type 而言，
我们显然不能实现算法来判断 type 之间是否等价了，
因为我们不能实现算法来判断 function 之间是否 behavioral equivalence。
但是我们可以实现算法来判断 operational equivalence，
然后把 behavioral equivalence 表达为有待证明的命题，
只有这样 dependent type system 才成立。

命题不限于 predicate 所代表的集合，
而可以是任何有待被判定真假的 statement。
像分析表达式与函数一样，
来分析一般的 statement、concept 与 thought，
可能就是分析哲学的起点。

比如，类型错误的表达式是没法 evaluate 成功的，
类型错误就对应于思想的错误。
