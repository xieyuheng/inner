---
title: Constraints
authors: [Gerald Jay Sussman, Guy Lewis Steele Jr.]
year: 1979
---

# 动机

这篇论文是 propagator 论文的前身，
没想到两篇论文竟然隔了三十多年。

这篇论文是 Sussman 设计语言的例子，
我们当然可以从 Sussman 的设计中学到很多语言设计的知识。

# The Language of Constraints

> A language is a means of communication of ideas.  A language
> generally has a "theme", the class of ideas which it is optimized
> for communicating. For example, most computer languages are designed
> for expressing algorithms. They are optimized for communicating
> imperative, procedural notions. The theme of the constraint language
> is declarative. It is good for expressing descriptions of structural
> relationships and physical situations.

但是并不是每个语言都需要被实现为有具体语法的语言，
我们可以首先尝试把这个语言浅嵌入到 JS 中，
这样可以避免处理具体语法，缩减实验的成本。

当想要设计具体语法的时候，
也许我们可以使用之前设计 inet 语言时的方案，
重载函数作用，来表示 build propagator 的过程。
比如，currying 的时候，参数不足，就把不足的位置作为 cell 返回。

## Simple Constraints

TODO 设计嵌入在 JS 中的语法。

## Networks of Constraints

没有像逻辑式语言一样，用同名的变量来代表连接，
可能是为了避免给很多 cell 命名。
但是逻辑式语言的实践告诉我们，使用同名变量是没问题的。

TODO 设计嵌入在 JS 中的语法。

TODO 设计类似 inet 的具体语法，JS 和 Lisp 各一个版本。

TODO 对比两种风格的语法。
