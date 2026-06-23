---
title: modern compiler implementation in ML
author: andrew w appel
year: 1998
---

# My Motive

[2026-06-21] 看了 1998-ssa-is-functional-programming-andrew-w-appel.pdf，
发现写的非常好，并且末尾有对这本书的广告。

在选择编译器 IR，和学习 SSA 的过程中，
我首先需要了解 SSA 所解决的问题，
而不应该直接学习 SSA。

也就是首先应该学习具体优化问题的例子。

# [2026-06-23] meta-lisp vs ML

用 lisp 语法框架，可以避免这本书前半部分的内容。

除了实现编译器方面的简化，
与 ML 相对比，meta-lisp 的设计本身就是删除「对程序语言的需求」的过程：

- 删除 dot 语法，删除用 lvalue 语法实现的副作用
- 删除带有参数的模块系统
- 删除 exception 机制

elon musk's five steps algorithm for engineering

- (1) make the requirements less dumb,
  the requirements are always dumb in some degree.
  first reduce the number of requirements,
  otherwise you get the perfect answer to the wrong question.
- (2) try to delete the (process) steps,
  be not afraid to delete and put back in.
- (3) only then, simplify it.
- (4) any given thing can be speed up.
  but speeding up something should not exist is absurd.
- (5) to automate it.

the order is the key, imagine doing it backward:

- automate something,
- sped it up,
- simplify it,
- and delete it.

# 5 Semantic Analysis

> The semantic analysis phase of a compiler connects variable
> definitions totheir uses, checks that each expression has a correct
> type, and translates theabstract syntax into a simpler
> representation suitable for generating machinecode.

## 5.1 SYMBOL TABLES

> This phase is characterized by the maintenance of _symbol tables_
> (also called _environments_) mapping identifiers to their types and
> locations. As the declarations of types, variables, and functions
> are processed, these identifiers are bound to "meanings" in the
> symbol tables. When _uses_ (nondefining occurrences) of identifiers
> are found, they are looked up in the symbol tables.

也就是 meta-lisp 中全局的 mod，以及局部的 env 和 ctx。

# 7 Translation to Intermediate Code

TODO

# Appendix: Tiger Language Reference Manual

一个有简化版的 ML 语法风格的 C：

- 没有范型只有简单类型。
- 有 while 和 for 循环。
- 有 record 和 dot syntax，用 lvalue 和 dot syntax 的做副作用。
