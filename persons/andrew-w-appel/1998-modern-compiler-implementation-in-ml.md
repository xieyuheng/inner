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

# 6 Activation Records

介绍了三种处理嵌套的函数的方式：

- static links
- displays
- lambda lifting

这里选择实现的是 static links。

> Should we use displays instead of static links? Perhaps; but the
> issue is more complicated. For languages such as Rascal and Tiger
> with block structure but no function variables, displays work well.

> But the full expressive power of block structure is obtained when
> functions can be returned as results of other functions, as in
> Scheme and ML. For such languages, there are more issues to consider
> than just variable-access time and procedure entry-exit cost: there
> is closure-building cost, and the problem of avoiding useless data
> kept live in closures. Chapter 15 explains some of the issues.

# 7 Translation to Intermediate Code

这里定义的 TREE IR 分 statement 和 expression 两层，
其中 expression 可以相互嵌套，
甚至 expression 内可以嵌套 statement -- 类似 `begin`。

这甚至还不如 EOC 中的 IR，
其中 expression 不能嵌套，
并且 expression 内不能嵌套 statement。

# 8 Basic Blocks and Traces

## 8.1 CANONICAL TREES

在 TREE 内部取消嵌套。

TREE 内部的转写被理解为 term rewriting system。

# 9 Instruction Selection

> The intermediate representation (Tree) language expresses only
> oneoperation in each tree node: memory fetch or store, addition or
> subtraction,conditional jump, and so on.  A real machine instruction
> can often perform severalof these primitive operations.

这里的思路不是想如何把 IR 翻译为 instructions，
而是反过来看 instructions 所对应的 IR。

> We can express a machine instruction as a fragment of an IR tree,
> called a _tree pattern_. Then instruction selection becomes the task
> of tiling the tree with a minimal set of tree patterns.

> The fundamental idea of instruction selection using a tree -- based
> intermediate representation is _tiling_ the IR tree. The _tiles_ are
> the set of tree patterns corresponding to legal machine instructions,
> and the goal is to cover the tree with nonoverlapping tiles.

这无疑是把可以用简单的翻译函数解决的问题，
转化为了需要复杂算法解决的问题。

这里提到的 maximal munch 算法，就是简单的翻译函数。

这里还提到了 tiling 问题对 CISC 和 RISC 的区别，
其实现代的 CISC 的 CPU 在拿到指令之后，
也是先翻译成更简单的指令然后才运行的。

# 17 Dataflow Analysis

TODO

# Appendix: Tiger Language Reference Manual

一个有简化版的 ML 语法风格的 C：

- 没有范型只有简单类型。
- 有 while 和 for 循环。
- 有 record 和 dot syntax，用 lvalue 和 dot syntax 的做副作用。
