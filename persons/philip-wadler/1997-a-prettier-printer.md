---
title: a prettier printer
author: philip wadler
year: 1997
---

# My Motive

[2025-10-06] 在实现 x-lisp，好久没读论文了。
需要给 x-lisp 实现 `pretty-print` 函数，
随便搜索了一下，竟然有相关的论文。

看来在 CS 领域，能想到的 worth solving problem 都有论文。

# Intro

> Joyce Kilmer and most computer scientists agree: there is no poem as
> lovely as a tree. In our love affair with the tree it is parsed,
> pattern matched, pruned — and printed. A pretty printer is a tool,
> often a library of routines, that aids in converting a tree into
> text. The text should occupy a minimal number of lines while
> retaining indentation that reflects the underlying tree. A good
> pretty printer must strike a balance between ease of use,
> flexibility of format, and optimality of output.

> Over the years, Richard Bird and others have developed the algebra
> of programming to a fine art. John Hughes (1995) describes the
> evolution of a pretty printer library, where both the design and
> implementation have been honed by an appealing application of
> algebra.

Richard Bird 有一本书就叫做 the algebra of programming，
另外他还和 philip wadler 一起写过 haskell 的书，
都值得一读。

John Hughes (1995) 的引用是：

- John Hughes.
  The design of a pretty-printer library.
  In J. Jeuring and E. Meijer, editors,
  Advanced Functional Programming,
  Springer Verlag LNCS 925, 1995.

> A widely used imperative pretty-printer is
> described by Derek Oppen (1980).

这里的引用是：

- Derek Oppen.
  Pretty-printing.
  ACM Transactions on Programming Languages and Systems, 2(4): 1980.

> The pretty printer presented here uses an algorithm equivalent to
> Oppen’s, but presented in a functional rather than an imperative
> style.  Further comparison with Hughes’s and Oppen’s work appears
> in the conclusions.

能够把 imperative code 变成 algebric code，
到底是什么意思？
读 Richard Bird 那本书应该可以系统学习到。

# 1 A simple pretty printer

TODO
