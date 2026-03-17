---
title: howe's method for higher order languages
author: andrew pitts
year: 2012
---

# My Motive

[2026-03-17] 从简介看来，
这篇论文处理了允许递归定义函数的 lambda calculus 中，
如何判断函数之间的等价的问题。

## 5.1 Introduction

> A fundamental requirement for any notion of equality between
> programs is that it be a _congruence_, in other words an equivalence
> relation that is compatible with the various syntactical constructs
> of the programming language.

> If one regards the meaning of a program to be its ≅-equivalence
> class, then compatibility says that this semantics is _compositional_
> -- the meaning of a compound phrase is a function of the meanings of
> its constituent sub-phrases.

> This book is concerned with coinductively defined notions of program
> equality based on the notion of bisimulation. For these, the
> property of being an equivalence relation is easy to establish,
> whereas the property of compatibility can sometimes be difficult to
> prove and indeed in some cases is false.

用 bisimulation 来定义函数之间的等价关系
-- 称作 applicative bisimilarity，
然后证明这个等价关系是 congruence 的。

> Howe’s definitive account of his method [How96] uses a general
> framework that can be specialised to a number of different
> functional languages and evaluation strategies. Here we prefer to
> explain the method by giving some concrete examples of it in
> action. To see the wood from the trees, the examples are as
> syntactically simple as possible.

> In fact we use applicative similarity and bisimilarity for the
> untyped λ-calculus with a call-by-value evaluation strategy as the
> running example; ...

正好是我在 lambda-lisp 中想要实现的。

> Section 5.6 explains an important consequence of this congruence
> result, namely that applicative bisimilarity for this language
> coincides with a Morris-style [Mor69] contextual equivalence, in
> which terms of the language are identified if they behave the same
> in all contexts (with respect to some fixed notion of observation).

这里的引用是：

- [Mor69] J. Morris.
  Lambda calculus models of programming languages.
  PhD thesis, MIT, 1969.

## 5.2 Call-by-value λ-calculus

从这里的介绍看来，不会直接讨论带有递归定义的 λ-calculus。

## 5.3 Applicative (bi)similarity for call-by-value λ-calculus

先定义 applicative simulation，
然后用 coinductive definition 定义 applicative similarity。

由于定义中的 forall ... exists ... 之后，又跟着一个 forall，
所以这个定义好像没法用来实现判定算法。

也许这个最后一个 forall 可以通过一个 fresh variable 来处理？
如果这样处理，就等价于 NbE 了！

在 luca 的 recursive subtype 算法中，
重点在于，遇到递归类型时，可以选择展开递归定义与否，
这类似 LTS 中的 non-deterministic。

在判断 lambda term 之间的等价时，有更多的选择：

- 展开递归定义。
- 展开 lambda reduction。

TODO

lambda-lisp -- save DelayedApply in trail!
