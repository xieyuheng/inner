---
title: a beginner s guide to mathematical logic
author: raymond smullyan
year: 2014
---

# 学习动机

[2025-01-05] 想要读 Yves Lafont 的 2017-linear-logic-pages，
但是感觉需要复习一下命题逻辑和谓词逻辑。

# Part I General Background

## Chapter 1 Genesis

> Just what is Mathematical Logic?
> More generally, what is Logic,
> whether mathematical or not?

> Many people have asked me what mathematical logic is, and what its
> purpose is. Unfortunately, no simple definition can give one the
> remotest idea of what the subject is all about. Only after going
> into the subject will its nature become apparent. As to purpose,
> there are many purposes, but again, one can understand them only
> after some study of the subject. However, there is one purpose that
> I can tell you right now, and that is to make precise the notion of
> a proof.

> It is important to understand the difference between a syllogism
> being valid and a syllogism being sound. A valid syllogism is one in
> which the conclusion is a logical consequence of the premises,
> regardless of whether the premises are true or not. A sound
> syllogism is a syllogism which is not only valid, but in which, in
> addition, the premises are true.

> I am amused by syllogisms that appear to be obviously invalid,
> but are actually valid!

```
Everyone loves my baby.
My baby loves only me.
----------------------------
Therefore, I am my own baby.
```

哈哈。

提到了布尔在 "An Investigation of the Laws of Thought" 中，
尝试分析斯宾诺莎的推理的有效性，而不在乎合理性。
感觉布尔这本书也值得看看，因为我也喜欢斯宾诺莎。

> The beginnings of mathematical logic went pretty much hand in hand
> with the nineteenth century development of set theory --
> particularly the theory of infinite sets founded by the remarkable
> mathematician Georg Cantor. Before discussing infinite sets, we must
> first look at some of the basic theory of sets in general.

要讲命题演算和谓词演算就必须介绍集合论。

- 对于 linear logic 来说，扮演集合论位置的是什么呢？
  我希望对 linear logic 之类的非古典逻辑，
  我也能获得和古典逻辑一样稳定的直觉。

在介绍 Complementation 时提出了 the universe of discourse 的概念：

> We now consider as fixed for the discussion a set I,
> which we will call the universe of discourse.

这对于区分类型论和谓词演算而言很重要。

> What the set I is will vary from one application to another. For
> example, in plane geometry, I could be the set of all points on a
> plane. For number theory, the set I could be the set of all whole
> numbers. In applications to sociology the set I could be the set of
> all people.

就 proof as program 而言，
这种需要约定 context 的情形，
类似于使用 dynamic scope 的 variable。

> The operations of union, intersection and complementation are the
> fundamental Boolean operations on sets. Other operations are
> definable by iterating these fundamental operations.

> For example, the set denoted A – B [the so called difference of A
> and B], which is the set of all elements of A that are not in B, can
> be defined in terms of the three fundamental operations, since A – B
> = A ∩ B~.

但是其实 difference 的定义并不依赖 context，
而 complementation 的定义依赖 context。

下面讨论 boolean equations，
首先要定义 boolean term，
这就是我们在写解释器时最熟悉的 Exp。

> We will use the capital letters A, B, C, D, E, with or without
> subscripts, to stand for arbitrary sets (just as in algebra we use
> the lower-case letters x, y, z to stand for arbitrary numbers).  We
> call these capital letters (with or without subscripts) _set
> variables_.  By a _term_ we mean any expression constructed
> according to the following rules:

> - (1) Each set variable standing alone is a term.

> - (2) For any terms t1 and t2,
>   the expressions (t1 ∪ t2), (t1 ∩ t2),
>   and t1~ are again terms.

> A Boolean equation is called _valid_ if it is true no matter what sets
> the set variables represent.

TODO
