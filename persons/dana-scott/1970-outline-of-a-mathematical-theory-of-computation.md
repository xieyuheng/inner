---
title: outline of a mathematical theory of computation
author: dana scott
year: 1970
---

# 学习动机

[2025-01-02] 想要理解 linear logic 的起源：
coherence semantics of the sum，
但是这是一种 denotational semantics，
所以需要先理解 denotational semantics。

# Abstract

> This paper introduces a theory of computation which is mathematical
> rather than operational in its approach.  The theory is based on the
> idea that data types can be partially ordered by a relation similar
> to that of approximation, and as a result can be considered as
> complete lattices.  properties of these lattices and the functions
> on them are discussed in an informal manner and shown to be very
> suitable for developing a mathematical theory of the semantics of
> programming languages.  A preliminary result of this approach is the
> construction of the first "mathematical" model for the A-calculus.

# O. Introduction

看起来像是一个不懂什么是 abstraction 的程序员，
认为既然函数使用一句句指令实现的，
在思考函数的时候就必须去思考一句句指令；
认为既然数据是由电子计算机实现的，
在思考数据的时候就必须去思考 bit pattern。

> Mathematical semantics tries to avoid these irrelevancies and should
> be more suitable to a study of such problems as the _equivalence_ of
> programs.

这并不是说去实现一个函数来判断 lambda term 之间的等价，
而是抽象掉 lambda term 的实现方式，来定义等价。
感觉和什么也没说一样。

# 1. The Problem of Self-Application

> Since a function (say, mapping integers to generally in itself an
> infinite object. it also becomes necessary to introduce some idea of
> finite approximation -- just as we do in a sense for real numbers.

看来 Dana 很喜欢无穷小分析的算术化。
但是 real number 是完全脱离计算机科学的。

这一章还提到了如何用函数来理解读取内存，
以及无类型的指针，甚至不是指针而是函数名。

# 2. Data Types and Mappings

> Getting down to particulars, we must ask: what exactly is a data
> type?  To simplify matters, we can identify a data type with the set
> D of all objects of that type.

> Now this structuring must not be confused with the idea of data
> structurts (lists, trees, graphs, etc.).  The kind of structure
> being discussed here is much more primitive and more general and has
> to do with the basic sense of approximation.

> Suppose or, x, y ∈ D are two elements of the data type,
> then the idea is not immediately to think of them as being
> completely separate entities just because they may be different.
> Instead y, say, may be a better version of what
> x is trying to approximate.

等等。
这不就是 propagator model 中
cell 保存 lattice 的现象吗？

> In fact, let us write the relationship x ⊆ y to mean intlLitively
> that y is consistent with x and is (possibly) more qccurate than x.
> For short we can say that x appI'oximate6 y.

> This intuitively understood relationship exists on most data types
> naturally, and it is part of the thesis of this paper that a data
> type should always be provided with such a relationship.

如果真的做到了，我实现 propagator model 时就省事了。

> AXIOM 1. A data type is a partially ordered set.

> AXIOM 2. Mappings between data types are monotonic.

> Maybe it would be better to talk about information;
> thus, x ⊆ y means that x and y want to approximate the same
> entity, but y gives more information about it.

> This means we have to allow "incomplete" entities, like x,
> containing only "partial" information.  (The way to do this in
> numerical calculation is called interval analysis. but we do not
> have the space here to be more specific.)  Allowing for partiality
> of arguments and values has the good effect that our functions
> become partial too; for even if the arguments aTe perfect the values
> may only be partial.

在 propagator model 中，这就是说 propagator 本身也是 partial 的。

# 3. Completeness and Continuity

TODO

# 4. Computability
# 5. Construction of Data Types
# 6. Conclusion
# 7. Background and References
