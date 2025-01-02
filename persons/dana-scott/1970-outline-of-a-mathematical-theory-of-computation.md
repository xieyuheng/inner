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

# 2. Data Types and Mappings
# 3. Completeness and Continuity
# 4. Computability
# 5. Construction of Data Types
# 6. Conclusion
# 7. Background and References
