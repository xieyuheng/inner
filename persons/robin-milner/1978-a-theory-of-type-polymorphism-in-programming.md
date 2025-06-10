---
title: a theory of type polymorphism in programming
author: robin milner
year: 1978
---

# My Motive

[2025-06-10] 通过 the little typer，
我学会了实现 dependent type system，
但是其实对 type system 的学习应该从更简单的入手，
应该从 Hindley-Milner type system 入手。

# Abstract

> The aim of this work is largely a practical one. A widely employed
> style of programming, particularly in structure-processing languages
> which impose no discipline of types, entails defining procedures
> which work well on objects of a wide variety.  We present a formal
> type discipline for such polymorphic procedures in the context of a
> simple programming language, and a compile time type-checking
> algorithm W which enforces the discipline.  A Semantic Soundness
> Theorem (based on a formal semantics for the language) states that
> well-type programs cannot “go wrong” and a Syntactic Soundness
> Theorem states that if W accepts a program then it is well typed.
> We also discuss extending these results to richer languages; a
> type-checking algorithm based on W is in fact already implemented
> and working, for the metalanguage ML in the Edinburgh LCF system,

# 1 Introduction

TODO
