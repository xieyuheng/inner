---
title: principal type schemes for functional programs
authors: [luis damas, robin milner]
year: 1982
---

# My Motive

[2025-06-14] 在学习 Hindley-Milner type system，
与 Milner 的原始论文相比，这篇后期的论文可能更简单。
读来试试看。

# 1 Introduction

> This paper is concerned with the polymorphic type discipline of ML,
> which is a general purpose functional programming language, although
> it was first introduced as a metalanguage (whence its name) for
> constructing proofs in the LCF proof system.[4] The type discipline
> was studied in [5] where it was shown to be semantically sound, in a
> sense made precise below, but where one important question was left
> open: does the type-checking algorithm -- or more precisely the type
> assignment algorithm (since types are assigned by the compiler, and
> need not be mentioned by the programmer) -- find the most general
> type possible for every expression and declaration?

> Here we answer the question in the affirmative,
> for the purely applicative part of ML.

TODO

# 2 The language
# 3 Type instantiation
# 4 Semantics
# 5 Type inference
# 6 The type assignment algorithm W
# 7 Completeness of W
