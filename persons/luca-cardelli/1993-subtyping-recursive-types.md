---
title: subtyping recursive types
authors: [roberto amadio, luca cardelli]
year: 1993
---

# My Motive

[2025-06-08] 如何处理递归定义的 structural type 之间的等价判断，
或者更确切地说，判断 compatible 以及 subtyping，
是我一直想解决的问题。

如果想实现带有 structural type 的 lisp，就又要解决这个问题。

看来这篇论文解决了这个问题。

# Abstract

> We investigate the interactions of subtyping and recursive types, in
> a simply typed λ-calculus. The two fundamental questions here are:
>
> 1. whether two (recursive) types are in the subtype relation,
> 2. and whether a term has a type.

> To address the first question, we relate various definitions of type
> equivalence and subtyping that are induced by a model, an ordering
> on infinite trees, an algorithm, and a set of type rules. We show
> soundness and completeness between the rules, the algorithm, and the
> tree semantics. We also prove soundness and a restricted form of
> completeness for the model.

> To address the second question, we show that to every pair of types
> in the subtype relation we can associate a term whose denotation is
> the uniquely determined coercion map between the two
> types. Moreover, we derive an algorithm that, when given a term with
> implicit coercions, can infer its least type whenever possible.

# 1 Introduction

作者为了研究 structural type + recursive type，
专门设计了两个语言 Amber 和 Quest，
在这一节的开头有引用到相关的论文。

TODO

## 1.1 Types
## 1.2 Subtypes
## 1.3 Equality of Recursive Types
## 1.4 Subtyping of Recursive Types
## 1.5 Algorithm outline
## 1.6 Formal development

# 2 A Simply Typed λ-calculus with Recursive Types

## 2.1 Types
## 2.2 Terms
## 2.3 Equations

# 3 Tree Ordering

## 3.1 Subtyping Non-recursive Types
## 3.2 Folding and Unfolding
## 3.3 Tree Expansion
## 3.4 Finite Approximations

# 4 An Algorithm

## 4.1 Canonical Forms
## 4.2 Computational Rules
## 4.3 Soundness and Completeness of the Algorithm
## 4.4 An Implementation

# 5 Typing Rules

# 5.1 Type Equivalence Rules
# 5.2 Completeness of Equivalence Rules
# 5.3 Subtyping Rules
# 5.4 Completeness of Subtyping Rules

# 6 A Per Model

## 6.1 Realizability Structure
## 6.2 Complete Uniform Pers
## 6.3 Completeness of an F-interpretation

# 7 Coercions

## 7.1 Definability
## 7.2 Inference

# 8 Conclusion

# 9 Acknowledgments

# References
