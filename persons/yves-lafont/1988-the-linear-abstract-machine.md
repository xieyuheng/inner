---
title: the linear abstract machine
author: yves lafont
year: 1988
---

# Abstract

> Linear Logic [6] provides a refinement of functional progamming and
> suggests a new implementation technique, with the followin features:
> - a synthesis of strict and lazy evaluation,
> - a clean semantics of side effects,
> - no garbage collector.

# Introduction

介绍想要解决的 functional progamming 中的问题。

论文的成果将包含：

- 新的类型系统；
- 新的实现方式。

# 1 sequent calculus

用 sequent calculus 介绍
intuitionistic logic 和
intuitionistic linear logic。

## 1.1 gentzen sequent calculus

只处理命题演算。

连词表：

- ∧ (conjunction)
- ⊤ (true)
- -> (implication)
- v (disjunction)
- ⊥ (false)

## 1.2 girard sequent calculus

只处理命题演算。

连词表：

- ⊗ (tensor product)
- 1 (tensor product)
- ⊸ (linear implication)
- & (direct product)
- t (direct unit)
- ⊕ (direct sum)
- 0 (direct zero)

## 1.3 examples of proofs

## 1.4 cliassical linear logic

> At first sight, Classical Linear Logic is to Intuitionistic Linear
> Logic what Classical Logic is to Intuitionistic Logic. For example,
> you have negation as a primitive connector, and the excluded middle
> law. However, Classical Linear Logic is mnstructive whereas
> Classical Logic is not!

# 2 categorical combinatory logic

## 2.1 the intuitionistic case

在 "type as theorem, progam as proof" 意义下，
介绍 intuitionistic logic 的 term 语法。

所谓 combinators 就是语言的 primitives，
类型是用范畴论中的 object 来表示的，
此时 term 代表范畴论中的 arrow。

## 2.2 linear combinators

one way of viewing the logic connectives of linear logic is:

- A ⊗ B (tensor product) --
  a value of type A in the stack and a value of type B in the stack.

- A & B (direct product) --
  a tuple of type (A, B) in the stack.

## 2.3 about the products

⊗ (tensor product) 与 ⊸ (linear implication) 有关。
而 & (direct product) 与 pair 的 `fst` 和 `snd` 有关。

- ⊗ -- strict pair
- & -- lazy pair

> `fst` and `snd` are not strict in their arguments
> (only one of them will be used).
> but linear `app` need both the function and the argument.

# 3 the modality

TODO
