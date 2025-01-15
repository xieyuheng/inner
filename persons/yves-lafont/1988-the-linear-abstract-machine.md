---
title: the linear abstract machine
author: yves lafont
year: 1988
---

# 学习动机

理解 linear logic 的连接词。

linear logic 连接词的解释之一：

| ⨂ | have(A and B)     |
| & | can(A) and can(B) |
| ⨁ | have(A or B)      |
| ⅋ | can(A) or can(B)  |

> Linear logic wise (A ⨂ B) is like the tuple (you have these two
> resources), while (A & B) is a choice you can make between two
> resources (but you cannot have both).  For example, if you have
> enough fuel to go to mars or mercury, but not both, you could say
> you have (Trip To Mars & Trip To Mercury) but not (Trip To Mars ⨂
> Trip To Mercury).

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

- ⨂ (tensor product)
- 1 (tensor product)
- ⊸ (linear implication)
- & (direct product)
- t (direct unit)
- ⨁ (direct sum)
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

注意，在引入 term 语法的时候，不得不修改规则转而去讨论范畴论，
而不是直接给 sequent calculus 的 inference rule 引入 term 语法，
就是因为作者不知道 concatenative language 也可以作为 term 语法。

## 2.2 linear combinators

one way of viewing the logic connectives of linear logic is:

- A ⨂ B (tensor product) --
  a value of type A in the stack and a value of type B in the stack.

- A & B (direct product) --
  a tuple of type (A, B) in the stack.

从 resource 的语义上来看，上面的解释是不对的。
但是也许 resource 只是语义之一，
毕竟 linear logic 也可以直接解释为古典 logic。

## 2.3 about the products

⨂ (tensor product) 与 ⊸ (linear implication) 有关。
而 & (direct product) 与 pair 的 `fst` 和 `snd` 有关。

- ⨂ -- strict pair
- & -- lazy pair

> `fst` and `snd` are not strict in their arguments
> (only one of them will be used).
> but linear `app` need both the function and the argument.

# 3 the modality

## 3.2 Of course!

在资源的语义下 `A` 可以被理解为是资源 -- `malloc` 之后的 pointer，
而 `!A` 是 `int` 或 `float` 之类的可以随意复制的 primitive value。
如果为 `A` 实现了 dup 和 drop 这两个函数的 interface，
那么 `A` 就可以转化为 `!A`。

## 3.3. intuitionistic Logic recovered

可以用 linear logic + of course 来实现 intuitionistic logic，
就像是一种对 intuitionistic logic 的 factoring。

# 后记

感觉后面的讨论很乱，applicative language
与用 sequent calculus 描述的 inference rule 一点也不匹配。
看这篇论文只能让我想实现一个叫
LinearScript 或者 LinearCat 的简单类型语言。
