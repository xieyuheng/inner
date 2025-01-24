---
title: implementing distributed systems using linear naming
author: alan bawden
year: 1993
---

# My Motive

[2025-01-24]
这篇论文好像与 1992-linear-graph-reduction--confronting-the-cost-of-naming
的内容完全一样，只是标题变了。

# Abstract

> Linear graph reduction is a simple computational model in which the
> cost of naming things is explicitly represented. The key idea is the
> notion of linearity. A name is linear if it is only used once, so
> with linear naming you cannot create more than one outstanding
> reference to an entity. As a result, linear naming is cheap to
> support and easy to reason about.

> Programs can be translated into the linear graph reduction model
> such that linear names in the program are implemented directly as
> linear names in the model. Nonlinear names are supported by
> constructing them out of linear names. The translation thus exposes
> those places where the program uses names in expensive, nonlinear
> ways.

> Two applications demonstrate the utility of using linear graph
> reduction: First, in the area of distributed computing, linear
> naming makes it easy to support cheap cross-network references and
> highly portable data structures, Linear naming also facilitates
> demand driven migration of tasks and data around the network without
> requiring explicit guidance from the programmer.

> Second, linear graph reduction reveals a new characterization of the
> phenomenon of state. Systems in which state appears are those which
> depend on certain global system properties. State is not a
> localizable phenomenon, which suggests that our usual object
> oriented metaphor for state is flawed.

# Chapter 1 Introduction

## 1.1 Linearity

### 1.1.1 What is it?

目前 linearity 的定义比较复杂，
只是因为 (if) 这样的 conditional 表达式存在。

如果 conditional 不是通过 (if) 在同一个 scope 中给出，
而是像 prolog 或 inet-lisp 中定义 clause 和 rule 一样，
那么 linearity 的定义就简单很多了。

TODO
