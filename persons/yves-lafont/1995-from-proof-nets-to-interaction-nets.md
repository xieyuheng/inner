---
title: From Proof-Nets to Interaction Nets
author: Yves Lafont
year: 1995
---

# 学习动机

[2025-01-01] 这篇论文在 1997-interaction-combinators 之前。
需要看一下从 1990-interaction-nets 开始 inet 的发展。

[2025-01-05] 看了很多 linear logic 的文献，我还是不理解它的语义。
既然 interaction nets 和 proof nets 是 linear logic 的 term language，
也许反过来可以理解 linear logic。

# 1 introduction

> If we consider the interpretation of proofs as programs, say in
> intuitionistic logic, the question of equality between proofs
> becomes crucial: The syntax introduces meaningless distinctions
> whereas the (denotational) semantics makes excessive
> identifications. This question does not have a simple answer in
> general, but it leads to the notion of proof-net, which is one of
> the main novelties of linear logic. This has been already explained
> in [Gir87] and [GLT89].

> The notion of interaction net introduced in [Laf90] comes from an
> attempt to implement the reduction of these proof-nets. It happens
> to be a simple model of parallel computation, and so it can be
> presented independently of linear logic. However, we think that it
> is also useful to relate the exact origin of interaction nets,
> especially for readers with some knowledge in linear logic. We take
> this opportunity to give a survey of the theory of proof-nets,
> including a new proof of the sequentialization theorem.

"The notion of interaction net introduced in [Laf90] comes from an
attempt to implement the reduction of these proof-nets."

正如 untyped lambda calculus 可以从
lambda calculus + intuitionistic logic 中分离出来，
作为计算模型来研究。

# 2 Multiplicatives

对于 natural deduction 而言，
用来描述证明的 term language 就是 lambda term。

对于这里所描述的 sequent calculus 而言，
如何设计 term language 就成了难题，
因为需要忽略 sequent 左右两个 multisets 中元素的顺序。

这就是引入 graph 来描述 term 的动机。
用一个 meta language 来构造 graph，
就是我实现 inet 时主要的 idea。

TODO 用 inet-lisp 的语法作为 term language，把这里的证明写出来。

TODO
