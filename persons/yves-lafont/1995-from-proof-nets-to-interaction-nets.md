---
title: From Proof-Nets to Interaction Nets
author: Yves Lafont
year: 1995
---

# 学习动机

这篇论文在 1997-interaction-combinators 之前。
需要看一下从 1990-interaction-nets 开始 inet 的发展。

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

# 2 Multiplicatives

回顾 linear logic 的连接词：

| ⊗ | mul | conj | have(A and B)     |
| & | add | conj | can(A) and can(B) |
| ⊕ | add | disj | have(A or B)      |
| ⅋ | mul | disj | can(A) or can(B)  |

也许可以直接用属性命名：

- conj-add -- cadd
- disj-add -- dadd
- conj-mul -- cmul
- disj-mul -- dmul

对于 natural deduction 而言，
用来描述证明的 term language 就是 lambda term。

对于这里所描述的 sequent calculus 而言，
如何设计 term language 就成了难题，
因为需要忽略 sequent 左右两个 multisets 中元素的顺序。

这就是引入 graph 来描述 term 的动机。
用一个 meta language 来构造 graph，
就是我实现 inet 时主要的 idea。

- 考虑 concatenative language 用对栈中元素的自由置换，
  可以很自然的想到 inet-forth。
  然后发现其实任何 meta language 都是可行的，
  比如也可以实现 inet-lisp。

TODO 分别用 inet-forth 和 inet-lisp 的语法作为 term language，把这里的证明写出来。

TODO
