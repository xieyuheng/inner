---
title: proof nets and the identity of proofs
author: Lutz Straßburger
year: 2006
---

# 学习动机

想要给 inet 加上依赖类型系统。
不能盲目地直接去加，
而是需要研究 inet 是什么 logic 的 term 语法，
当然就是 linear logic 的 term 语法，
但是如何形成对应关系我还不知道。

proof nets 是为了研究证明之间的等价关系而发展起来的，
在 lambda 演算中，等价关系在于 normalization，
可能证明之间的等价也在于此。
而 inet 之所允许有很简单的 normalize 算法，
就是因为其起源就是为了判断等价关系。

# 1 Introduction

## 1.1 The problem of the identity of proofs

> Although we are able to manipulate and transform proofs in various
> ways, we have no satisfactory notion telling us when two proofs are
> the same, in the sense that they use the same argument. The main
> reason is the lack of understanding of the essence of a proof, which
> in turn is caused by the bureaucracy involved in the syntactic
> presentation of proofs. It is therefore essential to find new
> presentations of proofs that are “syntax-free”, in the sense that
> they avoid “unnecessary bureaucracy”.

> Finding such presentations is, of course, of utter importance for
> logic and proof theory in its own sake. We can only speak of a real
> theory of proofs, if we are able to identify its objects.

## 1.2 Historical overview

> But even after Gentzen’s work, the natural question of asking for a
> notion of identity between proofs seemed silly because there are
> only two trivial answers: two proofs are the same if they prove the
> same formula, or, two proofs are the same if they are syntactically
> equal.

尝试定义 proofs 之间的等价，
尝试用 normalization 来定义，
在寻找合适的 normal form 时，
Prawitz 用了 cut elimination，
这等价于 inline 所有函数调用，
然后再通过 partial evaluation
化简为 normal form。

> Unfortunately, the problem of identifying proofs has not received
> much attention since the work by Prawitz and Lambek. Probably one of
> the reasons is that the fundamental problem of the bureaucracy
> involved in deductive systems (in which formal proofs are carried
> out) seemed to be an insurmountable obstacle. In fact, the problem
> seems so difficult, that it is widely considered to be “only
> philosophical”.

对于设计 dependent type system 与辅助证明系统的人来说，
证明之间的等价是核心的问题。

## 1.3 Proof nets

TODO

# 2 Unit-free multiplicative linear logic

## 2.1 Sequent calculus for MLL
## 2.2 From sequent calculus to proof nets, 1st way (sequent calculus rule based)
## 2.3 From sequent calculus to proof nets, 2nd way (coherence graph based)
## 2.4 From deep inference to proof nets
## 2.5 Correctness criteria
## 2.6 Two-sided proof nets
## 2.7 Cut elimination
## 2.8 *-Autonomous categories (without units)
## 2.9 Notes

# 3 Other fragments of linear logic

## 3.1 Multiplicative exponential linear logic (without units)
## 3.2 Multiplicative additive linear logic (without units)
## 3.3 Intuitionistic multiplicative linear logic (without unit)
## 3.4 Cyclic linear logic (without units)
## 3.5 Multiplicative linear logic with units
## 3.6 Mix
## 3.7 Pomset logic and BV

# 4 Intuitionistic logic

# 5 Classical Logic

## 5.1 Sequent calculus rule based proof nets
## 5.2 Flow graph based proof nets (simple version)
## 5.3 Flow graph based proof nets (extended version)
