---
title: introduction to bisimulation and coinduction
author: davide sangiorgi
year: 2011
---

# My Motive

[2026-03-15] 在 luca 关于如何检查递归定义的 type 之间的 subtype 关系的论文中，
所用到的，其实就是 bisimulation 和 coinduction。

- induction 是用来证明一个无穷集合满足某种属性。
- coinduction 是用来证明两个无穷集合之间满足某种关系。

因此，与 induction 一样，
coinduction 是非常核心的数学知识。
所以必须像掌握 induction 一样，熟练掌握。

递归定义的函数和类型，就是 coinduction 的主要来源，
我之前处理不好其等价关系与子类型关系，
就是因为不懂 coinduction。

这本书的作者是为了研究 pi-calculus 而写的。
pi-calculus 中也有很多要用到 coinduction 的地方。

[2026-03-16] 目标：

- 实现 meta-lisp 中对递归定义的类型的 unify 算法。
- 实现 lambda-lisp 中递归定义的函数之间的等价关系的判断。

# 0 General introduction

## 0.1 Why bisimulation and coinduction

> In intuitionistic topology, one prefers a more informative
> definition of closed sets, as sets satisfying certain closure
> properties; coinductive definitions are then very natural and
> elegant, and particularly convenient for computer-formalised
> mathematics [Val05, HH06].

这里的两个引用是：

- [Val05] S. Valentini.
  The problem of the formalization of constructive topology.

- [HH06] P. Hancock and P. Hyvernat.
  Programming interfaces and basic topology.

> Similarly, in constructive formalisation of the λ-calculus, the set
> of ‘divergent terms’ is not defined as the complement of the
> inductive set of ‘convergent terms’. One looks for an informative
> way of describing the meaning of divergence, and for this
> coinductive methods are very appropriate (see, e.g., Section 2.1.2).

这可能可以解决 lambda-lisp 中判断递归函数等价的问题。

> For instance, in Type Theory bisimulation and coinductive techniques
> have been proposed to prove the soundness of type systems [MT91], to
> define the meaning of equality between (recursive) types and then to
> axiomatise and prove such equalities [AC93, BH97] and to define
> coinductive types and manipulate infinite proofs in theorem provers
> [Coq94, Gim96].

这里的引用是：

- [MT91] R. Milner and M. Tofte.
  Co-induction in relational semantics.

- [AC93] R. M. Amadio and L. Cardelli.
  Subtyping recursive types.

- [BH97] M. Brandt and F. Henglein.
  Coinductive axiomatization of recursive type equality and subtyping.

- [Coq94] T. Coquand.
  Infinite objects in type theory.

- [Gim96] E. Gim´enez.
  Un Calcul de Constructions Infinies et son
  Application ´a la Verification des Systemes Communicants.

> ... to reason about elements of recursively defined domains and data
> types [Fio93, Pit94] and to reason about equivalence in sequential
> programs [Pit97].

引用是：

- [Fio93] M. Fiore.
  A coinduction principle for recursive data types based on bisimulation.

- [Pit94] A. M. Pitts.
  A co-induction principle for recursively defined domains.

- [Pit97] A. M. Pitts.
  Operationally-based theories of program equivalence.

> Bisimulation was derived from the notion of isomorphism with the
> objective of defining the meaning of equality on non-well-founded
> sets; in other words, for understanding what it means for two
> infinite sets to have ‘the same’ internal structure.

non-well-founded sets 包含了就 membership 而言递归定义的 set。
与逻辑中递归定义的 term 很相似，只不过 set 是无序且不重复的。

> Similarly, the development of Final Semantics [Acz88, RT94, RJ12],
> an area of Mathematics based on coalgebras and category theory and
> used in the semantics of programming languages, has been largely
> motivated by the interest in bisimulation. As a subject, Final
> Semantics is today well developed, and gives us a rich and deep
> perspective on the meaning of coinduction and its duality with
> induction.

引用是：

- [Acz88] P. Aczel.
  Non-well-founded Sets.
  CSLI lecture notes; no. 14, 1988.

- [RT94] J. Rutten and D. Turi.
  Initial algebra and final coalgebra semantics for concurrency.

- [RJ12] J. Rutten and B. Jacobs.
  (Co)algebras and (co)induction.
  In Sangiorgi and Rutten [SR12].

> Bisimulation is also popular in Philosophical Logic, specifically in
> Modal Logics; some of the most interesting results in the
> expressiveness of Modal Logics rely on the notion of bisimulation
> [BRV01].

引用是：

- [BRV01] P. Blackburn, M. de Rijke and Y. Venema.
  Modal Logic.
  Cambridge University Press, 2001.

## 0.2 Objectives of the book

用 complete lattices 的 duality
来解释 induction 和 coinduction 之间的 duality。

然后又 pi-calculus 为基础介绍 bisimulation。

## 0.3 Use of the book

这里推荐了一本学习 induction 的书：

- [Win93] G. Winskel.
  The Formal Semantics of Programming Languages.
  The MIT Press, 1993.

## 0.4 Structure of the book

> In Chapter 7 a method is shown for deriving bisimilarity that can be
> applied to virtually all languages whose terms are described by
> means of a grammar. The crux of the method is to set a bisimulation
> game in which the observer has a minimal ability to observe actions
> and/or states, and then to take the closure of this bisimulation
> under all contexts.

这看起来也可以解决 lambda-lisp 中等价判断的问题。

# 1 Towards bisimulation

TODO

# 2 Coinduction and the duality with induction

TODO
