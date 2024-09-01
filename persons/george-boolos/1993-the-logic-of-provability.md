---
title: The Logic of Provability
author: George Boolos
year: 1993
---

# 学习动机

开阔眼界。

促进我对形式系统元理论的理解，比如一致性和完备性。

促进我对古典逻辑的理解，主要是理解古典逻辑和构造主义之间的区别。

# 介绍

> Its subject is the relation between provability and modal logic, a
> branch of logic invented by Aristotle but much disparaged by
> philosophers and virtually ignored by mathematicians.  Here it
> receives its first scientific application since its invention.

> Modal logic is concerned with the notions of necessity and
> possibility. What George Boolos does is to show how the concepts,
> techniques, and methods of modal logic shed brillant light on the
> most important logical discovery of the twentieth century: the
> incompleteness theorems of Kurt Godel and the "self-referential"
> sentences constructed in their proof. The book explores the effects
> of reinterpreting the notions of necessity and possibility to mean
> provability and consistency. It describes the first application of
> quantified modal logic to formal provability as well as the results
> of applying modal logic to well-known formal systems of mathematics.

# Preface

> When modal logic is applied to the study of provability, it becomes
> provability logic. This book is an essay on provability logic.

还看不懂前言中技术性的部分，需要读了这本书之后再回顾。

# Introduction

注意，这篇引导会综述全书的主要内容，
看不懂的地方可以在看后面章节之后再回来看。

写书的方式不错，
基本上是有一个想要介绍的目标内容（目标知识或技巧），
然后反向推导出来前置的知识，
再合理安排出来一个线性的介绍顺序。

> The theme of the present work is the way in which modal logic, a
> branch of logic first studied by Aristotle, has been found to shed
> light on the mathematical study of mathematical reasoning, a study
> begun by David Hilbert and brought to fruition by Kurt Godel.

我们用下面的名字来写 modal logic 中的高阶谓词：

- □ -- 必然 -- `Necessarily(P)`
- ◇ -- 可能 -- `Possibly(P)`

以 `Necessarily` 为 primitive，
定义 `Possibly(P)` 为 `Not(Necessarily(Not(P)))`。

提到亚里士多德，
又让人想读亚里士多德的逻辑，
与 Jan Lukasiewicz 的注释了。
但是还是应该先读这本书，
这本书是为了避免劳动而读的闲书，
与这本书相比，Lukasiewicz 的注释
更加是在花园中散步，而不是劳动。

> We are going to use modal logic to study not the notion of necessity
> but that of formal provability, a concept at the heart of the
> subject of logic, and the fundamental notion studied in Kurt Godel's
> famous paper of 1931. "On formally undecidable propositions of
> Principia Mathematica and related systems I".

> We shall be interested in the effects of construing the box to mean
> "it is provable that..." rather than "it is necessary that..." and,
> dually, of taking the diamond to mean "it is consistent that..."
> rather than "it is possible that...".

> Here provability and consistency are taken with respect to some one
> formal system, usually classical first-order arithmetic ["Peano
> arithmetic" (PA)]. In our study of formal provability we shall pay
> particular attention to a system of propositional modal logic that
> we call GL, for Godel and M. H. Lob.

看到这里我感觉，
也许这种逻辑是利用 modal logic 所提供的形式工具，
在古典逻辑中分离出来一个构造主义逻辑的部分！

提到了 Kripke semantics (W, R, V)：

- W -- (possible) worlds
- R -- accessibility relation between worlds
- V -- evaluation of a sentence in a world

也许可以把 Kripke semantics 中的 world
理解为类型检查中不同的 context，
不同的 context 之间显然可以定义 accessibility relation，
即 context 的扩张。

> And a sentence Necessarily(A) is true at w if A is true at all
> worlds x such that wRx. (Thus the box acts like a universal
> quantifier over possible worlds.)
>
> A sentence is valid in a model <W, R, V>
> iff it is true at all worlds in W.

介绍了而 Godel 在著名论文
"On formally undecidable propositions..." 中用到的形式系统，
原来是一个简单类型论外加 Peano 算术。

- 类型仅限于 Nat，Set(Nat)，Set(Set(Nat))，等等。

TODO 在 cicada 中形式化 Godel 用到的形式系统。

TODO 我们可以把这个系统实现出来试试。

# 1 GL and other systems of propositional modal logic

TODO

# 2 Peano arithmetic
# 3 The box as Bew(x)
# 4 Semantics for GL and other modal logics
# 5 Completeness and decidability of GL and K, K4, T, B, S4, and S5
# 6 Canonical models
# 7 On GL
# 8 The fixed point theorem
# 9 The arithmetical completeness theorems for GL and GLS
# 10 Trees for GL
# 11 An incomplete system of modal logic
# 12 An S4-preserving proof-theoretical treatment ofmodality
# 13 Modal logic within set theory
# 14 Modal logic within analysis
# 15 The joint provability logic of consistency and omega-consistency
# 16 On GLB: The fixed point theorem, letterless sentences, and analysis
# 17 Quantified provability logic
# 18 Quantified provability logic with one one-place predicate letter
