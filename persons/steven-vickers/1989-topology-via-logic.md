---
title: topology via logic
author: steven vickers
year: 1989
---

# My Motive

[2025-06-12] 在学习 Dana Scott 的指称语义时，
比如 1969 年的论文 "A type theoretical alternative to ISWIM CUCH OWHY"，
发现 Scott 会自然地去讨论 lattice 之间的连续函数。

而这本书刚好就是为 Scott 的指称语义而写的，
可以把 lattice 上的连续性，
和 topology 公理所定义的连续性联系起来。

[2025-06-27] Scott 的 domain theory 的基础是 topology，
而 Milner 认为，在研究 formal system 时，
domain theory 和指称语义是核心重要的，
因为模型可以给看似任意的 formal system 设计以指导。

在读 luis damas 的 1985-type-assignment-in-programming-languages 时，
遇到了 domain theory 中 ω-chain 的概念，因此也想来预习一下。

# Preface

> Both [this book's] subject matter and its approach derive from a
> particular view of topology that has arisen in theoretical computer
> science.

> ... the story starts with the theory of _domains_, as founded by
> Scott and Strachey [71] to provide a mathematical foundation for
> the semantics of programming languages.

> As well as defining domains, Scott also showed that they could be
> put into a topological setting, and Mike Smyth [83] has developed
> the idea that this is not merely a technical trick. The topology
> captures an essential computational notion, under the slogan "open
> sets are semidecidable properties". This emphasis gives the open
> sets a life independent of the points of the topological space, and
> this led Smyth to make connections with an unexpected strand of
> mathematics, _locale_ theory.

> Modem applications of topology, many of them in algebraic geometry,
> have given point to two ideas.
>
> - First, interesting topological spaces need not satisfy the
>   Hausdorff separation axiom. This is well bom out in domains, whose
>   rich specialization ordering is entirely alien to Hausdorff
>   separation.
>
> - Second, it is often worth thinking of the open sets as forming an
>   algebraic structure -- _a frame_ -- in its own right. To a large
>   extent one can then ignore the points, and do _pointless_ or
>   _localic_ topology. Locale theory is well described in Johnstone's
>   excellent book [82].

> The traditional - spatial - motivation for general topology and its
> axioms relies on abstracting first from Euclidean space to metric
> spaces, and then abstracting out, for no obvious reason, certain
> properties of their open sets. I believe that the localic view helps
> to clarify these axioms, by interpreting them not as set theory
> (finite intersections and arbitrary unions), but as logic (finite
> conjunctions and arbitrary disjunctions: hence the title). Smyth's
> slogan implies that topology represents the logic of semidecidable
> properties. A modification of this, "geometric logic [topology] is
> the logic of finite observations," appeared in Abramsky [87], and I
> have tried to argue directly from these logical intuitions to the
> topological axioms, and to frames as the algebraic embodiment of
> them.

我一直对 topology 公理不满意，这本书也许可以解答疑惑。

# Introduction

> **A Historical Overview**

> The origins of topology are very different from the context we shall
> be working in, and it is probably as well to compare some different
> ideas of what it is.

> I - The first idea is that of rubber sheet geometry.

也许可以说这是微分几何的视角，
因为想要描述 rubber sheet 的 deformation，
就需要用到微分几何。

也许可以设计一个游戏来让人们体会这种经验，
即可以通过操作来修改几何对象的形状，
但是要保持拓扑不变。

> II - The study of boundaries.

> Since tearing is what makes a difference in rubber sheet topology,
> and since tearing creates new boundaries in the sheet, these would
> seem an important thing to look at. The characteristic of a boundary
> point of a set is that however closely you look at it, you can see
> some neighbouring points inside the set and some outside.

> Part of this will entail studying _closed sets_, which include all
> their boundary points Oiks a circle with its circumference), and
> _open sets_, which include none of their boundary points (like a
> circle without its circumference).

比如 cell complex 的构造中的 attaching map，
就是 attach 到 cell 的边界上的。

其实用边界来理解连续性的方式，
甚至可以追溯到亚里士多德。

> IIΙ - The abstract study of open and closed sets.

> The next step is one of abstraction.  We forget all the geometry and
> just take an abstract set of "points", a _topological space_. We
> specify certain subsets as being _open_ (their complements are the
> _closed_ subsets), and we make sure that certain axioms, due to
> Hausdorff, are satisfied.  Then we translate topological arguments
> from stage II into this abstract setting.

> IV - Locale theory.

> The next step is to forget even about the points, and just take an
> abstract set of "open sets", with abstract algebraic operations to
> represent union and intersection.  This structure is a _frame_.
> Sometimes, the points can be reconstructed from this frame of open
> sets.

> This may seem like the ultimate in abstraction, but we shall see how
> considerations of _logic_ make this an appropriate starting point
> from which to work backwards.

有趣！

Hausdorff spaces 满足 Hausdorff 分离公理，
即空间中的任意两个不同的点，可以被不相交的开集分离。
主流的拓扑学研究这种空间。

> In computer science, however, topology is used to explain
> approximate states of information: the points include both
> approximate points and more refined points, and these relate to the
> topology by the property that if an open set contains an approximate
> point then it must also contain any refinement of it. Thus the
> approximate point and its refinement cannot possibly be "housed orf"
> by disjoint open sets and the topological space cannot possibly be
> Hausdorff. This means that topology as used in computer science --
> at least for the methods described here -- runs in a different
> direction from the mainstream, even though it is still topology.

> This book approaches topology in an unusual way, starting from
> frames and an explanation in terms of logic, and ends up with
> unusual applications -- the non-Hausdorff topologies used in
> computer science.

提到了想要从 rubber sheet geometry 的视角了解拓扑学，
可以看 Martin Gardner:

- [63] More Mathematical Puzzles and Diversions, Bell [in Great Britain] 1963.
- [86] Knotted Doughnuts and Other Mathematical Entertainments, Freeman, NewYork, 1986.

# 1 Introduction
# 2 Affirmative and refutative assertions
# 3 Frames
# 4 Frames as algebras
# 5 Topology: the definitions
# 6 New topologies for old
# 7 Point logic
# 8 Compactness
# 9 Spectral algebraic locales
# 10 Domain Theory
# 11 Power domains
# 12 Spectra of rings
