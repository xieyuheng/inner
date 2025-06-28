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

# 1 Introduction

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
> their boundary points (like a circle with its circumference), and
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

# 2 Affirmative and refutative assertions

> In which we see a Logic of Finite Observations and take this as the
> notion we want to study.

> The final idea of what topology is, is _A Theory of Information_.
> To motivate this, we investigate some of the properties that an
> assertion might have, ...

我猜测，这里所说的 "a theory of information"，
就是 domain theory 中的 "more defined"，
也是 propagator model 中的 "more informative"。

> Given an assertion, we can therefore ask:
>
> - Under what circumstances could it be affirmed?
> - Under what circumstances could it be refuted?

> Notice that these affirmations and refutations are supposed to be
> done on the basis of what we can actually observe. An observation
> must be made in finite time, after a finite amount of work. To be
> emphatic on this point, we can refer to _finite_ observations.

从 propagator model 的角度看，
就一个 assertion 而言的新的 observations，
这就是向代表这个 assertion 的 cell 中增加（merge）信息。

在假设了「有限观察」这个前提之后，
我们发现命题的除了真伪之外，还有更复杂的属性：

> An assertion is affirmative iff it is true precisely in the
> circumstances when it can be affirmed.

> An assertion is refutative iff it is false precisely in the
> circumstances when it can be refuted.

下面的翻译中，「证实」与「证伪」的「证」字，
代表证据，可以代表「有限观察」：

- 一个断言是可证实的，当且仅当它恰好在能够被证实的情况下为真。
- 一个断言是可证伪的，当且仅当它恰好在能够被证伪的情况下为假。

Some examples:

- "Some babies have tartan eyes." -- 可证实，不可证伪。

- "All ravens are black." -- 不可证实，可证伪。

- "Smith is exactly six feet tall." -- 不可证实（因为测量有误差），可证伪。

- "Jones is between five foot eleven inches and six foot one."
  -- 需要具体分析如下：

> There are many heights of Jones for which this could be affirmed or
> refuted, but if, _as it happens_, Jones is exactly five foot eleven,
> we shall never be able to do either. This is a boundary case.

> We can make the assertion affirmative or refutative by excluding or
> including the boundary cases, in other words by specifying whether
> "between" means strictly or non-strictly between.

但是，如果 Jones 的身高真的在边界情况上，
那么任何（带有误差的）测量所得到的区间，
都有一部分在所给的区间内，而一部分在所给的区间外，
不论 "between" 的意义是开区间还是闭区间。
这么看来上面的分析也是有问题的。

下面分析 affirmative 和 refutative 和逻辑连词的关系。

> **(1) Negation**
>
> To affirm not P, we have to make some finite observation to show
> that Ρ is definitely false, in other words we must refute P. It
> follows that not P is affirmative iff Ρ is refutative.

> Therefore, negation transforms affirmative assertions into
> refutative ones and vice versa.

> As an example, take Ρ to be the statement "Some ravens are not
> black." This is affirmative but not refutative. Then not P is
> equivalent to "All ravens are black," which is refutative but not
> affirmative.

> An important consequence of this is that if we just want to talk
> about refutative assertions, we can take their negations and talk
> about affirmative ones instead. On this justification we shall
> generally restrict our attention to affirmative assertions.

> **(2) Disjunction**
>
> We can affirm P or Q either by affirming Ρ or by affirming Q.
>
> Therefore, Any disjunction, even an infinite one, of affirmative
> assertions is still affirmative.

尽管是 P or Q，但是整体的 affirmative，要求两个部分都是 affirmative，
因为 affirmative 的定义中包含了 "precisely in the circumstances"：

An assertion is affirmative iff it is true precisely in the
circumstances when it can be affirmed.

如果只有 Q 是 affirmative 的，而 P 不是 affirmative，但是 Q 为 false，
此时整个 assertion 可能是 true，即 P 可能是 true，
但是不能由 finite observation 来 affirm。

> **(3) Conjunction**
>
> We can affirm P and Q by affirming both Ρ and Q. If Ρ and Q are both
> affirmative, then so is P and Q: for P and Q is true iff both Ρ is
> true and Q is true.
>
> Again, this extends to any finite conjunction. But this time, to
> affirm an infinite conjunctions, we must affirm every single one and
> this will take an infinite amount of work.
>
> Therefore for conjunctions, Any finite conjunction of affirmative
> assertions is still affirmative.

在这里的论证中，不对称性来自 finite observation。

另外别忘了：

- 无穷的 disjunction 是 exists -- 类型论中的 Sigma；
- 无穷的 conjunction 是 forall -- 类型论中的 Pi。

> Of course, there may be special methods available to us in special
> cases. For instance, an assertion about all natural numbers could be
> thought of as an infinite conjunction of specialized assertions, one
> for each number. This can then be affirmed by an inductive
> proof. But this is not part of the general logic of affirmative
> assertions.

> The argument here makes no mention of the order in which the
> conjuncts are affirmed, nor of the number of times each is
> affirmed. There is really a tacit assumption that observations do
> not affect what is being observed, so that order and multiplicities
> don't matter. In Chapter 12 we shall see the concept of _quantales_,
> and these can be thought of as maintaining our present idea of
> disjunction, but replacing finite conjunctions by finite sequences
> of observations giving composite, _product_ observations.

> **(4) true and false**
>
> These can be seen as special cases, true and false being
> respectively the conjunction and disjunction of no assertions (the
> empty conjunction or disjunction, Λ0 or V0). These are related to
> the logical equivalences true and P <=> Ρ <=> false or P. They ought
> both therefore to be affirmative as special cases of our discussion
> so far.

> **(5) Implication**
>
> P —> Q ("if Ρ holds then so does Q") is again like a scientific law
> [conjecture], and can be refuted by affirming Ρ and refuting Q. We
> can deduce that if Ρ is affirmative and Q is refutative then P —> Q
> is refutative, and so it has a good standing in a mixed logic of
> affirmativity and refutativity.
>
> However, P —> Q is not (in general) affirmative. For a particular
> example, take Ρ to be any affirmative assertion and Q to be
> false. Then P -> Q is logically equivalent to not P, which is
> refutative, but not in general affirmative.
>
> Therefore, The logic of affirmative assertions must not include
> implication.

> **(6) Distributivity**

对于无限 disjunction 和有限 conjunction，
分配律都成立。

> **Summary**
>
> The discussion up to here has been rather informal, but we shall use
> it as our motivation for studying a logic of affirmative assertions
> that has:
>
> - arbitrary disjunctions, including both the empty disjunction false
>   and infinite disjunctions.
>
> - finite conjunctions, including the empty conjunction true.
>
> - two distributive laws: conjunction distributes over arbitrary
>   disjunction and disjunction distributes over finite conjunctions.
>
> It does NOT include negation, implication or infinite conjunctions.

> By taking a positive view of finite observations, we identify them
> with the affirmative assertions: an observation corresponds to the
> assertion "this observation can be made".

> And an assertion corresponds to the set (disjunction) of
> observations that affirm it.

想要 affirm 一个 assertion，需且只需找到其证据中的任意一个。

类似构造主义类型论中，想要证明一个命题，
需且只需找到一个属于这个类型的元素。
但是类型论中有 implication，而这里没有。

> Thus our logic of affirmative assertions can also be seen as a logic
> of finite observations.

> **Notes**
>
> This logic of affirmative assertions/finite observations is
> technically known as _propositional geometric logic_,
> "propositional" referring to the lack of variables or quantifiers.
> Full geometric logic has variables, equality, the arbitrary
> disjunctions and finite conjunctions that we have introduced, and
> the existential quantifier -- but not the universal quantifier.  It
> is a well-established body of mathematics, originating in algebraic
> geometry and its treatment by toposes, and an introduction for
> computer scientists can be found in Fourman and Vickers [85].

Fourman and Vickers [85]:
1985-theories-as-categories--michael-p-fourman--steven-vickers

> The identification of propositional geometric logic as a logic of
> finite observations appeared first in print in Abramsky [87],
> although it is, of course, related to Mike Smyth's idea [83] that
> open sets in a topological space are analogous to semi-decidable
> properties. It would be interesting to discover whether the full
> predicate geometric logic can be used in a similar way.

Abramsky [87]:
1987-domain-theory-and-the-logic-of-observable-properties

Mike Smyth [83]:
1983-power-domains-and-predicate-transformers--a-topological-view--m-b-smyth

# 3 Frames

> In which we set up an algebraic theory for the Logic of Finite
> Observations: its algebras arc frames.

# 3.1 Algebraicizing logic

TODO

# 3.2 Posets
# 3.3 Meets and joins
# 3.4 Lattices
# 3.5 Frames
# 3.6 Topological spaces
# 3.7 Some examples from computer science
# 3.8 Bases and subbases
# 3.9 The real line
# 3.10 Complete Heyting algebras

# 4 Frames as algebras
# 5 Topology: the definitions
# 6 New topologies for old
# 7 Point logic
# 8 Compactness
# 9 Spectral algebraic locales
# 10 Domain Theory
# 11 Power domains
# 12 Spectra of rings
