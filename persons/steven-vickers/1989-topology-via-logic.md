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

[2025-06-28] Scott 的 domain theory 的基础是 topology，
而 Milner 认为，在研究 formal system 时，
domain theory 和指称语义是核心重要的，
因为模型可以给看似任意的 formal system 设计以指导。

在读 luis damas 的 1985-type-assignment-in-programming-languages 时，
遇到了 domain theory 中 ω-chain 的概念，因此也想来预习一下。

[2025-06-29] 这里对序关系中的 "less than" 的理解，
是符合集合论直觉的，也就是与我对 propagator model 中的序关系的理解一致，
但是与 domain theory 中对序关系的理解相反。

如果真是这样，这本书也可以看成是 propagator model 的理论基础。

# My Notes

## Propagator model 的 lattice 与 domain theory

[2025-07-02] 可以说是理清了
propagator model 中使用的 lattice，
和 domain theory 之间的关系。

首先 domain 只是 ordered set 不是 lattice，
因为 domain theory 起源于给函数找指称语义，
而函数的定义域和值域一般都不是 lattice，
也不是 ordered set，只有添加了 undefined 元素之后，
才是构成了一个 flat 的 ordered set -- flat domain。

propagator model 正是加强了整个背景条件，
考虑的是 propagator 不是 function，
要求所有 propagator 都是 lattice 之间的同态。

Power domain 是 lattice。

## 类型系统 与 Galois connection

这本书中想要推广 topology space，
把开集从「集」解放出来，考虑抽象的开元素，
所以定义了 topology system `x |= a` 称作 "x satisfies a"。
其中 `a` 所属的集合带有 frame 结构，
而 `x` 所属的集合（point）不带任何结构 。

通过 frame 上的序关系，可以引出 point 集合上的序关系。

    forall a.  x |= a  ->  y |= a

这样就获得两个序关系之间的 Galois connection。

这与类型系统中的基本关系 `x: T` 很相似。
因此类型系统也能因此获得 value 和 type 之间的 Galois connection。

这里我们学到了要区分，
type 之间的序关系，
和 value 之间的序关系。
它们的方向是相反的。

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

如果 P 代表有限集合，那么 P -> Q 就是有限集合的 conjunction，
就像在程序语言中，record 对应于参数为有限集合的函数。

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

finite observation 是我们想要捕捉的直觉概念，
用以给逻辑判断（assertion）之「是否可证实」做进一步的分析。
下面就要发展代数类的数学结构来捕捉这一概念。

# 3.1 Algebraicizing logic

> We are going to apply a fairly standard trick in logic, which is to
> identify, in other words treat as equal, formulae that are logically
> equivalent. The logical connectives then become algebraic operators
> subject to laws, and we can use the methods of algebra to prove
> equivalences. For instance,
>
>     y or (x or y) = y or (y or x) = (y or y) or x = y or x = x or y

> The idea is that, for instance, x or y and y or x are really just
> different ways of saying the same thing. We want to concern
> ourselves with what we can say rather than how we can say them.

> We shall want to incorporate assumptions about the real world, for
> instance that two particular propositions `р3` and `р10` are
> inconsistent: `р3 and р10 = false`. This means that we shall be able
> to tailor a frame to the circumstances that its propositions are
> supposed to be talking about.

> To define frames, we shall need to investigate in more detail the
> way in which algebra will reflect logic. Building in more and more
> logic, we shall see in mm the concepts of posct, lattice,
> distributive lattice and, finally, frame.

# 3.2 Posets

Poset 是 partially ordered set（偏序集）的缩写。
要求其二元关系满足：自反性，传递性，反对称性。
保持 poset 结构的函数称为单调函数。

> We shall think of the elements of a poset as being propositions, and
> of "less than" as meaning "entails", or "is logically stronger
> than".

也就是说对序关系中的 "less than" 的理解还是符合集合论直觉的，
讲到 domain theory 的时候会反转过来吗？

> As we have already implied with our talk of "algebraicizing logic",
> the antisymmetry law of posets is often not present initially, but
> has to be imposed. We now formalize this.

preorder 就是 poset 去掉反对称性，
此时可以用双向的 "less than" 来定义一个新的定价关系，
满足反对称性，重新得到 poset。

也就是说，preorder 不需要假设基础等词，
可以让我们从 "less than" 定义 "equal to"。
相比之下，也许基础等词应该叫做 "the same as"。

最典型的 preorder 是有向图，
所诱导出的等价关系是「可达性」比如形成环，
环上的元素相互可达（这让人想起同伦类型论中等价代表路径）。

poset 的典型例子是无环有向图，
比如 lattice 的图示。

# 3.3 Meets and joins

> Thinking of "less than" as meaning "entails",
> we next wish to describe what corresponds to `and`
> and `or`. First, the _meets_, which correspond to `and`.

poset 中子集的 meet 定义为 greatest lower bound （gib or inf）。
poset 中子集的 join 定义为 least upper bound （lub or sup）。

然后定义作为 poset 中二元运算的 meet 和 join。

然后展示二元运算 meet 和 join 与 "less than" 的关系，
使得我们可以以满足公理的二元运算为基础，获得 poset 结构。

> In logic, meets are conjunctions and joins are disjunctions.

检查 conjunction 和 disjunction
的 introduction rules 以及 elimination rules
在所对应代数结构中成立。

注意，描述 inference rule 时所用到的 entails（横线），
必须被翻译为 poset 的 "less than"。

> In set theory, meets are intersections and joins are unions.

三种视角相互等价：

- algebra --  强调运算律与方程。
- logic -- 强调推演系统。
- set -- 强调元素之所属。

这么看来，传统的类型论，
其实也是「强调元素之所属」，
因此很接近集合论视角，
但是更强调推演系统。

也许 Curry 想要在 combinatory logic 中做的，
就是从 algebra 的视角来处理类型系统。
注意，在描述类型时 Curry 好像也不得不引入推演系统，
而不能把理论限制在代数之内来叙述。

> It is easy to see that if Ρ has all binary meets,
> and f preserves them, then f is monotone.

> Let's now pause to map out our route to frames.
>
> - Posets aren't guaranteed to have anything except the order "less
>   than";
>
> - lattices have all finite meets and joins;
>
> - distributive lattices obey an additional distributive law that
>   brings them closer to logic;
>
> - and frames have in addition all joins, and an infinite
>   distributivity law.

# 3.4 Lattices

> A poset Ρ is a lattice iff every finite subset has both a meet and a
> join.

> A function between two lattices is a lattice homomorphism iff it
> preserves all finite meets and joins.

注意，要求对有限集合存在 meet 和 join，
就也要求对空集存在，因此这种对 lattice 的定义要求存在 top 和 bottom。

其实，在术语使用上，不要求 lattice 带有 top 和 bottom，
用 bounded lattice 来定义带有 top 和 bottom 的 lattice。
这样更好。

满足两个分配律的 lattice 称为 distributive lattice。
注意，一个分配律，可以用来证明另一个。

Example 3.4.6: 对于全序集，可以定义 min 和 max，
因此形成 lattice（但是可能没有 top 和 bottom）。
并且这种 lattice 也是 distributive 的。

有两种基础的 non-distributive lattice 的反例。
Birkhoff [67] 证明了，每个 non-distributive lattice 中，
都能找到带有五个元素的子 lattice，与这两种反例之一同构。

# 3.5 Frames

> A frame is supposed to consist of the possible finite observations
> for some system, with equivalent observations identified, and the
> logic of finite observations (arbitrary disjunctions and finite
> conjunctions) built in as joins and meets.

Finite observation 在直觉上也非常符合 propagator model 中，
cell 所保存的信息，因此一个 cell 所保存的信息的类型就是 frame。

|            | order relation   | binary operation |
|------------|------------------|------------------|
| frame      | less than        | meet             |
| logic      | implies          | and              |
| set        | subset           | intersection     |
| propagator | more informative | merge            |

在 propagator model 中，
确实只会用到 finite merge，即 finite meet。
那么如何解释 frame 中的 join 呢？
为什么 join 需要是 infinite？
以及如何在 propagator model 中解释 frame 的其他概念？

> Definition 3.5.1  A poset A is a _frame_ iff
>
> (i) every subset has a join;
> (ii) every finite subset has a meet;
> (iii) binary meets distribute over joins.

> We write true for the empty meet (top) and false for the empty join
> (bottom).

> A function between two frames is a. frame homomorphism iff it
> preserves all joins and finite meets.

> Some authors use the terms locale or complete Heyting algebra (cHa)
> for what we have called a frame, and it is important to be aware of
> this. We follow Johnstone's [82] careful distinction between frames
> and locales (see Section 5.4), and also between frames and cHa's
> (Section 3.10).

> A frame is clearly a distributive lattice.

> It is also worth noting that clause (ii) of the definition is unnecessary:

因为对一个子集的下界的集合取 join，就得到这个子集的下确界。

> Such a poset, with all joins and hence also all meets, is called a
> complete lattice.

> This Proposition is at first sight rather curious, because it shows
> that even infinite meets exist in a frame. This will be discussed
> further in Section 3.10.

> We can now redefine frames as complete lattices satisfying the frame
> distributivity law.

# 3.6 Topological spaces

Topological space 是特殊的 frame，
即 power set 作为 frame 的 subframe。

> At present, topologies look like just a rather concrete way in which
> some particular frames can arise. But later we shall see how we can
> often reconstruct a topological space X from a frame A so that the
> frames ΩΧ and A are essentially the same (isomorphic).

# 3.7 Some examples from computer science

> **Finite observations on bit streams**

> The idea of a stream is that items of data are arriving one by one
> at a reading device. For simplicity, we assume that each item is a
> bit, 0 or 1.  We are going to describe a frame whose elements
> represent finite observations on some bit stream: in fact, we are
> going to describe three. The differences between them correspond to
> different physical assumptions and show how we can tailor a frame to
> our particular needs.

> The most elementary observations are the values of individual bits:
> so for each natural number n >= 1 we have two _subbasic_
> observations:
>
> - `(eq? (s n) 0)` -- the nth bit has been read as a zero
> - `(eq? (s n) 1)` -- the nth bit has been read as a one

或者简写做：

- `(s n 0)`
- `(s n 1)`

> Without doubt, we shall want never to read the same bit as both zero
> and one:
>
>     (and (s n 0) (s n 1)) = false
>
> We take
>
>     (or (s n 0) (s n 1))
>
> to mean that the nth bit has now been read, but we're not saying
> what its value was. On this interpretation, we build in an important
> physical assumption about streams:
>
>     (or (s n 0) (s n 1)) ≤ (or (s (add1 n) 0) (s (add1 n) 1))
>
> Recalling that `≤` means "implies", this means that you can't read
> a bit until you've read the previous one -- the bits come out
> strictly in order.

> We have now presented some "subbasic" propositions `(s n 0)` and `(s
> n 1)`, and some axioms that are supposed to hold for them. The idea
> is that this defines a logical theory, and that from this we can get
> a frame as a kind of Lindenbaum algebra. The mathematical
> justification for this, showing that this process does always lead
> to a well-defined frame, is non-trivial, and we postpone it to
> Chapter 4. For the present, and on the assumption that the frame
> does indeed exist, we shall show how we can play with the subbasics
> and the axioms to arrive at a more concrete definition of it.

应该以用 `01*` 的 string 来代表 finite observation，
以表示 stream starts with string，比如："01**1"。
我们可以称这种 string 为 prefix。

例如（index 从 1 开始，从 string 的左边开始数）：

```scheme
(make-prefix "01") =>
(and (s 1 0) (s 2 1))

(make-prefix "01*") =>
(or (and (s 1 0) (s 2 1) (s 3 0))
    (and (s 1 0) (s 2 1) (s 3 1)))
```

公理应该是：

```scheme
(≤ (s n 0) (s (add1 n) 0))
(≤ (s n 1) (s (add1 n) 1))
```

对于只有 `01` 的 string `l` `m` 来说：

```scheme
(implies (is-prefix-of l m) (≤ m l))
```

因为越长的 prefix 就是越多的 observation 的 meet，
包含更多的信息（在 frame 中被理解为是更「小」）。

反向的 implies 也成立。

这里用 upper closed sets 来构造 Alexandrov topology，进而得到 frame。
这样构造时，一个元素在 frame 中越小，它的 upper closed set 就越小。

TODO 这里构造 Alexandrov topology 的时候，
作者可能把生成元的下闭包写成上闭包了。

为什么不直接定义作为代数结构的 frame 呢？
可能是为了避免验证 frame 的公理。

> **Different physical assumptions**

> **(1) Different bits are read independently.**  This is more like an
> infinite read-only memory, where there is no obligation to read the
> data in order. Now, for instance,
>
>     (s 2 0) != (or (and (s 1 0) (s 2 0)) (and (s 1 1) (s 2 0)))
>
> The frame constructed here will have fewer equalities holding
> between the possible expressions than the first frame, so it has
> more elements.

> **(2) Time is not important.** We assume that every bit can be read
> sooner or later (so whatever is generating them is not allowed ever
> to stop). We don't distinguish between the bits that we have already
> read and those that we are going to read in the future, so the
> assumption is expressed by the formal axiom
>
>     (or (s n 0) (s n 1)) = true
>
> However, there are now more equalities holding than we had in our
> main example, so the frame here is smaller.

TODO 我没理解这里与原来例子的区别。

> **Flat domains**

> We have already seen that if `X` is any set, then `(powerset X)` is
> a frame. We can view the subsets as being observations on some
> object that has a value in `X`, `(subset-of S X)` being interpreted
> as
>
>     (or-for ((x S)) (it-is x))
>
> A singleton `{x}` represents an observation it's `x` that the object
> definitely has value `x`, while a larger set means that the object
> has been observed to be within some range, but not yet pinned down
> exactly.

这太符合 propagator model 了！

一般说 flat domain 的时候，并不构成 lattice，
描述 order set 的 graph 只有两层，
是 flat 的，所以才有 flat domain 这个命名。

但是这里是直接考虑 powerset 所构成的 frame（lattice），
这样就方便理解 propagator model 了：

|            | order relation   | binary operation | bottom        | top       |
|------------|------------------|------------------|---------------|-----------|
| frame      | less than        | meet             | bottom        | top       |
| logic      | implies          | and              | false         | true      |
| set        | subset           | intersection     | empty set     | whole set |
| propagator | more informative | merge            | contradiction | nothing   |
| domain     | less defined     | [trivial]        | error         | [no]      |

如果只考虑使用 flat domain 的 domain theory，
那么术语 less defined 是有问题的，
error 应该被理解为 over defined 而不是 less defined。

如果真的是这样，domain theory 和 propagator model 中，
使用 lattice 的方式就是相一致的了！
或者说使用 order set 的方式就是一致的了，
因为 flat domain 不是 lattice。

在 propagator model 中 bottom 总是代表 error（contradiction），
而不是代表 no information（nothing）。

可能由于一些作者在讨论 domain theory 时管 bottom 叫做 undefined，
所以我才会把 undefined 误解为 no information。

毕竟，一个 propagator 会把 nothing 映射成 nothing，
也会把 contradiction 映射成 contradiction。

注意，这里的构造还区分了两种 true：

- `(nothing <type>)` -- 知道类型，但是不知道具体的值。
- `any` -- 甚至不知道类型（也可以理解为动态类型）。

另外，这里所构造的 Alexandrov topology 是对 flat domain 所构造的，
而不是对 powerset 所形成的 lattice 构造的。

可以看出 frame 比 domain
和 domain 所得的 Alexandrov topology 的结构要丰富很多。

> **Function spaces**

> An important topic concerns the observations that can be made on
> functions. The general idea is that we know how to make observations
> on the result, and we also know how to manufacture arguments. A
> subbasic observation on a function `f` is then `[x => a]`, meaning
> "we have manufactured `x`, fed it to `f`, and observed a of the
> result `f(x)`". Let us imagine f as a black box.

这里可以看出 domain theory 只是给集合增加了 bottom，
但是 propagator model 中使用的其实是 frame，
既有 top（nothing）也有 bottom（contradiction 或 error）。

这里的例子也可能是 frame 这个术语的来源，
因为其 lattice 图示像是挂在墙上的相框。

注意，这里对 function 的 more informative 的讨论，
与函数类型的子类型关系不同。
其中对特殊参数 contradiction（文中称 nothing）的讨论，
类似 propagator 中的某个针对特殊值转换 order 方向的构造。

> If the function allows us to observe a on the basis of no input at
> all, it is not allowed to retract that if we subsequently put in a
> more solid x.

TODO 到底能不能把这里的 nothing
理解为 propagator model 中的 contradiction？
看上面的讨论感觉这样理解也不太对。

TODO 可否把这里对函数（partial function）的 finite observation 的讨论，
理解为，可以保存 propagator 的 cell？

# 3.8 Bases and subbases

> It's worth paying attention to a particular part of the argument in
> Section 3.7. Quite often in a frame we want to say that we start off
> with some simple assertions and generate the rest from these using
> the frame operations meet and join. The simple assertions are called
> _subbasic_, and they form a _subbasis_.

> In any frame, the steps of generation can be done as:
>
> - Step 1: Finite conjunctions of subbasics;
>
> - Step 2: Disjunctions of finite conjunctions of subbasics;
>
> - Step 3: Anything more complicated can be reduced to a step 2
>   expression by using the frame distributivity law.

> Sometimes we can do better and say that any assertion is a
> disjunction of subbasics (we don't have to bother with the finite
> conjunctions). Then the subbasis is called a _basis_, and its
> elements are _basic_.

> For example, given any subbasis, the finite conjunctions of
> subbasics form a basis.

# 3.9 The real line

> We now describe a frame of assertions for real numbers. It is based
> on the idea of taking measurements of some real-valued quantity, all
> measurements giving results that are _rational_, and _subject to
> error_. We write a subbasic assertion as `(q ± ε)` where `q`, a
> rational number, is the result, and `ε`, a _positive_ rational
> number, is the possible error.

这就是我大学的时候所看的分析学的书，
处理无穷小分析的方式。

这也符合 propagator model 处理 interval 的方式。

> If `x` is a given real number, then we write `X |= (q + ε)` (`x`
> _satisfies_ the assertion) iff `x` is a possible exact value for the
> quantity, i.e. iff `q-ε < x < q+ε`.

> Having the strict inequality < here enables us to avoid the
> embarrassing possibility of pinning a value down to an exact real
> number.  If we measure both `(0 ± 1)` and `(2 ± 1)`, then we
> deduce not that the quantity has a value of exactly `1`, but that
> something is wrong with the measurements.

> A finite conjunction of these subbasic measurements is either false
> (contradiction, fault in measurements) or another such
> measurement.

> Thus the subbasics form a basis.

对 conjunction 的封闭性，使得 subbasis 变成 basis。

> Disjunctions [maybe infinite] give us many new assertions, e.g.
>
> - x > 0
> - x != 0
> - x > square root of 2

open set 定义为不带有边界点的集合，
即，open set 中的每一个点，
都有一个这个点的开领域，
完全包含在这个 open set 中。

显然所有的 open set 都可以用 basis 的 disjunction 构造出来。

# 3.10 Complete Heyting algebras

> In Chapter 2, we carefully ruled out infinite conjunctions,
> implication and negation from our logic. Curiously enough, frames
> have operations to support these connectives. The reason for
> ignoring them is not that they don't exist, but that they are less
> well-behaved.

> We have already seen, in Proposition 3.5.2, that a frame is a
> complete lattice and hence has infinite meets. Apparently, this
> contradicts our argument in Chapter 2 that infinite conjunctions
> were inadmissible in the logic of finite observations.  However, the
> correct conclusion is that the infinite meets do not represent true
> conjunctions. In a topology,

也就是说，只有考虑了代数结构之间的同态，
才能捕捉 finite observation 的逻辑。

对 judgement 的判断，可以用同态（态射）表示！

frame 是 complete lattice，
complete lattice 如果有无限 join，
就也有无限 meet。

无限 meet 之所以不能出现在 finite observation 的 logic 中，
是因为它们不代表 true conjunction。

可以做无限的 conjunction，
但是无限个 assertion 的 true conjunction 的结果，
可能不是 true 了。

frame 的同态要保持有限 meet 和无限 join。
在考虑一个 frame 到 `{true, false}` 的同态时，
并不要求保持无限 meet，如果要求了很多同态就定义不了。

比如，当 `ε` 无限缩小时（无限交），`(q ± ε)` 闭集（一个点），
无限 meet 是无限交的内部（interior），为空（false）。

> Next, we look at implication.

> Definition 3.10.1 Let A be a lattice. A is a Heyting algebra iff for
> every a, b in A there is an element `a -> b` satisfying
>
>     c ≤ a -> b iff с and а ≤ Ь
>
> A Heyting algebra homomorphism is a function f between Heyting
> algebras that preserves finite meets and joins, and also the `->`
> operation:
>
>     f(a -> b) = f(a) -> f(b)
>
> A Heyting algebra A is a complete Heyting algebra (cHa) iff it is a
> complete lattice.
>
> A cHa homomorphism is a function between cHa's that is a Heyting
> algebra homomorphism and moreover preserves all joins.

> Proposition 3.10.2 Let A be a lattice.
> Then A is a frame iff it is a cHa.

在 frame 中，由于无限交存在，
所以可以直接用所有满足 `с and а ≤ Ь` 的 `c` 的无限交，
定义 `a -> b`。

反过来的证明，需要验证 frame 的分配律。

> In themselves, therefore, frames and complete Heyting algebras are
> the same thing. However, the notions become different when we
> consider homomorphisms, in other words when we compare two
> algebras. A frame homomorphism need not preserve the -> operation,
> and hence need not be a cHa homomorphism.

结构等价，但是同态不同的情况，我还是第一次见。

> If we define negation by
>
>     not a = a -> false
>
> then any Heyting algebra is a model for propositional intuitionistic
> logic. Roughly speaking, this is the same as classical logic except
> that the law of excluded middle, `x and not x` might fail.

> This means that any frame, and in particular that of open sets of a
> topological space, forms such a model.

这里有一段对 intuitionistic logic 的模型论的讨论。
值得深究。

# 4 Frames as algebras

> In which we see methods that exploit our algebraicizing of logic.

## 4.1 Semilattices

> Logic on the whole has both conjunctions and disjunctions, and so it
> seems natural to think in terms of lattices. However, in the
> algebraic development upon which we now embark, it is convenient to
> consider _semilattices_, which have only one of these.  As we shall
> see, to investigate the algebra of frames, we shall look first at
> the meet on its own and then at the joins when they are added in.

- meet-semilattice -- close over binary meet.
  binary to rule out the empty case from finite meet.
- join-semilattice -- close over binary join.
- lattice -- close over binary meet and binary join.
- bounded lattice -- has top and bottom.
- complete lattice -- close over infinite meet and join.

meet-semilattice 作为代数结构是：
交换幺半群（monoid）
外加幂等性（idempotence）。

只要把序关系 x less than y 定义为 x meet y = x
就和 poset 所定义的 semilattice 等价了。

> For completeness, we note that lattices (and hence distributive
> lattices) and Heyting algebras can also be described algebraically
> in this way (see Exercise 1). In a sense, frames can too. But the
> join in frames, being a possibly infiniiary operation (the join of
> infinitely many elements) falls slightly outside the scope of the
> usual algebraic methods, and Section 4.4 is devoted to showing that
> this doesn't matter.

## 4.2 Generators and relations

> Suppose we want to describe a frame. One method, which we have
> already seen informally (in Section 3.7) but not yet justified,
> presents a frame in four steps.
>
> - Step 1: Specify some subbasic elements (generators).
>
> - Step 2: Derive from these all possible joins of meets of
>   subbasics.
>
> - Step 3: Specify certain axiomatic relations to hold between
>   expressions of Step 2.  They can be of the form e1 ≤ e2
>   (inequations) or e1 = е2 (equations). It doesn't matter which you
>   use, because, as in Proposition 4.1.2, the two forms are
>   interconvertible.
>
> - Step 4: Deduce, just from the relations and the frame laws, when
>   any two given expressions must be equal. This, then, is an
>   equivalence relation on the Step 2 expressions.

> From one point of view, this is a method of logic. The generators
> are the primitive propositional symbols, the Step 2 expressions are
> well-formed formulae, the Step 3 relations (written as inequations)
> are the axioms, and the Step 4 equivalence is mutual entailment.

generators 只是被当作 primitive propositional symbols，
而不能是 predicate 所构成的 relation 吗？

前面的例子，比如 stream 中的关系 `(s i b)`，和 real line 中的区间，
难道都要被理解为 primitive propositional symbols 的集合？
只是这个集合本身带有一定结构，正是利用这些结构，
我们才能描述 propositional symbol 之间的 relation。
是这样吗？

这样说来，谓词演算中的 relation 也可以被理解为是在描述，
propositional symbols 的集合，只不过这些集合本身有一定的结构。

> However, it also represents a very general method of Universal
> Algebra, enabling us in a wide class of algebraic theories to
> present an algebra by writing down generators and relations for it.

即，generators 和 relations 所构成的 presentation theory，
最典型的是 presentation of group and groupoid。

下面说 infinite join 会带来困难，不是因为生成元有有限个，
而是因为在代数中，用运算构造新项的时候，只能用到有限多项。

比如，处理无穷多项的相加时，
就已经到了无穷级数理论，
而超越了代数的范围。

> Unfortunately, for the theory of frames the infinite joins give rise
> to obstacles to formalizing the general argument. In Section 4.4 we
> shall see that these can be overcome, but meanwhile we shall
> concentrate on developing the practical intuition.

> Assuming that the method does indeed define a frame, we write it as
>
>     Fr < generators | relations >
>
> and call this a _presentation_ of the frame.

用 lisp 的语法可以写成：

```scheme
(make-frame (<generator> ...)
  <relation>
  ...)
```

Example 4.2.1:

```scheme
(make-frame ()) => [true false]
```

Example 4.2.2:

```scheme
(make-frame (a b)) => [true false a b (and a b) (or a b)]
```

Example 4.2.4:

- 假设 `less` 和 `equal` 都代表 relation，
  而不是 `equal?` 这类 decidable 的谓词。

```scheme
(make-frame (a b) (less a b)) => [true false a b]

(equal-to-chart (and a b) a)
(equal-to-chart (or a b) b)
```

> In general, if Τ is any algebraic theory (described by operators
> and equational laws) for which this method works, then we write
>
>     Τ < generators | relations >
>
> for the T-algebra presented by the given generators and relations.

因此也许应该用一个一般的 deifne 类的语法关键词：

```scheme
(define-presentation <algebra> (<generator> ...)
  <relation> ...)
```

```scheme
(define-presentation frame (a b)) =>
[true false a b (and a b) (or a b)]

(define-presentation semilattice (a b)) =>
[true a b (and a b)]
```

## 4.3 The universal characterization of presentations

> As a preliminary, we quickly summarize the standard definitions of
> universal algebra.

> Definition 4.3.1 Let `Τ` be an algebraic theory. It has some
> _operators_, each with an _arity_ (the number of arguments it has),
> and some _laws_, each of the form `e1 = e2`, where `e1` and `e2` are
> expressions formed from a convenient stock of variables by applying
> the given operators. We define `A` to be a T-algebra iff
>
> - (i) `A` is a set (often known as the _carrier_ of the algebra);
>
> - (ii) for each operator `ω` of `Τ` (say `ω` is an n-ary
>   operator), `A` is equipped with a corresponding _operation_,
>   a function `ω: Α^n -> A`.

我不需要 a convenient stock of variables，可以临时引入约束变元。
我也常称 element 而不是 carrier。

| universal algebra  | logic     | programming |
|--------------------|-----------|-------------|
| algebraic theory   | syntax    | class       |
| operator           | function  | method      |
| law                | predicate | constraint  |
| (concrete) algebra | model     | object      |

假设用如下语法定义 algebra：

- 先保持最简的设计，不要使用 dependent type。

```scheme
(define-algebra <name>
  ((<operator> <arity>) ...)
  ((= <exp> <exp>) ...))
```

Examples 4.3.2:

```scheme
(define-algebra semilattice
  ((true 0) (meet 2))
  ((= (meet x y) (meet y x))
   (= (meet x (meet y z)) (meet (meet x y) z))
   (= (meet x true) x)
   (= (meet x x) x)))
```

还有一个例子是 frame，
有限交可以用二元运算处理，
如何在 lisp 语法中处理 frame 的无限并？

```scheme
(define-algebra frame
  ((true 0) (meet 2) (join-forall *))
  ((= (meet y (join-forall ((i I)) (x i)))
      (join-forall ((i I)) (meet y (x i))))))
```

需要特殊处理 `y` 在集合 `(set-forall ((i I)) (x i))` 中的情形，
此时结果是 `y` 本身，而不能把 `y` 分配到 `join-forall` 中。

这里还定义了 model for presentation，
但是 presentation 已经是具体的代数结构了，
所以这里定义的是 presentation 所代表的代数结构，
到另外一个具体代数的同态。

> Notice the two different uses of equations, in the laws that were
> part of Τ and in the relations in a presentation. In a law, an
> equation contains variables, and the equation must always hold,
> whatever values from an algebra are substituted for the
> variables. In a relation, the equation contains generators, and the
> equation must hold when the generators are given their particular
> values in a model.

law 中有变元，并且理解为全称量词下的约束变元；
而 relation 中，只允许出现常量（generators）。

这么说来 combinatory logic 中，关于 SKI 组合子的等式是什么？

因为带有全称量词，所以应该算是 law，
但是这些等式确是用来定义 SKI 三个元素的，
这三个元素又像是 magma 的生成元。

当然，另外一个区别是，SKI 的等式带有方向，这个方向代表了计算。

一种理解方式是消除 presentation 中对 relation 的限制，
像 law 一样，允许在其中使用全称量词与约束变元。
可以理解为，combinatory logic 是一种高阶的 universal algebra，
在这种高阶 algebra 的 presentation theory 中，没有固定的 operators，
所定义的 generators 就是 algebra 中的 operators。

在定义了 model for presentation（同态）之后，
还定义了 [model] is presented by presentation，
后者就是同构了。

书中 "presented by presentation"
是用 universal property 来定义的。

> Because the second condition applies to all models B, relating them
> to A by homomorphisms, it is called a _universal_ property. Notice
> carefully the word _unique_. It is an essential part.

其实把 presentation 本身当作具体的代数结构就可以了，
没必要再用同构来说明。

也许每次遇到范畴论中关于 universal property 的抽象废话的时候，
就可以把想要定义的满足给定 universal property 的存在，
当成是具体的存在本身！

## 4.4 Generators and relations for frames

> There are infinitary theories (such as that of complete Boolean
> algebras -- see Johnstone [82]) where this is insuperable and
> presentations simply don't present algebras. For frames,
> fortunately, presentations do present, but we have to argue slightly
> carefully to show this. The trick is to import our knowledge of the
> frame laws into Step 2 and say that all possible expressions are
> joins of finite meets of generators.

对于允许无穷多元素一起运算的代数结构（infinitary），
比如 frame 的无限 join，
其展示（presentation）理论成为难点。

这里的构造用到了 cover，coverage 和 满足 coverage 的 ideal。

在 join-semilattice 中，
cover 描述一个元素 x 如何被一组元素 u1, u2, ..., un 的并（join）所覆盖：

    x ≤ u1 ∨ u2 ∨ ... ∨ un

我们讨论的是 meet-semilattice，没有 join。
但是我们可以通过把有限子集作为 cover 添加进来。

先从 frame 的 presentation，
得到 meet-semilattice 的 presentation。

> First, rewrite the relations in equational form `e1 = e2`, where
> each expression `ei` is a join of finite meets of generators.

> Second, for each relation `e1 = e2` introduce a new generator `x`
> and replace the relation by `e1 = x` and `e2 = x`.

> Third, present a semilattice `S` as follows: its generators are
> those of the frame, and for each frame relation
>
>     (join (meet ...) ...) = x
>
> it has relations
>
>     (meet ...) ≤ x

> Fourth, generate a coverage `С` on `S` from the cover relations
>
>     {(meet ...)} -| x
>
> and take the frame `C-Idl(S)`.

这个过程像是代数中常见的 completion 过程。
也许和这个等同：https://en.wikipedia.org/wiki/Dedekind-MacNeille_completion

> Generators and relations are nowadays a standard method in algebra,
> as well as being related to the axiomatic methods of logic.
> Johnstone [82] (Notes on chapter II) discusses the history of their
> application to frames; he himself introduced the method of coverages
> that we describe.

可能需要看一下 Johnstone [82]。

> Technically, what makes universal algebra go wrong for complete
> Boolean algebras is that there is no free complete Boolean algebra
> on a countably infinite set of generators.

> Also, there can be no free cHa on a countably infinite set of
> generators: for if there were one, then the free complete Boolean
> algebra could be constructed as a quotient. This puts cHa's in stark
> contrast with frames, even though they are the same objects.

# 5 Topology: the definitions

> In which we introduce Topological Systems, subsuming topological
> spaces and locales.

## 5.1 Topological systems

这里描述的系统很像类型系统，就是把类型解释为 frame 中的元素。

这是当然的，因为这里描述的是 topological space 的推广，
把开集从集合推广为一般的抽象开元素（frame 中的元素）。
而 topological space 中 point 和 open set 的关系，
就是集合的属于关系，也就是元素和类型之间的关系。

这样的推广之后，就能不用考虑集合吗？
不是的，因为给定 frame 的元素 `a`，
所有满足 `x |= a` 的 `x` 就是 `a` 所对应的集合。

这样就得到了一个 frame 到集合的映射，
这种从 topological system 重新构造出 topological space 的方式，
称为 spatialization。

注意，不同的 frame 元素可能被映射到不同的集合。
也就是说，这里的「满足」关系，
与集合论的「属于」关系不同，
不满足集合论的外延公理。
因此一个 topological system 的 spatialization
作为 topological system 可能与原 system 不同。

## 5.2 Continuous maps

如果把一个 topological system 解释为类型系统（即一个程序语言），
那么这里的 continuous map 就成了语言之间的编译器。

但是想要的解释看来是在一个类型系统内，
把一个 topological system 解释为一个类型。

一个类型本身，就会形成一个由其子类型构成的 frame。

## 5.3 Topological spaces

> **Spatialization**

> We stick with topological spaces if we take the view that there is
> no reason to distinguish between two opens when they're satisfied by
> exactly the same points.  This is reasonable for fixed points like
> the real numbers, but perhaps it is less so when we're not quite
> sure what the points are, like the streams.

> If we want to, we can always convert a topological system D into a topological
> space, by comparing opens entirely by the points that satisfy them.
>
> **Definition 5.3.3** For each open `a`, its _extent_ in `D` is
> `{x in (pt D): x |= a}`.

给定一个 topological system，
就引出一个从 frame 到 powerset 所构成的 frame 的同态。

> It is often more helpful to know not that a topological system is a
> topological space, but that it is homeomorphic to one. We say that
> such a system is _spatial_.

## 5.4 Locales

> Just as in spatialization We thought of an open as being a set of
> points, in other words a function (the characteristic function of
> the set) from the points to 2, we can also think of a point as being
> a function from the opens to 2.

这里的 2 是有两个元素的 frame，代表 boolean 类型。

一个类型将所有的点分成两类，是否满足这个类型；
一个点也将所有的类型成两类，是为这个点所满足。

只单独给出一个 frame，
用它自身就可以构造一个 topological system：

> **Definition 5.4.1** Let `A` be a frame. The _locale_ corresponding
> to `A` is the topological system `D` defined by
>
>     Ω D = A
>     pt D = {frame homomorphisms x: A —> 2}
>     x |= a iff x(a) = true
>
> By abuse of language, we shall sometimes refer to "the locale A"
> rather than inventing a new symbol `D`.
>
> A topological system is _localic_ iff it is homeomorphic to a
> locale.

## 5.5 Spatial locales or sober spaces

> We know that a continuous map between spatial topological systems is
> completely determined by its function part; while a continuous map
> between localic topological systems is completely determined by its
> frame part. Therefore if the systems are both spatial and localic,
> there is a complete duality ("duality" implies the order reversal)
> between the continuous functions and the frame homomorphisms.

> The point of this is that we move freely between two rather
> different ways of reasoning.
>
> - On the points side, arguments are set theoretic with some extra
>   notions for topology. For instance if U and V are open sets, then
>   we show U ⊆ V by showing that if x is a point in U then it's also
>   in V.
>
> - On the frame side, arguments are logical or algebraic. To show
>   that U ≤ V in a frame, we show that it follows from the relations
>   with which we present the frame.  Thinking of the relations as
>   logical axioms, this amounts to a logical proof that U entails V.

> In locale theory, we can think of the axioms and algebra as giving
> the syntax of a frame, and the points as giving the semantics (each
> point is a model).

> The syntax is automatically sound (if U ≤ V then extent(U) ⊆
> extent(V)), but completeness is equivalent to spatiality (if
> extent(U) ⊆ extent(V) then U ≤ V).

> Therefore:
>
> A proof of spatiality of a locale shows completeness of a logical
> system.

> Our discussion shows that for a topological space, sober and localic
> mean the same. Localification of a space is usually called
> _soberification_.

> **Proposition 5.5.3** Hausdorff spaces are sober.

## 5.6 Summary

> The "topological systems" described in this section are new. My
> justification for introducing them is, first, that it seems
> pedagogically useful to have a single framework in which to treat
> both spaces and locales and, second, that with domains -- which are
> both spaces and locales -- it is useful not to have commit oneself
> to making them concretely either one or the other.

> Although the results are new in the sense that they use topological
> systems, they are essentially no more than rephrasings of the
> established connections between spaces and locales, as described in
> Johnstone [82].

# 6 New topologies for old

> In which we see some ways of constructing topological systems, and
> some ways of specifying what they construct.

## 6.1 Subsystems
## 6.2 Sublocales
## 6.3 Topological sums
## 6.4 Topological products

# 7 Point logic

> In which we seek a logic of points, and find an ordering and a weak
> disjunction.

> The satisfaction relation `x |= a` is supposed to describe some
> finitely observable relationship between points and opens, and so
> far we have been thinking of `a` as the observation and `x` as what
> is observed.

本体论意义上：

- "what is observed" 先存在在先，可以理解为动态类型语言中的 value；
- "finite observation" 依赖于被观察的对象，可以理解为对 value 的 predicate。

反过来：

- 由类型所表示的命题存在在先。
- 然后人们才寻找命题的证明，证明在后。
  同一个命题可能有很多不同证明。

这是不对的，因为是现有对命题的部分归纳验证，
充分理解了命题，人们才会去证明一个命题，证明是有限验证的极限。
也就是说，value 是本体，predicate 是衍生概念，
一个 predicate 刻画所有 value 的集合的一个子集。

> However, for `x` and `a` in isolation, there is no overriding reason
> for this; why can't we think of `x` as being the open, making an
> observation about `a` as a point?

## 7.1 The specialization preorder

> **Definition 7.1.1** Let `x` and `y` be two points. We say `y`
> specializes `x`, and write `x ⊑ у`, iff for every open `a`,
> if `x |= a` then `y |= a`.

Dana Scott 考虑函数之间的序关系的时候，考虑的就是这种序关系。
因此 `y` specializes `x` 也可以成为是，
`y` is more defined than `x`。

在带有 record 的类型论中，这种关系最明显：

```scheme
(⊑ [:x 1 :y 2] [:x 1 :y 2 :z 3])
(more-defined [:x 1 :y 2 :z 3] [:x 1 :y 2])
```

对于函数（partial function）而言，
"more defined" 就代表结果一致的情况下，
能处理更多参数。

> In other words:
>
> - `y` satisfies at least all the opens satisfied by `x`;
> - or we can say more about `y` than about `x`;
> - or `y` represents a superior, or more refined
>   state of information than `x`.
>
> This leads to various synonyms for specialization:
>
> - y specializes x
> - y refines x
> - x ⊑ y
> - x approximates y
> - x implies y (thinking of x and y as properties of opens).

在读 Scott 时，我一直觉得它所定义的序关系是反的，
但是看 `forall a.  x |= a  ->  y |= a` 这个逻辑蕴含关系，
感觉这个序关系的方向也是合理的，
毕竟集合之间的蕴含关系就有类似的形式：
`A ⊆ B` 定义为 `forall x.  x ∈ A  ->  x ∈ B`。

考虑 Galois connection 和 formal concept analysis，
也可以更好地理解这里序方向的反转。

为什么会觉得这个序关系设计反了？
因为对于 record type 而言，
对 attribute 个数的多少有相反的理解：

```scheme
(less-defined
 [:x 1 :y 2]
 [:x 1 :y 2 :z 3])

(subtype
 (tau :x int-t :y int-t :z int-t)
 (tau :x int-t :y int-t))
```

越大的集合包含更多的元素；
越具体的元素属于更多的集合。

比如 `[:x 1 :y 2 :z 3]` 比 `[:x 1 :y 2]` 更具体，
后者是属于类型 `(tau :x int-t :y int-t)` 前者也属于这个类型，
同时前者还属于类型 `(tau :z int-t)` 但是后者不属于这个类型。

所以说，首先应该区分元素之间的序关系，与类型之间的序关系。
这种考虑只有在 structural type system 中才有意义，
因为如果每次定义新类型时，也在定义新元素，
就没有属于关系之间的相互蕴含了。

这种反方向的序关系的出现，
引起我困惑的另一个原因是，
在 propagator model 中讨论 more informative 时，
我把它理解为了序关系中的 less，
并且把 merge 理解为了 lattice 中的 meet。
可能这是不对的，应该反过来，遵循这里的 specialization preorder。
因为 cell 中保存的并不是 type 而是 value。

也许以后在遇到序关系的时候，
都应该让所尝试捉的直觉上的 more or less，
与序关系中的 more or less 方向一致，
因此 more defined 和 more informative 都应该是序关系中的 more。
如果需要相反的方向，可以再取 dual。

比如，用 propagator model 实现类型系统时，
cell 中保存的就是被视为 value 的 type 了，
此时可以按需要取 dual。

> **Proposition 7.1.3** In any Hausdorff topological space,
> `x ⊑ y` iff `x = y` (the specialization ordering is discrete).

## 7.2 Directed disjunctions of points

value 之间的最典型的格与序关系，
是由 unification 与 substitution 构成的，
这里所描述的 value 之间的序关系是否类似？

这里论证如果想要把两个 value x y 之间的 join，
解释为逻辑的 disjunction，
就必须假设两个元素 x y 之间已经有了序关系。

后面会论证 value 之间的 meet 也不能解释为 conjunction。
但是这就已经与 propagator model 使用 lattice 的方式不同了，
因为 propagator model 要求 value 之间可以 merge。
究竟应该如何理解 propagator model 中的 lattice。

> **Definition 7.2.1** Let `⊑` be a preorder on a set `X`.  A subset
> `S ⊆ X` directed iff every finite subset of `S` has an upper bound
> in `S`. (Note that `S` cannot be empty, because it must contain an
> upper bound for the empty set.)

只要求存在 upper bound，
不要求对 join（least upper bound）封闭。

> The prime example is a linearly ordered subset of `X`.

## 7.3 The Scott topology

TODO

# 8 Compactness

> In which we define conjunctions of points and discover the notion of
> compactness.

## 8.1 Scott open filters
## 8.2 The Scott Open Filter Theorem
## 8.3 Compactness and the reals
## 8.4 Examples with bit-streams
## 8.5 Compactness and products

# 9 Spectral algebraic locales

> In which we see a category of locales within which we can do the
> topology of domain theory.

## 9.1 Algebraic posets
## 9.2 Spectral locales

> Reasoning that finite joins are nicer than infinite ones, we now
> investigate frames that can be presented without infinite joins. It
> turns out that this is related to algebraicity of the frame. In the
> next section we shall introduce algebraicity also for the dcpo of
> points, so that it is present on both sides.

> **Definition 9.2.1** A presentation of a frame is coherent iff there
> are no infinite joins in its relations.

> A locale D is spectral iff Ω D is coherent.

## 9.3 Spectral algebraic locales
## 9.4 Finiteness, second countability and co-algebraicity
## 9.5 Stone spaces

# 10 Domain Theory

> In which we see how certain parts of domain theory can be done
> topologicals.

## 10.1 Why domain theory?

用集合去解释程序中的类型，
就是让每一个类型对应一个集合，
此时会遇到和集合基数相关的悖论。

这里给出的产生悖论的例子，
还不是要给 lambda calculus 找 domain，
而是更贴近日常编程的例子。

> The problem here is with the function type, which is where the
> cardinality blows up. The solution is that we don't actually need
> all the functions that, set-theoretically, are supposed to exist;
> all the functions follow routines stored in the computer, and hence
> must be _computable_. These are far fewer than the general
> functions, few enough in fact to resolve the contradiction for us.

> Now we see that our "sets of possible values" for the types must
> have more than just their set structure, so that we can determine
> which functions to admit. We don't really need to investigate in
> detail what computability is, as long as the extra structure allows
> us to omit enough uncomputable functions. Speaking rather broadly,
> we might call a _domain_ a set with this extra structure, whatever
> it is.

## 10.2 Bottoms and lifting

TODO

## 10.3 Products
## 10.4 Sums
## 10.5 Function spaces and Scott domains
## 10.6 Strongly algebraic locales (SFP)
## 10.7 Domain equations

# 11 Power domains

> In which we investigate domains of subsets of a given domain.

## 11.1 Non-determinism and sets
## 11.2 The Smyth power domain
## 11.3 Closed sets and the Hoare power domain
## 11.4 Tne Plotkin power domain
## 11.5 Sets implemented as lists

# 12 Spectra of rings

> In which we see some old examples of spectral locales.

## 12.1 The Pierce spectrum
## 12.2 Quantales and the Zariski spectrum
## 12.3 Cohn's field spectrum
