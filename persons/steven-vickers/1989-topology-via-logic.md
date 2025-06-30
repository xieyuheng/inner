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

frame 是 complete lattice，
complete lattice 如果有无限 join，
就也有无限 meet。

无限 meet 之所以不能出现在 finite observation 的 logic 中，
是因为它们不代表 true conjunction。

可以做无限的 conjunction，
但是无限个 assertion 的 true conjunction 的结果，
可能不是 true 了。

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

## 4.1 Semilattices

TODO

## 4.2 Generators and relations
## 4.3 The universal characterization of presentations
## 4.4 Generators and relations for frames

# 5 Topology: the definitions

## 5.1 Topological systems
## 5.2 Continuous maps
## 5.3 Topological spaces
## 5.4 Locales
## 5.5 Spatial locales or sober spaces
## 5.6 Summary

# 6 New topologies for old

# 7 Point logic

# 8 Compactness

# 9 Spectral algebraic locales

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

这应该是我看到的对 domain theory 的动机最清晰的描述了。

## 10.2 Bottoms and lifting

TODO

## 10.3 Products
## 10.4 Sums
## 10.5 Function spaces and Scott domains
## 10.6 Strongly algebraic locales (SFP)
## 10.7 Domain equations

# 11 Power domains

# 12 Spectra of rings
