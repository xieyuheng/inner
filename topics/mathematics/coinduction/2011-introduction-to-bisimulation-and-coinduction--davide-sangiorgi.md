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

与 deepseek 关于这本书的讨论：
- https://chat.deepseek.com/a/chat/s/94755701-a749-404d-a861-08a33bbba4af

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

## 0.5 Basic definitions and mathematical notation

介绍集合论和关系，以及关系的诸多属性：

> - reflexive if x R x, for all x ∈ X;
> - symmetric if x R x′ implies x′ R x;
> - transitive if x R x′ and x′ R x′′ imply x R x′′;
> - irreflexive if there is no x with x R x;
> - antisymmetric if x R y and y R x implies x = y;
> - total if any pair of elements in the set are comparable
>   (x R y or y R x holds, for all x and y);
> - an equivalence if it is reflexive, symmetric and transitive;
> - a total order if it is antisymmetric, transitive and total [thus reflexive];
> - a partial order if it is reflexive, antisymmetric and transitive;
> - a preorder if it is reflexive and transitive;
> - well-founded if there are no infinite descending chains:
>   ... R xi R ... R x1 R x0;
> - non-well-founded if there are infinite descending chains.

> Note that if R is well-founded then R must be irreflexive.

> Moreover, if R is a well-founded relation on X, then any non-empty
> subset S of X has at least one minimal element (an element x with x
> ∈ S and such that there is no y ∈ S with y R x).

介绍函数作为特殊的关系，并且说明在讨论 partial function 时，
用关系来理解函数是有利的，因为一般说到函数时都要求 total function。

介绍 ordinal numbers，
并且说明它经常用来讨论 well-ordered [infinite] sets，
比如 array 的 lexical order。

# 1 Towards bisimulation

介绍 bisimulation 技术如何起源于 process calculi。

## 1.2 Interaction and behaviour

> In the example of Section 1.1, the program fragments
>
>     X := 2  and  X := 1; X := X + 1
>
> should be distinguished because they _interact_ in a different way
> with the memory. The difference is harmless within a sequential
> language, as only the initial and final states are visible to the
> rest of the world. But if other concurrent entities have access to
> the same memory locations, then the patterns of the interactions
> with the memory become significant because they may affect other
> activities.

> This brings up a key word: _interaction_. In concurrency,
> computation is interaction.  Examples are: an access to a memory
> cell, a query to a database and the selection of a programme in a
> washing machine. The participants of an interaction are the
> _processes_.

> The _behaviour_ of a process should tell us _when_ and _how_ the
> process can interact with the outside world -- its _environment_.
> Therefore we first need suitable means for representing the
> behaviour of a process.

> In the book, we will consider a particularly simple case: the
> interactions of the process with its environment are pure handshake
> synchronisations, without exchange of values. We hope this will make
> the material easier to understand. The transport of the bisimulation
> concept to other interaction models is the main topic of Chapter 7.

### 1.2.1 Labelled transition systems

> The behaviour of the machine is what we can observe, by interacting
> with the machine. This means experimenting with the machine:
> pressing buttons and seeing what happens. We can observe which
> buttons go down and when, which beverages [outputs] we can get and when.

用 labelled transition systems（LTS）来描述 process 的行为。
而定义 process 的就是 process 的行为，所以 LTS 就是 process 的定义。

也就是说，我们用 labelled directed graph 这个近乎拓扑的概念，
捕捉了 process 这个概念，为 process 这个概念提供了具体的数学模型。
至少就「行为」所定义的 process 之间的等价关系而言，捕捉到了。

就像给地铁画地图时，所用的 labelled directed graph，
抽象掉了地铁站的具体细节，只保留了坐地铁的人所主要关心的信息。

LTS 与 automata 非常相似：

- LTS 的 label 可能代表 output，而不只是 input。

- automata 中的「转移函数」被改成了「转移关系」，
  process 遇到 event 时，可能转移到多个 state。

- LTS 不指定起始状态与结束状态，
  这样更方便用代数工具处理。

  - 类似欧式空间和仿射空间之间的关系。

LTS 的组成部分：

- processes (states)
- actions (labels, events, input output)
- transition relation

### 1.2.2 Notation and terminologies for LTSs

## 1.3 Equality of behaviours

### 1.3.1 Equality in Graph Theory: isomorphism

凡是 isomorphic 的 graph 都代表相同的 process，
但是不是所有相同的 process 的 graph 都是 isomorphic 的。

这里给出的例子：

```
P1 -a-> P2
P2 -b-> P1

Q1 -a-> Q2
Q2 -b-> Q3
Q3 -a-> Q2
```

看来可以用拓扑变换（retraction）来完成等价。

那么，如果不考虑研究 process，
反过研究代数拓扑结构，
是否可以用 process 的方法，
来研究拓扑结构的不变量？

process 的 LTS，可以想象成是，
在一个 graph 构成的空间中运动的点。
把 graph 推广到高维的拓扑结构，
然后再找到对应于 process 的概念，
应该就能完成这种反向的研究。

### 1.3.2 Equality in Automata Theory: trace equivalence

由于固定了起始点，
所以 automata（允许 non-deterministic automata）之间的等价，
可以用其所能识别的 string 的集合之间的等价来定义。

就识别 string 而言，在 non-deterministic 的时候，
automata 可以尝试多个选择，其中一个能成功，就算成功识别了 string。
但是对 process 而言，没有这个选择，
而是需要能区分成功与失败路径（fig 1.4），
并且区分 process non-deterministic choice
和 deterministic choice（fig 1.3，fig 1.5）。

## 1.4 Bisimulation

process 之间的等价称为 bisimilarity，
被定义为存在 bisimulation，
也就是说可以构造出来 bisimulation。
之后会证明 bisimilarity 是等价关系。

构造 bisimulation 的过程，
就是 luca 在递归类型的 subtype 算法中使用的 taril，
taril 作为笛卡尔积的子集就是 bisimulation 关系。

可见这种定义有很强的算法属性，
定义中的「对于任意 P 的 action，存在对应的 Q 的 action」，
可以作为在构造算法中，检验 P Q 不相等的依据。

可以想象成一个运动的点，
在一个 process P 的 LTS 中所走出来的任何 path，
按照这个 path 在另一个 process Q 中也能走通。

> Reducing the size of the relation to exhibit, and hence relieving
> the proof work needed to establish bisimilarity results, is the
> motivation for the enhancements of the bisimulation proof method
> discussed in [PS12].

这里的引用是：

- [PS12] D. Pous and D. Sangiorgi.
  Enhancements of the bisimulation proof method.
  In Sangiorgi and Rutten [SR12].

> Example 1.4.6 ... Other useful methods for proving results of
> non-bisimilarity will be shown in Section 2.10.2, using the
> approximants of bisimilarity, and in Section 2.12, using games.

最简单的判定不相等的方法，来自 bisimulation 的定义本身，
其他的方案也有，上面提到的两种在下一章讨论。

> Exercise 1.4.11 ... In this exercise, when looking for the match for
> a given transition, there may be choices possible, because a state
> may have different outgoing transitions with the same label; in
> these cases, following the existential quantifier in the definition
> of bisimulation, we are asked to pick one, and we have to be careful
> to pick a good one.

对于这个练习想要说明的问题，想象点在图中的运动：
在 P 与 Q 的 bisimulation 中，
在 P 中的所有可能 path 都要能在 Q 中走通。
但是当 Q 中有 non-deterministic choice 时，
就要选择一个方向来尝试，所选择的方向可能走不通，
再尝试别的方向。

> Exercise 1.4.13
>
> - (1) Show that the union of two bisimulations on a
>   given LTS is also a bisimulation.

用点在图中的运动来考虑：
一个 bisimulation 找出了路径之间的一种对应方法，
另外一个 bisimulation 也找出了路径之间的一种对应方法，
当把二者和起来时，对于一个边，其对应的边就有了更多选择，
显然选择哪个都是可以的。

> - (2) Show that, in contrast, the intersection of two bisimulations
>   need not be a bisimulation.

对于一个边，其所对应的边的选择在取交集时可能为空，
导致没有所对应的边可选了。

> Theorem 1.4.14
>
> - (1) bisimilarity is an equivalence relation.

对称性来自二元关系的逆，
传递性来自二元关系的复合。

> - (2) bisimilarity itself is a bisimulation.

既然 bisimilarity 作为 LTS 上的关系，被定义为存在 bisimulation 关系，
那么 bisimilarity 作为关系，就是所有 bisimulation 关系的并。

这是集合论所决定的，考虑某集合 L 上的一元关系，也就是谓词 P，
假设 P 被定义为，存在满足某种性质 b 的谓词 Q。
那么 P 所定义的集合，就是所有满足性质 b 的谓词 Q 所定义的集合的并。

> Exercise 1.4.17 介绍 simulation 与 similarity，
> 即只要求 bisimulation 中的一个方向。

bisimilarity 是等价关系，
similarity 是 preorder（等价关系去掉对称性）。

两个方向的 similarity 可以定义 simulation equivalence，
bisimilarity 严格蕴含于 simulation equivalence，
是比 simulation equivalence 更细的等价关系。
证明「严格蕴含」的反例就是 Figure 1.4。

simulation 又严格蕴含于 trace equivalence。
反例是 Figure 1.7 的 P2 和 Q2。

trace equivalence 要求 P 中的一条路可以在 Q 中找到对应；
而 bisimilarity 要求 P 中的一条路可以在与 Q 中对应时，
每一步都能走到周围情况完全一样（对应）的位置；
而 similarity 要求每一步都能走到周围依然能走通的位置。

### 1.4.1 Towards coinduction

在 Theorem 1.4.15 可以看到 bisimilarity 被定义为满足某个条件 B 的最大集合 P。
证明一个元素 x 属于这个集合 P 的方式是，
证明存在一个满足这个条件 B 的集合 P1，
并且 x 属于 P1。

这种定义集合为满足某个条件最大集合的方式，就称作 coinductive definition。
这种证明某个元素属于所定义的集合的方式，就称作 coinduction。

这种看似循环的定义，与 inductive definition 类似（也是一种循环定义），
而这种模式化的证明方式，与 induction 类似（也是一种证明模式）。

一个 inductive definition 会定义一个集合 D，
induction 作为证明模式，是对某个属性（谓词）P 而言的，
所证明的是集合 D 的每个所有元素，都满足属性 P，
P 作为谓词所定义的集合是 D 的超集。
D 是满足 inductive definition 的最小集合。

相反 coinductive definition 定义的是最大集合。

# 2 Coinduction and the duality with induction

> After introducing bisimulation on processes in the previous chapter,
> we see here other examples of predicates and relations that are
> defined in a similar style, and proof techniques for them. This
> style is quite different with respect to that of ordinary inductive
> definitions and proofs. It is in fact the style of _coinduction_.
> Through the examples we will begin to build up some intuition about
> the difference between coinduction and induction. Then we will make
> these intuitions formal, using fixed-point theory.

从 luca 关于 subtype 的算法，还有 coinductive 的定义，
我更深刻地体会到了「证明」和「算法」之间的关系。
之后学习证明技巧的时候，都要考虑其算法属性。

> Intuitively, a set A is defined _coinductively_ if it is the
> _greatest_ solution of an inequation of a certain form; then the
> _coinduction proof principle_ just says that any set that is
> solution of the same inequation _is contained_ in A.

为什么说是 inequation？
是否是说 A 必须是具有序关系（preorder）性质的二元关系？
也就是说 coinductive definition 只能用来定义类似序关系的集合？

> Dually, a set A is defined _inductively_ if it is the _least_
> solution of an inequation of a certain form, and the _induction
> proof principle_ then says that any other set that is solution to
> the same inequation _contains_ A. As we will see, familiar inductive
> definitions and proofs can be formalised in this way.

看来不是，因为 inductive definition 所能定义的集合是任意结构的。

## 2.1 Examples of induction and coinduction

TODO
