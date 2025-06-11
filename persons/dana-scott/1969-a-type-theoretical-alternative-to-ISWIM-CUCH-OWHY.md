---
title: a type theoretical alternative to ISWIM CUCH OWHY
author: dana scott
year: 1969
---

# My Motive

[2025-06-11] 在看 Milner 1978 年的论文
"a theory of type polymorphism in programming"，
想要学习 Hindley-Milner 类型系统。
但是第三章开始有 denotational semantics 了，所以需要补习一下。

说 denotational semantics 是用复杂的东西解释简单的东西一点没错，
因为 Hindley-Milner 类型系统本身是算法与具体实现的结合，
根本不必引入 lattices 之类的东西就能解释清楚。

这篇论文好像才是 lambda calculus 的
denotational semantics 最早的论文。

Milner 在 1973 年的论文 "models of LCF" 中说：

> The logic of computable functions proposed by Dana Scott in 1969, in
> an unpublished note, has since been the subject of an interactive
> proof-checking program designed as a first step in formally based
> machine-assisted reasoning about computer programs.

Wikipedia Lambda calculus 页面中说：

> The fact that lambda calculus terms act as functions on other lambda
> calculus terms, and even on themselves, led to questions about the
> semantics of the lambda calculus. Could a sensible meaning be
> assigned to lambda calculus terms? The natural semantics was to find
> a set D isomorphic to the function space D → D, of functions on
> itself. However, no nontrivial such D can exist, by cardinality
> constraints because the set of all functions from D to D has greater
> cardinality than D, unless D is a singleton set.

其实 lambda expression 的集合显然是可数的。

> In the 1970s, Dana Scott showed that if only continuous functions
> were considered, a set or domain D with the required property could
> be found, thus providing a model for the lambda calculus. [42]

其中 [42] 所引用的就是这篇论文。

既然是要解决悖论，
那么 Scott 引入过多数学概念就情有可原了。
毕竟悖论会令人疯狂。

另外，这篇论文引入 lattices，
可能是为了把离散的函数处理成连续函数。
这种意义上说，这篇论文也是 HoTT 的先驱。
在 HoTT 中，所处理的连续函数不是 lattice 意义上的连续，
这种连续对代数拓扑而言太间接了；
HoTT 所处理的连续函数是 cell complex
这种代数拓扑的直接对象之间连续函数。
连续在于保持边界，而不是在于（经典拓扑空间的公理下）保持开集。

另外注意，这篇论文比 Martin Löf 系列论文还早。

# My Notes

[2025-06-11] 这种 D 到 D -> D 之间的对应，
我感到很熟悉，因为在讨论「算术的等级」时有过类似的构造，
比如先通过皮亚诺公理定义 Nat，
加法这个二元运算 add: Nat -> Nat -> Nat,
其实是把 Nat 中的元素映射到一个类型为 Nat -> Nat 的函数的集合。

不同的是，为了构造加法，
add(Nat) 只是 Nat -> Nat 的子集，
而为了寻求 lambda 演算的指称语义，
需要找到 D 与 D -> D 之间的同构。

可以说函数作用是最一般的，
不满足任何运算律的二元运算。

# Abstract

> The paper (first written in 1969 and circulated privately) concerns
> the definition, axiomatization, and applications of the hereditarily
> monotone and continuous functionals generated from the integers and
> the Booleans (plus “undefined” elements). The system is formulated
> as a typed system of combinators (or as a typed lambda-calculus)
> with a recursion operator (the least fixed-point operator), and its
> proof rules are contrasted to a certain extent with those of the
> untyped lambda-calculus.

> For publication (1993), a new preface has been added, and many
> bibliographical references and comments in footnotes have been
> appended.

# Preface (1993)

> This particular paper has, of course, an odd historical role: in it
> the author argues against the type-free calculi of Church and Curry,
> Kleene and Rosser, and their later uses by Bohm and Strachey. But
> then in November of 1969, after writing this report, the author
> himself saw that the method of monotone continuous functions (which
> grew out of traditional recursive function theory in discussing
> certain kinds of functionals) could be applied to posets other than
> just those generated from the integers (with bottom) by the very
> simple type constructors. In particular, it was recognized that
> there were posets whose continuous function spaces of self-maps
> could be identified with the given posets themselves. And so there
> actually were “traditional” models of lambda-calculus that could be
> defined without first going through the proof theory of the formal
> system itself (and which could be related to many other
> mathematically meaningful structures and theories as well).

> After writing with tiresome sarcasm about the lack of meaning in the
> type-free lambda-calculus introduced only formally, the author
> himself found an interesting kind of semantical interpretation for
> the “type-free” language. This total shift of gears is the reason
> the present paper was not published: the foundational program being
> advocated had apparently been completely outmoded by the discovery
> of the more general lattice-theoretic models.  However, the
> _axiomatic_ program laid out here had much to recommend it, and it
> was continued and extended in many directions by Milner, Plotkin,
> and many others, to whom the paper had been circulated privately -
> often at nth hand.

> On the other hand, the type-theoretical approach has not died out at
> all, because it has been taken over and absorbed into the
> applications of category theory to semantics and computation
> theory. The author is fond of saying that a category represents the
> “algebra of types”, just as abstract rings give us the algebra of
> polynomials, originally understood to concern only integers or
> rationals. One can of course think only of particular type systems,
> but, for a full understanding, one really needs also to take into
> account the general theory of types, and especially translations or
> interpretations of one system in another. Category theory together
> with the notion of functor and natural transformation between
> functors has been proved over and over again in the last
> half-century to be the appropriate way to go about these studies.
> The author himself does not always like or enjoy the discipline of
> category theory, which seems ofttimes to carry along very, very
> heavy machinery and odd terminology, but he long ago came to the
> conclusion that it is quite _unavoidable_. The extremely active
> current research in semantics also shows that it is an especially
> fruitful way to think. The book of Gunter [9] with its wide-ranging
> historical comments and references is adequate proof of this
> assertion.

值得一读：

- [9] Semantics of Programming Languages - Structures and Techniques,
  C.A. Gunter, 1992.

> The strange title of this paper ought perhaps to be explained. In
> 1966, Landin published an influential paper [14] which introduced a
> syntactical design style for programming languages, one of which he
> called ISWIM, standing for “If you See What I Mean”.

也就是说 Landin 的这篇论文是关于语法设计风格的？

> Also Böhm in 1966 published the paper [3] which named a language of
> combinators called CUCH, standing for “Curry-Church”.

> The author hoped to stop some proliferation by suggesting a return
> to the logically standard type-theoret- ical framework and thereby
> deter the creation of programming languages of doubtful foundation
> called (as a group) OWHY, standing for “Or What Have You.”

> No one really understood the joke, and the effort was doomed to be
> of no avail. And history proved the author to be too conservative in
> any case.

# 0 Introduction
# 1 Types
# 2 Interpretation
# 3 Axiomatization
# 4 Completeness
# 5 Conclusions
# 6 Afterthought (1993)
