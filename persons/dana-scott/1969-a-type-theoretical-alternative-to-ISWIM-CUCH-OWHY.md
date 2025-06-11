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

# My Summ
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

# 0 Introduction
# 1 Types
# 2 Interpretation
# 3 Axiomatization
# 4 Completeness
# 5 Conclusions
# 6 Afterthought (1993)
