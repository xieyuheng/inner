---
title: tree automata techniques and applications
authors: [hubert comon, max dauchet, rémi gilleron, florent jacquemard, denis lugiez, christof löding, sophie tison, marc tommasi]
year: 2008
book-homepage: "http://tata.gforge.inria.fr"
book-homepage-redirect: "https://inria.hal.science/hal-03367725"
---

# My Motive

[2025-07-13] 在读 roberto amadio 和 luca cardelli
的 "subtyping recursive types" 时，
看到 "1.4 Subtyping of Recursive Types" 说，
递归类型的等价判断问题是人们熟知的问题：

> The problem of equating recursive types such as `α` and `β` above
> can be related to well-known solvable problems, such as the
> equivalence of finite-state automata. However, the similar problem
> for subtyping has no well-known parallel.

但是我不熟知，并且这里没有给出引用。
也许这本书可以让我补充这方面的知识。

另外 automata theory 似乎是很多理论的发源地，
比如：

- Robin Milner 的 process calculus
- Petri nets
- Regular language
- Chomsky hierarchy
- 等等

# Introduction

> Actually using tree automata has proved to be a powerful approach to
> simplify and extend previously known results, and also to find new
> results. For instance recent works use tree automata for application
> in abstract interpretation using set constraints, rewriting,
> automated theorem proving and program verification, databases and
> XML schema languages.

介绍 tree automata 的历史与应用，提到了 type system。

> Our goal is to fill in the existing gap and to provide a textbook
> which presents the basics of tree automata and several variants of
> tree automata which have been devised for applications in the
> aforementioned domains. We shall discuss only _finite tree_
> automata, and the reader interested in infinite trees should consult
> any recent survey on automata on infinite objects and their
> applications (See the bibliography).

> The second main restriction that we have is to focus on the
> operational aspects of tree automata. This book should appeal the
> reader who wants to have a simple presentation of the basics of tree
> automata, and to see how some variations on the idea of tree
> automata have provided a nice tool for solving difficult problems.

"operational aspects" 指什么？
类似 operational semantics vs. denotational semantics 吗？

> Again, we recall that there is no presentation nor discussion of
> tree automata for infinite trees. This domain is also in full
> development mainly due to applications in program verification and
> several surveys on this topic do exist.

"tree automata for infinite trees" 指什么？
我们知道 finite automata 所能识别或者生成的 string 是无限的，
这里说的 finite tree automata 应该是指 automata state 的个数，
而不是所能生成的 tree 的大小吧？

> Tree Automata Techniques and Applications is composed of eight main
> chapters (numbered 1–8).
>
> - The first one presents tree automata and defines recognizable tree
>   languages. The reader will find the classical algorithms and the
>   classical closure properties of the class of recognizable tree
>   languages. Complexity results are given when they are available.
>
> - The second chapter gives an alternative presentation of
>   recognizable tree languages which may be more relevant in some
>   situations. This includes regular tree grammars, regular tree
>   expressions and regular equations. The description of properties
>   relating regular tree languages and context-free word languages
>   form the last part of this chapter.
>
> - In Chapter 3, we show the deep connections between logic and
>   automata. In particular, we prove in full details the
>   correspondence between finite tree automata and the weak monadic
>   second order logic with k successors. We also sketch several
>   applications in various domains.
>
> - Chapter 4 presents a basic variation of automata, more precisely
>   automata with equality constraints. An equality constraint
>   restricts the application of rules to trees where some subtrees
>   are equal (with respect to some equality relation). Therefore we
>   can discriminate more easily between trees that we want to accept
>   and trees that we must reject. Several kinds of constraints are
>   described, both originating from the problem of non-linearity in
>   trees (the same variable may occur at different positions).
>
> - In Chapter 5 we consider automata which recognize sets of sets of
>   terms. Such automata appeared in the context of set constraints
>   which themselves are used in program analysis. The idea is to
>   consider, for each variable or each predicate symbol occurring in
>   a program, the set of its possible values. The program gives
>   constraints that these sets must satisfy. Solving the constraints
>   gives an upper approximation of the values that a given variable
>   can take. Such an approximation can be used to detect errors at
>   compile time: it acts exactly as a typing system which would be
>   inferred from the program. Tree set automata (as we call them)
>   recognize the sets of solutions of such constraints (hence sets of
>   sets of trees). In this chapter we study the properties of tree
>   set automata and their relationship with program analysis.

这一章，感觉能同时和 type system 和 propagator model 联系起来。

> - Originally, automata were invented as an intermediate between
>   function description and their implementation by a circuit. The
>   main related problem in the sixties was the _synthesis problem_:
>   which arithmetic recursive functions can be achieved by a circuit?
>   So far, we only considered tree automata which accepts sets of
>   trees or sets of tuples of trees (Chapter 3) or sets of sets of
>   trees (Chapter 5). However, tree automata can also be used as a
>   computational device.  This is the subject of Chapter 6 where we
>   study _tree transducers_.

这一章设计新的计算模型，应该也很有意思。

> - In Chapter 7 we present basic properties of alternating
>   automata. As an application we consider set constraints that have
>   already been introduced in Chapter 5.

> - In Chapter 8 we drop the restriction for ranked trees that the
>   label of a node determines the number of successors of this node,
>   so we consider the model of unranked ordered trees. This model is
>   used for representing the structure of XML documents. We discuss
>   the basic model of hedge automaton, study its algorithmic and
>   closure properties, and then present some formalisms used in
>   practice to define classes of XML documents.

能用到 XML 上的工具，应该也能用到 sexp（或者说我的 x-data）上。

# Preliminaries

TODO

# 1 Recognizable Tree Languages and Finite Tree Automata

## 1.1 Finite Tree Automata
## 1.2 The Pumping Lemma for Recognizable Tree Languages
## 1.3 Closure Properties of Recognizable Tree Languages
## 1.4 Tree Homomorphisms
## 1.5 Minimizing Tree Automata
## 1.6 Top Down Tree Automata
## 1.7 Decision Problems and their Complexity
## 1.8 Exercises
## 1.9 Bibliographic Notes

# 2 Regular Grammars and Regular Expressions

## 2.1 Tree Grammar
### 2.1.1 Definitions
### 2.1.2 Regularity and Recognizabilty
## 2.2 Regular Expressions. Kleene’s Theorem for Tree Languages
### 2.2.1 Substitution and Iteration
### 2.2.2 Regular Expressions and Regular Tree Languages
## 2.3 Regular Equations
## 2.4 Context-free Word Languages and Regular Tree Languages
## 2.5 Beyond Regular Tree Languages: Context-free Tree Languages
### 2.5.1 Context-free Tree Languages
### 2.5.2 IO and OI Tree Grammars
## 2.6 Exercises
## 2.7 Bibliographic notes

# 3 Logic, Automata and Relations
# 4 Automata with Constraints
# 5 Tree Set Automata
# 6 Tree Transducers
# 7 Alternating Tree Automata
# 8 Automata for Unranked Trees
