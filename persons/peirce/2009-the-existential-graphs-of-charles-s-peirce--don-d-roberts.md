---
title: the existential graphs of charles s peirce
author: don d roberts
year: 2009
---

# 学习动机

在想要给 inet 加依赖类型系统的时候，
发现了 proof net，也就是说现代的发展已经又开始重视 graph 了。

- 在 proof net 中，inet 的 node 应该被理解为 application of inference rule。
  但是在 existential graph 中 node 被理解为 proposition，
  可能是某个 relation 作用于变量而形成的 proposition。

实现 inet 的技巧（meta language + graph language）
让我并不害怕处理 graph。

另外，和 Peirce 一样，
我也不满意 predicate logic 中的量词，
向看看如何用新的 graph 视角去理解它们。

# 1 Introduction

主要介绍人们对 existential graphs 的误解。

# 2 The development of existential graphs

## 2.1 Peirce's early emphasis on diagrammatic thinking and analysis

> In truth, no two things could be more directly opposite than the
> cast of mind of the logician and that of the mathematician .... The
> mathematician's interest in a reasoning is as a means of solving
> problems-both a problem that has come up and possible problems like
> it that have not yet come up. His whole endeavor is to find short
> cuts to the solution. The logician, on the other hand, is interested
> in picking a method to pieces and in finding what its essential
> ingredients are. He cares little how these may be put together to
> form an effective method, and still less for the solution of any
> particular problem. In short, logic is the theory of all reasoning,
> while mathematics is the practice of a particular kind of reasoning.

正如设计类型系统的人，和使用某个类型系统来写程序的人的区别。

## 2.2 The earliest applications of the diagrammatic method to the logic of relatives

这里介绍了 Peirce 用算术来理解 predicate logic 的方式。
- 这种理解方式有很大的扩展空间。

Sigma 和 Pi 就是 Peirce 最开始使用的。
Sigma 真的被理解为项的 sum，类似地 Pi 被理解为 product。

Peirce 给出的用图表示的命题，
像是在 inet 中构造 node。
但是一个问题是逻辑量词和顺序有关，
这个顺序在图上表示不出来。

> Two features of this notation -- the use of the line to represent
> individuals and the use of the line, simply drawn, as a sign of
> 'something' -- are basic in Peirce's final system of logic diagrams,
> the existential graphs.

这用 inet 中变量绑定到 HalfEdge 上，是一样的！

## 2.3 The influence of Kempe

## 2.4 The first system: the entitative graphs

## 2.5 The second system: the existential graphs

注意，这里的圈不能理解为集合意义上的包含，
反而是大圈 implies 小圈。

需要区分奇偶性，而且 node 的边界不能相交，
所以可以用内外边界的区别来代替奇偶性。

> The most unnatural feature of the system is its interpretation of
> the juxtaposition of two propositions as alternation.  For since the
> writing down of a proposition asserts it, the writing down of two
> propositions naturally suggests asserting them both (as in EG).

sequence calculus 中的 antecedent 被定义为 disjunction 也是不自然的。

# 3 ALPHA

ALPHA 只能处理命题演算。

按照这里画圈的限制，
也可以自然地用 sexp 来表示 ALPHA，
但是到 BETA 就需要一些语法设计才能用 sexp 了。

| connective | graph     | literal expression       |
|------------|-----------|--------------------------|
| P          | P         |                          |
| not(P)     | (P)       |                          |
| and(P, Q)  | P Q       |                          |
| or(P, Q)   | ((P) (Q)) | not(and(not(P), not(Q))) |
| if(P, Q)   | (P (Q))   | or(not(P), not(not(Q)))  |

可以用两个名字相同的 label 来表达 edge，
用 :keyword 的语法来表示 label，
label 以后缀表达式的形式作用于前面的集合。

(P :x (Q) :x)

注意，这里 P 和 Q 都是集合，即单参数的 predicate。

TODO

# 4 BETA
# 5 GAMMA
# 6 TINCTURED EXISTENTIAL GRAPHS
# 7 GRAPHICAL ANALYSIS AND OUTCROPPINGS
# APPENDIX 1. A Selective Chronology of Peirce's Work on Logic
# APPENDIX 2. Table of Logic Notations
# APPENDIX 3. EG Conventions and Rules
# APPENDIX 4. Completeness and Consistency
