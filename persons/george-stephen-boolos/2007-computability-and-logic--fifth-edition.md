---
title: computability and logic (fifth edition)
author: [george s. boolos, john p. burgess, richard c. jeffrey]
year: 2007
---

# My Motive

[2026-03-19] 在实现 lambda-lisp 时，
想要判断递归定义的函数之间的等价。

但是一般情况下，函数之间的等价是不可判定的，
如何找到一个可判定的子集？

如果找到了，就可以作为 dependent type system 的基础的等价关系。
因此要学习可判定性的基础知识。

# 1 Enumerability

> Our ultimate goal will be to present some celebrated theorems about
> inherent limits on what can be computed and on what can be proved.

> Before such results can be established, we need to undertake an
> analysis of computability and an analysis of provability.

> The main topic is a distinction between two different kinds of
> infinite sets, the enumerable and the nonenumerable.

## 1.1 Enumerability

> An _enumerable_, or _countable_, set is one whose members can be
> enumerated: arranged in a single list with a first entry, a second
> entry, and so on, so that every member of the set appears sooner or
> later on the list.

强调了 list 中所列的是元素的名字，而不是元素本身。

> A list that enumerates a set may be finite or unending. An infinite
> set that is enumerable is said to be _enumerably infinite_ or
> _denumerable_.

用直觉的方式定义可数集，
然后解释如何用以自然数为定义域的映射来捕捉这个直觉概念。

有限集当然是可数的。
与自然数集之间的双射，就展示了无穷集的可数。
如果考虑自然数集到某个集合的满射，
那么有限集的可数也可以用函数展示。

甚至偏函数（partial function）也可以，并且使用起来非常方便。
因为重要的是要求每个元素都有自然数与其对应，
而不是要求每个自然数都对应于一个元素。

## 1.2 Enumerable Sets

给出了很多具体的 enumerable sets 的例子。

# 2 Diagonalization

给出 non-enumerable set 的构造方式。

# 3 Turing Computability

> A function is _effectively computable_ if there are definite,
> explicit rules by following which one could in principle compute its
> value for any given arguments.

这章和后面几章，都是要尝试捕捉上面这个直觉定义。
通过证明很多尝试捕捉 effectively computable 的定义等价，
我们展示了我们成功捕捉到了这个直觉定义。

就像第一章，用 listing 给出 enumerability 的直觉定义，
然后用函数捕捉这个定义一样。

用形式定义捕捉直觉定义的过程，
其成功与否的判断，在于一致性与实用性。

> The instructions must be completely definite and explicit. They
> should tell you at each step what to do, not tell you to go ask
> someone else what to do, or to figure out for yourself what to do:
> the instructions should require no external sources of information,
> and should require no ingenuity to execute, so that one might hope
> to automate the process of applying the rules, and have it performed
> by some mechanical device.

尝试用来捕捉 effectively computable 这个直觉概念的定义，
之所以有用，就在于人们可以设计机器来帮助人类快速完成这类计算。
这就要求描述计算方式（算法）步骤明确且没有歧义等等。
但是现在 AI 的发展大大拓宽了机器可以帮助人类做的脑力劳动。

TODO

# 4 Uncomputability

## 4.1 The Halting Problem
## 4.2 The Productivity Function

# 5 Abacus Computability

## 5.1 Abacus Machines
## 5.2 Simulating Abacus Machines by Turing Machines
## 5.3 The Scope of Abacus Computability

# 6 Recursive Functions

## 6.1 Primitive Recursive Functions
## 6.2 Minimization

# 7 Recursive Sets and Relations

## 7.1 Recursive Relations
## 7.2 Semirecursive Relations
## 7.3 Further Examples

# 8 Equivalent Definitions of Computability

## 8.1 Coding Turing Computations
## 8.2 Universal Turing Machines
## 8.3 Recursively Enumerable Sets
