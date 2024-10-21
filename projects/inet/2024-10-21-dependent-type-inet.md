---
title: dependent type inet
date: 2024-10-21
---

我所研究的很多问题其实都可以归结为，
如何把依赖类型加入到各种不同的计算模型中。

# lambda calculus

首先总结一下为 lambda calculus 实现依赖类型系统的问题。

类型系统在于检查类型是否匹配，
匹配的定义方式有很多，最基础的就是类型是否等价。

当类型中可以有任意计算时，检查类型是否等价，
就需要判断 closure 在 partial evaluation 下是否等价。

之所以需要 partial evaluation，
是因为 type 可以依赖前面出现过的变量，
在类型检查阶段，只知道这些变量的类型，
而不知道这些变量的值（not yet a value）。

所以只要实现了 partial evaluation，
和对所得的 normal form 的等价判断，
就可以实现 dependent type 了。

# stack-based concatenative

stack-based concatenative 语言的类型系统对应于 sequent calculus。

直接将 sequent calculus 的 judgment
的 sequent 结构，以 stack 的形式囊括到了语言中。

适合用 two-level computation 来理解。

但是可以发现这两层计算是差异很大的，
类型这一层计算，要么判断「是否匹配」一类的计算，
要么是 eliminator 作用于 not-yet-a-value 这种平凡的计算。

可以说，真正在类型这一层实行任意计算，
还是一个没有被探索过的方向。

- 可以专门为了探索这个 two-level computation，
  而实现一个 prototype 语言试试。

# propagator

TODO

# inet

TODO
