---
title: general lattice theory
author: george grätzer
year: 1978
---

# My Motive

[2025-07-11] 在实现带有 structural type system 的 lisp -- x-lisp 时，
我想用 lattice theory 来理解 type system，
把整个 type system 当成是一个 lattice，
把每个 type 当成是其中的一个元素。

类型所构成的 lattice 应该用展示论（presentation theory）来研究：

- lattice 运算 join 和 meet 对应于类型（集合）的 union 与 join。

- 属于某个类型的元素的集合，
  就构成了 lattice 的表示论（representation theory），
  即用集合的 lattice 表示一般的 lattice。

- 类型构造子对应于 presentation 的 generators，
  或者应该称为 generator constructors，
  因为一个类型构造子可以构造任意多的 generators。
  就像把 combinatory logic 视为为 magma 代数时的 SKI。

- 类型构造子之间的关系对应于 presentation 的 relations 方程。
  比如 record type，tuple type 和 function type，
  在 union 和 inter 下所形成的关系。

因此想通过这本书对 lattice theory 有一个更一般的了解。

# Introduction

TODO
