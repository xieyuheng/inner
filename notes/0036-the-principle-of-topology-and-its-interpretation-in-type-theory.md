---
title: The principle of topology and its interpretation in type theory
date: 2022-05-20
---

In topology we study **adjacent relation** of any dimension,
i.e. adjacency between objects, and adjacency between adjacencies, up to any order.

- By topology, I mean algebraic topology,
  or called by its old name -- [combinatorial topology](https://en.wikipedia.org/wiki/Combinatorial_topology).

  I do not mean the axioms of [topological space](https://en.wikipedia.org/wiki/Topological_space) -- a set of axioms relating points and neighbourhoods,
  which is observed during [the arithmetization of analysis](https://en.wikipedia.org/wiki/Arithmetization_of_analysis),
  thus only useful when we give geometry back to topological objects.

The object of our study is [cell complex](https://en.wikipedia.org/wiki/CW_complex),
or say, we use cell complex to model adjacent relation.

Compare with [graph theory](https://en.wikipedia.org/wiki/Graph_theory),
in which we study [adjacent relation](https://en.wikipedia.org/wiki/Adjacency_matrix)
between 0 dimensional objects (called point, node, vertex),
and such relations as object (called edge) are 1 dimensional.

In cell complex, the adjacency can be inductively defined for any dimension.

The principle of topology is:

> To study **adjacency of adjacencies**.

# Interpretation of equivalent relation as adjacency in type theory

Equivalent relation can be interpreted as as adjacenct relation.

Elements of type `Equal(T, x, y)` can be interpreted as path between `x` and `y`,
and type of a object and be interpreted as its boundary.

But we should note that, this is only one way of study adjacenct relation,
adjacenct relation can be studied without the concept of equivalence.

It is exciting to know we can study two subjects together,
but it is also important to know it is only one way of interpretation.

Examples in which we almost forget "it is only one way of interpretation":

- [Homotopy type theory](https://en.wikipedia.org/wiki/Homotopy_type_theory)
  always wants to interpret higher order topological objects
  as higher order equivalenct relations.

- Forget that, concurrency is only one way of interpreting
  [linear logic](https://en.wikipedia.org/wiki/Linear_logic)'s additive connectives,
  it is enough to use the duality between input and output
  to interpret the duality between connectives.
