---
title: Boundaries as Types
author: Xie Yuheng
date: 2022-05-31
keywords: [Type theory, Algebraic topology]
---

# TODO Review notes

- topics/mathematics/algebraic-topology
- topics/mathematics/combinatorial-group-theory
- langs/cell-complex
  - a-language-for-equivalence
  - fibration
  - holding
  - square
  - at1-note
  - aeqea
  - main
  - note

# Abstract

A language to capture the concept of continuum,

- by viewing boundary as type,
- to formalize constructions in algebraic topology.

It can be viewed as higher dimensional algebraic structure in which,

- homotopy groups are its sub-algebras,
- and its abelianization are homology groups.

# Higher dimensional inductive datatypes

## TODO

- In inductive datatypes, a type constructor can take arguments
  and the arguments are to separated into two groups:
  - **fixed (parameters)** which do **NOT** vary between the data constructors.
  - **varied (indexes)** which can vary between the data constructors.

  When we introduce arguments to type constructor for complex,
  what is the meaning of the above?

  What is the topological interpretion
  of "fixed v.s. varied arguments"
  for inductive datatype?

## 1-dimensional

```cell-complex
complex Circle {
  base: Node
  loop: Edge(base, base)
}

complex Circle2 {
  a: Node
  b: Node
  f: Edge(a, b)
  g: Edge(b, a)
}
```

## 2-dimensional

Four ways to glue a square.

```cell-complex
complex Sphere {
  south: Node
  middle: Node
  north: Node

  south_long: Edge(south, middle)
  north_long: Edge(middle, north)

  surface: Face(south_long, north_long, -north_long, -south_long)
}

complex Torus {
  origin: Node

  toro: Edge(origin, origin)
  polo: Edge(origin, origin)

  spoke: Face(toro, polo, -toro, -polo)
}

complex KleinBottle {
  origin: Node

  toro: Edge(origin, origin)
  cross: Edge(origin, origin)

  surface: Face(toro, cross, -toro, cross)
}

complex ProjectivePlane {
  start: Node
  end: Node

  left_rim: Edge(start, end)
  right_rim: Edge(end, start)

  surface: Face(left_rim, right_rim, left_rim, right_rim)
}
```

## 3-dimensional

```cell-complex
complex D3Torus {
  o: Node

  a: Edge(o, o)
  b: Edge(o, o)
  c: Edge(o, o)

  // NOTE If we use `Equal` type (identity type),
  //   the above constructors will be:
  // a: Equal(D3Torus, o, o)
  // b: Equal(D3Torus, o, o)
  // c: Equal(D3Torus, o, o)

  ap: Face(c, b, -c, -b)
  bp: Face(a, c, -a, -c)
  cp: Face(b, a, -b, -a)

  // NOTE Syntax inspired by logic programming and relational algebra,
  //   suppose we use `'symbol` to denotes logic variables.
  s: Body(
    ap('b3, 'c2, 'a2, 'c0),
    bp('b0, 'a2, 'c1, 'b3),
    cp('c0, 'c1, 'c2, 'b0),
  )
}
```
