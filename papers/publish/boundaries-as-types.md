---
title: Boundaries as Types
author: Xie Yuheng
date: 2022-05-31
keywords: [Type theory, Algebraic topology]
---

# TODO

- example algebra of 1-dimension
  - take presentation of group as example
  - take interesting groups as example (fundamental polygon)

- example algebra of 2-dimension
- example algebra of 3-dimension -- for `Pi(3)(S(2))`

- about implementation:

  if we think about how to implement higher order incidence relation
  by objects and pointers,
  and we store pointers at both direction at every order,
  the implementation will be a graph model of cell-complex.

- product space -- and boundary operator over it

- review old notes

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

# Problems

- We are trying to find a way to general algebraic structure to higher dimension,
  how can 2-dimensional sphere has non trivial 3-dimensional algebraic structure?

  - Hopf fibration describe partition of the ball of `S3`,
    but in our language, we can not describe this detailed partition.

---

- We can define partition on cell-complex,

- When defining a map between two spaces,
  we must also define the map for all possible partitions
  (like [surreal number](https://en.wikipedia.org/wiki/Surreal_number)?).

- When we define map between the same dimension,
  our way of definition already give a definition
  for all possible partitions.

- We can map element cross dimension,
  but must also define the map for all possible partitions.

- Then we can define Hopf fibration in a
  intuitive and pure topological way.

# Abstract

A language to capture the concept of continuum,

- by viewing boundary as type,
- to formalize constructions in algebraic topology.

It can be viewed as higher dimensional algebraic structure in which,

- homotopy groups are its sub-algebras,
- and its abelianization are homology groups.

# Introduction

## Cell-complex as a generalization of graph theory

Note that graph (with nodes and edges),
and cell-complex (with faces and bodies and higher),
are all only syntax, i.e. without interpretion.

They are not model.

For example, we can interpret a directed graph as a causal model.

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

## Different meanings of higher-order-ness

Higher order inductive type is one way of
introducing higher-order-ness into our type system,
another way is higher-order functions.

Is univalent axiom a relation between this two orders?

Is it true that,
if we can view other higher-order-ness
as models of higher order inductive type,
we can use the algebra of the later to simplify our reasoning?

- Higher category theory already provides an example.

- One problem is that a function type is directed
  (we do not have general method to reverse any function),
  but in a cell-complex, the a cell has no direction

  - But we used minus sign when composing edges in `path`,
    is it not about direction?

    It is not.
    We can only say a cell has "orientation",
    in the sense that, for example, when composing an edge,
    we can use one of its endpoints,
    the minus sign in `path` only means
    to change the layout of an edge
    to compose it by another endpoint.

## Limitation of our construction

The following notions are not built-in our language:

- Subset.
- Quotient.

## Naming and syntax keywords

| dimension | generator (cell) | element (complex) | spherical element |
| --------: | ---------------- | ----------------- | ----------------- |
|         0 | vertex           |                   | `endpoints`       |
|         1 | edge             | `path`            | `polygon`         |
|         2 | face             | `surface`         | `polyhedron`      |
|         3 | block            | `building`        | `polychoron`      |

Notes:

- By "element" we mean elements in an algebraic structure.

- A spherical element can be used to specify
  the attaching map of a higher dimensional generator,
  i.e. specifying how the boundary of the generator
  is glued to the spherical element.

  After the gluing, the generator becomes part of the space,
  and the spherical element becomes the generator's boundary
  (boundaries as types).

## 0-dimensional

When we define inductive datatypes
without any higher order data constructors,
although the defined datatypes already have interesting structures,
we are only defining generators of 0-dimensional nodes.
The nodes can be recursively parameterized, thus infinity many,
and they can be parameterized in interesting ways.

When defining a complex, we can specify higher order generators,
the generators can be composed to get elements of the algebra.
The generators can also be parameterized, thus infinity many,

## 1-dimensional

### Circle

```cicada
datatype Circle {
  base: Circle
  loop: endpoints { base -base }
}
```

## 2-dimensional

### Four ways to glue a square

```cicada
datatype Sphere {
  south: Sphere
  middle: Sphere
  north: Sphere
  south_long: endpoints { south -middle }
  north_long: endpoints { middle -north }
  disk: polygon {
    south_long north_long -north_long -south_long
  }
}
```

```cicada
datatype Torus {
  origin: Torus
  toro: endpoints { origin -origin }
  polo: endpoints { origin -origin }
  spoke: polygon { toro polo -toro -polo }
}
```

```cicada
datatype KleinBottle {
  origin: KleinBottle
  toro: endpoints { origin -origin }
  cross: endpoints { origin -origin }
  disk: polygon { toro cross -toro cross  }
}
```

```cicada
datatype ProjectivePlane {
  start: ProjectivePlane
  end: ProjectivePlane
  left_rim: endpoints { start -end }
  right_rim: endpoints { end -start }
  disk: polygon { left_rim right_rim left_rim right_rim }
}
```

## 1-dimensional algebra

```cicada
check path (Torus) {
  toro toro toro polo
}: endpoints (Torus) {
  origin -origin
}

check path (Torus) {
  toro
}: endpoints (Torus) {
  origin -origin
}

check path (Torus) {
  relf(origin)
}: endpoints (Torus) {
  origin -origin
}

check path (Torus) {
  toro relf(origin)
}: endpoints (Torus) {
  origin -origin
}
```

## 3-dimensional

### Torus3

```cicada
datatype Torus3 {
  o: Torus3
  a: endpoints { o -o }
  b: endpoints { o -o }
  c: endpoints { o -o }
  ap: polygon { c b -c -b }
  bp: polygon { a c -a -c }
  cp: polygon { b a -b -a }
  // The syntax use logic variable and linear unification
  // to specify how edges of polygons are glued together.
  s: polyhedron {
    ap { 'b3 'c2 -'a2 -'c0 }
    bp { 'b0 'a2 -'c1 -'b3 }
    cp { 'c0 'c1 -'c2 -'b0 }
  }
}
```

## 2-dimensional algebra

```cicada
check surface (Torus3) {
  refl(refl(o)) { ... }
}: polygon (Torus3) {
  refl(o) -refl(o)
}

check surface (Torus3) {
  refl(a) { ... }
}: polygon (Torus3) {
  a -a
}

check surface (Torus3) {
  refl(a) { ... }
  refl(a) { ... }
}: polygon (Torus3) {
  a a -a -a
}

check surface (Torus3) {
  ap { 'b3 'c2 = 'c0 'a2 }
  bp { 'b0 'a2 = 'b3 'c1 }
  refl(a) {  ... }
}: polygon (Torus3) {
  ...
}
```

## Homotopy group `S3`

<https://en.wikipedia.org/wiki/Homotopy_groups_of_spheres>

For `n >= 2`, `Pi(n)` is abelian.

- [Eckmann–Hilton argument](https://en.wikipedia.org/wiki/Eckmann%E2%80%93Hilton_argument).

```cicada
datatype S1 {
  base: S1
  rim: endpoints { base -base }
}

datatype S2 {
  south: S2
  north: S2
  meridian: endpoints { south -north }
  disk: polygon { meridian -meridian }
}

datatype S3 {
  // TODO
}
```

TODO How to do 3-dimensional algebra?

```cicada
check building (S3) {
  ball
}: polyhedron (S3) {
  disk { 'x = 'y }
  disk { 'x = 'y }
}
```

TODO How come a sphere `S2` has an 3-dimensional algebraic structure in it?

- Degenerated 3-dimensional elements in `S2` has non-trivial structure?

```cicada
check building (S2) {
  relf(disk)
}: polyhedron (S2) {
  TODO
}

check building (S2) {
  relf(relf(rim))
}: polyhedron (S2) {
  TODO
}
```

TODO How to define `Pi(3)(S(2))`?

```cicada
function Pi3S2 (s3: S3): S2 {
  return match (s3) {
    case base => S2.base
    case rim => S2.rim
    case disk => S2.disk
    case ball => TODO
    // It seems there is almost not information in definition of `S3`,
    //   thus `Pi3S2` must be about the structure of
    //   the higher dimensional algebra itself.
  }
}
```

## Hopf fibration

<https://en.wikipedia.org/wiki/Hopf_fibration>

# Essential concepts

## Continuous function

All function can be defined in our language is continuous.

If we view spaces as algebras,
a function between two spaces `A` and `B`
is a homomorphism between the two algebras.

To define a function from `A` to `B`,
we must specify how to map each generators of `A` to an element of `B`,
we can check a function is well defined by viewing boundaries as types.

Take _subdivision_ as an example,
if `B` is a subdivision of `A`,
we know `A` and `B` are equivalent,
if we view the equivalence as two functions
-- `f: A -> B` and `g: B -> A`
-- it is easy to define `f`,
while to define `g` we must map some generators
to trivial elements (`refl`) of `A`.

## Product space

TODO How to understand `Torus` as `Circle * Circle`?

```cicada
boundary(
  path (Pair(Circle, Circle)) {
    cons(base, loop)
  }
)

=> TODO

endpoints (Pair(Circle, Circle)) {
  cons(base, base) -cons(base, base)
}
```

```cicada
boundary(
  surface (Pair(Circle, Circle)) {
    cons(loop, loop)
  }
)

=> TODO
```

## Fibration

Every map can be viewed as a fibration,
the homotopy lifting property also hold.

```cicada
function Fiber(
  implicit E: Type,
  implicit B: Type,
  f: (E) -> B,
  y: B,
): Type {
  return exists (x: E) Id(B, f(x), y)
}
```

## Manifold

A closed manifold is defined as a connected finite homogeneous complex.

TODO
