---
title: Boundaries as Types
author: Xie Yuheng
date: 2022-05-31
keywords: [Type theory, Algebraic topology]
---

# TODO

- example algebra of 1-dimension
- example algebra of 2-dimension

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

## Limitation of our construction

The following notions are not built-in our language:

- Subset.
- Quotient.

## Naming and syntax keywords

| dimension | generator (cell) | element (complex) | spherical element |
|----------:|------------------|-------------------|-------------------|
|         0 | vertex           |                   | `endpoints`       |
|         1 | edge             | `path`            | `polygon`         |
|         2 | face             | `surface`         | `polyhedron`      |
|         3 | body             | `complex(3)`      | `polytope(3)`     |

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
complex Circle {
  base: Circle
  loop: endpoints { base base }
}
```

## 2-dimensional

### Four ways to glue a square

```cicada
complex Sphere {
  south: Sphere
  middle: Sphere
  north: Sphere
  south_long: endpoints { south middle }
  north_long: endpoints { middle north }
  disk: polygon {
    south_long north_long = south_long north_long
  }
}
```

```cicada
complex Torus {
  origin: Torus
  toro: endpoints { origin origin }
  polo: endpoints { origin origin }
  spoke: polygon { toro polo = polo toro }
}

check path (Torus) { toro toro toro polo }: endpoints (Torus) { origin origin }
check path (Torus) { toro }: endpoints (Torus) { origin origin }
check path (Torus) { relf(origin) }: endpoints (Torus) { origin origin }
check path (Torus) { toro relf(origin) }: endpoints (Torus) { origin origin }

complex KleinBottle {
  origin: KleinBottle
  toro: endpoints { origin origin }
  cross: endpoints { origin origin }
  disk: polygon { toro cross = -cross toro }
}

complex ProjectivePlane {
  start: ProjectivePlane
  end: ProjectivePlane
  left_rim: endpoints { start end }
  right_rim: endpoints { end start }
  disk: polygon { left_rim right_rim left_rim right_rim }
}
```

## 3-dimensional

### Torus3

```cicada
complex Torus3 {
  o: Torus3
  a: endpoints { o o }
  b: endpoints { o o }
  c: endpoints { o o }
  ap: polygon { c b = b c }
  bp: polygon { a c = c a }
  cp: polygon { b a = a b }
  // The syntax use logic variable and linear unification
  // to specify how edges of polygons are glued together.
  s: polyhedron {
    ap { 'b3 'c2 = 'c0 'a2 }
    bp { 'b0 'a2 = 'b3 'c1 }
    cp { 'c0 'c1 = 'b0 'c2 }
  }
}

check path (Torus3) { a }: endpoints (Torus3) { o o }
check path (Torus3) { refl(o) }: endpoints (Torus3) { o o }

check surface (Torus3) { refl(refl(o)) }: polygon (Torus3) { refl(o) = refl(o) }

check surface (Torus3) { refl(a) }: polygon (Torus3) { a = a }

check surface (Torus3) {
  ap { b3 c2 a2 c0 }
  bp { b0 a2 c1 b3 }
  refl(a) {  ... }
}: polygon (Torus3) { ... }
```

### Hopf fibration

```cicada
complex S1 {
  base: S1
  rim: endpoints { base base }
}

complex S2 {
  base: S2
  rim: endpoints { base base }
  disk: polygon { rim = rim }
}

complex S3 {
  base: S3
  rim: endpoints { base base }
  disk: polygon { rim = rim }
  ball: polyhedron {
    disk { 'x = 'y }
    disk { 'x = 'y }
  }
}
```

TODO How come a sphere has an 3-dimensional algebraic structure in it?

- `surface (S2) { relf(relf(rim)) }`
- `surface (S2) { relf(disk) }`

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
  path (Product(Circle, Circle)) {
    cons(base, loop)
  }
)

cons(
  boundary(path (Circle) { refl(base) }),
  boundary(path (Circle) { loop })
)

cons(
  endpoints (Circle) { base base },
  endpoints (Circle) { base base },
)

endpoints (Product(Circle, Circle)) {
  cons(base, base)
  cons(base, base)
}
```

```cicada
boundary(
  surface (Product(Circle, Circle)) {
    cons(loop, loop)
  }
)

cons(
  boundary(surface (Circle) { refl(loop) }),
  boundary(surface (Circle) { refl(loop) })
)

cons(
  polygon (Circle) { loop },
  polygon (Circle) { loop },
)

polygon (Product(Circle, Circle)) {
  cons(loop, loop)
}
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
