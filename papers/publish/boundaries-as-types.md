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

# Cell-complex as a generalization of graph theory

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

```cell-complex
complex Circle {
  base: Circle

  edge {
    loop: endpoints (Circle) { base base }
  }
}
```

## 2-dimensional

### Four ways to glue a square

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

complex Torus {
  origin: Torus

  edge {
    toro: endpoints (Torus) { origin origin }
    polo: endpoints (Torus) { origin origin }
  }

  face {
    spoke: polygon (Torus) { toro polo = polo toro }
  }
}

check path (Torus) { toro toro toro polo }: endpoints (Torus) { origin origin }
check path (Torus) { toro }: endpoints (Torus) { origin origin }
check path (Torus) { relf(origin) }: endpoints (Torus) { origin origin }
check path (Torus) { toro relf(origin) }: endpoints (Torus) { origin origin }

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

### Torus3

```cell-complex
complex Torus3 {
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

```cell-complex
complex Torus3 {
  o: Torus3

  edge {
    a: endpoints (Torus3) { o o }
    b: endpoints (Torus3) { o o }
    c: endpoints (Torus3) { o o }
  }

  face {
    ap: polygon (Torus3) { c b = b c }
    bp: polygon (Torus3) { a c = c a }
    cp: polygon (Torus3) { b a = a b }
  }

  body {
    s: polyhedron (Torus3) {
      ap { b3 c2 a2 c0 }
      bp { b0 a2 c1 b3 }
      cp { c0 c1 c2 b0 }
    }
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

```cell-complex
complex S1 {
  base: Node

  rim: Edge(base, base)
}

complex S2 {
  base: Node

  rim: Edge(base, base)

  disk: Face(rim, -rim)
}

complex S3 {
  base: Node

  rim: Edge(base, base)

  disk: Face(rim, -rim)

  ball: Body(
    disk('x, 'y),
    disk('x, 'y),
  )
}
```

TODO How come a sphere has an 3-dimensional algebraic structure in it?

- `surface (S2) { relf(relf(rim)) }`
- `surface (S2) { relf(disk) }`

```cell-complex
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
