---
title: Boundaries as Types
author: Xie Yuheng
date: 2022-05-31
keywords: [Type theory, Algebraic topology]
---

# Introduction

## The Plan

A group or a groupoid when defined by a presentation,
can be viewed topologically as vertexes, edges and faces,
where edges are the generators,
faces are the relations between generators,
i.e. a 2-dimensional cell-complex.

And the reverse is also true,
given a 2-dimensional cell-complex,
one can view it as a groupoid.

If a 2-dimensional cell-complex is a manifold,
we can transform it into an equivalent cell-complex
which is built from a fundamental polygon,
and the study of such groupoid can be reduce to
the study of fundamental group of fundamental polygon.

The presentations of 2-dimensional manifold
as fundamental group can be normalized,
to give a complete classification
of 2-dimension manifolds.

Can we generalize the above to 3-dimension?

Given a 3-dimensional cell-complex,
we can view it as a presentation
of a 2-dimensional algebraic structure,
where elements are faces and relations are bodies.

**Conjecture:**
If a 3-dimensional cell-complex is a manifold,
we can transform it into an equivalent cell-complex
which is built from a fundamental polyhedron,
but the study of the 2-dimensional algebraic structure
can not be reduced to the study of a fundamental group,
because a lot of information in the fundamental polyhedron
will be lost if we insist on reduce to group.

But the algebraic structure of fundamental polyhedron
is still easier to study than that of general cell-complex.

**Conjecture:**
The presentations of 3-dimensional manifold
as algebraic structure of fundamental polyhedron
can be normalized to given a complete classification
of 3-dimensional manifolds.

## About Algebraic Structures

The elements of a groupoid,
when viewed as a cell-complex,
are (undirected) _paths_ built from _edges_.

A 3-dimensional cell-complex,
when viewed as an algebraic structure,
its elements are _surfaces_ built from _faces_.

For edges, there are only two ways to do composition
(this is where the inverse of groupoid elements come from),
but for faces, there are `n` ways to do composition,
where `n` is the number of edges of the a face.

Composition of two cells -- `A` and `B` must be viewed as
removing some common boundaries of `A` and `B`,
and the result must still be a cell
-- i.e. having spherical boundary.

New syntactic devices are needed to represent
the compositions easily,
the new syntactic devices are about
placeholder variables and unification,
just like the logic variables and unification of relations
in logic programming languages like Prolog.
This syntactic difficulty is the main reason,
why higher dimensional algebraic structures
are not well studied in the past.

# TODO

Example algebra of 1-dimension

- Take presentation of group as example
- Take interesting groups as example (fundamental polygon)

Example algebra of 2-dimension

Example algebra of 3-dimension -- for `Pi(3)(S(2))`

About implementation:

- If we think about how to implement higher order incidence relation
  by objects and pointers,
  and we store pointers at both direction at every order,
  the implementation will be a graph model of cell-complex.

- Should we use hypergraph to encode higher order incidence relation?

Product space -- and boundary operator over it.

Review old notes:

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

## Problem of Hopf fibration

We are trying to find a way to generalize algebraic structure to higher dimension,
how can 2-dimensional sphere has non trivial 3-dimensional algebraic structure?

- Hopf fibration describe partition of the ball of `S(3)`,
  but in our language, we can not describe this detailed partition.

Maybe we can define partition on cell-complex.

When defining a map between two spaces,
we must also define the map for all possible partitions

- Maybe Like [surreal number](https://en.wikipedia.org/wiki/Surreal_number).

When we define map between the same dimension,
our way of definition already give a definition
for all possible partitions.

We can map element cross dimension,
but must also define the map for all possible partitions.

Then we can define Hopf fibration in a
intuitive and pure topological way.

## Problem of the separation between fixed parameters and varied indexes

In inductive datatypes, a type constructor can take arguments
and the arguments are to separated into two groups:

- **fixed (parameters)** which do NOT vary between the data constructors.
- **varied (indexes)** which can vary between the data constructors.

When we introduce arguments to type constructor for complex,
what is the meaning of the above separation?

What is the topological interpretion
of "fixed v.s. varied arguments"
for inductive datatype?

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

They are NOT model.

For example, we can interpret a directed graph as a causal model.

# Higher dimensional inductive datatypes

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

The following concepts are not built-in our language:

- Subset.
- Quotient.

## Naming and syntax keywords

| dimension | generator (cell) | element (complex) | spherical element (spherical complex) |
| --------: | ---------------- | ----------------- | ------------------------------------- |
|         0 | vertex           |                   | `endpoints`                           |
|         1 | edge             | `path`            | `polygon`                             |
|         2 | face             | `surface`         | `polyhedron`                          |
|         3 | block            | `building`        | `polychoron`                          |

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

[question] How to understand cell-complex as inductively defined set?

- 0-dimensional cell-complex is inductively defined set.

## 1-dimensional

### Circle

```cicada
datatype Circle {
  base: Circle
  loop: endpoints [ base, base ]
}
```

## 2-dimensional

### Four ways to glue a square

```cicada
datatype Sphere {
  south: Sphere
  middle: Sphere
  north: Sphere
  southLong: endpoints [ south, middle ]
  northLong: endpoints [ middle, north ]
  disk: polygon [
    southLong, northLong, -northLong, -southLong
  ]
}
```

```cicada
datatype Torus {
  origin: Torus
  toro: endpoints [ origin, origin ]
  polo: endpoints [ origin, origin ]
  spoke: polygon [ toro, polo, -toro, -polo ]
}
```

```cicada
datatype KleinBottle {
  origin: KleinBottle
  toro: endpoints [ origin, origin ]
  cross: endpoints [ origin, origin ]
  disk: polygon [ toro, cross, -toro, cross ]
}
```

```cicada
datatype ProjectivePlane {
  start: ProjectivePlane
  end: ProjectivePlane
  leftRim: endpoints [ start, end ]
  rightRim: endpoints [ end, start ]
  disk: polygon [ leftRim, rightRim, leftRim, rightRim ]
}
```

## 1-dimensional algebra

```cicada
space Torus {
  check path [ toro, toro, toro, polo ]: endpoints [ origin, origin ]
  check path [ toro ]: endpoints [ origin, origin ]
  check path [ relf(origin) ]: endpoints [ origin, origin ]
  check path [ toro, relf(origin) ]: endpoints [ origin, origin ]
}
```

## 3-dimensional

### Torus3

```cicada
datatype Torus3 {
  o: Torus3
  x: endpoints [ o, o ]
  y: endpoints [ o, o ]
  z: endpoints [ o, o ]
  zFace: polygon [ z, y, -z, -y ]
  yFace: polygon [ x, z, -x, -z ]
  zFace: polygon [ y, x, -y, -x ]
  body: polyhedron {
    xFace [ Y3, Z2, -X2, -Z0 ]
    yFace [ Y0, X2, -Z1, -Y3 ]
    xFace [ Z0, Z1, -Z2, -Y0 ]
  }
}
```

The idea is that to specify the attaching map
from polyhedron to 2-skeleton of the space,
we can use a syntax like logic programming,
i.e. to use meta variable and linear unification
to specify how edges of polygons are glued together.

- Note that, polygons are elements of the boundary of polyhedron.

### Torus3 -- Cubical

Using `I` (`Interval`) as an coordinate system.

- How to coordinate polygon and polyhedron?
  as simple as `I`, because to use `C` as a coordinate system
  is to have a function `(C) -> ...`.

```cicada
datatype Torus3 {
  o: Torus3

  x: (I) -> Torus3 with { case (I::0) => o case (I::1) => o }
  y: (I) -> Torus3 with { case (I::0) => o case (I::1) => o }
  z: (I) -> Torus3 with { case (I::0) => o case (I::1) => o }

  xFace: (I, I) -> Torus3 with {
    case (I::0, i) => z(i)
    case (I::1, i) => z(i)
    case (i, I::0) => y(i)
    case (i, I::1) => y(i)
  }

  yFace: (I, I) -> Torus3 with {
    case (I::0, i) => x(i)
    case (I::1, i) => x(i)
    case (i, I::0) => z(i)
    case (i, I::1) => z(i)
  }

  zFace: (I, I) -> Torus3 with {
    case (I::0, i) => y(i)
    case (I::1, i) => y(i)
    case (i, I::0) => x(i)
    case (i, I::1) => x(i)
  }

  body: (I, I, I) -> Torus3 with {
    case (I::0, i, j) => xFace(i, j)
    case (I::1, i, j) => xFace(i, j)
    case (i, I::0, j) => yFace(i, j)
    case (i, I::1, j) => yFace(i, j)
    case (i, j, I::0) => zFace(i, j)
    case (i, j, I::1) => zFace(i, j)
  }
}
```

The syntax of cubical type theory
does not provide enough information.

It seems it is using some conversion
about overloading map to map of boundary,
which works only in special cases.

### Torus3 -- Cubical -- My Way

```cicada
datatype Torus3 {
  o: Torus3

  // To introduce a 1-dim element,
  // we map `boundary(I)` to the 0-skeleton,
  // i.e. previously introduced 0-dim elements.

  x: boundary(I) -> skeleton(0, Torus3) { case (I::0) => o case (I::1) => o }
  y: boundary(I) -> skeleton(0, Torus3) { case (I::0) => o case (I::1) => o }
  z: boundary(I) -> skeleton(0, Torus3) { case (I::0) => o case (I::1) => o }

  // Alternative syntax:

  x: skeleton(1, Torus3) with {
    // - `x` is part of `skeleton(1, Torus3)`, which is a subspace of `Torus3`.
    // - To introduce a 1-dim element, we need to use a 0-spherical complex
    //   as the coordinate system of the 1-dim element's boundary.
    // - The coordinate system is the domain of the attaching map.
    // - The 0-spherical complex we will use is `boundary(I)`
    //   i.e. two endpoints of the `I` -- `I::0` and `I::1`.
    // - We use a case function called -- `attach`,
    //   to specify the attaching map.
    attach(boundary(I)): skeleton(0, Torus3) {
      case (I::0) => o
      case (I::1) => o
    }
  }

  // By using `boundary(I)` as coordinate,
  // we can get `x`'s boundary by applying `x`
  // to elements of `boundary(I)` -- `I::0` and `I::1`.

  // This means we overload the syntax of function application
  // to get the boundary of higher inductive elements.
  // This is the idea of lambda encoding.

  // To introduce a 2-dim element,
  // we map `boundary(I, I)` to 1-skeleton,
  // i.e. previously introduced 1-dim elements.

  // We should NOT write the following:

  xFace: boundary(I, I) -> skeleton(1, Torus3) {
    case (I::0, I::path) => z
    case (I::1, I::path) => z
    case (I::path, I::0) => y
    case (I::path, I::1) => y
  }

  // Because it is not enough to specify
  // the target of `(I::0, I::path)`
  // we also need to specify
  // the target of `(I::0, boundary(I::path))`

  xFace: boundary(I, I) -> skeleton(1, Torus3) {
    case (I::0, I::path) => z with {
      case (I::0, I::0) => z(I::0)
      case (I::0, I::1) => z(I::1)
    }
    case (I::1, I::path) => z with {
      case (I::0, I::0) => z(I::0)
      case (I::0, I::1) => z(I::1)
    }
    case (I::path, I::0) => y with {
      case (I::0, I::0) => y(I::0)
      case (I::0, I::1) => y(I::1)
    }
    case (I::path, I::1) => y with {
      case (I::0, I::0) => y(I::0)
      case (I::0, I::1) => y(I::1)
    }
  }

  // Alternative syntax:
  // - Maybe we should call the `attach` method `boundary`,
  //   and not to overload function application -- `z(I::0)`
  //   but to overload dot -- `z.boundary(I::0)`.
  //   - The overloading of dot occurred during the design of
  //     - fulfilling class.
  //     - data constructors as static methods.
  // - How to understand this in type theory?
  // - How to understand this in set theory?

  xFace: skeleton(2, Torus3) with {
    attach(boundary(I, I)): skeleton(1, Torus3) {
      case (I::0, I::path) => z with {
        // The type of the case function inside `with`:
        //   (boundary(I::0, I::path)) -> boundary(z)
        case (I::0, I::0) => z(I::0)
        case (I::0, I::1) => z(I::1)
      }
      case (I::1, I::path) => z with {
        case (I::0, I::0) => z(I::0)
        case (I::0, I::1) => z(I::1)
      }
      case (I::path, I::0) => y with {
        case (I::0, I::0) => y(I::0)
        case (I::0, I::1) => y(I::1)
      }
      case (I::path, I::1) => y with {
        case (I::0, I::0) => y(I::0)
        case (I::0, I::1) => y(I::1)
      }
    }
  }

  // Note that, in the code above, we get `z`'s boundary
  // by applying `z` to elements of `boundary(I)`.

  // In the same way, by using `boundary(I, I)` as coordinate,
  // we can get `xFace`'s boundary by applying `xFace`
  // to elements of `boundary(I, I)`,
  // which will be used when introducing 3-dim elements.

  body: boundary(I, I, I) -> skeleton(2, Torus3) with {
    case (I::0, I::path, I::path) => xFace with {
      case (I::0, I::0, I::path) => xFace(I::0, I::path) with {
        // We also need to specify the boundary of this map.
        case (I::0, I::0, I::0) => xFace(I::0, I::path)(I::0)
        case (I::0, I::0, I::1) => xFace(I::0, I::path)(I::1)
      }
      case (I::0, I::1, I::path) => xFace(I::1, I::path) with { ... }
      case (I::0, I::path, I::0) => xFace(I::path, I::0) with { ... }
      case (I::0, I::path, I::1) => xFace(I::path, I::1) with { ... }
    }
    ...
  }
}
```

The most important idea is that when specifying a mapping
from a n-dim element `A` to composition of n-dim elements `B`,
we also need to specify how each element of the boundary of `A`
is mapped to the boundary of `B`, and recursively
specifying the mapping down to 0-dim.

Should we call this **the principle of continuity**?

[question] How to understand the use of `with` by boundary as type?

[question] How to understand the use of `with` in set theory?

## 2-dimensional algebra

```cicada
space Torus3 {
  check surface { refl(refl(o)) [ ... ] }: polygon [ refl(o), -refl(o) ]
  check surface { refl(x) [ ... ] }: polygon [ x, -x ]
  check surface { refl(x) [ ... ] refl(x) [ ... ] }: polygon [ x, x, -x, -x ]
  check surface {
    xFace [ B3, C2, -A2, -C0 ]
    yFace [ B0, A2, -C1, -B3 ]
    refl(x) [  ... ]
  }: polygon [ ... ]
}
```

## Homotopy group `S3`

<https://en.wikipedia.org/wiki/Homotopy_groups_of_spheres>

For `n >= 2`, `Pi(n)` is abelian.

- [Eckmann–Hilton argument](https://en.wikipedia.org/wiki/Eckmann%E2%80%93Hilton_argument).

```cicada
datatype S1 {
  base: S1
  rim: endpoints [ base, base ]
}

datatype S2 {
  south: S2
  north: S2
  meridian: endpoints [ south, north ]
  disk: polygon [ meridian, meridian ]
}

datatype S3 {
  // TODO
}
```

TODO How to do 3-dimensional algebra?

```cicada
space S3 {
  check building { ball }: polyhedron S3 {
    disk [ X, -Y ]
    disk [ X, -Y ]
  }
}
```

TODO How come a sphere `S2` has an 3-dimensional algebraic structure in it?

- Degenerated 3-dimensional elements in `S2` has non-trivial structure?

```cicada
space S2 {
  check building { relf(disk) }: polyhedron {
    TODO
  }

  check building { relf(relf(rim)) }: polyhedron {
    TODO
  }
}

```

TODO How to define `Pi(3)(S(2))`?

```cicada
function Pi3S2 (s3: S3): S2 {
  case (S3::base) => S2::base
  case (S3::rim) => S2::rim
  case (S3::disk) => S2::disk
  case (S3::ball) => TODO
  // It seems there is almost not information in definition of `S3`,
  //   thus `Pi3S2` must be about the structure of
  //   the higher dimensional algebra itself.
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
space Pair(Circle, Circle) {
  check path [ cons(base, loop) ]: endpoints [ cons(base, base), cons(base, base) ]
  check surface { cons(loop, loop) }: polygon [ ... ]
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
