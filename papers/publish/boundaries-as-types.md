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
    we can use one of its endpoint,
    the minus sign in `path` only means
    to change the layout of an edge
    to compose it by another endpoint.

[question] Why using path type we can view `Equal` as a `Pi` type?

- Thus prove equal under structure like `Pair` and `class` really easy.

```
i: I |- A: Type
i: I |- M: A
|- M[0/i] = x : A[0/i]
|- M[1/i] = y : A[1/i]
-----------------------
|- (i: I) => M: (i: I) -> A with {
  case (0) => x
  case (1) => y
}
```

```
i: I |- A: Type
i: I |- M: A
-----------------------
|- (i: I) => M: (i: I) -> A
```

[question] What is the general relation
between topological (homotopical) spaces
and the `Equal` type?

## Limitation of our construction

The following concepts are not built-in our language:

- Subset.
- Quotient.

## Naming and syntax keywords

| dim | cell        | complex    | spherical complex   |
|     | (generator) | (element)  | (spherical element) |
|----:|-------------|------------|---------------------|
|   0 | vertex      |            | `endpoint`          |
|   1 | edge        | `path`     | `polygon`           |
|   2 | face        | `surface`  | `polyhedron`        |
|   3 | block       | `building` | `polychoron`        |

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
  loop: endpoint [ base, base ]
}
```

## 2-dimensional

### Four ways to glue a square

```cicada
datatype Sphere {
  south: Sphere
  middle: Sphere
  north: Sphere
  southLong: endpoint [ south, middle ]
  northLong: endpoint [ middle, north ]
  disk: polygon [
    southLong, northLong, -northLong, -southLong
  ]
}
```

```cicada
datatype Torus {
  origin: Torus
  toro: endpoint [ origin, origin ]
  polo: endpoint [ origin, origin ]
  spoke: polygon [ toro, polo, -toro, -polo ]
}
```

```cicada
datatype KleinBottle {
  origin: KleinBottle
  toro: endpoint [ origin, origin ]
  cross: endpoint [ origin, origin ]
  disk: polygon [ toro, cross, -toro, cross ]
}
```

```cicada
datatype ProjectivePlane {
  start: ProjectivePlane
  end: ProjectivePlane
  leftRim: endpoint [ start, end ]
  rightRim: endpoint [ end, start ]
  disk: polygon [ leftRim, rightRim, leftRim, rightRim ]
}
```

## 1-dimensional algebra

```cicada
space Torus {
  check path [ toro, toro, toro, polo ]: endpoint [ origin, origin ]
  check path [ toro ]: endpoint [ origin, origin ]
  check path [ relf(origin) ]: endpoint [ origin, origin ]
  check path [ toro, relf(origin) ]: endpoint [ origin, origin ]
}
```

## 3-dimensional

### Torus3

```cicada
datatype Torus3 {
  o: Torus3
  x: endpoint [ o, o ]
  y: endpoint [ o, o ]
  z: endpoint [ o, o ]
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

What information are lost in cubical model?

If you compose two squares together,
you can view the result as a square,
but the number of ways to do further composition,
is reduce from 6 to 4.

### Torus3 -- Cubical -- My Way

To introduce a 0-dimensional element,
it is enough to give the name of the data constructor,
and to specify its type.

But, to introduce a (n+1)-dimensional element `A`,
we also need to specify an attaching map
from an n-dimensional spherical complex
(which will also be used as a coordinate system
to reference `A`'s boundary),
to n-skeleton of the current spece.

Overloading the syntax of function application
to get the boundary of higher inductive elements,
is the idea of lambda encoding.

```cicada
datatype Torus3 {
  o: Torus3

  // To introduce a 1-dimensional element,
  // we map `Boundary(I)` to the 0-skeleton,
  // i.e. previously introduced 0-dimensional elements.

  x: Skeleton(1, Torus3) with {
    Coordinate: Boundary(I),
    // - `x` is part of `Skeleton(1, Torus3)`, which is a subspace of `Torus3`.
    // - To introduce a 1-dimensional element, we need to use a 0-spherical complex
    //   as the coordinate system of the 1-dimensional element's boundary.
    // - The coordinate system is the domain of the attaching map.
    // - The 0-spherical complex we will use is `Boundary(I)`
    //   i.e. two endpoint of the `I` -- `I::0` and `I::1`.
    // - We use a case function called -- `boundary`,
    //   to specify the attaching map.
    boundary(Boundary(I)): Skeleton(0, Torus3) {
      case (I::0) => o
      case (I::1) => o
    }
  }

  y: Skeleton(1, Torus3) with {
    boundary(Boundary(I)): Skeleton(0, Torus3) {
      case (I::0) => o
      case (I::1) => o
    }
  }

  z: Skeleton(1, Torus3) with {
    boundary(Boundary(I)): Skeleton(0, Torus3) {
      case (I::0) => o
      case (I::1) => o
    }
  }

  // By using `Boundary(I)` as coordinate,
  // we can get `x`'s boundary by applying `x`
  // to elements of `Boundary(I)` -- `I::0` and `I::1`.

  // To introduce a 2-dimensional element,
  // we map `Boundary(I, I)` to 1-skeleton,
  // i.e. previously introduced 1-dimensional elements.

  // We choose to not overload function application -- `z(I::0)`
  // but to overload dot -- `z.boundary(I::0)`.

  // The overloading of dot also occurred during the design of
  // - fulfilling class.
  // - data constructors as static methods.

  xFace: Skeleton(2, Torus3) with {
    boundary(Boundary(I, I)): Skeleton(1, Torus3) {
      case (I::0, I::path) => z with {
        // A syntax keyword `type` to annotate the type of case function.
        // TODO Should map coordinate space to coordinate space.
        type (Boundary(I::0, I::path)) -> Boundary(z)
        case (I::0, I::0) => z.boundary(I::0)
        case (I::0, I::1) => z.boundary(I::1)
      }
      case (I::1, I::path) => z with {
        case (I::0, I::0) => z.boundary(I::0)
        case (I::0, I::1) => z.boundary(I::1)
      }
      case (I::path, I::0) => y with {
        case (I::0, I::0) => y.boundary(I::0)
        case (I::0, I::1) => y.boundary(I::1)
      }
      case (I::path, I::1) => y with {
        case (I::0, I::0) => y.boundary(I::0)
        case (I::0, I::1) => y.boundary(I::1)
      }
    }
  }

  yFace: Skeleton(2, Torus3) with {
    ...
  }

  zFace: Skeleton(2, Torus3) with {
    ...
  }

  // Note that, in the code above, we get `z`'s boundary
  // by applying `z` to elements of `Boundary(I)`.

  // In the same way, by using `Boundary(I, I)` as coordinate,
  // we can get `xFace`'s boundary by applying `xFace`
  // to elements of `Boundary(I, I)`,
  // which will be used when introducing 3-dimensional elements.

  body: Skeleton(3, Torus3) with {
    boundary(Boundary(I, I, I)): Skeleton(2, Torus3) {
      case (I::0, I::path, I::path) => xFace with {
        // TODO Should map coordinate space to coordinate space.
        type (Boundary(I::0, I::path, I::path)) -> Boundary(xFace)
        case (I::0, I::0, I::path) => xFace.boundary(I::0, I::path) with {
          // TODO Should map coordinate space to coordinate space.
          // TODO No need to specify further.
          type (Boundary(I::0, I::0, I::path)) -> Boundary(xFace.boundary(I::0, I::path))
          case (I::0, I::0, I::0) => xFace.boundary(I::0, I::path).boundary(I::0)
          case (I::0, I::0, I::1) => xFace.boundary(I::0, I::path).boundary(I::1)
        }
        case (I::0, I::1, I::path) => xFace.boundary(I::1, I::path) with {
          ...
        }
        case (I::0, I::path, I::0) => xFace.boundary(I::path, I::0) with {
          ...
        }
        case (I::0, I::path, I::1) => xFace.boundary(I::path, I::1) with {
          ...
        }
      }
      ...
    }
  }
}
```

The most important idea is that when specifying a mapping
from a n-dimensional element `A` to composition of n-dimensional elements `B`,
we also need to specify how each element of the boundary of `A`
is mapped to the boundary of `B`, and recursively
specifying the mapping down to 0-dim.

Should we call this **the principle of continuity**?

[question] How to understand the use of `with` by boundary as type?

- `Boundary(xFace)` is a set, and a subset of `Skeleton(1, Torus3)`,
  whose elements are specified by `xFace.boundary(c)` where `c: Boundary(I, I)`.
- If we have a principle to view all set as type,
  then clearly `Boundary(xFace)` should be viewed as a type.

### Torus3 -- Cubical -- My Way Revised

To design how to introduce a (n+1)-dimensional cell `A` into a cell-complex,
we need to specify a attaching map from the boundary of the cell `A`
to the n-skeleton of the cell-complex.

We do this by giving the the boundary of the cell `A`
a spherical coordinate space -- endpoint, polygon, polyhedron, ...

A spherical coordinate space is a special cell-complex,
thus we need to design how to define continuous map between two cell-complexes.

To define a continuous map between two cell-complexes `A` and `B`,
we need to specify how each cell (say `x`) of `A` is mapped to
composition of cells (say `w * y * z` (without defining `*` for now)) of `B`
of the same dimension.

But the above is not enough,
because there are many ways (many orientations)
by which the cell `x` can be mapped to `w * y * z`.

TODO Using group as example to explain the above judgment.

To specify the orientation of the map,
we must specify another map between two cell-complexes --
from the spherical coordinate space of the boundary `x`
to the spherical coordinate space of the boundary `w * y * z`.

The orienting map must not be degenerated.

Map between two spherical cell-complexes is special,
it is enough to just specify how cells are mapped to composition of cells,
no need to further specify the orientation of the map,
because it is unique.

TODO Proof the above judgment.

- To proof it, we must fully understand it first.

[question] Is the above judgment also true for manifolds?

- No, it is not true for the `Loop` -- `Gon(1)`.

  It is not about the space (both the domain space and the image space)
  is spherical or manifold or not,
  but about does the space have local loop or not.

TODO Why the above judgment is not true for general cell-complexes?

TODO For the above design to work, we must be able to get
the spherical coordinate space of the boundary
of the composite element -- `w * y * z`.

Maybe composition should be defined by composition of coordinate spaces.

------

In what sense "boundaries as types"?

Maybe in the sense that, when defining a function between cell-complexes,
the boundary relation between elements of the cell-complexes,
are used to check whether the function is continuous.

------

How should we implement boundary?

We should not apply `Boundary` to space like `Boundary(I)`,
but we can apply `Boundary` to element of space like `Boundary(xFace)`,
we can get an element of `Boundary(xFace)`
by referring to an element of the coordinate space,
and to judge whether two elements of `Boundary(xFace)` are equivalent,
we judge the equivalent between the image of the two elements
under the attaching map.

------

We should not say we are using `Boundary([I, I])` as coordinate space at all,
we should construct the coordinate space -- the square (polygon) -- directly.

Note that, the above definition of `Boundary` gives a boundary relation
between elements of a space, which feels much like the "belongs to" relation of set theory,
and the "belongs to" relation of type theory.

Boundary relation between elements of product space is easy to define,
just apply the boundary operator on one element of the tuple,
for example, `[x, z]` is part of the boundary of `[x, y]`,
if `z` is part of the boundary of `y`,
or say, `[x, z]: Boundary([x, y])`,
if and only if `z: Boundary(y)`.

------

It seems to implement higher inductive type, we need two features:

- data constructor can have `with` properties.
- later data constructor can depends on previous data constructors.

This is a generalization of what we need to define cell-complex.

Now `Skeleton` is a special kind of datatype modifier,
which requires special kind of `with` properties.

[question] What is the generalization (or say, a generalization) of `Skeleton`?

------

TODO

we should use `inductive` as keyword (like coq) -- instead of `datatype`

[keyword] inductive constraint

[keyword] static properties of data constructor

```cicada
datatype Endpoint {
  start: Endpoint
  end: Endpoint
}

datatype Gon(n: Nat) {
  // `Fin(n)` has 0, 1, ..., n-1.
  vertex(i: Fin(n)): Gon(n)
  edge(i: Fin(n)): Skeleton(1, Gon(n)) with {
    // The `with` keyword in `datatype` means
    // once we constructed a data,
    // we can use the dot syntax
    // to get the properties out of it.
    Coordinate: Endpoint,
    attach(endpoint: Endpoint): Skeleton(0, Gon(n)) {
      case (Endpoint::start) => vertex(i)
      case (Endpoint::end) => vertex(Fin::add1Mod(i))
    }
  }
}

datatype Torus3 {
  o: Torus3

  // To introduce a 1-dimensional element,
  // we map `Endpoint` to the 0-skeleton,
  // i.e. previously introduced 0-dimensional elements.

  x: Skeleton(1, Torus3) with {
    Coordinate: Endpoint,
    // - `x` is part of `Skeleton(1, Torus3)`, which is a subspace of `Torus3`.
    // - To introduce a 1-dimensional element, we need to use a 0-spherical complex
    //   as the coordinate system of the 1-dimensional element's boundary.
    // - The coordinate system is the domain of the attaching map.
    // - The 0-spherical complex we will use is `Endpoint`.
    // - We use a case function called -- `attach`,
    //   to specify the attaching map.
    attach(endpoint: Endpoint): Skeleton(0, Torus3) {
      case (Endpoint::start) => o
      case (Endpoint::end) => o
    }
  }

  y: Skeleton(1, Torus3) with {
    Coordinate: Endpoint,
    attach(endpoint: Endpoint): Skeleton(0, Torus3) {
      case (Endpoint::start) => o
      case (Endpoint::end) => o
    }
  }

  z: Skeleton(1, Torus3) with {
    Coordinate: Endpoint,
    attach(endpoint: Endpoint): Skeleton(0, Torus3) {
      case (Endpoint::start) => o
      case (Endpoint::end) => o
    }
  }

  // By using `Endpoint` as coordinate,
  // we can get `x`'s boundary by applying `x`
  // to elements of `Endpoint`.

  // To introduce a 2-dimensional element,
  // we map `Gon(4)` (polygon) to 1-skeleton,
  // i.e. previously introduced 1-dimensional elements.

  xFace: Skeleton(2, Torus3) with {
    Coordinate: Gon(4),
    attach(square: Gon(4)): Skeleton(1, Torus3) {
      case (Gon::edge(0)) => z orient {
        type (Gon::edge(0).Coordinate) -> z.Coordinate
        case (Endpoint::start) => Endpoint::start
        case (Endpoint::end) => Endpoint::end
      }

      case (Gon::edge(1)) => z orient id
      case (Gon::edge(2)) => z orient id
      case (Gon::edge(3)) => y orient id
    }
  }

  yFace: Skeleton(2, Torus3) with {
    ...
  }

  zFace: Skeleton(2, Torus3) with {
    ...
  }

  body: Skeleton(3, Torus3) with {
    attach(cube: Cube): Skeleton(2, Torus3) {
      case (Cube::xFrontFace) => xFace orient {
        type equivalent Type {
          (Cube::xFrontFace.CoodBoundary) -> xFace.CoodBoundary
        = (Gon(4)) -> Gon(4)
        }
        case (Gon::edge(0)) => Gon::edge(0)
        case (Gon::edge(1)) => Gon::edge(1)
        case (Gon::edge(2)) => Gon::edge(2)
        case (Gon::edge(3)) => Gon::edge(3)
      }
      ...
    }
  }
}
```

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
  rim: endpoint [ base, base ]
}

datatype S2 {
  south: S2
  north: S2
  meridian: endpoint [ south, north ]
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
  check path [ cons(base, loop) ]: endpoint [ cons(base, base), cons(base, base) ]
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
