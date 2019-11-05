---
title: Categorical Intuitions
---

# Categorical Intuitions

Notes taken from "Categorical Manifesto", by Joseph Goguen, 1989.

## Category

To each species of mathematical structure,
there corresponds a category whose objects have that structure,
and whose morphisms respect [preserve] it.

## Isomorphism

Two objects have the same structure iff they are isomorphic,
and an "abstract object" is an isomorphism class of objects.

## Functor

To any natural construction on structures of one species,
yielding structures of another species,
there corresponds a functor
from the category of the first species
to the category of the second.

For example, in the category of types in a programming language,
type constructors are endo-functors,
and endo-functors are often containers.

Functor can also be called natural-construction,
which will let the term `natural-transformation` make sense.

## Natural transformation

To each natural translation
from a construction `F : A -> B`
to a construction `G : A -> B`
there corresponds a natural transformation `F => G`.

This captures the concept of "natural translation".

The naturality condition of natural-transformation
state squares commute.

Which can be viewed as stating that
the arrows in the two embeddings
are `orthogonal` to the transforming arrows.

This concept was the historical origin of category theory,
since Eilenberg and MacLane (1945) used it to formalise
the notion of an equivalence of homology theories,

and then found that for this definition to make sense,
they had to define functors,

- A homology theory is a functor.

and for functors to make sense,
they had to define categories.

- A homology theory is a functor,
  from the category of topology spaces
  to the category of abelian-groups.

## Limit

A diagram D in a category C can be seen as a system of constraints,
and then a limit of D represents all possible solutions of the system.

## Adjoint

To any canonical construction from one species of structure to another
corresponds an adjunction between the corresponding categories.

This captures the concept of "canonical construction".

## Colimit

TODO

## Comma category

Comma categories are another basic construction that
first appeared in lawvere's thesis.

They tend to arise when morphisms are used as objects.

TODO
