---
title: C System
---

# C System

## Terminology

The C System was introduced by John Cartmell under the name "contextual categories",
but since the algebraic structure so introduced is not invariant under equivalences of categories.
The Terminology "pre-category" and "C System" was suggested by Vladimir Voevodsky.

## [John Cartmell] [1985] Generalised algebraic theories and contextual categories

- **[Xie]**
  This paper revises John Cartmell's 1978 thesis.

### 1. Introduction

- **[Xie]**
  The aim of this paper is to provide semantics to type theories by constructing algebraic structures.
  Such semantics if developed can guide the implementation of type theories.

  Given a syntactic theory (or say, logical deduction system) (such as type theory),
  we wish to find an algebraic structure as its model.

Example relationship between syntactic theory and algebraic structure,

- Propositional Theory of Classical Logic and Boolean Algebra,
- Propositional Theory of Intuitionistic Logic and Heyting Algebra,
- Single Sorted Algebraic Theory and Lawvere's notion of an Algebraic Theory,
- Equational Theory of the language of the typed lambda-calculus and Cartesian Closed Category,
- Theory of Higher Order Intuitionistic Logic and Topos,
- Coherent Theory and Grothendieck Site,

in each case there can be defined the notion of model of a given theory in a given structure,
and thus the structures provide a semantics for the theories.

We call such semantics **algebraic semantics**.

Intuitively, an algebraic semantics for a syntactic theory is "right",
if it is minimal and most general.
(or say, free from unnecessary codification.)

This intuitive concept can be captured by
categorical equivalence between
the categories of syntactic theory,
and the categories of the algebraic structure.

The claim of this paper is that the contextual categories (that will be developed)
provide us with the algebraic semantics of generalised algebraic theories.

## 2. Generalised algebraic theories

The notion of a generalised algebraic theory is a generalisation
of the notion of a many-sorted algebraic theory in just the following manner.

Whereas the sorts of a many-sorted algebraic theory are constant types
in the sense that they are to be interpreted as sets,
the sorts of a generalised algebraic theory need not all be constant types
some of them being nominated as variable types (dependent types),
in which case they are to be interpreted as families of sets.

The type or types over which the variation of a variable ranges must always be specified.

Thus, a generalised algebraic theory consists of
- (i) a set of sorts, each with a specified role either as a constant type
  or else as a variable type varying in some way,
- (ii) a set of operator symbols, each one with its argument types
  and its value type specified (the value type may vary as the argument varies),
- (iii) a set of axioms. Each axiom must be an identity between similar well-formed expressions,
  either between terms of the same possibly varying type or else between type expressions.

- **[Xie]**
  The term "generalised algebraic theories" can be viewed as
  using **abstract class** of programming languages to formalize algebraic structures.

  An algebraic structure can be single sorted, such as `semigroup_t`, `group_t` and `ring_t` (the sort is elements), for example,

  ``` js
  class semigroup_t {
    // (i) a set of sorts
    elem_t: type
    // (ii) a set of operator symbols
    mul(x: elem_t, y: elem_t): elem_t
    // (iii) a set of axioms
    mul_associative(x: elem_t, y: elem_t, z: elem_t): eqv_t(mul(x, mul(y, z)), mul(mul(x, y), z))
  }
  ```

  An algebraic structure can can also be many sorted,
  such as `category_t` (the sorts are objects and arrows),

  ``` js
  class category_t {
    // (i) a set of sorts
    object_t: type
    morphism_t(object_t, object_t): type
    // (ii) a set of operator symbols
    id(a: object_t): morphism_t(a, a)
    compose[a: object_t, b: object_t, c: object_t](
      f: morphism_t(a, b),
      g: morphism_t(b, c),
    ): morphism_t(a, c)
    // (iii) a set of axioms
    id_left[a: object_t, b: object_t](f: morphism_t(a, b)): eqv_t(compose(id(a), f), f)
    id_right[a: object_t, b: object_t](f: morphism_t(a, b)): eqv_t(compose(f, id(b)), f)
    compose_associative[a: object_t, b: object_t, c: object_t, d: object_t](
      f: morphism_t(a, b),
      g: morphism_t(b, c),
      h: morphism_t(c, d),
    ): eqv_t(compose(f, compose(g, h)), compose(compose(f, g), h))
  }
  ```

  Note that,
  the formalization of category theory (the definition of abstract class `category_t`),
  involves **dependent record type**,
  for the type `morphism_t` is indexed by elements of `(object_t, object_t)`.

  When thinking about providing semantics for type theory,
  the type theory we wish to talk about is a type theory
  in which mathematical practice can be naturally formalized.

  Such a type theory include Martin-Löf's type theory,
  thus this paper can also be viewed as providing algebraic semantics to Martin-Löf's type theory.

  In my view,
  to formalize mathematical practice in a natural way,
  we also need the following features,
  - Subtype (inheritance) -- to avoid re-implementation,
  - Quotient types -- which occurs very often in mathematical constructions,
  - Fulfilling type system -- to use type constructors in a flexible way.
