---
title: My research as an adventure game
subtitle: Be explicit and clear about our goals and progress, to keep direction and help planning.
author: Xie Yuheng
---

> The idea of game, basically, is this:
> that the nature of the world is musical.
> i.e. everything that is happening is its own point.
>
> -- ["The Joker"](https://www.organism.earth/library/document/the-joker), Alan Watts

# TODO

learn about general constraint logic programming framework.

it is import that mod has a simple map from name to values.

- `whereabouts` need to change.

`whereabouts` maybe we can avoid using refresh, by generate fresh at compile time.

implement type constraint for `whereabouts`.

learn about general nominal logic.

implement nominal logic in `whereabouts`.

implement alphaLeanTab -- learn this way Ray's book about first-order logic.

implement explicit-substitution

- nominal logic is important for just understanding the substitution used in paper.

implement `lowdmin` again, use constraint idea to handle boundary relation.

- ctx should not just be a mapping, but be like solution.
- subdivision and surreal number

try the ideas about graph and groupoid in `lowdmin`

- the cubical version must provide a cubical-complex based model of cubical type theory

If `datatype` can be used to define infinite set,
`datatype` with higher dimensional constructors
must also be able to define infinite cell-complex,
we should start from 1-dim and try to
define infinite graph by `datatype`.

When see examples of hott or cubical type,
write them in cicada syntax.

------

The rules about appling elements of `Equal` as functions,
which can be used to prove simple structural `Equal` like `equalPair`.

Might be the rules we are looking for in the design of language for cell-complex,
where are also a lots of "applying under structural".

------

VV said playing with expressions of proofs should be
as easy as playing with expressions of polynomials.
But bound variables and substitution made it not so easy.

What if we have a language without lambda, only top-level named functions?
Do we still need to distinguish `Value` from `Exp` and use `Closure` to handle scope?

- In normal implementation, top-level `Stmt` is different from `Exp`.
- Glued evaluation need to handle top-level name and local name (`Closure`) differently.
- With module system, such a language is useful enough.
- When writing a compiler, there is a pass which makes all closures top-level.
- Prolog is such a language.
- C is such a language.
- Maybe `whereabouts` should be such a language.

We should implement a language to try this idea.

Maybe we should make `whereabouts` explicitly such a language,
to simplify its implementation.

------

Redesgin the syntax of BCPL and implement it in `cicada-lang/bcpl`.

------

Closed data can be implemented by pure abstract classes.

- We should explore this.
- [Gerald Jay Sussman keynote and discussion - reClojure 2021](https://www.youtube.com/watch?v=EbzQg7R2pYU)

------

> JavaScript and c-family language design.

Review the SICP JavaScript version.

------

> Get back to numbers.

When I study probability theory,
I am unwilling to use just numbers.
This might be wrong, because numbers are still
the must important object in mathematics.

- draw algebraic geometry
- simulate classical mechanics
- draw differential geometry

------

During study of "Causality -- Models, Reasoning and Inference",
I found that maybe we should revisit the idea of [orbit](https://withorbit.com/).

- Repo: https://github.com/andymatuschak/orbit
- Because I keep forgetting:
  - definition of terms
  - import intuitions
  - typical examples
- and: https://activerecall.com
- New design of mimor
  - one card one xml element,
    one file many cards,
    one directory one deck
    (maybe with a JSON config file).

------

Is it easier to use hyper graph to model cell-complex?

Re-study homotopy theory and algebraic topology.

- Study algebraic geometry, by which Hopf fibration are defined.

Review notes about homotopy type theory.

V.V.'s did not use `Iso` in his univalent axiom,
but changed the definition of `Iso`!

- If `Iso <-> isEquiv` why we can not use `Iso` to define univalent axiom?

Understand homotopy type theory with cell-complex as basic objects.

- Interpret type theory by cell-complex (model of a logic system)
- Revisit [Curry–Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)
  every now and then.
  - ~/persons/de-bruijn/on-the-roles-of-types-in-mathematics.pdf

------

Word problems of semigroup, monoid and group.

- About undecidable.

Term syntax for linear logic -- simple types first.

- How does the algebra of linear logic connectives
  different from boolean lattice?

Use linear logic as type system of inet.

Implement optimal beta-reduction by inet.

------

Analysis Christopher Alexander's "A Pattern Language",
to use patterns to record my knowledge.

- Case study the structure of Martin Fowler's homepage.
- Readonly.Link should support writing patterns.

# Main quests

## Cicada Language

The goal is to design a language for foundations of mathematics.

This quest is beautifully formulated by Vladimir Voevodsky as the following:

> A formal deduction system together with a correspondence
> between its components and objects and actions in the world of mathematical thoughts
> which can be used to formalize all subject areas of mathematics
> is called a foundational system for mathematics or "Foundations of Mathematics".

The language must have enough features to formalize mathematic theorems and proofs.

- Currently we are still in lack of some important features:

  - **[side quest]** Recursion and termination-check
  - **[side quest]** Subtyping

- We already implemented inductive types,
  but we learned that it is possible to use lambda encoding
  to greatly simplify the implementation of inductive types.

  - **[side quest]** Lambda encoding and self types

    To avoid exponential lambda terms during encoding,
    we need to do graph based implementation.

  - **[side quest]** Interaction nets

    One graph based implementation of lambda is Lamping's optimal beta-reduction,
    which is based on interaction nets.

  - **[side quest]** Linear logic

    The type system of interaction nets is linear logic.

We must actually use this language to formalize a lot of mathematics,
because without applications, language design project is not complete,
and applications will provide feedback to improve our design.

- **[side quest]** Formalize category theory
- **[side quest]** Formalize algebraic topology

**[paper]** Maybe the idea of fulfilling types is new and worth sharing as a paper.

- Fulfilling types make it easy to formalize some concepts of category theory.

## JoJo Calculus

Use postfix notations as syntax.

A language on which categorical semantics would fail.

To view a type system as a homomorphism between monoids.

## Boundaries as Types

Cell-complex and higher dimensional algebraic structure.

> To find an algebraic definition of infinity groupoids
> that would satisfy the Grothendieck correspondence.

Links:

- [papers/publish/boundaries-as-types.md](papers/publish/boundaries-as-types.md)
- [topics/homotopy-type-theory/cubical-type-theory.md](topics/homotopy-type-theory/cubical-type-theory.md)
- [topics/homotopy-type-theory/hott.md](topics/homotopy-type-theory/hott.md)

The only way of visualizing topological space is to give it geometry,
and one way of introducing geometry is using _algebraic geometry_.

**[problem]** From the point of view of type theory,
how "Boundaries as Types" project different from HoTT?

**[problem]** Can we use such implementation in geometric modeling?

**[problem]** Graph theory has many models,
we have many ways to interpret nodes and edges and their relations,
but how to interpret faces and bodies of cell-complex?

- Think about things can not be modeled by graph:

  - A face as a relation between relations.
  - A face as an equivalence between equivalences.
  - A face as a relation of a multiple arguments.

- The direction of an edge as many interpretions

  - Movement.
  - Causal relation.

- how to generalize this to face and body?

## Causal Inference Engine

The goal is to design a causal inference engine,
following the steps of Judea Pearl.

- by a library and its API.
- or by a probabilistic programming language.

- **[problem]** How does such a causal inference engine
  relate to logic deduction system and "model by classes and objects"?

# Side quests

## Recursion and termination-check

The most simple termination-check is based on the general inductive principle.
i.e. not only use the case of `n-1` as hypothesis in inductive step,
but also be able to use the case of `n-2`, `n-3`, ... (if available).

## Interaction nets and linear logic

> These two topics can be studied separately,
> but based on the principle of type theory,
> we should study terms and types together.

To use interaction nets to implement Lamping's optimal beta-reduction.

- Maybe when using graph, we can also handle infinite normal form easily.

References about optimal beta-reduction:

- [Aaron Stump online lectures](https://www.youtube.com/channel/UCKQa6Ls95RhShE0kQsiXzVw).
- The Lamping algorithm paper.
- The Lambdascope paper.

## Symbolic systems

**[project]** `monoid-presentation`

- One of linear logic's model is *phase semantics*,
  in which the definition of *phase space* is based on monoid.

- Thus it is interesting to be able to play with monoid via presentation.

- **[project]** `group-presentation`

- Group presentation is much more well studied than monoid presentation.

- **[project]** `xieyuheng/algebra`

- We are good at implementing programming languages,
  which means we are good at playing with expressions.

  Thus it is beneficial to apply our skills to other problem domain
  which requires playing with expressions.

  For example, we can practice implementing a computer algebra system
  based on `bigint` of JavaScript.

  We are specially interested in polynomial and ring.

  - Remember Coq's `ring` tactic.

  Possible topics:

  - `Integer`
    - OOP API over `bigint` of JavaScript.
  - `Fraction`
  - `ContinuedFraction`

## Logic programming

Remember that inference rules of a logic are relations,
and can be formalized in logic programming languages like Prolog.

But Prolog can only do proof search,
we also want to write proof by hand,
even if we do understand the computational aspect of the logic yet,
i.e. we do not know how to evaluate terms or how to normalize terms.

**[project]** `cicada-lang/relation`

- JSON based logic programming language.
- Be able to write proof be hand.
- Learn Prolog for real.

**[side quest]** Extending Martin Löf's type theory

- There are many ways to extend Martin Löf's type theory.

  - Inductive types.
  - Higher inductive types.

- What are the general principles for introducing new types?

- Maybe we can use logic programming language to explore different inference rules.

**[problem]** Is inductive types already enough to formalize inference rules?

- Inductive type constructors are relations.

- But note that, a relation defined by inductive type constructor
  might be not reversible (not pure logic programming).

  For example, we can formalize judgments of dependent types by
  inductive type constructors, by when evaluation occurs in the it,
  it is no longer reversible.

## Lambda-cube, lambda encoding and self types

**[project]** `lambda-disco`

To implement lambda-cube with self types,
and use them to understand lambda encoding of inductive types.

## Formalize category theory

Category theory is the most basic target for formalization.

We can also try to apply the formalization
to categorical semantics and categorical logic.

- Take rules about product and sum as basic examples.
- Inversion principle of inference rule is special case of adjoint functor.
- Categorical semantics of inductive type -- F-algebra, and initial algebra.
- Categorical semantics of dependent type -- also about adjoint functor.

- **[problem]** Can we really use categorical semantics to guide
  the design of programming language's type systems?

The archetype of categorical logic is
cartesian closed category and simply typed lambda calculus.

- https://en.wikipedia.org/wiki/Cartesian_closed_category
- https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
- https://en.wikipedia.org/wiki/Categorical_logic

References:

- categorical-semantics: ~/watching/person/bartosz-milewski

  - [Bartosz's youtube channel](https://www.youtube.com/user/DrBartosz)

  Progress:

  - Category Theory 4.1: Terminal and initial objects
  - Category Theory 4.2: Products
  - Category Theory 5.1: Coproducts, sum types

- https://ncatlab.org/nlab/show/categorical+model+of+dependent+types

# General practice

## Problem solving

### Polya

~/inner/person/polya/mathematics-and-plausible-reasoning

- vol-2--patterns-of-plausible-inference.md

## software design

### TDD

~/watching/tdd/james-shore

### OOP

Mimic the style of "99 Bottles of OOP",
and apply it to other programming exercises.

- Maybe use Chinese ballads as examples.

- References:

  - https://en.wikipedia.org/wiki/99_Bottles_of_Beer
  - "The Complexity of Songs", Knuth, Donald.
