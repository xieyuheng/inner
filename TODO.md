---
title: My research as an adventure game
subtitle: Be explicit and clear about our goals and progress, to keep direction and help planning.
author: Xie Yuheng
---

- use patterns to organize knowledges like martin fowler

# Main quests

## Cicada Language

The goal is to design a language for foundations of mathematics.

- The language must have enough features to formalize mathematic theorems and proofs.

  Currently we are still in lack of some important features:

  - Recursion and termination-check.
  - Subtyping.

  We already implemented inductive types,
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

- We must actually use this language to formalize a lot of mathematics,
  because without applications, language design project is not complete,
  and applications will provide feedback to improve our design.

  - **[side quest]** Formalize category theory

Patterns:

- **[pattern]** To summarize patterns in our implementation of cicada language.

  - Statements v.s. expressions.
  - Bidirectional type checking.
  - NbE.
  - Inductive types.
  - Implicit and vague arguments.

# Side quests

## Interaction nets and linear logic

> These two topics can be studied separately,
> but based on the principle of type theory,
> we should study term and type together.

Using inet to implement lamping optimal reduction.

- To learn from aaron stump lectures.
- Maybe we can handle infinite normal form.

References:

- The lamping paper
- The lambdascope paper

## Symbolic systems

- **[project]** `monoid-presentation`

  One of linear logic's model is *phase semantics*,
  in which the definition of *phase space* is based on monoid.

  Thus it is interesting to be able to play with monoid via presentation.

- **[project]** `group-presentation`

  Group presentation is much more well studied than monoid presentation.

- **[project]** `xieyuheng/algebra`

  We are good at implement programming languages,
  which means we are good at playing with expressions.

  Thus it is good to apply our skill to other problem domain
  which requires playing with expressions.

  For example, we can practice by
  implementing computer algebra based on `bigint` of JavaScript.

  We are specially interested in polynomial and ring.

  - Remember Coq's `ring` tactic.

  Possible topics:

  - `Integer` -- OOP API over `bigint` of JavaScript.
  - `Fraction`
  - `ContinuedFraction`

## Logic programming

Remember that inference rules of a logic are relations,
and can be formalized in a logic programming language.

- **[project]** `cicada-lang/relation`

  - json based logic programming language
  - learn prolog for real

## Lambda-cube, lambda encoding and self types

- **[project]** `lambda-disco`

  To implement lambda-cube with self types,
  and use them to understand lambda encoding of inductive types.

## Formalize category theory

- categorical-semantics: ~/watching/person/bartosz-milewski

  - [Bartosz's youtube channel](https://www.youtube.com/user/DrBartosz)

  Progress:

  - Category Theory 4.1: Terminal and initial objects
  - Category Theory 4.2: Products
  - Category Theory 5.1: Coproducts, sum types

# General practice

## Problem solving

- polya: ~/inner/person/polya/mathematics-and-plausible-reasoning

  - vol-2--patterns-of-plausible-inference.md

## software design

- tdd: ~/watching/tdd/james-shore
