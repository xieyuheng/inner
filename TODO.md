---
title: My research as an adventure game
subtitle: Be explicit and clear about our goals and progress, to keep direction and help planning.
author: Xie Yuheng
---

# Main quests

## Cicada Language

The goal is to design a language for foundations of mathematics.

This quest is beautifully formulated by Vladimir Voevodsky as the following:

> A formal deduction system together with a correspondence
> between its components and objects and actions in the world of mathematical thoughts
> which can be used to formalize all subject areas of mathematics
> is called a foundational system for mathematics or "Foundations of Mathematics".

- The language must have enough features to formalize mathematic theorems and proofs.

  Currently we are still in lack of some important features:

  - **[side quest]** Recursion and termination-check
  - **[side quest]** Subtyping

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

  - Telescope.

    Telescope is used in data construction, function application and class telescope.

    Node that, in our implementation, telescope is `env` (not `ctx`).

    Telescope is about scope and binding.
    formal semantics of natural language, also study scope and binding.

    - <https://en.wikipedia.org/wiki/Scope_(formal_semantics)>
    - <https://en.wikipedia.org/wiki/Binding_(linguistics)>

- **[pattern]** Maybe the idea of fulfilling types is new and worth sharing as a paper.

  For fulfilling types make it easy to formalize some concepts of category theory.

## JoJo calculus

A language on which categorical semantics would fail.
A language whose type system scale to higher dimensions.

## Cell complex

Higher dimensional algebraic structure.

> To find an algebraic definition of infinity groupoids
> that would satisfy the Grothendieck correspondence.

- **[problem]** From the point of view of type theory,
  how lowdim project different from HOTT?

  # formalization of algebraic topology

- **[problem]** Can we use such implementation in Geometric modeling?

# Side quests

## Recursion and termination-check

The most simple termination-check is based on the general inductive principle.
i.e. not only use the case of `n-1` as hypothesis in inductive step,
but also be able to use the case of `n-2`, `n-3`, ... (if available).

## Interaction nets and linear logic

> These two topics can be studied separately,
> but based on the principle of type theory,
> we should study term and type together.

Using Interaction nets to implement lamping optimal reduction.

- To learn from aaron stump lectures.
- Maybe we can handle infinite normal form.

References:

- The Lamping algorithm paper.
- The Lambdascope paper.

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
and can be formalized in logic programming languages like Prolog.

But Prolog can only do proof search,
we also want to write proof by hand,
even if we do not have understand the computational aspect of the logic yet,
i.e. we do not know how to evaluate terms or how to normalize terms.

- **[project]** `cicada-lang/relation`

  - JSON based logic programming language.
  - Be able to write proof be hand.
  - Learn Prolog for real.

- **[side quest]** Extending Martin Löf's type theory

  There are many ways to extend Martin Löf's type theory.

  - Inductive types.
  - Higher inductive types.

  What are the general principles for introducing new types?

  Maybe we can use logic programming language to explore different inference rules.

- **[problem]** Is inductive types already enough to formalize inference rules?

  Inductive type constructors are relations.

  假设在 datatype 中定义数据构造子时，
  其类型所代表的是纯逻辑式编程。
  即所有的关系都是可逆的。

  但是在 dependent type system 的 judgment 中，情况不是如此，
  因为 evaluate 不是可逆的。

## Lambda-cube, lambda encoding and self types

- **[project]** `lambda-disco`

  To implement lambda-cube with self types,
  and use them to understand lambda encoding of inductive types.

## Formalize category theory

Category theory is the most basic target for formalization.

We can also try to apply the formalization
to categorical semantics and categorical logic.

- Take rules about product and either as examples.
- Inversion principle of inference rule is special case of adjoint functor.
- Categorical semantics of inductive type -- F-algebra, and initial algebra.
- Categorical semantics of dependent type -- also about adjoint functor.

- **[problem]** Can we really use categorical semantics to guide
  the design of programming language's type systems?

The archetype of categorical logic is
cartesian closed category and simply typed lambda calculus.

- <https://en.wikipedia.org/wiki/Cartesian_closed_category>
- <https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus>
- <https://en.wikipedia.org/wiki/Categorical_logic>

References:

- categorical-semantics: ~/watching/person/bartosz-milewski

  - [Bartosz's youtube channel](https://www.youtube.com/user/DrBartosz)

  Progress:

  - Category Theory 4.1: Terminal and initial objects
  - Category Theory 4.2: Products
  - Category Theory 5.1: Coproducts, sum types

- <https://ncatlab.org/nlab/show/categorical+model+of+dependent+types>

# General practice

## Problem solving

### Polya

- ~/inner/person/polya/mathematics-and-plausible-reasoning

  - vol-2--patterns-of-plausible-inference.md

## software design

### TDD

- ~/watching/tdd/james-shore

### OOP

- 模仿 "99 Bottles of OOP" 用中文的童谣来讲解 OOP

  References:

  - <https://en.wikipedia.org/wiki/99_Bottles_of_Beer>
  - "The Complexity of Songs", Knuth, Donald.
