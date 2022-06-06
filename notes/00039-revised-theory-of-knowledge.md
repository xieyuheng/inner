---
title: Revised theory of Knowledge
date: 2022-06-05
---

An update to [00006-peirce-s-pragmatic-theory-about-knowledge.md](00006-peirce-s-pragmatic-theory-about-knowledge.md),
after I learned Judea's work about causation.

# Introduction

In this note, we try to understand logic theories and type system
along Peirce's theory about knowledge.

We do this by a series of definitions.

The most important definition comes from
[Peirce](https://en.wikisource.org/wiki/The_Fixation_of_Belief):

> Knowledge is fixation of belief.

# Knowledge and belief

**Knowledge** is fixation of belief.

**Belief** is that upon which a man is prepared to act.

- What to be believed are causal relations,
  for example, I believe if I do `A`, it will cause `B`,
  and `B` is the result desired by me,
  thus I remember this causal relation
  as a function `f: A -> B`.

  - The performer of the function might be the nature.

    > If I setup the situation `A`, by nature's law,
    > (after some evaluation) `B` will be the resulting situation.

    This interpretation of function type is different from
    the constructive interpretation:

    > A function `f: A -> B`
    > is a way of constructing an element of type `B`
    > given an element of type `A`.

    Two interpretations are the same,
    if we view the the nature as a big computer.

# Logic, judgment and inference

**Logic** is the system (or language) in which we express our beliefs.

**Judgment** is an attitude that a person takes towards expressions.

- When we come to know something, we are making a judgment.

**Logic inference rule** is formalized definition of judgment (belief).

- By giving algorithm to check whether a group of data
  can be viewed as evidence of the judgment,
  which is the same as type checking algorithm.

**Introduction rule** use a group of data constructors to define a type constructor.

- This is one way of giving such algorithm,
  because type (judgment) can only be introduced by these data constructors.

**Elimination rule** TODO

- [problem] I do not understand the duality between introduction and elimination.
  when introduction rules are given how to generate elimination rules?

- [problem] This means we use introduction rules to define type, how about elimination?

  - The duality between introduction rules and elimination rules
    might be explained by the concept of
    [adjunction in category theory](https://en.wikipedia.org/wiki/Adjoint_functors).

# Computation and program

**Computation** is directed changes (simulation of action).

**Computational model** is rule about how to change the state in the next step.

- For examples:
  - Beta reduction of lambda calculus.
  - Turing machine.

**Program** is carefully prepared init state.
