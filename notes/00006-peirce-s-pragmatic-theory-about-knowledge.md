---
title: Peirce's pragmatic theory about knowledge
date: 2021-09-03
---

Try to understand logic theories and type system along Peirce's theory about knowledge.

> Knowledge is fixation of belief

# Terminology

- knowledge := fixation of belief.

- belief := that upon which a man is prepared to act.

- logic := the system we use to express beliefs.

- judgment := an attitude that a person takes towards expressions.

  - when we come to know something, we are making a judgment.

- logic inference rule := formalized definition of judgment (belief).

  - by giving algorithm to check whether a group of data
    can be viewed as evidence of the judgment,
    which is the same as type checking algorithm.

  - introduction rule := use a group of data constructors to define a type constructor.

    - this is one way of giving such algorithm,
      because type (judgment) can only be introduced by these data constructors.

  - elimination rule := TODO

    - [problem] I do not understand the duality between introduction and elimination.
      when introduction rules are given how to generate elimination rules?

  - [problem] this means we use introduction rules to define type, how about elimination?

    - the duality between introduction rules and elimination rules
      might be explained by the concept of
      [adjunction in category theory](https://en.wikipedia.org/wiki/Adjoint_functors).

- computation := directed changes (simulation of action).

  - computational model := rule about how to change the state in the next step.

    - for examples:

      - beta reduction of lambda calculus
      - turing machine

  - program := carefully prepared init state.
