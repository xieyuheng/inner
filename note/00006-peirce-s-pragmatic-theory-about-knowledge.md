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

- inference rule := definition of judgment (belief).

  - by giving algorithm to check whether a group of data
    can be viewed as evidence of the judgment,
    which is the same as type checking algorithm.

  - use a group of data constructors to define a type constructor,
    is one way of giving such algorithm,
    because type (judgment) can only be introduced by these data constructors.

  - [problem] this means we use introduction rules to define type, how about elimination?

    - the duality between introduction rules and elimination rules
      might be explained by the concept of
      [adjunction in category theory](https://en.wikipedia.org/wiki/Adjoint_functors).

- computation := directed changes (simulation of action).
