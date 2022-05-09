---
title: Linear Logic
author: Jean-Yves Girard
year: 1986
---

# I. Introduction and abstract

Linear logic decomposes the operations of intuitionistic logic into more primitive ones.

Linear negation is constructive and involutive.

Linear sequent calculus is sequent calculus without weakening and contraction.

Proof-nets are natural deduction of linear logic which supports multiple conclusions.

Applications in computer science:

- If we do not use exponentials, we can control normalization.

- Linear logic connectives have obvious meaning in terms of parallel computation.

  We have two levels of connectives.

  - Multiplicative -- communication without problems of synchronization.
  - Additive -- handles synchronization.

- **Xie**: In actor model based language, we also have two levels.

  - Functional programming.
  - Message passing.

- To model the concept of cost.

- Applications to logic programming.

# II. Linear logic explained to a proof-theorist

## II.1. The maintenance of space in sequent calculus

Space on the left/right and right of a sequent.

| classical | intuitionistic     | minimal |
| --------- | ------------------ | ------- |
| `n + m`   | `n + 1` or `n + 0` | `n + 1` |

Why intuitionistic logic and minimal logic are constructive
but classical logic is not?

Or why we can prove [existence and disjunction property](https://en.wikipedia.org/wiki/Disjunction_and_existence_properties)
for intuitionistic logic but not for classical logic?

- **Xie**: The **disjunction property** is satisfied by a [theory](<https://en.wikipedia.org/wiki/Theory_(mathematical_logic)>) if,
  whenever a sentence `A ∨ B` is a theorem,
  then either `A` is a theorem, or `B` is a theorem.

Because we can not do contraction on the right of a sequent in intuitionistic logic,
the only way by which we can prove a disjunction
is to use its right (introduction) rule.

> To forbid contraction (and weakening), is to be linear.

## II.2. Linear logic as a sequent calculus

If contraction is forbidden, the following two equivalent ways of
writing the right rule of conjunction will become different:

```
|- A, C
|- B, D
-------------- M
|- A ∧ B, C, D

|- A, C
|- B, C
----------- A
|- A ∧ B, C
```

- Rule `M` treats the contexts by juxtaposition
  and yields the **multiplicative conjunction** -- `⊗` (times).

- Rule `A` treats the contexts by identification
  and yields the **additive conjunction** -- `&`(with).

We can infer rule for other connectives by using De Morgan dual.

We will find that negation is involutive and constructive,
because negation was not the source of non-constructive-ness,
contraction is the real source.

## II.3. Strength of linear logic

To translate intuitionistic logic to linear logic,
we need to introduction two exponentials -- `!` and `?`.

We can use structural rules on them.

Intuitionistic logic can be viewed as the adjunction of the modalities.

- **Xie**: TODO I do not understand adjunction (in category theory) yet.

Translate intuitionistic logic to linear logic.

- Recursive applications of the translation to subexpressions are omitted.

| intuitionistic | linear    |
| -------------- | --------- |
| `A ∧ B`        | `A & B`   |
| `A ∨ B`        | `!A ⊕ !B` |
| `A -> B`       | `!A -o B` |

## II.4. Subtlety of linear logic

- **Xie**: TODO I do not understand the concepts from proof theory.

## II.5. The semantics of linear logic: phases

- **Xie**: TODO I do not understand this yet.

  - References:
    - https://ncatlab.org/nlab/show/phase+semantics
    - https://ncatlab.org/nlab/show/quantale

# III. Linear logic explained to a (theoretical) computer scientist

> Logic to computer science is like geometry to physics.

## III.1. The semantics of linear logic: coherent spaces

- **Xie**: TODO I do not understand this yet.

## III.2. Proof-nets: a classical natural deduction

- **Xie**: TODO I do not understand this yet.

  The author try to use proof-nets as (type theoretical) terms for linear logic.

## III.3. Normalization for proof-nets

TODO

## III.4. Normalization for proof-nets

TODO
