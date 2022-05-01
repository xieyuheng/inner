---
title: Linear Logic
author: Jean-Yves Girard
years: [1986, 1996]
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
  - Additive -- handles synchronization

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

- **Xie**: The **disjunction property** is satisfied by a theory if,
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

TODO
