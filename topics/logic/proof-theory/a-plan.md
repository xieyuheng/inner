---
title: a plan
---

# TODO

one language can use different type systems,
just as one proof style can be applied to different logics.

classical logic and logic programming

how different proof styles are correspond to each other?

# plan

- in sequent calculus style, I will show:

  1. the proof theory of intuitionistic logic
  2. why class logic is not constructive
  3. how to restrict semantics to get linear logic

- no inductively defined type in this version.

- no conj, because we have data stack.

- no duality,
  because the dogma of duality
  is in conflict with the dogma of constructivism.

- we _do_ need disj,
  but literal disj is only used for types, not for values,
  because literal disj of values
  will introduce the concept of non-deterministic.

- (+) is a type-constructor,
  and hypothetical object of this type
  will be used by (case)

- objects of disj type can be returned,
  only because of different types are returned
  in different branches of a (case)

- this is also true for arrow
  we use literal arrow for types,
  but not for values.

- (->) is a type-constructor,
  and hypothetical object of this type
  will be used by 'apply' and 'compose'
