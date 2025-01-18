---
title: Why causation?
date: 2022-05-12
---

Why we summarise our knowledge about the world
in the form of _cause and effect relation_ (causation)?

For example, it is harder for us to understand:

- Probability theory.
- Quantum mechanics.

Because we inductively learned that
this form of knowledge is useful,
it helps us predicate the future,
and the main data of the induction
are the cases where **we are the cause**.

After my action, I can consistently predicate its results,
and this kind of events happen many many times a day.

For examples:

- In physics, we use the concept of "force" to understand movement,
  that is where we try to use a cause -- the force, to understand movement.

# [2022-06-01] About the relation between logic and causation

This note was first called "Why logic?",
and open with "Why we use logic to understand the world?",
but from "The Book of Why" and Judea Pearl's work,
I understand that logic is not about causation at all.

Thus this note is wrong.

This note should not be called "Why logic?" but "Why causation?".

If we think about inference rules in a logic system.

A judgment takes some data as arguments,
and a group of inference rules for a judgment
is a definition of the judgment as a relation
(thinking about Prolog or inductive datatype in dependent type system).

The relations are not about cause and effect at all.

- There is a Prolog book called "Clause and Effect",
  but not "Cause and Effect".

Relation has no direction, but when we use the idea of "bidirectional type checking"
to turn a type checking judgment to a function that does type checking,
we introduced a direction to the relation.

**Logic is not about, but programming is about causation**

This means when we model the world by programming,
when we write functions (or classes and their methods),
we are creating causal models.

The dependencies between reactive data is the arrows in causal diagrams.

# [2025-01-18] New understanding after linear logic

The note above is wrong again,
logic is about causation,
i.e. about the model that we used to understand the world.

This is very clear in linear logic,
where `A -o B` can be a summarization of how a machine works.
