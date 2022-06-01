---
title: Why logic?
date: 2022-05-12
---

Why we use logic to understand the world?

The core of logic is causation,
i.e. **cause and effect relation**.

So we ask, why we summarise our knowledge about the world
in the form of causation?

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

# Errata

## [2022-06-01] About the relation between logic and causation

By "The Book of Why" and Judea Pearl's work,
I understand that logic is not about causation at all.

Thus this note is wrong.

This note should not be called "Why logic?" but "Why we understand by causation?".

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
