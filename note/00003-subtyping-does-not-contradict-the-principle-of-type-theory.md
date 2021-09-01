---
title: Subtyping does not contradict the principle of type theory
date: 2021-09-02
---

Subtyping, **when expressed by record type**,
does not contradict the [principle of type theory][00001].

The concept of *subtype* come from the concept of *subset* of set theory.

In set theory, the definition of *subset*, *union* and *intersection*,
all depend on *membership relation* between elements and sets,
while type theory require us to define them *purely syntacticly*
(without membership relation).

When *subtype*, *union type*, and *intersection type*,
are firstly defined for *record type*
(via relation between records' fields),
and then structurely extended to other types,
the definition is indeed purely syntactic.

Thus, subtyping, when expressed by record type,
does not contradict the principle of type theory.


[00001]: 00001-the-principle-of-type-theory.md
