---
title: The principle of type theory
date: 2021-08-27
---

# The principle

The principle of type theory is:

> Let's study **terms** and **types** together.

# Before type theory

In programming, we do *not* write types, we only write terms.

In logic, we do *not* write terms, we only write types
-- propositions and their inference rules.

# After type theory

In programming, we can interpret a type as a collection of values that terms might evaluate to.
Here, we care about **evaluation**.

In logic, we can interpret a term as a recording of steps of applications of inference rules (i.e. proof).
Here, we care about **inferencing** or **deduction**.

- Before type theory, logic does not have terms.

  For example, Prolog does not use named clauses.

  - If we use named clause, we can use the name to express choices, and write proof by hand.

    If we do not have named clause, we can use order of clauses and index,
    but the resulting proof might be hard to understand.

  In Prolog, we only care about searching for what can be inferenced,
  but do not care about writing down how a conclusion is inferenced,
  and let the machine check our proof.
