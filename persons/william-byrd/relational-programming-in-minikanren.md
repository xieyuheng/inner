---
title: Relational programming in minikanren
subtitle: Techniques applications and implementations
author: William Byrd
year: 2009
---

# Abstract

<question>
  What is the meaning of "nominal" in "nominal unification"?

  <answer>
    TODO
  </answer>
</question>

<question>
  What is the meaning of "triangular" and  "idempotent"
  in "triangular rather than idempotent substitutions"?

  <answer>
    TODO
  </answer>
</question>

# Introduction

<question>
  How nominal unification can be used to support
  variable binding and scope when implementing
  a relational interpreter for a subset of Scheme?

  <answer>
    TODO
  </answer>
</question>

<question>
  How tabling can be used to improve termination behavior?

  <answer>
    Tabling is a form of memoization:
    the answers produced by a tabled relation
    are "remembered" (that is, stored in a table),
    so that subsequent calls to the relation
    can avoid recomputing the answers.
  </answer>
</question>

<project>
  Use logic programming to implement an
  extremely flexible lean tableau theorem prover.
</project>

<lookback>
  The "walk"-based algorithm for variable lookup in triangular substitutions.

  <generalization>
    Can this idea be used in other graph processing problems?

    For examples:

    - implementing cell-complex
    - implementing hypergraph

  </generalization>
</lookback>

## 1.2 Structure of this Dissertation

With the exception of two early chapters (Chapters 2 and 5),
each technical chapter in this dissertation is divided into
one of three categories: techniques, applications, or implementations.

- Technique chapters describe language features and idioms for writing relations,
  such as disequality constraints (Chapter 7) and nominal logic (Chapter 9).

- Application chapters demonstrate how to write interesting,
  non-trivial relations in miniKanren;
  these applications demonstrate the use of
  many of the language forms and idioms presented
  in the technique chapters.

- Implementation chapters show
  how to implement the language extensions
  presented in the technique chapters.

## 1.3 Relational Programming

<question>
  What is relational programming?

  <answer>
    Relational programming is a discipline of logic programming
    in which every goal is written as a "pure" relation.
  </answer>
</question>

<question>
  What is a "pure" relation?

  <answer>
    A relation is pure, if it produces meaningful answers,
    even when all of its arguments are unbound logic variables.
  </answer>
</question>

<question>
  What is a ground term?

  <answer>
    A term is ground if it does not contain unassociated logic variables.
  </answer>
</question>

<question>
  What are Prolog's operators that dependent upon the groundness of terms?

  <answer>
    cut (!), var/1, copy_term/2.
  </answer>
</question>

<question>
  Why operators that dependent upon the groundness of terms
  inhibit relational programming?

  <answer>
    Because to be a pure relation is to produces meaningful answers,
    even when all of its arguments are unbound logic variables.

    Thus a pure relation must not dependent upon the groundness of terms.
  </answer>
</question>

<question>
  What is the most critical property of relational programming?

  <answer>
    The most critical property is relations terminate whenever possible.
  </answer>
</question>

<question>
  What is mode restrictions of a relation in Prolog?

  <answer>
    Some position of a relation is viewed as input,
    input must be fully ground,
    and we can not pass fresh variable into it.
  </answer>
</question>

<question>
  What is the finite failure property of a goal?

  <answer>
    If a goal is asked to produce an answer,
    yet no answer exists, that goal should fail
    in a finite amount of time.
  </answer>
</question>

<question>
  Why we can not ensure the finite failure property for all goals?

  <answer>
    Because Gödel and Turing showed that
    it is impossible to guarantee termination
    for all goals we might wish to write.
  </answer>
</question>

<question>
  What are some techniques
  to achieve finite failure property
  in our relational programming?

  <answer>
    The use of clever data encoding,
    nominal unification, tabling,
    and the derivation of bounds on the maximum size of terms
    allows a careful relational programmer
    to write surprisingly sophisticated programs
    that have finite failure property.
  </answer>
</question>
