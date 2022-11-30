---
title: Relational programming in minikanren
subtitle: Techniques applications and implementations
author: Will Byrd
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
    TODO
  </answer>
</question>

<project>
  Use logic programming to implement an
  extremely flexible lean tableau theorem prover.
</project>

<lookback>
  The "walk"-based algorithm for variable lookup in triangular substitutions.   The "walk"-based algorithm for variable lookup in triangular substitutions.

  <generalization>
    Can this idea be used in other graph processing problems?

    For examples:

    - implementing cell-complex
    - implementing hypergraph

  </generalization>
</lookback>

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

