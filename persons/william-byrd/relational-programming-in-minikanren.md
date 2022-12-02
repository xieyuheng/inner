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
  What is the meaning of "triangular" and "idempotent"
  in "triangular rather than idempotent substitutions"?

  <answer>
    Example of triangular substitution is `y = 5; x = y`,
    as idempotent substitution it would be `y = 5; x = 5`

    In an idempotent substitution,
    a variable that appears as key of an key-value entry
    never appears as value.
  </answer>
</question>

<question>
  What are the advantages of using triangular substitution?

  <answer>
    One advantage of triangular substitutions is that
    they can be easily extended using cons,
    without side-effecting or rebuilding the substitution.

    This lack of side-effects permits sharing of substitutions,
    while substitution extension remains a constant-time operation.

    This sharing, in turn, gives us backtracking for free
    -- we just "forget" irrelevant associations
    by using an older version of the substitution,
    which is always a suffix of the current substitution.
  </answer>
</question>

<question>
  What are the disadvantages of using triangular substitution?

  <answer>
    The major disadvantage is that variable lookup
    is both more complicated and more expensive
    than with idempotent substitutions.

    We need to use `walk` function,
    and `walk` can diverge if used on a substitution containing a circularity,
    for example, in `x = x` and `x = y; y = x`.

    In idempotent substitution, `walk` is the same as `deepWalk`.
  </answer>
</question>

<question>
  Why we need occur-check?

  <answer>
    Because `walk` can diverge if used on a substitution containing a circularity,
    for example, in `x = x` and `x = y; y = x`.

    `walk` will not diverge in the case of `x = [x]`,
    but in this case simple implement of `reify` will diverge.
  </answer>
</question>

<question>
  Why occur-check is expensive?

  <answer>
    Because it must perform a complete tree walk on its second argument.
  </answer>
</question>

<question>
  Can we omit occur-check since it is expensive?

  <answer>
    Apt and Pellegrini (1992) point out that,
    in practice, omission of the occurs check is usually not a problem.
    However, the type inferencer presented in section 9.3
    requires sound unification to prevent self-application from typechecking.

    <code>
    (check () (lambda (f) (f f)) (-> A B)) [fn] {
      (check ([f A]) (f f) B) [ap] {
        (check ([f A]) f (-> C B)) [var]
        (check ([f A]) f B) [var]
        // To not unify B with (-> C B)
        // we need occur-check.
        // How about f: (-> C (-> C (-> ...)))
      }
    }
    </code>
  </answer>
</question>

# 1 Introduction

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

# 3 Implementation I: Core miniKanren

## 3.1 Variables, Substitutions, and Unification

<question>
  What is a substitution?

  <answer>
    A substitution s is a mapping between logic variables and values (also called terms).
  </answer>
</question>

## 3.2 Reification

<question>
  What is reification?

  <answer>
    Reification is the process of
    turning a miniKanren term into a Scheme value
    that does not contain logic variables.
  </answer>
</question>

## 3.3 Goals and Goal Constructors

TODO

## 3.4 Impure Operators

TODO
