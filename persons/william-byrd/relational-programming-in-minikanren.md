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

<question>
  What is a goal?

  <answer>
    A goal can be pursued, to pursue a goal
    we take the goal and a solution,
    and return a list of solutions.
  </answer>
</question>

# 4 Implementation II: Optimizing walk

## 4.1 Why walk is Expensive

<question>
  Why walk is expensive?

  <answer>
    In the worst case, walk is quadratic in the length of the substitution.

    For example, lookup v in ((y . z) (x . y) (w . x) (v . w)).
  </answer>
</question>

<question>
  How do Prolog implementations based on
  the Warren Abstract Machine (Aït-Kaci 1991)
  represent variable?

  <answer>
    They do not use explicit substitutions to represent variable associations.
    Instead, they represent each variable as a mutable box,
    and side-effect the box during unification.

    This makes variable lookup extremely fast,
    but requires remembering and undoing these side-effects during backtracking.

    In addition, this simple model assumes a depth-first search strategy,
    whereas our purely functional representation can be used
    with interleaving search without modification.
  </answer>
</question>

<question>
  When using triangular substitutions (or even idempotent substitutions),
  the entire substitution must be examined to determine that a variable in unassociated.

  Can you think of a trick to avoid this?

  <answer>
    The trick is called "Birth Records".

    To avoid examining the entire substitution
    when walking an unassociated variable,
    we will add a birth record to the substitution
    whenever we introduce a variable.

    For example, to run the goal (exist (x y) (≡ 5 x))
    we would add the birth records (x . x) and (y . y)
    to the current substitution,
    then run (≡ 5 x) in the extended substitution.

    Unifying x with 5 requires us to walk x:
    when we do so, we immediately encounter the birth record (x . x),
    indicating x is unassociated.

    Unification then succeeds,
    adding the association (x . 5) to the substitution
    to produce ((x . 5) (x . x) (y . y) ...).
  </answer>
</question>

<question>
  Is the "Birth Records" trick really good?

  <answer>
    Maybe not.

    Because in the worst case our situation has not improved:
    if a variable is introduced at the beginning of a program,
    but is not unified until the end of the program,
    the birth record will occur at the very end of the substitution,
    and lookup will still take linear time.

    Indeed, the situation is even worse, since the birth records
    more than double the length of the substitution that must be walked!

    Fortunately, in most real-world programs variables are unified
    shortly after they have been introduced.

    This locality of reference means that,
    in practice, birth records significantly reduce
    the cost of walking unassociated variables.
  </answer>
</question>

## 4.3 Eliminating assq and Checking the rhs

<question>
  What is an important property of
  the triangular substitutions produced by unify?
  which can be used to optimize
  walk of unassociated variable.

  <answer>
    In the substitution ((x . y) . s),
    the variable y will never appear
    in the left-hand-side (lhs)
    (appear as key) of any binding in s.

    Because if y occurred as key,
    it will be walked.

    This can be used to optimize
    walk of unassociated variable,
    because if y occurred as value during walk,
    we already know it is unassociated.

    Note that the "Birth Records" trick
    breaks this property in a trivial way.
  </answer>
</question>

## 4.4 Storing the Substitution in the Variable

<question>
  What is the trick of "Birth Substitution"?

  <answer>
    When combining the birth records optimization
    with checking for the walked variable
    in the rhs of each association,
    we wish to avoid polluting the substitution with birth records,
    which not only lengthen the substitution
    but also violate important invariants of our substitution representation.

    Instead of adding birth records to the substitution,
    we can add a "birth substitution" to each variable
    by storing the current substitution in the variable when it is created.
  </answer>
</question>

<question>
  What we should be careful about when changing the implementation of walk?

  <answer>
    We should be careful about the effect of the change on reification.

    Because reification uses deepWalk to create the renaming substitution
    (or say substitution with reified variables).
  </answer>
</question>
