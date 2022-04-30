---
title: Language, Proof and Logic
course:
- kind: Stanford open course
- link: https://lagunita.stanford.edu/courses/Philosophy/LPL-SP/SelfPaced/course
---

# Chapter 1: Atomic Sentences

- A formal language for FOL,
  with syntax difference between constant symbol -- `socrate`,
  and predicate symbol -- `Man`,
  and function symbol -- `father_of`.

  They are called "symbol",
  because we need a model to make them meaningful.

- Xie:
  FOL is an untyped language.

  A type-constructor in a typed language
  is of type `(a : A) -> type`
  it can be viewed as a predicate in FOL.

  A predicate in a typed language
  is of type `(a : A) -> bool-t`
  it also can be viewed as a predicate in FOL.

  A type-constructor give us an algorithm
  to generate all elements of the type-constructor,
  as long as the arguments of the type-constructor is of first-order.

  A predicate, in general, does not induces an algorithm
  to generate all elements for which the predicate is true.

- Xie:
  We can have type-constructor

  ``` js
  odd-t : (n : nat-t) -> type
  even-t : (n : nat-t) -> type
  ```

  But we do not have type-constructor

  ``` js
  prime-t : (n : nat-t) -> type
  ```

  The definition of `prime-t` require the unique-prime-factorization

  - https://en.wikipedia.org/wiki/Factorization
    https://en.wikipedia.org/wiki/Fundamental_theorem_of_arithmetic

- All FOL model have basic identical predicate.

  - Xie:
    Although identical predicate is present in all FOL models,
    but it is given for each specific model, not for FOL itself.

- Xie:
  For general model theory
  we need existing theory (always set theory)
  to assign truth value to all atomic sentences.

  - This assignment can be viewed as
    defining subjects and defining predicates.

- Xie:
  Every type in type theory can be translated to
  a predicate in a FOL model together with
  - generation of elements in the model which satisfy the predicate only
    (we can say "elements of the type"),
  - basic equivalent relation between elements of the type.

- Xie:
  Function symbol's meaning rely on model,
  and there are not function definition in FOL.

  Thus in FOL we can only handle functions by axioms about them.

# Chapter 2: The Logic of Atomic Sentences

- What does it mean when we say that sentence S
  is a logical consequence of sentence T?

  S is a logical consequence of T if
  whenever T is true, S must be true as well

  it is impossibile for T to be true and S false

  - Xie:
    left player can play in both S and T
    (two games are dependent on each other)
    whenever she loss in S, she have winning strategy to win in T.

- an argument is a chain of consequence

  an argument is valid if its consequences
  must follow from its premises

  an argument is sound (in some model) if it is valid
  and its premises are true

- In everyday life, we rarely construct
  long chains of inference
  but think, for example, of mathematics
  and related disciplines.
  In these situations we often build large theories
  by proving results, and then using
  these results in later proofs, and then using
  those results yet later.

  - Xie:
    this is where we need to make use the knowledge
    we learned from developing software

- Fitch notation
  https://en.wikipedia.org/wiki/Fitch_notation

  - example :

    ``` js
    #1 Cube (c)
    #2 c = b
    ------
    #3 Cube (b) -- =-elim (#1 #2)
    ```

- Xie:
  comparing to game semantics,
  inference rules are highly non-symmetric

- Xie:
  basic equivalent relation is part of every FOL model
  this means for a class to be used as a model of FOL
  it must have a basic equivalent relation

- Xie:
  substitution principle is closely related with
  unification in our implementation

- Xie:
  in our interpretation
  we need to give every inference rule
  a constructive meaning

  what is the meaning of `=-elim`?
  maybe :
  by `c = b`
  we can view `Cube (b)` as `Cube (c)`

  or we can view an element of `Cube (b)`
  as an element of `Cube (c)`

- Xie:
  models can be used to provide counterexamples of sentence

# Chapter 3: The Boolean Connectives

# Chapter 4: The Logic of Boolean Connectives

# Chapter 5: Methods of Proof for Boolean Logic

# Chapter 6: Formal Proofs and Boolean Logic

# Chapter 7: Conditionals

# Chapter 8: The Logic of Conditionals

- logical consequence
  if premises are true, the conclusion also must be true.

  - tautology consequence -- propositional logic
  - logical consequence -- predicate logic
  - analytical consequences -- with a specific model

- soundness of deductive system :
  provable -> logical consequence

  - Xie:
    the definition of logical consequence depends on model
    in the case of propositional logic
    the model is boolean algebra

  - Xie:
    if we define "true" as provable (constructable)
    we get soundness for free

- completeness of deductive system :
  logical consequence -> provable

# Chapter 9: Introduction to Quantification

- Xie:
  inference rules of propositional connectives
  are simple functions of its direct parts

  inference rules of predicative quantifiers
  are not functions of its direct parts

  to define inference rules of predicative quantifiers
  FOL uses the concept of "satisfiable"

  the concept of "satisfiable" dependents on model

  ``` js
  // All P's are Q's
  (x : P) -> Q (x)

  // Some P's are Q's
  ((x : P), Q (x))

  // No P's are Q's
  (x : P) -> not Q (x)
  not (x : P, Q (x))

  (x : P) -> (Q (x) -> absurd_t)
  (x : P, Q (x)) -> absurd_t

  // Some P's are not Q's
  (x : P, not Q (x))
  not ((x : P) -> Q (x))

  (x : P, (Q (x) -> absurd_t))
  ((x : P) -> Q (x)) -> absurd_t
  ```

# Chapter 10: The Logic of Quantifiers

- no general procedure to decide first-order validity

- contrapositive

  ``` js
  A -> B
  not B -> not A
  (B -> C) -> (A -> C)

  A -> B -> C
  ```

- De Morgan's laws For Quantifiers

  ``` js
  not ((x : P) -> Q (x))
  f : ((x : P) -> Q (x)) -> absurd_t
  f = (h) => g.right (h (g.left))

  ((x : P), not Q (x))
  g : ((x : P), (Q (x) -> absurd_t))
  g = (p, (q) => f ((_) => q))


  ((x : P) -> not Q (x))
  f : (x : P) -> Q (x) -> absurd_t

  not ((x : P), Q (x))
  g : (x : P, Q (x)) -> absurd_t
  ```

- distribution

  ``` js
  (x : N) -> P (x) and Q (x)
  ((x : N) -> P (x)) and ((x : N) -> Q (x))

  (x : N, P (x) or Q (x))
  ((x : N, P (x)) or (x : N, Q (x)))
  ```

# Chapter 11: Multiple Quantifiers

- Xie:
  is there a better way to express "exactly one"
  and in general to count distinct objects of a class

- Prenex Normal Form

  - Xie:
    we might can not be able to get Prenex Normal Form
    in a constructive way

- Xie:
  number of quantifiers measures
  the complexness of the game of logic

  just as the number of (non-curry) arrows measures
  the complexness of the game of type

# Chapter 12: Methods of Proof for Quantifiers

- Universal Elimination

  ``` js
  f : (x : N) -> P (x)
  t : N
  ---------------
  f (t) : P (t)
  ```

- Xie:
  note that "forall" is understood as "many and" (many product)
  but its inference rule is like that of arrow (exponential)

- Existential Introduction

  ``` js
  t : N
  x : P (t)
  ------------------
  (t, x) : (t : N, P (t))
  ```

- Xie:
  note that "exists" is understood as "many or" (many sum)
  but its inference rule is like that of and (product)

- Xie:
  we should use "x -> y" to denote exponential in arithmetic

- Xie:
  in type theory
  - "forall" (as many product) is modeled as
    dependent exponential (dependent arrow)
  - "exists" (as many sum) is modeled as dependent product

- example

  ``` js
  f : (x : M, Small (x)) -> Tet (x)
  g : (x : M, Tet (x)) -> Adjoins (x, d)
  h : (e : M, Small (e))
  ---------------------
  { e = h.left
    s = h.right
    t = f (e, s)
    ad = g (e, t)
    (e, t, ad)
  } : (x : M, Tet (x), Adjoins (x, d))
  ```

- Existential Elimination
  (this is understood as naming in the lecture)

  ``` js
  h : (t : N, P (t))
  ------------------
  h.left : N

  h : (t : N, P (t))
  ------------------
  h.right : P (h.left)
  ```

- Universal Introduction

  ``` js
  ------------------
  (c) => {
    // something of type P (c)
  } : (x : N) -> P (x)
  ```

  - Xie:
    this is like function definition in programming language

  the lecture also intro
  "general conditional proof" for dependent arrow

- Xie:
  this chapter showed that
  the symmetry and structures of logic are not expressed in
  the theory of FOL quantifiers

- there is no complete axiomatization of arithmetic
  can be expressed in FOL

  https://en.wikipedia.org/wiki/G%C3%B6del%27s_incompleteness_theorems

# Chapter 13: Formal Proofs and Quantifiers

# Chapter 14: More about Quantification

- at least n
  at most n
  exactly n

- Xie:
  using bijection would be more composable

- Russel's analysis of "the"x

- Xie:
  general quantifier theory

  | expression       | named quantifier expression |
  |------------------|-----------------------------|
  | (x : N, P (x))   | exists (x : N) P (x)        |
  | (x : N) -> P (x) | forall (x : N) P (x)        |
  | (x) => f (x)     | lambda (x : N) f (x)        |
  | exists unique    | unique (x : N) P (x)        |
  | in general       | <quantifier> (x : N) P (x)  |

# Summary

- modal logic -- about possible worlds

- linear logic -- about the using up resource (premises)

- the information we based on to do inference are sentences,
  this might be generalized to any information (any object).
