# Inductive Families

------
- Author: Peter Dybjer
- Date: 1994
------

# 1 Introduction

When extending Martin-Löf's type theory,
instead of using a general purpose construction which is part of the theory,
we giving external criteria for correct extensions of the theory.

The main point here is that **Martin-Löf's type theory
is a theory of inductive definitions formulated in natural deduction**.
Each set former (logical constant) is defined inductively by its introduction rules.
The elimination rule expresses a principle of definition by recursion (proof by induction).
Equality rules express how these definitions are eliminated (proofs are normalised).

- **[Xie]** Elimination rule is about how to use a piece of information,
  when the information is introduced inductively, so will the elimination rule.

First we specify what it means to be a correct definition of a set former
by giving formal criteria for the formation and introduction rules.

- **[Xie]** The term "set former" is called "type constructor" by some authors.

Then we show how such a definition determines the elimination and equality rules
by a so called **inversion principle**. We also give an alternative formulation
where recursive definitions are presented schematically.

The scheme is for **monomorphic, intensional type theory**
and covers **strictly positive, iterated, generalised induction**.
This does not cover all forms of intuitionistically meaningful induction,
and thus not all ways of forming sets in Martin-Löf's type theory.
(An example is the definition of a universe in Tarski's style.)

# 2 The Theory of Logical Types

The theory is formulated in the style of Martin-Loef's type theory
with four forms of judgements:

``` js
P type
p : P
p == q : P
P == Q
```

Rules of type formation:

``` js
--------
set type

A : set
--------
A type
```

- **[Xie]**
  The rule above does not make the property that
  `one object only has one type` invalid
  because the judgement `A type` is not `A : type`.

``` js
A type
{ a : A |- T[a] type }
-----------------------
(a : A) -> T[a] type
```

The rules of object formation:

``` js
{ a : A |- p[a] : T[a] }
-----------------------------
(a) => p[a] : (a : A) -> T[a]

p : (a : A) -> T[a]
q : A
--------------------
p(q) : T[q]
```

- **[Xie]**
  In the paper,
  `(a) => p[a]` is written as `(a)p[a]`,
  `(a : A) -> T[a]` is written as `(a : A)T[a]`,
  while, I distinguish them by `=>` vs `->`,
  instead of by the existence of type annotation.

The equality rules are typed beta- and eta-conversion:

``` js
q : A
{ a : A |- p(q) : T[q] }
----------------------------------------
{ (a) => p(a) } (q) == p(q) : T[q]

p : (a : A) -> T[a]
------------------------------------
p == (a) => p(a) : (a : A) -> T[a]
```

Moreover, equality is an equivalence relation
and we may everywhere substitute equals for equals:

``` js
// Reflective

A type
---------
A == A

p : A
---------
p == p : A

// Symmetric

A == A1
-------
A1 == A

p == p1 : A
------------
p1 == p : A

// Transitive

A == A1
A1 == A2
--------
A == A2

p == p1 : A
p1 == p2 : A
--------
p == p2 : A

// Substitution in type

p : A
A == A1
-------
p : A1

p == p1 : A
A == A1
--------
p == p1 : A1

// Equivalence formation

A == A1 : set
-------------
A == A1

// Substitution in function type

A == A1
{ a : A |- B[a] == B1[a] }
--------------------------------------
(a : A) -> B[a] == (a : A1) -> B1[a]

// Substitution in expression

p == p1 : (a : A) -> B[a]
q == q1 : A
-----------------------
p(q) == p1(q1) : B[q]
```

A **theory** in the framework sense is specified by
giving a finite sequence of typings of fresh constants `c : A`
and a finite sequence of equalities between terms `p == q : A`.

Formation, introduction, and elimination rules for the set formers
are given by typing fresh constants.
For example, the universe introduction rule above would be given by

``` js
universe_introduction : (a : U) -> (b : (x : T(a)) -> U) -> U
```

# 3 A Scheme for Inductive Definitions
