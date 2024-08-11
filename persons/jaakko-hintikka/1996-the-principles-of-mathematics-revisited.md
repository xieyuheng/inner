---
title: the principles of mathematics revisited
author: jaakko hintikka
year: 1996
---

# 1 The Functions of Logic and the Problem of Truth Definition

- Uninterpreted nonlogical axiom systems might be thought of as
  pertaining to certain structures as such,
  while the corresponding interpreted systems
  deal with the actual instantiations of these structures.

  - interpreted:
    thermodynamics
    geometry

  - uninterpreted:
    group theory
    field theory
    lattice theory

- x -
  this is the difference between class and object

  but is Hilbert's "Foundations of Geometry" a class or an object?
  - it is said to be a nonlogical axiom system by the author.

  maybe we should think of it as being
  directly constructing an object without abstracting it to a class

- two functions of logic

  - descriptive function
    -- an unambiguous language
    -- model theory or logical semantics

  - deductive function
    -- fixation of beliefs
    -- proof theory

- x -
  Tarski's model theory dependents on set theory
  how about game semantics?

- x -
  what is an interpretation?
  - an object instantiating a class?
  - or an morphism from one class to another?

- critics of Tarski's truth-conditional semantics
  https://plato.stanford.edu/entries/tarski-truth/
  in favor of verificationist semantics
  https://en.wikipedia.org/wiki/Verificationism

# 2 The Game of Logic

- x -
  the choice of falsifier lies in "forall"
  which is a dependent arrow type in our language

- game rule of FOL

  - with a model
    this means
    atomic sentences are interpreted to true or false

  - with quantifier with typed domains

  - instead of explicit semantics mapping
    I simply view a sentence as a game

  - L is verifier, R is falsifier

## or

```cicada
S1 or S2

L_choices (S1 or S2) = [
  "first of or",
  "second of or",
]

L_move ("first of or", S1 or S2) = S1
L_move ("second of or", S1 or S2) = S2
```

## and

```cicada
S1 and S2

R_choices (S1 and S2) = [
  "first of and",
  "second of and",
]

R_move ("first of and", S1 and S2) = S1
R_move ("second of and", S1 and S2) = S2
```

## exists

```cicada
exists (x : N) S (x)

L_choices (exists (x : N) S (x)) = [
  // list of all elements of N
]

L_move (b, exists (x : N) S (x)) = S (b)
```

## forall

```cicada
forall (x : N) S (x)

R_choices (forall (x : N) S (x)) = [
  // list of all elements of N
]

R_move (b, forall (x : N) S (x)) = S (b)
```

## not

not S is a new game
S -- where the role of verifier and falsifier are switched

- if we were to allow interpreting negation like this
  it means a sentences or a type must be modeled as a game
  instead of a position of a game
  for we a position does not contain
  the information about players

- we might also add a sign to position
  and use this sign and the game type
  to decide which player is to make move

## atomic sentence

A is true -- verifier win
A is false -- falsifier win

# object introduced by falsifier can be used by the verifier

- how should we understand the following:

  in game semantics of FOL
  once the game moves beyond a "forall"
  substitution happens to the succedent type

  it is also the case in our language
  but it happens in the type

  but what happens in the function body
  is different from FOL

  in the function body
  once the falsifier made his choose on a "forall"
  this is recorded by an object of the antecedent type
  and the object can be used by the verifier
  to construct a return value of the succedent type

# falsifier indeed has meaningful choices

- it might be clear that
  falsifier indeed has meaningful choices
  in conjunctions

  if we write all "exists" explicitly
  hence enforcing an order on how the game ought to be played

  - which means before the argument-subgame
    in "exists" is played out
    we can not play the body-subgame of "exists"

  in this view, in the following game
  there will no longer be any seemingly contradictory

  ```cicada
  exists (x : N) even_t (x) and odd_t (x)
  ```

  it means, to view a conjunctions as a game,
  further analysis of the conjunction,
  by means of quantifiers like "exists", is needed

  ```cicada
  conj {
    x : N
    even : even_t (x)
    odd : odd_t (x)
  }
  ```

- x -
  it seems game semantics is specially applicable
  for defining the semantics of quantifiers

# about logic variables

- a logic variable is an implicit argument

- is it true that
  every implicit argument can be made explicit?

  - even in the case of antecedent subgame of arrow?

# about truth

- x -
  the author worries about that
  game semantics did not capture the concept of truth

  this can be released by viewing "truth" as "to be sure"
  i.e. winning strategy

  a player is sure about herself when playing a game
  if she has a winning strategy

# about constructive-ness

- if game semantics can handle negation
  is game semantics constructive?
  what is constructive-ness?

- or why in game semantics,
  we can simply handle negation by switching players?

  maybe this do not hurt constructive-ness
  because if falsifier indeed has a winning strategy
  it will can be viewed a function of type (P) -> void_t

- also note that
  excluded middle does not hold in game semantics
  because not all game has winning strategy
  for one of its player

- x -
  example of combinatorial games
  that do not have winning strategy for one of its player

# fool's paradise

- ordinary first-order logic is a fool's paradise in that
  it offers a poor and indeed misleading sample of
  the variety of things that can happen in logic in general.

# 3 Frege's Fallacy Foiled: Independence-Friendly Logic
