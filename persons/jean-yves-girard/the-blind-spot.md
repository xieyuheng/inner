---
title: The blind spot
subtitle: Lectures on logic
author: Jean-Yves Girard
year: 2011
---

# 1 Existence vs. essence

A dichotomy:

- **Essentialists**. Those who think that everything is already there,
  that one can but repeat archetypes.

  In logic, they believe in "inverted foundations":
  a system can be explained by a deeper "meta-system";
  which in turn can be explained by a meta-meta-system...
  and this never ends.

  Examples: Thomas Aquinus, set-theory, Cantor, Dedekind, etc.

- **Existentialists**. Those who do not find "deep" the fact of
  defining truth as supposedly done by Tarski
  "A ∧ B is true when A is true and B is true".

  The best possible law has value insofar as one can justify it,
  i.e., show the effect of non-observance.

  The existentialist world is a lawless world, in which
  contestation is total and constant.

  - **Xie**: Hegelian dialectic.

  The opposition essence/existence is at the very heart of typing (Section 6.F).

## 1.2 Essentialist and existentialist projects

### 1.2.1 Set-theory

### 1.2.2 The hilbertian project

A system must be consistent.

It cannot prove the absurdity, i.e., a formula and its negation.

This is the origin of the idea of a consistency proof.

### 1.2.3 Brouwer's project

Brouwer's intuitionism, claims the primacy of intuition over the language.

Brouwer did not reject infinity (contrary to hilbertian finitism
which only sees it as a façon de parler [a manner of speaking]),
but he refused the most Thomist, "actual", aspects of infinity;
especially set-theory and the idea that one could
define a function of a real variable pointwise, value by value.

Some principles, valid in the finite domain,
cease to work in the infinite case,
typically the excluded middle.

The usual justification of the tertium non datur is that
a formula A has a truth value (is true or false).

However, while one can compute a truth value in the finite case,
no algorithm can cope with the verification of infinitely many steps:
this is why Brouwer called it in question in the infinite case.

This approach is very modern, in the sense that

> truth does not exist independently of the means, the protocols, of verification.

- **Xie**: Per Martin-Löf's notion of judgements.

### Two (imaginary) confrontations between Brouwer and Hilbert

- **Modus Ponens**: for Hilbert, logical consequence must essentially
  avoid enormous mistakes, e.g., proving the absurdity;
  this bestows a second-class citizenship to most logical formulas:
  they don't make sense by themselves, they should not cause mischief,
  that's all.

  A contrario [on contrary], for intuitionists,
  Modus Ponens is more than a legal advisor,
  it is a door open on a new world:
  the application of a function (A -> B) to an argument (A)
  yielding a result (B).

  Proofs are no longer those sequences of symbols
  created by a crazy bureaucrat:
  they become functions, morphisms.

  - **Xie**: "morphisms" means to use structures in category theory
    as denotational semantics of logic.

    This is kind of study is called
    [categorical logic](https://en.wikipedia.org/wiki/Categorical_logic).

- **Tertium non datur**: Hilbert accepts the excluded middle;
  not that he believes in a preexisting truth value for A,
  but because it simplifies matters: "A property which means nothing,
  but which costs nothing as well".

  Technically speaking, this argument is viable.

  Indeed, one knows (since 1932: Gödel, one more result from him, Section 4.1.3)
  that classical logic can be faithfully translated in intuitionistic logic:
  it suffices to put double negations "everywhere".

  In particular, the excluded middle, once translated as ~~(A ∨ ~A)
  becomes intuitionistically provable and it is not more "risky" to add it.

  However, one can answer back that it is not because
  one is warranted impunity (consistency)
  that one is entitled to commit a crime
  (enunciate an unjustified principle).

## 1.3 Gödel and after

Surely Gentzen committed the irrecoverable mistake of showing that a
proof establishes a truth, but what he actually produced was not a
truism one could be afraid of (axioms are true, rules preserve truth
and so...).

He gave, in a hesitating terminology,
an interactive definition of truth,
the proof appearing as a winning strategy
in a sort of "game of truth".

Yes, this proof is even less convincing than the second one,
which already convinced only the true believers; but, who cares?
This was the first interactive interpretation of logic.

In order to carry out his consistency Program,
Gentzen had to create tools,
essentially the "calculus of sequents".

This calculus, a weapon created for a dubious battle,
remains one of the main logical achievements of the XXth century.

We shall study it in detail,
starting with the original formulation
and introducing more and more sophisticated avatars:
natural deduction, proof-nets, geometry of interaction, etc.

At the end of the book, Gentzen's theorem, "cut-elimination",
will eventually be rephrased in terms of ... operator algebras.

## 1.A Essentialism vs. platonism

TODO

# 2 Incompleteness

# 3 Classical sequents: LK

# 4 Intuitionistic logic: LJ, NJ

# 5 Functional interpretations

# 6 System F

## 6.F Essence, existence and typing

### 6.F.1 Locative phenomena.

There are indeed two readings of polymorphism,
depending if one starts with essence or existence.

- **Essence**. In this reading, the type is primitive, one constructs it,
  then one takes care of the objects.

  There is no real polymorphism,
  there is only one form (essence) for a given object.

  This is the viewpoint followed
  by the category-theoretic interpretations of logic.

- **Existence**. But one can instead contend that
  objects are anterior to their type, seen as an essence.

  This is the viewpoint of subtyping, this is also the viewpoint of ludics:
  an object may have several types, be representative of several essences.

### 6.F.2 Typing as essence

The difference between "pure" and typed objects
is the distinction between things as they are
and things as they should be.

This is also the difference between handicraft and industry.

- **Xie**: The latter means specification and interface.

### 6.F.3 Typing and computation

The activity consisting in manufacturing unique,
irreplaceable -- unless by themselves -- objects,
is called handicraft and does not stand mediocrity;
in opposition to industry which contents itself
with a not quite exciting contractual minimum,
in the style of Mc Donald's.

Industry is obviously (and unfortunately) right.

Witness computer science:
one does not want programs written by little wizards,
one wants structures, modules, that one can
unscrew, modify, reassemble, etc.

### 6.F.4 Reducibility and essence.

TODO

# 7 The category-theoretic interpretation

# 8 Coherent spaces

# 9 Linear logic

# 10 Perfection vs. imperfection

# 11 Proof-nets

# 12 A hypothesis: polarisation

# 13 Designs and behaviours

# 14 Ludics: the reconstruction

# 15 Orthodox exponentials

# 16 Heterodox exponentials

# 17 Quantum coherent spaces

# 18 Nets and duality

# 19 The feedback equation

# 20 Babel Tower vs. Great Wall

# 21 Finite GoI

# Envoi. The phantom of transparency
