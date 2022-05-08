---
title: "Linear Logic: Its syntax and semantics"
subtitle: Preface of "Advances in Linear Logic", proceedings of "Linear Logic Workshop", 1993
author: Jean-Yves Girard
year: 1995
---

# 1 THE SYNTAX OF LINEAR LOGIC

## 1.1 The connectives of linear logic

When designing new logic like linear logic,
we should introduces new connectives
instead of modify classical or intuitionistic connectives.

### 1.1.1 Exponentials: actions vs situations

To view causality as reaction (physical or chemical).

Use exponentials `!` and `?` to mark stable truth,
i.e. the absence of any reaction.

Intuitionistic implication

```
A => B
```

can be decomposed into exponential and linear implication:

```
!A −o B
```

### 1.1.2 The two conjunctions

```plaintext
A = to spend $1,
B = to get a pack of Camels,
C = to get a pack of Marlboro.
```

- **Xie**: Here, each proposition is viewed as an action, performed by an agent.

  Actions do side effects on the agent and its environment.

  `A ⅋ B` can be viewed as perform two actions one by one,
  thus called "par", which is French word for "through".

  `A −o B` is `~A ⅋ B`, with the definitions above, it means,
  I spend $1 then you give me a pack of Camels.

  - Read in this way, `⅋` is symmetric, because it does not matter,
    whether I give money first or get Camels first.

### 1.1.3 Linear negation

TODO

### 1.1.4 States and transitions

The familiar notion of

```
theory = classical logic + axioms
```

should be replaced by:

```
theory = linear logic + axioms + current state
```

We can use `!` for the axioms.

- **Xie**: The "current state" part means that
  proved propositions in the context can be used and erased.

  For example, commutative algebra can be updated into non-commutative algebra.

### 1.1.5 The expressive power of linear logic

The introduction of new connectives
is the key to a more manageable way of formalizing.

### 1.1.6 A Far West: non-commutative linear logic

- **Xie**: Stack-based semantics are mentioned here.

  Lambek's syntactic calculus is also mentioned.

  - In syntax the order matters.

  Using explicit modalities to express structural rules，
  increase the expressive power of the logic.

  If we ever need to model something where the order matters,
  we might try those logics.

## 1.2 Linear sequent calculus

### 1.2.1 Structural rules

A sequent is an expression:

```
A1, ..., An |- B1, ..., Bm
```

which means

> A1 and ... and An imply B1 or ... or Bm.

The actual meaning of the words "and", "imply", "or" in a sequent,
is wholly in the structural group and it is not too excessive to say that
a logic is essentially a set of structural rules!

- **Xie**: Linear logic opens the gate to substructural logics,
  like opening the gate to Non-Euclidean geometries.
  Many substructural logics seem not useful for now,
  but maybe in the future they will be useful.

### 1.2.2 Linear sequent calculus

`Γ |- Δ` is the same as `|- ~Γ, Δ`.

**Identity / Negation**

```
--------- identity
|- A, ~A
```

```
|- Γ, A
|- ~A, Δ
--------- cut
|- Γ, Δ
```

Or two-sided version:

```
-------- identity
A |- A
```

```
Γ |- Δ, A
A, Λ |- Π
------------- cut
Γ, Λ |- Δ, Π
```

Similarly the following rules might be easier to understand in two-sided version.

**Structure**

```
|- Γ
------ exchange (Γ' is permutation of Γ)
|- Γ'
```

**Logic**

```
----- one
|- 1
```

```
|- Γ
-------- false
|- Γ, ⊥
```

Note that, we only need intro rule for `⊗`,
the only elim rule is the cut rule.

Cut rule and forming hypotheses
captures the concept of causality.

```
|- Γ, A
|- B, Δ
---------------- times
|- Γ, A ⊗ B, Δ
```

```
|- Γ, A, B
------------ par
|- Γ, A ⅋ B
```

In a sequent of linear logic,

- comma on the left means `⊗`;
- comma on the right means `⅋`;
- `|-` in the middle means `-o`.

```
-------- true
|- Γ, ⊤
```

No rule for zero.

```
|- Γ, A
|- Γ, B
------------ with
|- Γ, A & B
```

```
|- Γ, A
------------- left plus
|- Γ, A ⊕ B
```

```
|- Γ, B
------------- right plus
|- Γ, A ⊕ B
```

```
|- ?Γ, A
---------- of course
|- ?Γ, !A
```

```
|- Γ
---------- weakening
|- Γ, ?A
```

```
|- Γ, A
---------- dereliction
|- Γ, ?A
```

```
|- Γ, ?A, ?A
------------- contraction
|- Γ, ?A
```

```
|- Γ, A
------------- for all (x is not free in Γ)
|- Γ, ∀x A
```

```
|- Γ, A[t/x]
------------- there is
|- Γ, ∃x A
```

`∀x A` is like many `&`s,
and `∃x A` is like many `⊕`s,
i.e. they are explained by additive connectives.

## 1.3 Proof-nets

### 1.3.1 The determinism

Only when a (sequent calculus style) formal system
has cut-elimination theorem (the Hauptsatz),
we view it as a logic.

Linear logic has cut-elimination theorem.

There is an algorithm transforming
any proof of a sequent in linear logic
into a cut-free proof of the same sequent.

- **Xie**: TODO I do not understand Gentzen's proof of
  the Hauptsatz for intuitionistic logic yet.

  Note that, the Hauptsatz is the same as
  normalization in lambda-calculus.

### 1.3.2 Limitations of natural deduction

Let us assume that we want to use natural deduction to deal with proofs in
linear logic, then we run into problems:

Natural deduction is not equipped to deal with classical symmetry:
several hypotheses and one (distinguished) conclusion.

To cope with symmetrical systems
one should be able to accept several conclusions at once.

But then one immediately loses the tree-like structure of natural deductions,
with its obvious advantage: a well-determined last rule.

- **Xie**: In programming language design,
  we learned to introduce special syntax
  for binding multiple return values.

  Thus, use natural deduction for linear logic,
  we simply need to handle multiple return values,
  and limit the use of variable reference to once.

### 1.3.3 The identity links

To overcome the problems of natural deduction,
in the context of the multiplicative fragment of linear logic,
i.e. the only connectives `⊗` and `⅋` (and also implicitly `—o`).

The idea is to put everything in conclusion;
however, when we pass from a hypothesis to a conclusion
we must indicate the change by means of a negation symbol.

There will be two basic links enabling one to replace a
hypothesis with a conclusion and vice versa -- `axiom` and `cut`.

By far the best explanation of these two links can be taken from electronics.
Think of a sequent `Γ` as the interface of some electronic equipment,
this interface being made of plugs of various forms `A1`,..., `An`;
the negation corresponds to the complementarity between male and female plugs.
Now a proof of `Γ` can be seen as any equipment with interface `Γ`.

For instance the axiom link is such
a unit and it exists in everyday life as the extension cord.

Now, the cut link is well explained as a plugging.

- **Xie**: In our implementation of interaction nets, we call plugs "ports",
  and we can use `wire` to cut a circle to a wire with ports `A` and `~A` ,
  and use `connect` to connect them back to a circle.

  In general `connect` can connect any two matching ports.

  | girard electronics | interaction nets |
  | ------------------ | ---------------- |
  | plug               | port             |
  | extension cord     | wire             |
  | plugging           | connect          |

  Circle is not forbidden in our implementation,
  we are free to introduce circles into a net,
  we are also free to remove circles from a net.

### 1.3.4 Proof-structures

- **Xie**: The author use the term "times link" and "par link",
  which is a mistake, because, in a graph, an edge (link) connect two nodes,
  and we can not connect an edge to an edge without the help of nodes.

  Instead, we should view "times" and "par" as operations to build compound ports,
  they are simply two (nominally different) ways of grouping ports together.

### 1.3.5 Proof-nets

TODO

### 1.3.6 Cut-elimination for proof-nets

TODO

### 1.3.7 Extension to full linear logic

TODO

## 1.4 Is there a unique logic?

### 1.4.1 LU

By the turn of the century the situation concerning logic was quite simple:
there was basically one logic (classical logic)
which could be used (by changing the set of proper axioms)
in various situations.

Logic was about pure reasoning.

Brouwer's criticism destroyed this dream of unity:
classical logic was not adapted to constructive features
and therefore lost its universality.

By the end of the century we are now faced with
an incredible number of logics.

Is still logic about pure reasoning?

In other terms, could there be a way to reunify logical systems
-- let us say those systems with a good sequent calculus
-- into a single sequent calculus.

Could we handle the (legitimate) distinction classical/intuitionistic
not through a change of system, but through a change of formulas?

Is it possible to obtain classical effects
by restricting one to classical formulas? etc.

Note that the various systems represented should freely communicate
(and for instance a classical theorem could have
an intuitionistic corollary and vice versa).

### 1.4.2 LLL and ELL

TODO

# 2 THE SEMANTICS OF LINEAR LOGIC

## 2.1 The phase semantics of linear logic

The most traditional, and also the less interesting semantics of linear logic
associates values to formulas, in the spirit of classical model theory.
Therefore it only modelizes it only modelizes provability, and not proofs.

### 2.1.1 Phase spaces

TODO

- **Xie**: TODO We use monoid act on itself to denote element of `-o`.

- **Xie**: TODO Note the relation between the use of monoid and stack-based postfix notations.

### 2.1.2 Interpretation of the connectives

TODO

### 2.1.3 Soundness and completeness

TODO

## 2.2 The denotational semantics of linear logic

### 2.2.1 Implicit versus explicit

TODO

## 2.3 Geometry of interaction

TODO

## 2.4 Game semantics

What could be the general pattern of a convincing game semantics?

## 2.4.1 Plays, strategies etc

Imagine a game between two players.

Linear negation is the interchange of players.

A type is a game, and its proof is a winning strategy.

- **Xie**: What is a play?
  Is it the new concept introduced by the idea of game semantics?
