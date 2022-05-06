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

TODO

# 2 THE SEMANTICS OF LINEAR LOGIC

## 2.1 The phase semantics of linear logic

TODO