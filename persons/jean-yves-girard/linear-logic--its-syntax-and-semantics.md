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

The most important linear connective is linear negation (`nil`).

Since linear implication will eventually be rewritten as `~A ⅋ B`,
`nil` is the only negative operation of logic.

Linear negation behaves like transposition in linear algebra
(`A —o B` will be the same as `~B —o ~A`),
i.e. it expresses a duality, that is, a change of standpoint:

```
action of type A = reaction of type ~A
```

Other aspects of this duality are

- output/input
- answer/question
- send/receive

The main property of `~` is that `~~A` can, without any problem,
be identified with `A` like in classical logic.

But linear logic has a very simple constructive meaning,
whereas the constructive contents of classical logic
is by no means ... obvious.

The involutive character of "nil" ensures De Morgan-like laws
for all connectives and quantifiers. e.g.

```
∃x A = ~(∀x ~A)
```

which may look surprising at first sight,
especially if we keep in mind that
the existential quantifier of linear logic is _effective_:
typically, if one proves `∃x A`,
then one proves `A[t/x]` for a certain term `t`.

This exceptional behaviour of "nil" comes from the fact that
`~A` negates (i.e. reacts to) a single action of type `A`,
whereas usual negation only negates some (unspecified) iteration of `A`,
what usually leads to a
[Herbrand disjunction](https://en.wikipedia.org/wiki/Herbrand%27s_theorem)
of unspecified length,
whereas the idea of linear negation is not connected to
anything like a Herbrand disjunction.

Linear negation is therefore more primitive, but also stronger
(i.e. more difficult to prove) than usual negation.

### 1.1.4 States and transitions

The familiar notion of

```
theory = classical logic + axioms
```

should be replaced by:

```
theory = linear logic + axioms + current state
```

We can use `!` for the axioms, they are stable truth.

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

  Using explicit modalities to express structural rules,
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
is wholly in the structural group of rules,
and it is not too excessive to say that
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

  | Girard's electronics | Interaction nets |
  | -------------------- | ---------------- |
  | plug                 | port             |
  | extension cord       | wire             |
  | plugging             | connect          |

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

A proof-structure is nothing but a graph
whose vertices are (occurrences of) formulas
and whose edges are links.

Moreover each formula
is the conclusion of exactly one link
and the premise of at most one link.

The formulas which are not premises
are the conclusions of the structure.

Inside proof-structures,
let us call proof-nets those which can be obtained as
the interpretation of sequent calculus proofs.

Of course most structures are not nets:
typically the definition of a proof-structure
does not distinguish between `⊗`-links and `⅋`-links
whereas conjunction is surely different from disjunction.

The question which now arises is
to find an independent characterization of proof-nets.

Let us explain why this is essential:

1. If we define proof-nets from sequent calculus, this means that we
   work with a proof-structure together with a sequentialization, in
   other terms a step by step construction of this net.

   But this sequentialization is far from being unique, typically
   there might be several candidates for the "last rule" of a given
   proof-net.

   - **Xie**: We see Girard avoid sequentialization, but In our
     implementation, we are actually doing a sequentialization,
     i.e. finding the terms syntax of sequent calculus is our goal.

     And our implementation is indeed a step by step construction of
     interaction nets.

     I think Girard want to avoid sequentialization, because in proof
     theory, we want to proof meta theorems about our formal system,
     only by means of pure syntactic analysis.

     But maybe we can use a sequentialized syntax to build graph first,
     and then prove meta theorems based on graph.

     In practical implementation, we need a sequentialized syntax to
     describe graph anyway.

   In practice, we may have a proof-net with a given sequentialization
   but we may need to use another one: this means that we will spend
   all of our energy on problems of commutation of rules, as with old
   sequent calculus, and we will not benefit too much from the new
   approach.

   Typically, if a proof-net ends with a splitting `⊗`-link, (i.e. a
   link whose removal induces two disconnected structures), we would
   like to conclude that the last rule can be chosen as `⊗`-rule;
   working with a sequentialization this can be proved, but the proof
   is long and boring, whereas, with a criterion, the result is
   immediate, since the two components inherit the criterion.

2. The distinction between "and" and "or" has always been explained in
   semantical terms which ultimately use "and" and "or"; a purely
   geometrical characterization would therefore establish the
   distinction on more intrinsic grounds.

### 1.3.6 Cut-elimination for proof-nets

| Rule             | Sequent | Girard's graph |
| ---------------- | ------- | -------------- |
| cut              | `~A A`  | below link     |
| axiom (identity) | `A ~A`  | above link     |

Cut-elimination rewritings:

- `{ A ~A }` => `{ }`

- **Xie**: The target of reduction is `A ~A`,
  it might be introduced by identity axiom or other rules.

  We can see cut-elimination means partial evaluation,
  thus also means normalization.

  Maybe we can use the idea of NbE for inet,
  after normalizing a net, we can read it back
  to our sequentialized syntax.

  Equivalence between nets will still be implemented at graph level,
  `readback` only serve as a tool to help us understand the graph.

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

## Xie: Two kinds of modelling

By model theory of a logic,
we mean a mathematic structure to model the space of propositions of the logic.

- This is what Girard means by saying "it (phase space) only modelizes provability".

By denotational semantics of a logic,
we mean a mathematic structure to model the space of terms of the logic.

- This is why, in categorical logic, we say that
  "Cartesian closed category is the internal language
  of simply typed lambda calculus.".

## 2.1 The phase semantics of linear logic

The most traditional, and also the less interesting semantics of linear logic
associates values to formulas, in the spirit of classical model theory.
Therefore it only modelizes it only modelizes provability, and not proofs.

### 2.1.1 Phase spaces

- **Xie**: Girard said this semantics is less interesting,
  because it only modelizes provability.

  But we can actually view an element of a fact as a proof.

### 2.1.2 Interpretation of the connectives

TODO

### 2.1.3 Soundness and completeness

TODO

## 2.2 The denotational semantics of linear logic

### 2.2.1 Implicit versus explicit

First observe that the cut rule is a way to formulate modus ponens.

It is the essential ingredient of any proof.

If I want to prove B, I usually try to prove a useful lemma A and,
assuming A, I then prove B.

All proofs in nature, including the most simple ones,
are done in this way.

Therefore, there is an absolute evidence that
the cut rule is the only rule of logic
that cannot be removed:
without cut it is no longer possible to reason.

Now against common sense Gentzen proved his Hauptsatz;
for classical and intuitionistic logics
(and remember that can be extended to linear logic without problems).

This result implies that we can make proofs without cut,
i.e. without lemmas (i.e. without modularity, without ideas, etc.).

Formal reasoning (any reasoning) is about implicit data.
This is because it is more convenient to forget.

So, when we prove `A V B`, we never know which side holds.

- **Xie**: By "making implicit explicit"
  Girard means inlining and partial evaluation.

  In SICP's primitive/composition/abstraction,
  implicit corresponds to abstraction.

### 2.2.2 Generalities about denotational semantics

- **Xie**: The purpose of denotational semantics,
  is to model the space of proofs.

  The model should respect the reduction (cut-elimination) of proof.

  A model can a abstract structure like in categorical logic,
  or be a concrete structure (for example, a proof is a set).

A proof will be interpreted as a [**coherent space**](https://en.wikipedia.org/wiki/Coherent_space),
which is simplified [Scott domain](https://en.wikipedia.org/wiki/Scott_domain)

These spaces were first intended as denotational semantics
for intuitionistic logic, but it turned out that
there were a lot of other operations hanging around.

Linear logic first appeared as a kind of linear algebra built on coherent spaces;
then linear sequent calculus was extracted out of the semantics.

- **Xie**: The above is important historical note about linear logic.

Recently Ehrhard, see [9], this volume, refined coherent semantics
into hypercoherences, with applications to the question of sequentiality.

### 2.2.3 Coherent spaces

A **coherent space** is a **reflexive undirected graph**.

| coherent space     | graph |
| ------------------ | ----- |
| atom               | node  |
| coherence relation | edge  |

Where edges between nodes are unique, because they are viewed as relation.

A [**clique**](<https://en.wikipedia.org/wiki/Clique_(graph_theory)>) is a [complete subgraph](https://en.wikipedia.org/wiki/Complete_graph).

We will interpret a linear logic proposition (a type) as a coherent space.

- **Xie**: Since a coherence space is an undirected graph,
  we can view it as a category (with a [representable functor](https://en.wikipedia.org/wiki/Representable_functor)),
  and the space of propositions a category of categories.

  After then, we can seek pure algebric definitions of connectives.

In fact a coherent space can also be
presented as a set of cliques
(a node is a 1-node clique).

We will interpret a proof of a proposition as a clique of the coherent space.

TODO interpret connectives

### 2.2.4 Interpretation of MALL

TODO interpret identity rule

TODO interpret cut rule

### 2.2.5 Exponentials

TODO

### 2.2.6 The bridge with intuitionism

TODO

### 2.2.7 The bridge with classical logic

TODO

## Xie: The model and the logic are one

Recall model theory.

In logic we use inference rules to prove equations between connectives,
and we say a mathematic structure is the logic's model,
if the equations agree with the equivalent relation of the model.

- Logic equation is defined by "iff",
  a model might also be able to interpret inequation,
  which is defined by "if".

We often use an algebric structure as model.

Since we are using postfix notations as syntax,
the space of terms itself is a monoid.

- Elements in the monoid is typed by linear Logic propositions,
  we may need to introduce an _error element_,
  to make product between monoid elements totel,
  otherwise we will be using category (instead of monoid)
  to model the space of terms.

The language by which we record the steps of inferences is algebric,
i.e. the language by which we do proof is algebric.

Thus the logic is its own model.

The model and the logic are one.

- Note that, in categorical logic,
  the denotational semantics is already an algebric structure,
  just the term syntax is not optimized for
  function composition (but optimized for function application).

  In categorical logic, a constraint is that
  the semantics should be an algebric structure based on category theory.

  This constraint is not necessary in general.

- The internal syntax of linear logic
  is said to be [symmetric monoidal (closed) category](https://en.wikipedia.org/wiki/Symmetric_monoidal_category),
  or to be [braided monoidal category](https://en.wikipedia.org/wiki/Braided_monoidal_category)
  which is more expressive about exchange rule.

## 2.3 Geometry of interaction

By the electronic analogy,
we can get a very good interpretation
of cut-elimination for `⊗` and `⅋`.

- **Xie**: The connectives `⊗` and `⅋` group ports together,
  one for the negatives one for the postives.

## 2.4 Game semantics

What could be the general pattern of a convincing game semantics?

## 2.4.1 Plays, strategies etc

Imagine a game between two players.

Linear negation is the interchange of players.

A type is a game, and its proof is a winning strategy.

- **Xie**: What is a play?
  Is it the new concept introduced by the idea of game semantics?
