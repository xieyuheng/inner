---
title: Cubical Type Theory
subtitle: a constructive interpretation of the univalence axiom
authors: [Cyril Cohen, Thierry Coquand, Simon Huber, and Anders Moertberg]
year: 2016
---

# Abstract

This paper presents a type theory in which
it is possible to directly manipulate n-dimensional cubes
(points, lines, squares, cubes, etc.)
based on an interpretation of dependent type theory in a cubical set model.

This enables new ways to reason about identity types,
for instance, function extensionality is directly provable in the system.
Further, Voevodsky's univalence axiom is provable in this system.

We also explain an extension with some higher inductive types
like the circle and propositional truncation.

Finally we provide semantics for this cubical type theory
in a constructive meta-theory.

<question>
  What is the cubical set model a set theoretical model?

  <answer>
    TODO
  </answer>
</question>

# 1 Introduction

This work is a continuation of the program started in [6, 13]
to provide a constructive justification of Voevodsky’s univalence axiom [27].

This axiom allows many improvements for the formalization of mathematics in type theory:
function extensionality, identification of isomorphic structures, etc.

In order to preserve the good computational properties of type
theory it is crucial that postulated constants have a computational interpretation.

Like in [6, 13, 22] our work is based on a nominal extension of λ-calculus,
using names to represent formally elements of the unit interval [0, 1].

<question>
  What is the meaning of "nominal extension" here?

  <answer>
    The names are not variable referencing something in the context or environment,
    but are special kind of variable that can be used as value.
  </answer>
</question>

<question>
  Does this so called "nominal extension" relates to nominal logic?

  <answer>
    TODO
  </answer>
</question>

This paper presents two main contributions.

The first one is a refinement of the semantics presented in [6, 13].
We add new operations on names corresponding to the fact that
the interval [0, 1] is canonically a de Morgan algebra [3].
This allows us to significantly simplify our semantical justifications.
In the previous work, we noticed that it is crucial for
the semantics of higher inductive types [26] to have a “diagonal” operation.
By adding this operation we can provide a semantical justification
of some higher inductive types and we give two examples
(the spheres and propositional truncation).
Another shortcoming of the previous work was that using path types as equality types
did not provide a justification of the computation rule
of the Martin-L¨of identity type [19] as a judgmental equality.
This problem has been solved by Andrew Swan [25],
in the framework of [6, 13, 22], who showed that
we can define a new type, equivalent to,
but not judgmentally equal to the path type.
This has a simple definition in the present framework.

The second contribution is the design of a type system inspired by this semantics
which extends Martin-L¨of type theory [20, 19].
We add two new operations on contexts:
- addition of new names representing dimensions
- and a restriction operation.

Using these we can define a notion of extensibility
which generalizes the notion of being connected by a path,
and then a Kan composition operation that expresses that
being extensible is preserved along paths.
We also define a new operation on types which expresses that
this notion of extensibility is preserved by equivalences.
The axiom of univalence, and composition for the universe,
are then both expressible using this new operation.

# 2 Basic type theory

The syntax of contexts, terms and types is specified by:

```
Γ, ∆ ::= () | Γ, x : A // Contexts

t, u, A, B ::=
    x
  | λx : A. t | t u | (x : A) → B  // Π-types
  | (t, u) | t.1 | t.2 | (x : A) × B // Σ-types
  | 0 | s u | natrec t u | N // Natural numbers
```

```whereabouts
Ctx []
Ctx [[name, exp] | rest] -- { Exp exp Ctx rest }

Exp Exp::var(name)

Exp Exp::fn(name, argType, ret) -- { Exp argType Exp ret }
Exp Exp::ap(target, arg) -- { Exp target Exp arg }
Exp Exp::pi(name, argType, retType) -- { Exp argType Exp retType }

Exp Exp::cons(car, cdr) -- { Exp car Exp cdr }
Exp Exp::car(target) -- { Exp target }
Exp Exp::cdr(target) -- { Exp target }
Exp Exp::sigma(name, argType, retType) -- { Exp argType Exp retType }

Exp Exp::zero
Exp Exp::add1(prev) -- { Exp prev }
Exp Exp::natRec(base, step, target) -- { Exp base Exp step Exp target }
Exp Exp::nat
```

```whereabouts
// Subst means the noun "substitution"
Subst []
Subst [[name, exp] | rest] -- { Subst rest }

CheckSubst [ctx, [], []] -- { Ctx ctx }
CheckSubst [ctx, [[name, exp] | restSubst], [[name, type] | restTypes]] -- {
  Substitute [restSubst, type, resultType]
  Check [ctx, exp, substitutedType]
  Ctx ctx
  Subst restSubst
  Ctx restTypes
  CheckSubst [ctx, restSubst, restTypes]
}
```

Lemma 1. Substitution is admissible:

- **Xie:** Although the authors are developing system
  for formalizing mathematics,
  but they have very low standard about being accurate.

```whereabouts

<any judgment> [ctx, <exps> / subst] // apply the subst
-------------------------------- {
  <any judgment> [types, <exps>]
  CheckSubst [ctx, subst, types]
}
```

Inference rules of the basic type theory:

```whereabouts
TODO
```

# 3 Path types

As in [6, 22] we assume that we are given
a discrete infinite set of names
(representing directions) i, j, k, ...

We define I to be the free de Morgan algebra [3] on this set of names.
This means that I is a bounded distributive lattice
with top element 1 and bottom element 0 with an involution 1 − r satisfying:

```
1 − 0 = 1
1 − 1 = 0
1 − (r ∨ s) = (1 − r) ∧ (1 − s)
```

- **Xie:** To make the rules clear, use `~r` instead of `1 − r`.

  ```
  ~0 = 1
  ~1 = 0
  ~(r ∨ s) = (~r) ∧ (~s)
  ```

- **Xie:** Note that an element of interval type `I`
  is said to be "representing direction",
  i.e. orientation of the glueing,
  in the context of building cell-complex.

The set I also has decidable equality,
and as a distributive lattice,
it can be described as the free distributive lattice
generated by symbols i and ~i [3].
As in [6], the elements in I can be thought as
formal representations of elements in [0, 1],
with r ∧ s representing min(r, s)
and r ∨ s representing max(r, s).
With this in mind it is clear that (1 − r) ∧ r != 0
and (1 − r) ∨ r != 1 in general.

## 3.1 Syntax and inference rules

Contexts can now be extended with name declarations:

```
Γ, ∆ ::= ... | Γ, i : I
```

```whereabouts
Ctx []
Ctx [[name, exp] | rest] -- { Exp exp Ctx rest }
Ctx [[name, i] | rest] -- { I i Ctx rest }
```

# 4 Systems, composition, and transport

# 5 Derived notions and operations

# 6 Glueing

# 7 Universe and the univalence axiom

# 8 Semantics

# 9 Extensions: identity types and higher inductive types

# 10 Related and future work

# A Details of composition for glueing

# B Univalence from glueing

# C Singular cubical sets
