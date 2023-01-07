---
title: Essentials of Constraint Programming
authors: [Thom Frühwirth, Slim Abdennadher]
year: 2003
---

# A. Foundations from Logic

## A.1 First-Order Logic: Syntax and Semantics

A first-order language can be viewed as a class and its instances.

The signature of the language is the class,
an interpretation of the language is an instance of the class.

Take `Arithmetic` for example.

```
class Arithmetic {
  Element: Type

  zero: Element
  one: Element

  add(Element, Element): Element
  mul(Element, Element): Element

  Eq(Element, Element): Proposition
  Gt(Element, Element): Proposition
  Lt(Element, Element): Proposition
}
```

Logic symbols are function over `Proposition`.

```
Truth: Proposition
Absurd: Proposition
Not(Proposition): Proposition
And(Proposition, Proposition): Proposition
Imply(Proposition, Proposition): Proposition
```

`forall` and `exists` are syntax keywords
that introduce bound variables.

(We do not use "proposition as type" here.)

```
forall (x: Element, y: Element) Eq(add(x, y), add(y, x)) : Proposition
exists (x: Element) Eq(x, add(one)) : Proposition
```

`Proposition` is a subtype of `Type` with additional axioms
-- primitive functions without computational meaning.

TODO

`I, n |= F`

`I |= F`

Maybe we should not understand first-order language
by "proposition as type" at all.

An interpretation is actually an interpreter
in the sense of EOPL's "writing an interpreter".

If we only use one `env`,
we get not only first-order language,
but also higher-order language,
and in normal interpreter,
we are not limited to one universe.

But when "writing an interpreter",
we do not have `forall` and `exists` in our language.

## A.2 Basic Calculi and Normal Forms

A _calculus_ is a deduction system defined by a list of inference rules.

If we want to have a term-syntax to record proof,
we will inevitably reach for function as term of `forall`
and pair as term of `exists`.

The _resolution calculus_ is a deduction system,
in which no term-syntax is required.

But maybe we still can provide a term-syntax for resolution calculus,
just to record the steps, not to use the reducion of term as a computational model.

But maybe the satisfiability can be verified without the proof (steps of deduction).

- But the `find` query can only handle `exists`, not `forall`.
