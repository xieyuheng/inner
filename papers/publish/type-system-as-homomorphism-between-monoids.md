---
title: Type System as Homomorphism between Monoids
author: Xie Yuheng
date: 2022-05-06
keywords: [Sequent Calculus, Linear Logic, Syntax Design, Monoid]
---

# Abstract

We demonstrate how to view a type system as a homomorphism between two monoids,
where the space of types is monoid, the space of terms is also a monoid,
and the homomorphism is the `infer` function of the type system.

We use a concrete example to demonstrate this idea,
in our example the space of types is _linear logic propositions_,
and the space of terms is a _programming language like Forth and Joy_.

Some key points of our demonstration:

- Negation of linear logic should NOT be interpreted as "implying false",
  but be interpreted as type of _linear assignment_ (thus constructive).

- Linear logic additive connectives can be interpreted without concurrency.

- Type errors will be captured by a special `Error` element.

# Introduction

First a homomorphism between two monoids, can be viewed as a type system.
We can view this homomorphism as the `infer` function,
given a term it can infer the type of this term.

- TODO Note about the principle of type theory.
- TODO Use presentation of of monoids to show some examples.

The reverse is also true, given a type system,
we can construct two monoids to be the space of terms and types.
And we can interpret concepts in type system as equations in monoid.

# Example: a syntax for the terms of linear logic

Let's define two concrete monoids and a homomorphism between them,
to see the idea of this paper in action.

We call the homomorphism `infer`, its domain **terms**, its codomain **types**.

The types will be _linear logic propositions_.

The terms will be a _programming language
like [Forth](https://en.wikipedia.org/wiki/Forth_(programming*language))
and [Joy](https://en.wikipedia.org/wiki/Joy*(programming*language))_.

We use whitespace as infix notation
to denote the binary operation
of the monoids (instead of using an explicit infix notation like `•`).

if `x` and `y` are elements, so is `x y`.

Since `infer` is a homomorphism, we have:

```
infer(x y) = infer(x) infer(y)
```

## The plan

Our plan is to take two free monoids of symbols as base monoids to start with.

Then we define ways to construct new elements in these two monoids,
meanwhile refine the definition of `infer` for these new elements.

And we refine the base monoids to be not just any symbols,
but symbols introduced by datatype definitions.

## Two free monoids of symbols and a trivial homomorphism

A element of **the monoid of terms** is a list of symbols,
chosen from the first four lower case latin alphabets -- `a b c d`,
such as:

- `a`
- `a b`
- `a b b a`
- `a c d c`

A element of **the monoid of types** is a list of symbols,
chosen from the first four upper case latin alphabets -- `A B C D`,
such as:

- `A`
- `A B`
- `A B B A`
- `A C D C`

The empty list is the **identity element** of both monoids,
for convenience we also explicitly write them as `empty` and `Empty` in equations.

And we define the `infer` to be the trivial homomorphism
from lower case to upper case,
for examples:

```
infer(a) = A
infer(a b) = infer(a) infer(b) = A B
```

## Abstraction of both term and type

If `x` is a term, so is `{ x }`.

If `X` is a type, so is `{ X }`.

- We call this way of constructing new elements **abstraction**.

We refine the definition of `infer` for abstraction:

```
infer({ x }) = { infer(x) }
```

In the monoid of terms, we introduce a postfix operator `apply`
to denote application of abstraction.

We then introduce the following equivalent relation
for elements constructed by abstraction.

**Abstraction of term can be applied:**

```
{ x } apply = x
```

Note that `apply` is only defined for terms, but not for types.

## Linear assignment of term, and negation of type

If `X` is a type, `let (x: X) ... x ...` is a term.

- We call this way of constructing new terms **linear assignment**,
  it is linear in the sense that an assigned variable
  must be referenced once and only once.

If `X` is a type, `X neg` is also a type.

- We call this way of constructing new types **negation**.

  Note that this is NOT `X`'s inverse element in the monoid,
  but only a _postfix notation_ to construct new element
  (If we really can get the inverse element of any element,
  we will be working within group instead of monoid).

We refine the definition of `infer` for linear assignment and negation:

```
infer(let (x: X) ... x ...) = A neg infer(...) X infer(...)
```

i.e. negation is the type of linear assignment.

For example:

```
infer(let (x: A) b x c) = A neg B A C
```

We then introduce the following equivalent relation
for elements constructed by linear assignment and negation.

**Linear assigned term can be substitute into the place of reference:**

```
y let (x: A) ... x ... = ... y ...
```

**Negation cancels an type from the right side:**

```
X X neg = Empty
```

**Negation is involutive:**

```
X neg neg = X
```

We introduce a syntactic shorthand for writing negation in abstraction,

```
{ X -- Y }
```

is the same as

```
{ X neg Y }
```

For example:

```
infer({ let (x: A) b x c }) = { A -- B A C }
```

## Datatype definitions

At the beginning we took _the free monoid of symbols_ as our base monoid,
instead of using any symbols as generaters,
now we refine this base monoid by a limitation saying:

- Only symbols defined by `datatype` keyword are elements of our monoid.

An exmaple of datatype definition is the following:

```
datatype Nat {
  zero { -- Nat }
  add1 { Nat -- Nat }
}
```

It defines one _type constructor_ -- `Nat`,
and two _data constructors_ -- `Nat.zero` and `Nat.add1`.

It also defines the homomorphism for these elements:

- Let's call the homomorphism `infer`.

```
infer(Nat.zero) = Nat
infer(Nat.add1) = Nat neg Nat
```

## TODO

TODO define `Type`

TODO define `error` element and `Error` type

TODO use monoid equations to interpret type variable and unification

# Linear logic propositions

TODO

# Examples

## Stack permutation

```
claim swap { 'A 'B -- 'B 'A }

define swap {
  let (x) let (y) x y
}
```

## Nat

```
datatype Nat {
  zero { -- Nat }
  add1 { Nat -- Nat }
}

claim add { Nat Nat -- Nat }

define add {
  match {
    zero {}
    add1 { add Nat.add1 }
  }
}

rule Nat.zero add {}
rule Nat.add1 add { add Nat.add1 }

claim two { -- Nat }

define two {
  Nat.zero Nat.add1
  Nat.zero Nat.add1
  add
}
```

## Trivial

```
datatype Trivial {
  sole { -- Trivial }
}
```

## List

```
datatype List {
  null { 'A List }
  cons { 'A 'A List -- 'A List }
}

claim append { 'A List 'A List -- 'A List }

rule List.null append {}
rule List.cons append { let (head) append head List.cons }

define append {
  match {
   null {}
   cons { let (head) append head List.cons }
  }
}

claim six_soles { -- Trivial List }

define six_soles {
  List.null Trivial.sole List.cons Trivial.sole List.cons Trivial.sole List.cons
  List.null Trivial.sole List.cons Trivial.sole List.cons Trivial.sole List.cons
  append
}
```

# Dependent type system as an endomorphism of one monoid

TODO
