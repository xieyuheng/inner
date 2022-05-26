---
title: Type System as Homomorphism between Monoids
author: Xie Yuheng
date: 2022-05-06
keywords: [Sequent Calculus, Linear Logic, Syntax Design, Monoid]
---

# Abstract

The space of types (linear logic propositions) is monoid,
the space of terms is also a monoid.

Negation should NOT be interpreted as "implying false",
but be interpreted as type of _linear assignment_ (thus constructive).

Linear logic additive connectives can be interpreted without concurrency.

As presentation of of monoids.

# Introduction

If we have a homomorphism between two monoids,
we can view this homomorphism as the `infer` function,
given a term it can infer the type of this term.

The reverse is also true, given a type system,
we can construct two monoids to be the space of terms and types.

And we can interpret concepts in type system as equations in monoid.

# Example: a syntax for the terms of linear logic

We can view a type system as a homomorphism between two monoids
(or an endomorphism of one monoid).

The domain of the homomorphism is the space of terms,
the codomain of the homomorphism is the space of types.

Let's define a concrete monoid to see this idea in action.

TODO mention linear logic.

We take _the free monoid of symbols_ as the base monoid (the building block).

- An element of the monoid is defined to be a list of symbols.

- The empty list is the identity element of the monoid,
  we also write it as `empty` for convenience.

We use whitespace as infix notation
to denote the binary operation
of the monoid (instead of using an explicit infix notation like `•`).

If `A` and `B` are elements, so is `A B`.

Example elements:

- `A`
- `A B`
- `A B B A`
- `A C D C`

We define two ways to construct new elements.

- **Negation:**

  We preserve `neg` as a special symbol to denote negation.

  If `A` is an element, `A neg` is also an element.

  Note that this is NOT `A`'s inverse element,
  but only a _postfix notation_ to construct new element
  (If we really can get the inverse element of any element,
  we will be working within group instead of monoid).

- **Abstraction (anonymous sequent):**

  If `A` is an element, so is `{ A }`,
  and we call it "abstraction".

We specify three equivalent relations between monoid elements:

- **Negation cancels an element from the right side:**

  ```
  A A neg == empty
  ```

- **Negation is involutive:**

  ```
  A neg neg == A
  ```

- **Abstraction can be applied:**

  We preserve `apply` as a special symbol to denote application of abstraction.

  ```
  { A } apply == A
  ```

We introduce a syntactic shorthand for writing negation in abstraction,

```
{ A B -- C D }
```

is the same as

```
{ A neg B neg C D }
```

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

TODO define `Type`

TODO define `error` element and `Error` type

TODO use monoid equations to interpret type variable and unification

TODO define linear assignment

```
claim swap { 'A 'B -- 'B 'A }

define swap {
  let (x) let (y) x y
}
```

# Linear logic propositions

TODO

# Examples

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
