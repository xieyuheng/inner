---
title: On the Meaning and Construction of the Rules in Martin-Löf's Theory of Types
author: Roland Backhouse
year: 1986
---

# Abstract

> We describe a method to construct the elimination and computation
> rules from the formation and introduction rules for a type in
> Martin-Löf's theory of types. The construction is based on an
> understanding of the inference rules in the theory as judgements in
> a pre-theory. The motivation for the construction is to permit
> disciplined extensions to the theory as well as to have a deeper
> understanding of its structure.

# 1 The Pre-Theory

- **Xie**: In this paper, the author write application of lambda expression as `((x)P)(x)`.

The rules of the pre-theory (and of the theory) prescribe the
formation of derivations and from derivations one may abstract
judgements.

A *derivation* is thus a sequence of *statements* each of which is either
a *primitive statement* or a *context*.

- **Xie**: A derivation is a function (a lambda annotated by argument type).

Contexts are written as an assumption followed by a (sub-) derivation:

```
|[ assumption, ... |> (sub-)derivation, ... ]|
```

A *judgement* is formed from a derivation by the simple process of
eliding all but the last statement in the derivation and in all its
sub- derivations.

- **Xie**: A judgement is a pi type,
  thus the author use the same syntax for lambda and pi.

An example derivation would be:

```
p0
|[ a0
|> p1
   |[ a1
   |> p2
   ]|
]|
```

where `p0`, `p1` and `p2` are primitive statements and `a0`, `a1` are assumptions.

An example judgement obtained by eliding all
but the last statement in each derivation is the following:

```
|[ a0
|> |[ a1
   |> p2
   ]|
]|
```

which may be read as "assuming `a0` and assuming `a1` then `p2`".

- **Xie**: If we write the above derivation as function body:

  ```
  p0
  (a0) => {
    p1
    (a1) => {
      p2
    }
  }
  ```

  Or:

  ```
  p0
  (a0) => {
    p1
    (a1) => p2
  }
  ```

  If we write the above judgement as function body:

  ```
  (a0) => {
    (a1) => {
      p2
    }
  }
  ```

  Or:

  ```
  (a0) => (a1) => p2
  ```

  If we write the above judgement as pi type:

  ```
  (a0) -> (a1) -> p2
  ```

Each *rule* in the pre-theory (and in the theory) consists of a set of
*premises* and a *conclusion*, in the usual way. The application of a
rule permits a derivation to be extended by adding a statement to the
end of the derivation or to the end of a sub derivation provided that
the extended derivation includes statements preceding the added
statement that match the premises in the same way that the added
statement matches the conclusion. An axiom is a rule that has no
premises; thus application of an axiom permits a derivation to be
extended at an arbitrary point.

- **Xie**： A rule is function (or primitive function),
  we can apply a function to expressions (variables) in scope
  provided that the type matches.

  An axiom is a constant value (or zero-arity function).

```
|[ X: Type
|> |[ y: El(X)
   |> |[ Y: F(El(X), (x)Type)
     |> Y(y): Type
     ]|
   ]|
]|
```

- **Xie**: Note that, in this paper,
  the author uses `F` for function type,
  and `El(X)` instead of just `X`.
  and in dependent function type
  the bound variable is written after the argument type,
  such as `Y: F(El(X), (x)Type)` which means `Y: (x: X) -> Type`.

``` cicada
example(X: Type, y: X, Y: (x: X) -> Type): Type {
  Y(y)
}

example(Nat, 1, (x) => String)
```
