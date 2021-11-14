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

# 1. The Pre-Theory

- **Xie**: In this paper, the author write application of lambda expression as `((x)P)(x)`,
  which means `(x) => P` applied to `x`.

The rules of the pre-theory (and of the theory) prescribe the
formation of derivations and from derivations one may abstract
judgements.

A *derivation* is thus a sequence of *statements* each of which is either
a *primitive statement* or a *context*.

- **Xie**: A derivation is a function (a lambda annotated by argument type).

Contexts are written as an assumption followed by a (sub-) derivation:

```
|[ assumption,
   ...
|> (sub-)derivation,
   ...
]|
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

Just those rules that we explicitly employ are given below.
For these rules we explain their meaning in an ad hoc way.

We do not, however, attempt to give any meaning to the word category:
the reader must accept that certain expressions denote "categories".

The first rule is as an axiom -- "Type" denotes a category.

```
------------ Type Formation
Type cat
```

- **Xie**: The term "category" as it is used here, denotes universe in type theory.

  In our theory where `Type: Type`, it simply denotes `Type`.

  Thus the basic "Type Formation" rule in our theory is just `Type: Type`,
  and the following `C cat` can also be written as `C: Type`.

Contexts may be introduced into a derivation via the assumption rule.

```
C cat
------------ Assumption
|[ x: C
|>
]|
```

For each type A the elements of A form a category.

```
A: Type
------------ Element formation
El(A) cat
```

Function categories are obtained by discharging assumptions.

```
A cat
|[ x: A
|> B(x) cat
]|
------------ Function formation
F(A, B) cat
```

`F(A, B)` is the category of functions
that map an object `x` of the category `A`
into an object of the category `B(x)`.

- **Xie**: Note that, the author uses `F` for function type, and `El(X)` instead of just `X`.

The final rule we need in the pre-theory is the rule of function elimination.

```
a: A
c: F(A, B)
------------ Function elimination
c(a): B(a)
```

An example of a derivation using these rules is as follows.
Comments are included to aid the readers.
Also, the symbol "==" has been used to denote definitional equality.

```
Type cat          // Type Formation
|[ X: Type        // Assumption
   El(X) cat      // Element formation
   |[ x: El(X)    // Assumption
   |> Type cat    // Type Formation
   ]|

   F(El(X), (x)Type) cat  // Function formation
                          // ((x)Type)(x) === Type

|> |[ y: El(X)                 // Assumption
   |> |[ Y: F(El(X), (x)Type)  // Assumption
      |> Y(y): Type            // Function elimination
                               // ((x)Type)(y) === Type
      ]|
   ]|
]|
```

The judgement obtained from this derivation by eliding all but the
last statement in every sub derivation is the following.

```
|[ X: Type
|> |[ y: El(X)
   |> |[ Y: F(El(X), (x)Type)
      |> Y(y): Type
      ]|
   ]|
]|
```

In words, assuming `X` is a type, `y` is an element of `X` and `Y` is
a function mapping elements of `X` into the category of types, then
`Y` applied to `y` is a type.

- **Xie**: In dependent function type,
  the bound variable is written after the argument type,
  - such as `Y: F(El(X), (x)Type)` which means `Y: (x: X) -> Type`,
  this is because the author want to use lambda
  as the only way for introducing bound variable,
  - remind that `(x)Type` means `(x) => Type`,
  - thus `F(El(X), (x)Type)` can be viewed as `F(El(X), (x) => Type)`,
  in general the author can write `F(A, B)`, where `B` might be `(x) => ...`.

  This is actually how we represent pi type in a implementation.

- **Xie**: We can write the example in our language as the following:

  ``` cicada
  example(X: Type, y: X, Y: (x: X) -> Type): Type {
    Y(y)
  }

  example(Nat, 1, (x) => String)
  ```

# 2. The Rules of Type Theory

TODO
