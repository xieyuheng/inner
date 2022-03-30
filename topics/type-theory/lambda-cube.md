---
title: Lambda Cube
subtitle: Why truncate the rule for Pi instead of keeping it structural?
---

# The Lambda Cube

[ [WIKIPEDIA](https://en.wikipedia.org/wiki/Lambda_cube) ]

## (λ→) Simply typed lambda calculus

> Term depends on Term.

```scheme
(define-rule
 (check (extend ctx x A) t B)
 (check ctx (lambda (x) t) (-> A B)))
```

### Examples

```scheme
(check (list (tuple A Prop))
  (lambda ((x A)) x) (Pi ((x A)) A))

(check (list (tuple A Prop))
  (lambda ((x A)) x) (-> A A))
```

## (λ2) System F

> Term depends on Type.

```scheme
(define-rule
 (check ctx t B)
 (check ctx (lambda ((A Type)) t) (Pi ((A Type)) B)))

(claim id (Pi ((A Type)) (-> A A)))
(define id (lambda ((A Type) (x A)) x))
```

### Notes about syntax

In literature, people use upper-case `Λ` to write abstraction over type variables.

So, instead of

```scheme
(λ ((A Type)) (λ ((x A)) x))
```

People write

```scheme
(Λ (A) (λ ((x A)) x))
```

And people drop the type in `Pi` if its type.

Instead of

```scheme
(Pi ((A Type)) (-> A A))
```

People write

```scheme
(Pi (A) (-> A A))
```

To keep things simple, I do not use such notations.

### Examples

```scheme
(define Absurd (Pi ((A Prop)) A))

(check (list) ;; empty context
  (lambda ((Anything Prop) (falsehood Absurd)) (falsehood Anything))
  (Pi ((Anything Prop) (falsehood Absurd)) Anything))
```

### Refereces

- "Types and Programming Languages", Benjamin Pierce, 2002.
  - V Polymorphism
    - Ch. 23. Universal Types
    - Ch. 25. An ML Implementation of System F

## (λω) System Fω

> Type depends on Type.

```scheme
(claim Tree (-> Type Type))
(define (Tree A)
  (Pi ((B Type))
    (-> (-> A B) (-> B B B) B)))
```

From a computing point of view, λω is extremely strong,
and has been considered as a basis for programming languages.

### Examples

```scheme
(claim and (-> Prop Prop Prop))
(define and
  (lambda ((A Prop) (B Prop) (C Prop))
    (-> (-> A B C) C)))
```

## (λP) Lambda-P

> Type depends on Term.

```scheme
(define-rule
 (check (extend ctx x A) B Type)
 (check ctx (Pi ((x A)) B) Type))
```

### Examples

```scheme
(declare A Prop)
(declare a0 A)
(declare P (-> A Prop))
(declare Q Prop)

(claim apply
  (-> (Pi ((x A)) (-> (P x) Q))
      (Pi ((x A)) (P x))
      Q))

(define apply
  (lambda ((z (Pi ((x A)) (-> (P x) Q)))
           (y (Pi ((x A)) (P x))))
    ((z a0) (y a0))))
```

## (Fω) System Fω

> Term depends on Type; <br>
> Type depends on Type.

## (λC) Calculus of constructions

> Term depends on Type; <br>
> Type depends on Term; <br>
> Type depends on Type.

```scheme
(define-grammer
  (<exp>
   <var>
   <sort>
   (<exp> <exp>)
   (lambda ((<var> <exp>)) <exp>)
   (Pi ((<var> <exp>)) <exp>)))
```

The calculus of constructions has both the predicate expressiveness of λP
and the computational power of λω,
hence why λC is also called λPω,
so it is very powerful, both on the logical side
and on the computational side.

# Common properties

All systems in the cube enjoy

- The [Church-Rosser property](https://en.wikipedia.org/wiki/Church-Rosser_property);
- The [subject reduction property (type preservation)](https://en.wikipedia.org/wiki/Subject_reduction);
- The uniqueness of types.
  - This property will be break if we introduce subtying.

All of these can be proven on generic pure type systems.

Any term well-typed in a system of the cube is strongly normalizing,
although this property is not common to all pure type systems.

No system in the cube is Turing complete.

# The rules

```scheme
(define-rule axiom
  (check ctx Prop Type))

(define-rule start
  (check ctx A Sort)
  (check (ctx x A) x A))

(define-rule weakening
  (check ctx A B)
  (check ctx C Sort)
  (check (ctx x C) A B))

(define-rule application
  (check ctx C (Pi ((x A)) B))
  (check ctx a A)
  (check ctx (C a) (subst B x a)))

(define-rule conversion
  (check ctx A B)
  (beta-equal B B')
  (check ctx B' Sort)
  (check ctx A B'))
```

The difference between the systems is in the pairs of sorts
that are allowed in the following two typing rules
-- `product` and `abstraction`.

Where in following `SortLeft` can depend on `SortRight`.

|          Axiom | Meaning              |
| -------------: | -------------------- |
| `(Prop, Prop)` | Term depends on Term |
| `(Prop, Type)` | Type depends on Term |
| `(Type, Prop)` | Term depends on Type |
| `(Type, Type)` | Type depends on Type |

Note that, the naming -- `Prop` and `Type` are borrowed from Coq.

```scheme
(define-rule product
  ;; NOTE This rule can also be viewed as `pi-abstraction`.
  (check ctx A SortLeft)
  (check (extend ctx x A) B SortRight)
  (check ctx (Pi ((x A)) B) SortRight))

(define-rule abstraction
  (check ctx A SortLeft)
  (check (extend ctx x A) B SortRight)
  (check (extend ctx x A) b B)
  (check ctx (lambda ((x A)) b) (Pi ((x A)) B)))
```

# Relation to other systems

- The system [Automath](https://en.wikipedia.org/wiki/Automath) is similar to λ2 from a logical point of view.

- The ML-like languages, from a typing point of view, lie somewhere between λ→ and λ2,
  as they admit a restricted kind of polymorphic types,
  that is the types in prenex normal form.
  However, because they feature some recursion operators,
  their computing power is greater than that of λ2.

- The Coq system is based on an extension of λC

  - with a linear hierarchy of universes (rather than only one untypable `Type`),
  - and the ability to construct inductive types.

- Pure type systems can be seen as a generalization of the cube,
  with an arbitrary set of sorts, axiom, product and abstraction rules.

  Conversely, the systems of the lambda cube
  can be expressed as pure type systems

  - with two sorts `(set Prop Type)`,
  - the only axiom `(set (tuple Prop Type))`,
  - and a set of rules `R` such that
    ```scheme
    (set (tuple Prop Prop Prop))
    <= R <=
    (set (tuple Prop Prop Prop)
         (tuple Prop Type Type)
         (tuple Type Prop Prop)
         (tuple Type Type Type))
    ```

| System of the cube | Logical System                             |
| -----------------: | ------------------------------------------ |
|                 λ→ | (First-order) Propositional Calculus       |
|                 λ2 | Second-order Propositional Calculus        |
|                 λω | Weakly Higher Order Propositional Calculus |
|                 λω | Higher Order Propositional Calculus        |
|                 λP | (First order) Predicate Logic              |
|                λP2 | Second-order Predicate Calculus            |
|                λPω | Weak Higher Order Predicate Calculus       |
|                 λC | Calculus of Constructions                  |

# Pure type system

[ [WIKIPEDIA](https://en.wikipedia.org/wiki/Pure_type_system) ]

```scheme
(define-rule axiom
  (includes axioms (tuple SortLeft SortRight))
  (check ctx SortLeft SortRight))

(define-rule product ;; pi abstraction
  (includes axioms (tuple SortLeft SortRight))
  (includes rules (tuple SortLeft SortRight SortFinal))
  (check ctx A SortLeft)
  (check (extend ctx x A) B SortRight)
  (check ctx (Pi ((x A)) B) SortFinal))

(define-rule abstraction
  (includes axioms (tuple SortLeft SortRight))
  (check ctx A SortLeft)
  (check (extend ctx x A) B SortRight)
  (check (extend ctx x A) b B)
  (check ctx (lambda ((x A)) b) (Pi ((x A)) B)))
```

# Structural Lambda abstraction

In the `product` rule above,
the structural relation between `lambda` and `Pi` is truncated.

**Why truncate the rule for `Pi` instead of keeping it structural?**

Maybe this is because when talking about `(Pi ((x A)) B)`'s type,
we do not want to know its structural details,
we just want to say it is a type.

Also, another difference between `lambda` and `Pi` is that,
we want to write `(lambda (x) b)` instead of `(lambda ((x A)) b)`,
i.e. we want typed lambda a al Curry.

**How about the following structural abstraction rule?**

Suppose we have the following:

|      New | Old      |
| -------: | -------- |
| `(Pi 0)` | `lambda` |
| `(Pi 1)` | `Pi`     |
| `(Pi 2)` | `...`    |

```scheme
(define-rule product-structural
  (check (extend ctx x A) B B2)
  (check ctx ((Pi n) ((x A)) B) ((Pi n+1) ((x A)) B2)))
```

We will also need `(Type 0)`, `(Type 1)`, ...
and subtype relation `(Pi n) < (Type n)`,
because if we can not view `(Pi)` as `Type`
we will not be able to write list of functions -- `(List (-> A B))`.

Or, how about just using `lambda` and have the following rule.

- With the help of subtyping `(lambda ...) < Type`.

```scheme
(define-rule abstraction
  (check (extend ctx x A) b B)
  (check ctx (lambda ((x A)) b) (lambda ((x A)) B)))
```

What is the implication of this kind of structural lambda abstraction rule?

- Suppose we do not have subtyping, we will not be able to a useful `and`.
- If we have subtyping, this might be interesting and worth implementing.
