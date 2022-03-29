---
title: Lambda Cube
subtitle: Why truncate the rule for Pi instead of keep it structural?
---

# TODO

- change the syntax of the rules for automath-like to sexp
- change the syntax of the rules for structural version to sexp

# The Lambda Cube

[ [WIKIPEDIA](https://en.wikipedia.org/wiki/Lambda_cube) ]

## (λ→) Simply typed lambda calculus

> Term depends on Term.

```scheme
(define-rule
 (check (extend ctx x A) t B)
 (check ctx (lambda (x) t) (-> A B)))
```

## (λ2) System F

> Term depends on Type.

```scheme
(define-rule ;; Maybe wrong
 (check ctx t B)
 (check ctx (forall (A) t) (Pi ((A Type)) B)))

(claim id (forall (A) (-> A A)))
(define id (lambda (x) x))
```

## (λω) System Fω

> Type depends on Type.

```scheme
(claim Tree (-> Type Type))
(define (Tree A)
  (Pi ((B Type))
    (-> (-> A B) (-> B B B) B)))
```

## (λP) Lambda-P

> Type depends on Term.

```scheme
(define-rule
 (check (extend ctx x A) B Type)
 (check ctx (Pi ((x A)) B) Type))
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

- `(Prop, Prop)` -- terms can depend on terms.
- `(Prop, Type)` -- types can depend on terms.
- `(Type, Prop)` -- terms can depend on types.
- `(Type, Type)` -- types can depend on types.

Note that, the naming -- `Prop` and `Type` are borrowed from Coq.

```scheme
(define-rule product ;; pi abstraction
  (check ctx A SortRight)
  (check (extend ctx x A) B SortLeft)
  (check ctx (Pi ((x A)) B) SortLeft))
```

The structural relation between `lambda` and `Pi`
is truncated in the above rule,
we do not have `Pi2` and the following rule:

```scheme
(define-rule product-structural
  (check ctx A SortRight)
  (check (extend ctx x A) B SortLeft)
  (check (extend ctx x A) B B2)
  (check ctx (Pi ((x A)) B) (Pi2 ((x A)) B2)))
```

**Why truncate the rule for `Pi` instead of keep it structural?**

Maybe this is because when talking about `(Pi ((x A)) B)`'s type,
we do not want to know its structural details,
we just want to say it is a type.

If we have the following:

- (Pi 0) lambda
- (Pi 1) Pi
- (Pi 2) ...

We will also need `(Type 0)`, `(Type 1)`, ...
and subtype relation `(Pi n) < (Type n)`,
because if we can not view `(Pi)` as `Type`
we will not be able to write list of functions -- `(List (-> A B))`.

```scheme
(define-rule abstraction
  (check ctx A SortRight)
  (check (extend ctx x A) B SortLeft)
  (check (extend ctx x A) b B)
  (check ctx (lambda ((x A)) b) (Pi ((x A)) B)))
```

Another difference between lambda and Pi is that,
we want to write `(lambda (x) b)` instead of `(lambda ((x A)) b)`,
i.e. we want typed lambda a al Curry.

# Comparison between the systems

## (λ→)

```scheme
(check (list (tuple A Prop))
  (lambda ((x A)) x) (Pi ((x A)) A))

(check (list (tuple A Prop))
  (lambda ((x A)) x) (-> A A))
```

## (λ2)

```scheme
(define Absurd (Pi ((A Prop)) A))

(check (list) ;; empty context
  (lambda ((Anything Prop) (falsehood Absurd)) (falsehood Anything))
  (Pi ((Anything Prop) (falsehood Absurd)) Anything))
```

## (λω)

```scheme
(claim and (-> Prop Prop Prop))
(define and
  (lambda ((A Prop) (B Prop) (C Prop))
    (-> (-> A B C) C)))
```

From a computing point of view, λω is extremely strong,
and has been considered as a basis for programming languages.

## (λP)

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

## (λC)

The calculus of constructions has both the predicate expressiveness of λP
and the computational power of λω,
hence why λC is also called λPω,
so it is very powerful, both on the logical side
and on the computational side.

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
    ``` scheme
    (set (tuple Prop Prop Prop))
    <= R <=
    (set (tuple Prop Prop Prop)
         (tuple Prop Type Type)
         (tuple Type Prop Prop)
         (tuple Type Type Type))
    ```

| System of the cube | Logical System                             |
|--------------------|--------------------------------------------|
| λ→               | (First-order) Propositional Calculus       |
| λ2                | Second-order Propositional Calculus        |
| λω               | Weakly Higher Order Propositional Calculus |
| λω               | Higher Order Propositional Calculus        |
| λP                | (First order) Predicate Logic              |
| λP2               | Second-order Predicate Calculus            |
| λPω              | Weak Higher Order Predicate Calculus       |
| λC                | Calculus of Constructions                  |

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

## automath-like

let's try a dependent system

```js
<exp> = <var> | <fn> | <ap>
<fn> = [<var>: <exp>] <exp>
<ap> = { <exp> } <exp>
```

- note that, in this `<var> <var>` is not a `<exp>`
  which is the only different from jojo calculus

we can use `<fn>` as type of `<fn>`

```js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : [x: A] C
```

we can use `(- ...)` as type of `<fn>`

```js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : (- A) C
```

we can use pi type as type of `<fn>`

```js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : pi[x: A] C
```

## pure type system

if we use pi type, we can get something like the pure type system

suppose we have the following judgments,

```js
<symbel> in sorts
(<symbel>, <symbel>) in axioms
(<symbel>, <symbel>, <symbel>) in rules
```

then we can formulate the following rules,

```js
(s1, s2) in axioms
---------------------- (axiom)
ctx_empty |- s1 : s2

exists s1, s2 in sorts
(s1, s2, s3) in rules
ctx |- A: s1
ctx, x: A |- B: s2
------------------------ (pi abstraction)
ctx |- pi[x: A] B : s3

exists A
ctx |- f: pi[x: A] B
ctx |- a: A
------------------------ (lambda application)
ctx |- { a } f : subst(B, x, a)

exists s1, s2 in sorts
ctx |- A: s1
ctx, x: A |- b: B
ctx, x: A |- B: s2
---------------------------------- (lambda abstraction)
ctx |- [x: A] b : pi[x: A] B

exists B2
ctx |- A: B2
beta_reduction(B2, B1)
exists s in sorts
ctx |- B1: s
---------------------- (conversion)
ctx |- A: B1
```

## lambda as type

let's only write application and abstraction for simplicity

```js
exists A
ctx |- f: [x: A] B
ctx |- a: A
------------------------ (lambda application)
ctx |- { a } f : subst(B, x, a)

ctx, x: A |- b: B
---------------------------------- (lambda abstraction)
ctx |- [x: A] b : [x: A] B
```

what is the implication of this
simple structural lambda abstraction rule?

**We need to implement this!**
