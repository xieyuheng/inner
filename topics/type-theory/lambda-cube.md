---
title: Lambda Cube
subtitle: Why complicated sorts?
---

(λ→) Simply typed lambda calculus

```scheme
(define-rule
 (check (extend ctx x A) t B)
 (check ctx (lambda (x) t) (-> A B)))
```

(λ2) System F

```scheme
(define-rule ;; Maybe wrong
 (check ctx t B)
 (check ctx (forall (A) t) (Pi ((A Type)) B)))

(claim id (forall (A) (-> A A)))
(define id (lambda (x) x))
```

(λω) System Fω

```scheme
(claim Tree (-> Type Type))
(define (Tree A)
  (Pi ((B Type))
    (-> (-> A B) (-> B B B) B)))
```

(λP) Lambda-P

```scheme
(define-rule
 (check (extend ctx x A) B Type)
 (check ctx (Pi ((x A)) B) Type))
```

(Fω) System Fω

System Fω provides both terms that depend on types and types that depend on types.

(λC) Calculus of constructions

The clear border that exists in λ→ between terms and types is somewhat abolished,
as all types except the universal are themselves terms with a type.

```scheme
(define-grammer
  (<exp>
   <var>
   <sort>
   (<exp> <exp>)
   (lambda ((<var> <exp>)) <exp>)
   (Pi ((<var> <exp>)) <exp>)))
```

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

;; NOTE The difference between the systems is in the pairs of sorts
;; that are allowed in the following two typing rules:

;; NOTE The following rule means `SortLeft` can depend on `SortRight`.
;;   (Prop, Prop) -- terms can depend on terms.
;;   (Prop, Type) -- types can depend on terms.
;;   (Type, Prop) -- terms can depend on types.
;;   (Type, Type) -- types can depend on types.

(define-rule product
  (check ctx A SortRight)
  (check (extend ctx x A) B SortLeft)
  (check ctx (Pi ((x A)) B) SortLeft))

;; NOTE The structural relation between `lambda` and `Pi`
;;   is truncated in the above rule,
;;   we do not have `Pi2` and the following rule:
(define-rule product-structural
  (check ctx A SortRight)
  (check (extend ctx x A) B SortLeft)
  (check (extend ctx x A) B B2)
  (check ctx (Pi ((x A)) B) (Pi2 ((x A)) B2)))

;; Maybe this is because when talking about `(Pi ((x A)) B)`'s type,
;; we do not want to know its structural details,
;; we just want to say it is a type.

;; If we have the following:
;;   (Pi 0) lambda
;;   (Pi 1) Pi
;;   (Pi 2) ...
;; We will also need `(Type 0)`, `(Type 1)`, ...
;; and subtype relation `(Pi n) < (Type n)`,
;; because if we can not view `(Pi)` as `Type`
;; we will not be able to write list of functions -- `(List (-> A B))`.

(define-rule abstraction
  (check ctx A SortRight)
  (check (extend ctx x A) B SortLeft)
  (check (extend ctx x A) b B)
  (check ctx (lambda ((x A)) b) (Pi ((x A)) B)))

;; Another difference between lambda and Pi is that,
;; we want to write `(lambda (x) b)` instead of `(lambda ((x A)) b)`.
```
