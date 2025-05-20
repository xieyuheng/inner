---
title: propositions as sessions
author: philip wadler
year: 2014
---

# My Motive

[2025-04-30] 在实现无类型的 inet 之后，
想要再次考虑类型系统。
了解到 [par-lang](https://github.com/faiface/par-lang)
是对 linear logic 的完整解释，
并且可以编译到 inet combinators。
par-lang 就是受这篇论文的启发，所以要读一下。

# 3 Classical linear logic as a process calculus

> Types. Propositions, which may be interpreted as session types,
> are defined by the following grammar:

| paper | lisp            | meaning                            |
|-------|-----------------|------------------------------------|
| X     | X               | propositional variable             |
| X^⊥   | (dual-t X)      | dual of propositional variable     |
| A ⨂ B | (times-t A B)   | "times", output A then behave as B |
| A ⅋ B | (par-t A B)     | "par", input A then behave as B    |
| A ⨁ B | (plus-t A B)    | "plus", select from A or B         |
| A & B | (with-t A B)    | "with", offer choice of A or B     |
| !A    | (of-course-t A) | "of course!", server accept        |
| ?A    | (why-not-t A)   | "why not?", client request         |
| ∃X.B  | (forall X B)    | existential, output a type         |
| ∀X.B  | (exists X B)    | universal, input a type            |
| 1     | one-t           | unit for ⨂                         |
| ⊥     | bottom-t        | unit for ⅋                         |
| 0     | zero-t          | unit for ⨁                         |
| ⊤     | top-t           | unit for &                         |

> Processes. Our process calculus is a variant on the pi-calculus
> (Milner et al. 1992). Processes are defined by the following
> grammar:

| process     | lisp                                 | meaning              |
|-------------|--------------------------------------|----------------------|
| x↔y         | (link x y)                           | link                 |
| νx:A.(P\|Q) | (fresh ([x A]) (run P Q))            | parallel composition |
| x[y].(P\|Q) | (do (x y) (run P Q))                 | output               |
| x(y).P      | (do (@ x y) P)                       | input                |
| x[inl].P    | (do (x inl) P)                       | left selection       |
| x[inr].P    | (do (x inr) P)                       | right selection      |
| x.case(P,Q) | (choice [(@ x inl) P] [(@ x inr) Q]) | choice               |
| !x(y).P     | (! (@ x y) P)                        | server accept        |
| ?x[y].P     | (! (x y) P)                          | client request       |
| x[A].P      | (do (x A) P)                         | output a type        |
| x(X).P      | (do (@ x X) P)                       | input a type         |
| x[].0       | (do (x))                             | empty output         |
| x().P       | (do (@ x) P)                         | empty input          |
| x.case()    | (choice)                             | empty choice         |

这里说：

> in `x[y].(P|Q)`, name `y` is bound in `P` (but not in `Q`),
> and in `x(y).P`, name `y` is bound in `P`.

但是在 Milner 的 pi-calculus 中，output 是不引入 binding 的。

> unlike pi-calculus, both output and input names are bound.
> ... `x[y].P` in our calculus behave like `νy.x<y>.P` in pi-calculus.

也就是说 `x[y].(P|Q)` 不是 `(do (x y) (run P Q))`，
而是 `(run (fresh (y) (x y) P) Q)`。

> Judgements take the form
> `P |- x1: A1, ..., xn: An`
> indicating that process `P` communicates along each channel
> named `xi` obeying the protocol specified by `Ai`.

```scheme
(obey P [x1 A1] ... [xn An])
(obey P . (record [x1 A1] ... [xn An]))
```

> Erasing the process and the channel names from the above yields
> `|- A1, ..., An` and applying this erasure to the rules in Figure 1
> yields the rules of classical linear logic, as given by Girard (1987).

```scheme
(sequent A1 ... An)
```

在描述 inference rule 时，我将使用如下语法：

```scheme
(define-inference-rule <rule-name>
  <conclusion-judgement>
  <premise-judgement>
  ...)
```

## 3.1 Structural rules

```scheme
(define-inference-rule axiom-rule
  (obey (link w x) [w (dual-t A)] [x A]))

(define-inference-rule cut-rule
  (obey (fresh ([x A]) (run P Q)) . (merge E1 E2))
  (obey P [x A] . E1)
  (obey Q [x (dual-t A)] . E2))
```

## 3.2 Output and input

```scheme
(define-inference-rule output-rule
  (obey (run (fresh (y) (x y) P) Q) [x (times-t A B)] . (merge E1 E2))
  (obey P [y A] . E1)
  (obey Q [x B] . E2))

(define-inference-rule input-rule
  (obey (do (@ x y) R) [x (par-t A B)] . E)
  (obey R [y A] [x B] . E))
```

> Example. We give a series of examples inspired by Internet commerce,
> based on similar examples in Caires & Pfenning (2010). Our first
> example is that of a sale, where the client sends a product name and
> credit card number to a server, which returns a receipt.

```scheme
(claim buy (-x name-t credit-t (-o receipt-t bottom-t)))
(claim sell (-o name-t credit-t (-x receipt-t one-t)))

(define (buy x)
  (fresh (name)
    (x name)
    (run
      (put-name name)
      (fresh (credit)
        (x credit)
        (run
          (put-credit credit)
          (do (@ x receipt)
              (@ x)
              (get-receipt receipt)))))))

(define (sell x)
  (@ x name) (@ x credit)
  (fresh (receipt)
    (x receipt)
    (run
      (compute name credit receipt)
      (do (x)))))
```

为什么要 `fresh` 一个 channel 出来然后再 send 或 receive，
而不是直接 send 或 receive？

```scheme
(claim put-name name-t)
(define (put-name x) (x "tea"))

(claim put-credit credit-t)
(define (put-credit x) (x 123))

(claim get-receipt receipt-t)
(define (get-receipt x)
  (@ x receipt)
  ...)

(define (buy x)
  (put-name x)
  (run
    (put-credit x)
    (get-receipt x)))
```

`(claim buy protocol)` 其实是说，`buy` 的参数是一个 channel，
这个 channel 满足 protocol。
是否应该直接把 `buy` 当作 channel？

```scheme
(claim buyer (-x name-t credit-t (-o receipt-t bottom-t)))
(claim seller (-o name-t credit-t (-x receipt-t one-t)))

(define buyer
  (buyer "tea")
  (buyer 123)
  (@ buyer receipt)
  (... receipt))

(define seller
  (@ seller name)
  (@ seller credit)
  (let ([receipt (compute name credit)])
    (seller receipt)))
```

## 3.3 Selection and choice

```scheme
(define-inference-rule select-left-rule
  (obey (do (x inl) P) [x (plus-t A B)] . E)
  (obey P [x A] . E))

(define-inference-rule select-right-rule
  (obey (do (x inr) P) [x (plus-t A B)] . E)
  (obey P [x B] . E))

(define-inference-rule choice-rule
  (obey (choice [(@ x inl) P] [(@ x inr) Q]) [x (with-t A B)] . E)
  (obey P [x A] . E)
  (obey Q [x B] . E))
```

## 3.4 Servers and clients

```scheme
(define-inference-rule server-accept-rule
  (obey (! (@ x y) P) [x (of-course-t A)] . (why-not-t E))
  (obey P [y A] . (why-not-t E)))

(define-inference-rule client-request-rule
  (obey (! (x y) P) [x (why-not-t A)] . E)
  (obey P [y A] . E))

(define-inference-rule weaken-rule
  (obey P [x (why-not-t A)] . E)
  (obey P . E))

(define-inference-rule contract-rule
  (obey (subst P x* x) [x (why-not-t A)] . E)
  (obey P [x (why-not-t A)] [x* (why-not-t A)] . E))
```

## 3.5 Polymorphism

```scheme
(define-inference-rule exists-rule
  (obey (do (x A) P) [x (exists X B)] . E)
  (obey P [x (subst B X A)] . E))

(define-inference-rule forall-rule
  (obey (do (@ x X) P) [x (forall X B)] . E)
  (obey P [x B] . E))

(define-inference-rule one-rule
  (obey (do (x)) [x one-t]))

(define-inference-rule bottom-rule
  (obey (do (@ x) P) [x bottom-t] . E)
  (obey P . E))

;; no rule for zero

(define-inference-rule top-rule
  (obey (choice) [x top-t] . E))
```

## 3.6 Commuting conversions

TODO

## 3.7 Cut elimination

TODO

# 4 A session-typed functional language

TODO

# 5 Related work

这里有很多关于 session type 的引用。

TODO
