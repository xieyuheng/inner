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

| paper | lisp          | meaning                            |
|-------|---------------|------------------------------------|
| X     | X             | propositional variable             |
| X^⊥   | (dual X)      | dual of propositional variable     |
| A ⨂ B | (times A B)   | "times", output A then behave as B |
| A ⅋ B | (par A B)     | "par", input A then behave as B    |
| A ⨁ B | (plus A B)    | "plus", select from A or B         |
| A & B | (with A B)    | "with", offer choice of A or B     |
| !A    | (of-course A) | "of course!", server accept        |
| ?A    | (why-not A)   | "why not?", client request         |
| ∃X.B  | (forall X B)  | existential, output a type         |
| ∀X.B  | (exists X B)  | universal, input a type            |
| 1     | one           | unit for ⨂                         |
| ⊥     | bottom        | unit for ⅋                         |
| 0     | zero          | unit for ⨁                         |
| ⊤     | top           | unit for &                         |

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
| x().P       | (do (@ x))                           | empty input          |
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

Fig. 1. CP, classical linear logic as a session-typed process calculus.

```scheme
(define-inference-rule axiom-rule
  (obey (link w x) [w (dual A)] [x A]))

TODO
```

## 3.1 Structural rules

TODO

## 3.2 Output and input

TODO

## 3.3 Selection and choice

TODO

## 3.4 Servers and clients

TODO

## 3.5 Polymorphism

TODO

## 3.6 Commuting conversions

TODO

## 3.7 Cut elimination

TODO

# 4 A session-typed functional language

TODO

# 5 Related work

这里有很多关于 session type 的引用。

TODO
