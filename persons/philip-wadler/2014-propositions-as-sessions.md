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

TODO
