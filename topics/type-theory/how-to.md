---
title: How to ____
---

# how to make progress in programming language design

newton observes phenomenon in nature,
and only try to give theory to explain the phenomenon.
the theory is always ready to be improved.
theory can be improved if we can compare two theories
and say one is better than the other.
it is by this way progress can be made.

what phenomenon are we observing when designing programming languages?
to make progress, how should we compare two languages?

we must constrain our designs to one genera
(applicative, concatenative)

we must eliminate superficial attributes
(module system, concrete syntax, closure (consistent replacement or substitution))

we say A is better than B when considering attribute X

we can use concept lattice to conclude the relations between our designs

# how lambda calculus handles branching

in church encoding of bool

let true = (x) => (y) => x
let false = (x) => (y) => y

true select the first arg
false select the second arg

they can handle branching
when used as value
