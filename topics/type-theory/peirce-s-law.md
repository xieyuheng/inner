---
title: peirce's law
---

(((x) -> y) -> x) -> x
is always true

because for it to be false
its consequent must be false
and its antecedent must be true

- if `(p) -> q == false`
  we know `p == true` and `q == false`

x == false
(((x) -> y) -> x) == true

because its antecedent is true, we know

((x) -> y) == false
or
x == true

- if `(p) -> q == true`
  we know `p == false` or `q == true`

since we already know `x == false`
we know `(x -> y) == false`
which means `x == true` and `y == false`

this is a contradiction
because we know `x == false`
