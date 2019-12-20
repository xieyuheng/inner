---
title: Computer Science and Homotopy Theory
---

# Computer Science and Homotopy Theory

- Place: IAS
- Date: 2011

------

## Type theory

Type theory is developed as an alternative to logic and set theory.
Type theory is some kind of combination of logic and set theory.

Set theory uses logical symbols
like `not`, `and`, `or`, `->`,
and `exists`, `forall`.

And in ZFC, we have only two predicate symbols,
one for membership `x in y`,
and one for equivalent `x == y`.

In set theory, a so called set is actually a tree.

Axioms of set theory,

``` js
empty_set : {
  [exists unique x]
  [forall y]
  not(y in x)
}

unit_set : {
  [forall x]
  [exists unique y]
  [forall z in y]
  z == x
}
```
