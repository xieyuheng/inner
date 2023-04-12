---
title: Computer Science and Homotopy Theory
author: Vladimir Voevodsky
year: 2011
venue: Institute for Advanced Study
---

[ [YOUTUBE](https://www.youtube.com/watch?v=UvDeVqzcw4k) ]

Type theory is developed as an alternative to logic and set theory.
Type theory is some kind of combination of logic and set theory.

Set theory uses logical symbols

- like `not`, `and`, `or`, `->`,
- and `exists`, `forall`.

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

with the axioms above we can build trees from `empty_set`.

Two sets are equal iff they have the same elements,
which means equality is defined as equality of structure of trees.

Two constrains to build trees that represent sets.

- A set can not have duplicated elements,
  which means all the branches of the tree are different
  (in the sense of the equality defined above).

- The depth of the tree must be finite.
  Note that, the breadth might be not finite (for example the set of natural numbers).

------

How does one encode group theory in set theory?

We start from the definition of pair.

``` js
pair(x, y) := {x, {x, y}}
```

Note that if `x == y`, we will have `{x, {x, x}}`,
if we use `eval` to define semantics,
and assume `eval({x, x}) == {x}`,
the above definition of pair will be ok.

From `pair` we can define `tuple`,
and define a group as a tuple of sets.

Before we do so, we also need to encode map as set.

------

I am not trying to make fun of you,
that is precisely how mathematics is suppose to encoded in set theory.

Computer scientists need to work with formalization of mathematical theories,
but obviously it is impossible to work with this kind of formalization,
so they invented something called type theory.

In logic, elementary pieces are sentences,
in type, theory elementary pieces are sequents.

``` js
x1 : T1, x2 : T2, ..., xn : Tn |- r : R
// or
x1 : T1, x2 : T2, ..., xn : Tn |- r(x1, x2, ..., xn) : R(x1, x2, ..., xn)
```

in which `x1, x2, ..., xn` are variables,
and `T1, T2, ..., Tn` and `r, R` are expressions.

The way one can work with those things is that
there are rules (inference rules) which permit one
to construct new legal sequents from old ones.

- Xie: In normal implementations of type systems,
  the above level of abstraction is always omitted,
  one often define expressions and implement `check` and `infer` directly.

------

The second half of the title -- "Homotopy Theory",
is about providing semantics to type theories.

People have been looking for good semantics for type theories for many many years.

The semantics where types are interpreted as sets
and members of a type interpreted as members of the set
never worked very well.

What we discover recently is that the correct semantics of type theories
is to interpret types as homotopy types.

Then a lot of rich syntaxes of type theories find its reflection
in the structure of homotopy category in totally amazing ways.

The more we work on it, the more we understand that
this is the way to approach things like foundations of mathematics.
