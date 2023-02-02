---
title: Zermelo–Fraenkel Set Theory
---

# Note

## Info

- [Lectures by Richard E. Borcherds](https://www.youtube.com/playlist?list=PL8yHsr3EFj52EKVgPi-p50fRP2_SbG2oi)
- [Wikipedia / Zermelo–Fraenkel set theory](https://en.wikipedia.org/wiki/Zermelo%E2%80%93Fraenkel_set_theory)
- [Wikipedia / Set theory](https://en.wikipedia.org/wiki/Set_theory)

## View set as tree

It is helpful to view a set as a rooted tree (with unordered children).

- Can we say, ZFC set theory axioms is about tree and recursion?

## Pattern matching between sets

How to do pattern matching between sets?
Where the elements of a set might have pattern variables.

We need to use this in hypergraph rewriting.

Is it necessary to compute the permutation?
Is there any smart way of doing this?
Because permutation which is very costly -- `n!`.

# 1. Axiom of extensionality

Two sets are equal (are the same set) if they have the same elements.

```cicada
let Extensionality = (x, y) =>
  (forall (z) Equivalent(In(z, x), In(z, y))) -> Equal(x, y)
```

If we view a set as a tree,
this axiom is about the unordered-ness of a node's children.

- Think about implementing set by list,
  we can define `setEqual` by `listMember`,
  which will forget the orderness and element-duplication of list.

Because `In` is the only relation in set theory,
if we define `Equal` as by substitution principle
(equivalent relation can be proved).

```cicada
let Equal = (x, y) => And(
  forall (z) Equivalent(In(z, x), In(z, y)),
  forall (w) Equivalent(In(x, w), In(y, w)),
)
```

The axiom can be reformulated as

```cicada
let Extensionality = (x, y) =>
  (forall (z) Equivalent(In(z, x), In(z, y))) ->
   forall (w) Equivalent(In(x, w), In(y, w))
```

# 2. Axiom of regularity (also called the axiom of foundation)

Every non-empty set x contains a member y such that x and y are disjoint sets.

```cicada
let NonEmpty = (x) => exists (a) In(a, x)
let Disjoint = (x, y) => not exists (z) And(In(z, x), In(z, y))

let Eegularity = (x) => NonEmpty(x) -> exists (y) And(In(y, x), Disjoint(x, y))
```

Implications:

- No `a in a`.
- No `a in b` and `b in a`.
- We can have `a0 in a1 in a2 ...`, but not `... a2 in a1 in a0`,
  i.e. if we view `in` as an order,
  there is always a minimal element in the chain.
- Every set has an ordinal rank
  ([von Neumann hierarchy](https://en.wikipedia.org/wiki/Von_Neumann_universe)).

Useful for studying models of set theory,
almost never used in everyday mathematics.

Zermelo's 1908 paper does not include this axiom, added later in 1930.

Viewing set as tree, if `a in a` is allowed,
we can not implement `treeEqual` by recursion down the tree,
because there might be not end.

Viewing set as tree, all branches need to be finite in length.

- Not that the limitation comes from "using tree",
  if we use graph instead of tree,
  we can use circle to represent `a in a`.

This axiom is also called the axiom of foundation,
because it asserts all sets are well-founded set.

- In set theory, a set x is called a _well-founded set_ if the set
  membership relation is [well-founded](https://en.wikipedia.org/wiki/Well-founded_relation)
  on the [transitive closure](https://en.wikipedia.org/wiki/Transitive_set) of x.

- Non well-founded sets are used in [non-standard analysis](https://en.wikipedia.org/wiki/Nonstandard_analysis).
