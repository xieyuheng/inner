---
title: Categorical Semantics
---

# Categorical Semantics

## The Plan

First, we implement category theory in a programming language with dependent type and record type.
Where an abstract mathematical structure can be implemented as an abstract class (record type).
- Dependent type will be used to express
  axioms of the abstract mathematical structure,
  which involves equivalent relations.

Specially, category can be implemented as an abstract class, `category_t`.

``` js
class category_t {
  object_t: type
  morphism_t(object_t, object_t): type
  ...
}
```

Second, when we want to implement a type theory,
we define the `type_t` as a concrete datatype.

Each formation rule is a data constructor of the `type_t`.

``` js
datatype type_t {
  case pi_type(...): type_t
  case sigma_type(...): type_t
  ...
}
```

Then we provide categorical semantics to our type theory,
by implement an instance of the abstract class `category_t`,
of which the `object_t` is `type_t`.

``` js
type_category = new category_t {
  object_t = type_t
  ...
}
```

We need to give semantics to inference rules of the type theory.

The validity of a group of introduction rules and elimination rule will be ensured by adjoint functors.

## Set theory

It is important to use the category of sets to gain intuitions
about universal constructions in category in general,
just like in programming, it is important to understand concrete examples
before designing an abstract class.

It it true that every universal constructions can be understand in the category of sets?
It it right that every universal constructions should be understand in the category of sets first?

## Context

Context is the same as telescope (of de Bruijn).

Context is also the same as dependent record
(a dependent record is a chain of dependent sum with named),
thus **a context should be viewed as an object of the category**
(the category we are trying to build, to provide semantics to type theory).

We denote context by `{ x: A; y: B[x]; ... }`.

Dependent function type `pi (x : A) -> B[x]` can be viewed as `{ x: A } -> B[x]`.

Types with free variable only make sense in context,
for example, if `x` and `y` are variables, `unit_t -> f(x, y)` does not make sense,
while `unit_t -> { x: A; y: B[x]; ... } -> f(x, y)` does make sense.

Thus the category we will use in implementation, must not be the category of types,
but be a category parameterized over `ctx` -- `contextual_category_t(ctx, cat)`,
where `ctx` is a context object of the category `cat`.

In `contextual_category_t(ctx, cat)`
(is this the same as the contextual pre-category (a.k.a. C-system)),

``` js
object_t == cat.morphism_t[A: cat.object_t](ctx, A)
morphism_t(A, B) == cat.morphism_t(A, B)
```

From the example above, we know that
**a variable should also be viewed as object of the contextual category**.

We task is to design the universal constructions of
the concept of context and variable in the category.

TODO pullback and substitution of context
