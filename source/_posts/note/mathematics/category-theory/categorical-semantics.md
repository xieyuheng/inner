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
