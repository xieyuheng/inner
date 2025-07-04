---
title: Model theory can be implemented by class and object
date: 2021-08-27
---

# The implementation

[Model theory][] studies the relation between formal theory and model.

[model theory]: https://en.wikipedia.org/wiki/Model_theory

In a general programming language,
we can implement this relation by class and object,
or say, mapping this relation to relation between class and object.

Take equivalence relation for example:

- We use TypeScript's interface (record type) instead of class,
  because it is too verbose to define simple class in TypeScript.

We prepare by defining basic functions in boolean algebra, for first-order logic.

``` typescript
function imply(x: boolean, y: boolean): boolean {
  return !x || y
}

function iff(x: boolean, y: boolean): boolean {
  return imply(x, y) && imply(y, x)
}
```

Then the theory of equivalence relation can be defined by a polymorphic function type,
and record of laws (which can be used to test whether a given function is equivalence relation).

``` typescript
type Equivalence<T> = (a: T, b: T) => boolean

function EquivalenceLaws<T>(eq: Equivalence<T>) {
  return {
    reflexive: (x: T) => eq(x, x),
    symmetric: (a: T, b: T) => iff(eq(a, b), eq(b, a)),
    transitive: (a: T, b: T, c: T) => imply(eq(a, b) && eq(b, c), eq(a, c)),
  }
}
```

Take group theory for another example (using the equivalence relation defined above):

``` typescript
interface Group<G> {
  eq: Equivalence<G>
  mul(a: G, b: G): G
  id: G
  inv(a: G): G
}

function GroupLaws<G>({ eq, mul, id, inv }: Group<G>) {
  return {
    mul_associative: (a: G, b: G, c: G) => eq(mul(mul(a, b), c), mul(a, mul(b, c))),
    id_respect_mul: (a: G) => eq(mul(id, a), a) && eq(mul(a, id), a),
    inv_respect_mul: (a: G) => eq(mul(inv(a), a), a) && eq(mul(a, inv(a)), a),
  }
}
```

Given this definition, `Group<G>` is the theory,
and its instances are the models of the theory.

# The term "interpretation"

The term "interpretation" is used to describe the relation between theory and model,
but unlike "interpreter of a lambda calculus",
there is no computation in the "interpretation" in model theory.

# Change the "result type"

In the theory above, `boolean` is used as the result type of axioms,
we can change the result type to other more complicated types, to get [many-valued logic][].

[many-valued logic]: https://en.wikipedia.org/wiki/Many-valued_logic

# Meta theorems

The meta theorems (such as soundness and completeness) about a logic theory,
is defined by the relation between pure syntactic inference in the theory and its models' properties.

But note that, the definition also depends on the algebra of the "result type" -- Boolean algebra.

For example, in type theory, the result type of axioms is the algebra of category.

How does this influence our consideration of meta theorems?
