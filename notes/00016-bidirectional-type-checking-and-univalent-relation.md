---
title: Bidirectional type checking and univalent relation
date: 2021-09-12
---

# Univalent relation

A [binary relation][] `R: (A, B)` is called **univalent**, if it satisfies

```
forall (x: A, y: B, z: B, R(x, y), R(x, z)) -> y == z
```

It means `R` can be viewed as a function of type `A -> B`,
the definition describe the single-value-ness of function,
i.e. a value in the domain `A` determines a single value in codomain `B`.

- Note that, in relation database, the column `A` is the **primary key** of `R`.
  and if `R3: (A, B, C)` can be viewed as function `(A, B) -> C`,
  `(A, B)` will be the **composite key** of `R3`.

For n-ary relation, we can summarize its single-value-ness,
by saying "it can be viewed as a function of type __".

- e.g. `R4: (A, B, C, D)` can be viewed as a function of type `(A, B) -> (C, D)`.

[binary relation]: https://en.wikipedia.org/wiki/Binary_relation

# Bidirectional type checking

Bidirectional type checking is about translating a group of inference rules to a type checking function.

Let's take [simply typed lambda calculus][] as a example.

[simply typed lambda calculus]: https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus

First, we define its `Exp` and `Type` datatype:

```
datatype Exp {
  var(name: String): Exp
  ap(target: Exp, arg: Exp): Exp   // function application
  fn(name: String, ret: Exp): Exp  // function abstraction
}

datatype Type {
  atomic(name: String): Type
  arrow(arg_t: Type, ret_t: Type): Type
}
```

We use the relation `Check: (Ctx, Exp, Type)` to describe typing judgment of the inference rules.

```
Check(ctx, f, arraw(A, B))
Check(ctx, a, A)
----------------------- function-elim
Check(ctx, ap(f, a), B)

Check(ctx.extend(x, A), ret, B)
----------------------------------- function-intro
Check(ctx, fn(x, ret), arraw(A, B))
```

And we try to translate it into a type checking function:

- `check: (Ctx, Exp, Type) -> Bool`

We observe `function-elim`, and find that,
we must be able to infer the type of `f` to get `arraw(A, B)`.

```
check(ctx, ap(f, a), B) {
  arraw(A, B) = infer(ctx, f) // we must be able to infer the type of `f`
  ...
}
```

Thus, we must translate the relation `check` into a pair of functions:

- `check: (ctx, exp, type) -> bool`
- `infer: (ctx, exp) -> type`

we also find that, if we can infer `f`, we can also infer `ap(f, a)`:

```
infer(ctx, ap(f, a)) {
  arraw(a, b) = infer(ctx, f)
  check(ctx, f, arraw(a, b))
  check(ctx, a, a)
  return b
}
```

For `function-intro`, we try to implement `infer` again, but we do not know the type of `x`.

- `Check: (Ctx, Exp, Type)` is not univalent on its third argument `Type`.

```
infer(ctx, fn(x, ret)) {
  ... // We do not know the type of `x`
}
```

But we can implementation `check` For `function-intro`:

```
check(ctx, fn(x, ret), arraw(A, B)) {
  check(ctx.extend(x, A), ret, B)
}
```

Here is the story:

- To solve the problem:

  > (A) translate `Check` to `check`.

- We in turn solve the problem: translate:

  > (B) `Check` to `check` & `infer`.

- We generalize the problem of finding `infer`:

  > (C) given a relation, we want to check whether it is univalent in a given column.

  We do this generalization, because of inference rules are relations.

Problem: Can we find a method to solve (C) for any relation?

For example, why in the case `function-intro`, `Check` is not univalent in its third column?

```
Check(ctx.extend(x, A), ret, B)
----------------------------------- function-intro
Check(ctx, fn(x, ret), arraw(A, B))
```

Maybe we can do univalent check, by propagating what is unknown, like the following:

- Suppose `arrow(A, B)` is unknown.
- Thus `A` is unknown.
- Thus `A` should not occur in `ctx.extend(x, A)`.
- Thus univalent check fail.

# Categorical semantics of bidirectional type checking

Categorical semantics can help us understand
the relationship between intro-rules and elim-rules,
thus will also help us understand  bidirectional type checking.
