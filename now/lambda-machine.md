---
title: Lambda Machine
---

# Choosing syntax for lambda expressions

If we use the following syntax for lambda abstraction,

```
(x) => add(1, x)
```

We will need special syntax to write lambda application for it,
instead of just using `f(x)`.

The following use of `( ... )` for grouping
is what I want to avoid.

```
((x) => add(1, x))(2)
```

Maybe we should use explicit `apply`,
in this case syntax for array `[ ... ]` is required.

```
apply((x) => add(1, x), [2])
```

Pure postfix notation requires explicit `apply`,
but no need for syntax for array.

```
2 [ (x) 1 x add ] apply
```

In this case, if we manipulate manually,
we will write

```
2 (x₁) 1 x₁ add
```

for

```
2 add1
```

instead of

```
2 [ (x) 1 x add ] apply
```

Maybe we should just go back to sexp,
so that everything is simple and clear.

```
((lambda (x) (add 1 x)) 2)
```

# Lambda Calculus

The core of the λ-calculus is based on little more than a well deﬁned
concept of variables, variable scoping and the orderly substitution of
variables by expressions.

The λ-calculus is a **closed language**, meaning that its semantics
can be deﬁned on the basis of the equivalence of expressions (or
terms) of the calculus itself.

TODO Naive substitutions suﬃce to weakly normalize since other than
top-level reductions, including those of partial applications, are
ruled out, which precludes name clashes.
