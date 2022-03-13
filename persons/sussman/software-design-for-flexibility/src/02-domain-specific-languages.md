---
title: "2: Domain-Specific Languages"
---

# What is a system of combinators?

A system of combinators is a set of primitive parts
and a set of means of combining parts such that
the interface specifications of the combinations
are the same as those of the primitives.

# Why require combined parts and primitives to have the same interface?

This enables construction without accidental interactions between the parts.

# What are the advantages of combinator system?

Combinator systems have the significant advantage that
they are easy to build and to reason about.

# What are the limitations of combinator system?

TODO We will discuss in section 3.1.5.

# How to build combinator system?

We must identify a set of primitive components
and a set of combinators that combine components
so as to make compound components
with the same interface as the primitive components.

# 2.1.1 Function combinators

We can think of function combinators as implementing wiring diagrams
that specify how a function is built by combining its parts.

For example, functional composition represents a box made of two subboxes
so that the output of the first feeds the input of the second.

A program that implements this idea is straightforward:

```scheme
(define (compose f g)
  (lambda args
    (f (apply g args))))
```

For any language there are primitives,
means of combination, and means of abstraction.
A combinator language defines primitives and means of combination,
inheriting its means of abstraction
from the underlying programming language.

In our example, the primitives are functions,
and the means of combination are the combinators `compose`,
`parallel-combine`, `spread-combine`, and others we may introduce.

## Multiple return values

One can use `values`, `call-with-values`, and `let-values`
to handle function with multiple return values.

The API might comes from the use of CPS as an implementation technique.

But the API is not as easy to use, even comparing to return a list.

The most nature API of multiple return values is in stack-based language.

Note that, "single return value" is a very valuable constraint,
it gives us a semantic in which one expression evaluates to another expression.

The semantic of stack-based language is more complicated,
where we execute a statement to get an effect on the stack.
