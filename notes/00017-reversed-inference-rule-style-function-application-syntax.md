---
title: Reversed-inference-rule style function application syntax
date: 2021-09-12
---

# Reversed-inference-rule

By changing function application syntax,
we can mimic the way people write inference rules
(but the direction of inferencing is reversed).

Standard function application syntax:

```
f(a: A): T
g(f(a: A): T, b: B): R
```

Can be translated to reversed-inference-rule style (like Horn clause of Prolog):

```
T
---- f
A
---- a

R
---- g
{
  T
  ---- f
  A
  ---- a
}
{
  B
  ---- b
}
```

# Does reverse necessary?

```
f(a: A): T

// =>

T
---- f
A
---- a

// non-reverse

---- a
A
---- f
T

g(f(a: A): T, b: B): R

// =>

R
---- g
{
  T
  ---- f
  A
  ---- a
}
{
  B
  ---- b
}

// non-reverse

{
  ---- a
  A
  ---- f
  T
}
{
  ---- b
  B
}
---- g
R
```

It seems reversing is not necessary,
and non-reverse is more natural.

When writing inference rules, non-reverse might be natural,
but when writing proofs, reversing make the tree grows down naturally
(without a lot of editing efforts).

# Named arguments

```
g(f(a: A): T, b: B): R

// named arguments

first: {
  ---- a
  A
  ---- f
  T
}
second: {
  ---- b
  B
}
---- g
R

// write function in []

first: {
  ---- [a]
  A
  ---- [f]
  T
}
second: {
  ---- [b]
  B
}
---- [g]
R
```

# Traditional syntax for writing inference rules

Comparing our syntax with the traditional syntax for writing inference rules:

- It (the traditional syntax) uses concrete syntax ambiguously.
- It does not use closure.
- It uses declarative pattern, like the `(syntax-rules)` of scheme,
  to express common collection like list and map.
