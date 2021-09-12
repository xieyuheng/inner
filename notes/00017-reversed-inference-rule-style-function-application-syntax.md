---
title: Reversed-inference-rule style function application syntax
date: 2021-09-12
---

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
{ T
  ---- f
  A
  ---- a }
{ B
  ---- b }
```

# Traditional syntax for writing inference rules

Comparing our syntax with the traditional syntax for writing inference rules:

- It (the traditional syntax) uses concrete syntax ambiguously.
- It does not use closure.
- It uses declarative pattern, like the `(syntax-rules)` of scheme,
  to express common collection like list and map.
