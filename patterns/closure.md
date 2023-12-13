---
title: Closure
---

# Problem

Closure is a technique to solve
the problem of consistent substitution
(or say the problem of lexical scope).

Consistent substitution can be implemented by
recursively renaming, which is not as performant as closure.

# Solution

```
closure = { env, function }
```

To apply a closure to arguments,
is to extend the env with the new bindings,
and evaluate the function body in the new env.

# Problem introduced by closure

In an interpreter, using closure
will make `Value` different from `Exp`.
