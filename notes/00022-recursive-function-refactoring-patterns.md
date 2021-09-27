---
title: Recursive function refactoring patterns
date: 2021-09-27
tags: [recursion, refactoring, scheme]
---

# When to take an extra argument?

``` cicada
(Exp) -> Value
(Ctx, Exp) -> Value
```

When we want to maintain an extra state.

When doing this, the state can not be returned,
it can only be passed down to the same group of receive functions.

# When to return a higher-order function?

``` cicada
(Exp) -> Value
(Exp) -> (Ctx) -> Value
```

Due to currying, this is the same as "taking an extra argument".

This is useful when we wish to view evaluation as a function
from the domain `Exp` to another domain `(Ctx) -> Value`.

# When to return an extra result?

I am not sure.

# When to take a higher-order function?

``` cicada
(Ctx, Exp) -> Value
(Ctx, Exp, Narrator) -> Value
```

When we want to inject a dependency.

Instead of print out information directly,
we can inject a `Narrator` and pass the information to it.

# More patterns and practices

See "The Little Schemer" and "The Seasoned Schemer".
