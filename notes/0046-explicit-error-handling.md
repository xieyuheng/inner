---
title: Explicit error handling
date: 2023-09-08
---

The programming language [Zig](https://ziglang.org)
uses explicit error handling,
by using union type between normal value and error
to represent a function that might fail,
and introducing a new way of using `try` and `catch`,
to early return a value if it is an error.

While in C++, the keywords `try` and `catch`
are used in dual but isomorphic way.

We say Zig has explicit error handling,
while C++ has implicit error handling,
not because the syntax is use in a dual way
(since they are isomorphic, there is no difference),
but because in Zig, exception are expressed
explicitly in the type system (by union type),
while in C++ a function throw an exception or not,
is not expressed in the type system.

# Limitation of Zig's way

One limitation of using union type to express error,
is that in the language, every function must return one value,
thus a stack-based language can not use this way.

But in a stack-based language,
we can still express exception in the type system,
so that our handling of error is still explicit.
