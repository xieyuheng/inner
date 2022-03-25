---
title: My Beliefs
subtitle: Explicitly spoken out to be proven wrong.
author: Xie Yuheng
---

# Knowledge is fixation of belief

After read Charles Sanders Peirce's works,
I **define** knowledge as fixation of belief.

Beliefs are useful for a man, in the sense of
helping him to make decisions in life.

Beliefs might be wrong.

Thus it is useful to explicitly spoken out his beliefs,
prepare them to be proven wrong.

Otherwise he might have wrong beliefs that are implicit,
which will mislead him in life.

# Object Oriented Programming is necessary for writing code that's easy to change

After I read Sandi Metz's two books --
"Practical Object Oriented Design" and "99 bottles of OOP",
I **define** good code structure as easy to change.

I **belief** _SOLID principles_ are necessary
for writing code that's easy to change.

I also **belief** the use of _Design Patterns_ is a good way
of recording knowledge that's easy to share.

## Practical Effects

I want to add (simple single inheritance) OOP to lisp.

## Maybe Wrong, But ...

Note that FP (Functional Programming) is orthogonal to OOP.

If we disallow side-effect in an OOP language, the language became pure FP,

The key technique of OOP -- dependency injection, is still effective in FP,
injecting a dependency to a class' constructor, is just
passing a record of (high order) functions to a (even higher order) function,
which returns a record of functions (instance of the class).

Thus dependency injection can be viewed as,
using very high order functions,
with the help of record types.
