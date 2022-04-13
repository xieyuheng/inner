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
passing a record of functions to a higher order function,
which returns a record of functions (instance of the class).

Thus dependency injection can be viewed as,
using very high order functions,
with the help of record types.

## About Lambda Encoding

We know that lambda can encode inductive datatypes,
but how about record types (with OOP in mind)?

We can view function as special record (object) with an `apply` method,
but how about the other way around -- encoding record by lambda?

Maybe its not possible, because record is about _polymorphism_,
i.e. sending the same _message_ to different objects might resolve different methods.

While there is not polymorphism in lambda calculus.

We can encode _list_ by lambda,
and use them to pass a group of functions to higher order function.
But doing so, we will be using index (instead of named message) to get back methods.

In record, the order does not matter, but in list, the order matters.

We might encode string by lambda and use them as message,
but such encoding will not be practical at all.

# We'd better learn by implementing and teaching

I define better learning as more clear understanding.

Clear understanding allow us to use the techniques in new ways.

I believe that the best way to learn something is to implement it.

Because during implementation,
we have to explain our understanding to a machine,
which help us testing our understanding to force it to be clear.

And I believe learning is even better if we teach it to others.

Because the act of expressing ourself will yield results
that help us review our understanding in the future.

I define teaching as try to express to be understood by others,
and enjoy feedback from others.

Forms of teaching:

- Dialogue
- Puzzle

Note that, feedback is the key, both for learner and the teacher.
