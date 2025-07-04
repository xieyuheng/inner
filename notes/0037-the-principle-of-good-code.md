---
title: The principle of Good Code
date: 2022-05-26
---

> The principle of good code is clear understanding.

Not only holding clear understanding of your code,
but to regain clear understanding after you become a half stranger to it.

Clear understanding means:

- We can change the code to meet new requirements quickly.

- We can prove that our code is correct.

  - "A proof is any completely convincing argument" -- Errett Bishop.

- We are clear about which parts of our code are still not correct yet.

There are many ideas for getting clear understanding.

# Dan Friedman's style

Dan Friedman's style is **small code**.

When the code is so small,
so that you can think through it
while you are taking a shower,
the understanding is very clear.

# Functional programming

The argument of functional programming is that,
a function is easier to be understood
when it has no side effect.

# Object oriented programming

The argument of object oriented programming is that,
with the ability to easily [inverse a dependency](https://en.wikipedia.org/wiki/Dependency_inversion_principle),
we can understand part of the code that we currently care about,
without understanding the whole.

# Frameworks

Given some time to study a framework,
code written in the framework will be familiar to us,
and familiarity helps clear understanding.
