---
title: Object oriented programming and functional programming are orthogonal
date: 2021-09-09
---

Object oriented programming and functional programming are orthogonal,
they are orthogonal language features, (or anti-features).

- structural programming -- anti goto
- functional programming -- anti side effects
- object oriented programming -- anit function pointers (a little farfetched)
  - When doing dependency injection, do not use function pointers,
    instead, use record type to pass group of high order functions as argument,
    and to return group of high order functions as result.

# References

This summary is learned from Bob Martin.

- [Robert C Martin - Functional Programming; What? Why? When?](https://www.youtube.com/watch?v=7Zlp9rKHGD4)

Note that, Bob Martin get this "anti-feature" series,
because his historical view starts from structural programming.
