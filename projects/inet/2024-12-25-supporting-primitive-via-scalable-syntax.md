---
title: supporting primitive via scalable syntax
date: 2024-12-25
---

scalable syntax 有两种：

- 一种是 lisp（甚至不需要 macro system）。
- 一种是 forth（要求可扩展 word reader）。

how to find rule? by predicate.
can not use type, because i do not understand type system of inet yet.

the builtin primitive functions + builtin primitive datatypes,
(without interaction nets) forms a forth-like language.

- [maybe] implement xforth and let inet be an extension of xforth.

a primitive function is generic in the sense that,
if it is applied to a wire (not-yet-value in NbE),
it will build a eliminatior node.
