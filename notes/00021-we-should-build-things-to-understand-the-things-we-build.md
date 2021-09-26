---
title: We should build things to understand the things we build
date: 2021-09-27
tags: [cicada, record type, implicit arguments]
---

# 缺乏理解之案例

案例一：

当实现 record type 时，在改用 null object pattern 之后，
我的第一次尝试没能解决 inheritance 的问题。

就像「怎样解题」中 Polya 所说的，
退一步重新清晰叙述所要解决的问题，
对问题有了更清晰的理解，就简单了。

我解决问题的技巧还不够，需要认真学习「怎样解题」。

案例二：

在实现 implicit arguments 时，
在支持 named implicit arguments 时，
初次尝试遇到了 bug，我 debug 了很久，
还以为是因为没有 `Value.deep_walk` result type 而导致的。
（需要作为 infer-elaboration 的返回值的 result type）

其实并不需要用 `Value.deep_walk` 来做 reify，
Closure apply to solution of unification 就是 reify。

# How to build tools for understanding?

- How to make it easier to debug?
- How to make it easier to understand?
  - Not just the structure of the code,
    but also the behavior of the system.
- How to make it easier to test?
- How about "when in doubt, evaluate."?
- How to improve internal error message to help debug?

# Elaboration in type checker

Currently,
the result data type of check-elaboration is `Core`,
the result data type of infer-elaboration is `{ t: Value, core: Core }`.

Maybe we should enrich the result type to propagate explanation of the process of elaboration.

Usage:
- Turn on interactive explanation for a REPL session.
- Mark (or say, decorate) an expression to be log explanation during its elaboration.
