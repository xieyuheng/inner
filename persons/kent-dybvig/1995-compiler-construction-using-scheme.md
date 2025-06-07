---
title: compiler construction using scheme
authors: [erik hilsdale, michael ashley, kent dybvig, daniel friedman]
year: 1995
---

# My Motive

[2025-06-07] 已经在看 jeremy siek
和 abdulaziz ghuloum 的编译器课程了，
而这篇论文介绍的是更早的 dan 风格的编译器课程。

# Abstract

>  This paper describes a course in compiler design that focuses on
>  the Scheme implementation of a Scheme compiler that generates
>  native assembly code for a real architecture. The course is
>  suitable for advanced undergraduate and beginning graduate
>  students. It is intended both to provide a general knowledge about
>  compiler design and implementation and to serve as a springboard to
>  more advanced courses. Although this paper concentrates on the
>  implementation of a compiler, an outline for an advanced topics
>  course that builds upon the compiler is also presented.

# 1 Introduction

> A good course in compiler construction is hard to design. The main
> problem is time. Many courses assume C or some similarly low-level
> language as both the source and implementation language.  This
> assumption leads in one of two directions. Either a rich source
> language is defined and the compiler is not completed, or the source
> and target languages are drastically simplified in order to finish
> the compiler.

# 2 The Compiler

> The compiler accepts a subset of legal Scheme programs as defined in
> the Revised 4 Report, a subset strong enough to compile itself.

那个时候还是 r4rs。

> - the language is syntactically restricted so that
>   the only numbers accepted are integers in a bounded range,
> - all lambda expressions have a fixed arity, i.e., no rest arguments.
> - programs cannot have free variables other than
>   references to primitives in operator position,
> - symbols cannot be interned at runtime,
> - first-class continuations and I/O are not supported,
> - derived syntax is not directly supported,
> - garbage-collection is not provided, and
> - the runtime library is minimal.

> The compiler is described below, back to front. The run-time
> execution model is described first.  The representation of the
> environment and control fixes the target of the compiler and
> motivates the structure of the compiler's intermediate language.
> The code generator generates its assembly code from the intermediate
> language, and the front end translates core Scheme programs to
> intermediate programs.

这看来与 jeremy siek 和 abdulaziz ghuloum 的课程都不同。

# 2.1 The Run-time Model

这里所描述的 run-time model，
没有 forth 的 run-time model 简单，
但是支持 closure。

与 forth 相比的主要区别就是，
forth 用两个 stack，
而一般的 run-time 用一个 stack。

也可以基于这里所描述的 run-time model 写一个 vM，
作为 intermediate language 的 specification。

# 2.2 Code Generation

TODO

# 3 The Course

TODO

# 4 Advanced Coursework

TODO

# 5 Conclusions

TODO
