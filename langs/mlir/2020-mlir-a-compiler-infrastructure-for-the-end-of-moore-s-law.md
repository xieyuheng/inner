---
title: MLIR a compiler infrastructure for the end of moore's law
author: chris lattner
year: 2020
---

# My Motive

[2026-07-06] SSA 与 propagator model 的设计几乎完全一致，
而 MLIR 算是个 propagator model 设计了一个具体语法。

# 1 Introduction

> A common characteristic of these popular systems is their “one size
> fits all” approach -- a single abstraction level to interface with
> the system:
>
> - the LLVM Intermediate Representation (IR) is roughly “C with vectors”,
> - and JVM provides an “object-oriented type system with a garbage collector” abstraction.
>
> This “one size fits all” approach is incredibly valuable—and in
> practice, the mapping to these domains from ubiquitous source
> languages (C/C++ and Java respectively) is straightforward.

> At the same time, many problems are better modeled at a higher- or
> lower-level abstraction, e.g.  source-level analysis of C++ code is
> very difficult on LLVM IR. We observe that many languages (including
> e.g. Swift, Rust, Julia, Fortran) develop their own IR in order to
> solve domain-specific problems, like language/library-specific
> optimizations, flow-sensitive type checking (e.g. for linear types),
> and to improve the implementation of the lowering process.
> Similarly, machine learning systems typically use “ML graphs” as a
> domain-specific abstraction in the same way.

相比于这些特殊的 IR，MLIR 才是所谓 "one size fits all"。

> While the development of domain specific IRs is a well studied art,
> their engineering and implementation cost remains high.

其实使用 LLVM 和 MLIR 这种通用工具的代价才是最高的。
读这种广告类论文的时候一定要小心。

> The MLIR project aims to directly tackle these programming language
> design and implementation challenges—by making it very cheap to
> define and introduce new abstraction levels, and provide “in the
> box” infrastructure to solve common compiler engineering
> problems. MLIR does this by
>
> (1) standardizing the Static Single Assignment (SSA)-based IR data structures,
> (2) providing a declarative system for defining IR dialects,
> (3) providing a wide range of common infrastructure:
> - documentation
> - parsing and printing logic
> - location tracking
> - multithreaded compilation support
> - pass management

# 2 Design Principles

> **Little builtin, everything customizable**
>
> The system is based on a minimal number of fundamen- tal concepts,
> leaving most of the intermediate representation fully customizable.
> A handful of abstractions -- types, operations and attributes, which
> are the most common in IRs -- should be used to express everything
> else.

> A success criterion for customization is the possibility to express
> a diverse set of abstractions including
> - machine learning graphs,
> - ASTs,
> - mathematical abstractions such as polyhedral,
> - Control Flow Graphs (CFGs)
> - and instruction-level IRs such as LLVM IR,
> all without hard coding concepts from these abstractions into the system.

TODO

# 3 IR Design Details

TODO
