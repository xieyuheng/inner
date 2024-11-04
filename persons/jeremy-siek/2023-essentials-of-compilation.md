---
title: Essentials of Compilation
subtitle: An Incremental Approach
authors: [Jeremy G. Siek, Ryan R. Newton]
year: 2023
---

# 学习动机

动机首先来自 Indiana 大学的传统，
来自 Dan 介绍解释器的 EOPL。

其次我想模仿 uxn 写一个 vm，
但是与之以 byte 为 value 的设计不同，
我想要：

- 64-bits 的 word。
- tagged-value。
- garbage collection。

为了这个目标，可以从这本书里学：

- runtime tagged-value encoding
- garbage collection 实现方式

等等。

与 Jeremy 使用 racket 和 python 不同，
我将用 JS/TS 来做练习。

# Preface

> We take you on a journey through constructing your own compiler for
> a small but powerful language. Along the way we explain the
> essential concepts, algorithms, and data structures that underlie
> compilers.

继承了 Abdulaziz Ghuloum 的论文
"An Incremental Approach to Compiler Construction"。

> We develop your understanding of how programs are mapped onto
> computer hardware, which is helpful in reasoning about properties at
> the junction of hardware and software, such as execution time,
> software errors, and security vulnerabilities.

> For those interested in pursuing compiler construction as a career,
> our goal is to provide a stepping-stone to advanced topics such as
> just-in-time compilation, program analysis, and program
> optimization.

> For those interested in designing and implementing programming
> languages, we connect language design choices to their impact on the
> compiler and the generated code.

就是我了！

> A compiler is typically organized as a sequence of stages that
> progressively translate a program to the code that runs on
> hardware. We take this approach to the extreme by partitioning our
> compiler into a large number of _nanopasses_, each of which performs
> a single task. This enables the testing of each pass in isolation
> and focuses our attention, making the compiler far easier to
> understand.

还继承了 chez-scheme 的 nanopass。
上面这段也算是包含了一个对 compiler 的定义。

> The most familiar approach to describing compilers is to dedicate
> each chapter to one pass. The problem with that approach is that it
> obfuscates how language features motivate design choices in a
> compiler. We instead take an _incremental_ approach in which we build
> a complete compiler in each chapter, starting with a small input
> language that includes only arithmetic and variables. We add new
> language features in subsequent chapters, extending the compiler as
> necessary.

对 incremental 的定义。

# 1 Preliminaries

TODO

# 2 Integers and Variables

# 3 Register Allocation

# 4 Booleans and Conditionals

# 5 Loops and Dataflow Analysis

# 6 Tuples and Garbage Collection

# 7 Functions

# 8 Lexically Scoped Functions

# 9 Dynamic Typing

# 10 Gradual Typing

# 11 Generics

# A Appendix
