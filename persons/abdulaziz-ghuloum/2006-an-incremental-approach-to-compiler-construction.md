---
title: an incremental approach to compiler construction
author: abdulaziz ghuloum
year: 2006
---

# My Motive

[2025-06-06] 十多年前刚刚学习 scheme 的时候，
我尝试过通过 aziz 的篇论文，
还有所对应的 tutorial，
还有 aziz 的 ikarus scheme 源代码，
来学习写编译器。
但是我失败了。

如今又在跟 jeremy siek 的
essentials of compilation（EOC）一书来学写编译器，
因此想回顾一下之前所学的内容。

平且看看，EOC 中 program 和 c-program 之分所带来的，
代码组织方面的复杂性，在 aziz 手上是如何解决的。
在学习 aziz 的方法之外，也可以看看 nanopass 的解决方案。

# 1 Introduction

> Compilers have traditionally been regarded as complex pieces of
> software. The perception of complexity stems mainly from traditional
> methods of teaching compilers as well as the lack of available
> examples of small and functional compilers for real languages.

> The usual approach to introducing compilers is by describing the
> structure and organization of a finalized and polished compiler.
> The sequencing of the material as presented in these books mirrors
> the passes of the compilers. Many of the issues that a compiler
> writer has to be aware of are solved beforehand and only the final
> solution is presented. The reader is not engaged in the process of
> developing the compiler.

# 2 Preliminary Issues

> To develop a compiler, there are a few decisions to be made.
> The source language, the implementation language, and the target
> architecture must be selected. The development time frame must be
> set. The development methodology and the final goal must be
> decided. For the purpose of our tutorial, we made the decisions
> presented below.

## 2.1 Our Target Audience

> We assume that the reader has basic knowledge of C and the C
> standard library (e.g. malloc, printf, etc.). Although our compiler
> will produce assembly code, some functionality is easier to
> implement in C; implementing it directly as assembly routines
> distracts the reader from the more important tasks.

## 2.2 The Source Language

> In our tutorial, we choose a subset of Scheme as the source
> programming language. The simple and uniform syntax of Scheme
> obviates the need for a lengthy discussion of scanners and parsers.
> The execution model of Scheme, with strict call-by-value evaluation,
> simplifies the implementation. Moreover, all of the Scheme
> primitives in the subset can be implemented in short sequences of
> assembly instructions.

> Although not all of Scheme is implemented in the first compiler, all
> the major compiler-related issues are tackled. The implementation is
> a middle-ground between a full Scheme compiler and a toy compiler.

## 2.3 The Implementation Language

> Scheme is chosen as the implementation language of the compiler.

## 2.4 Choosing The Target Architecture

> We choose the Intel-x86 architecture as our target platform. The x86
> architecture is the dominant architecture on personal computers and
> thus is widely available.

> Talking about compilers that are detached from a particular
> architecture puts the burden on the reader to make the connection
> from the abstract ideas to the concrete machine. Novice compiler
> writers are unlikely to be able to derive the connection on their
> own.  Additionally, the compiler we develop is small enough to be
> easily portable to other architectures, and the majority of the
> compiler passes are platform independent.

## 2.5 Development Time Frame

> The development of the compiler must proceed in small steps where
> every step can be implemented and tested in one sitting. Features
> that require many sittings to complete are broken down into smaller
> steps. The result of completing every step is a fully working
> compiler. The compiler writer, therefore, achieves progress in every
> step in the development.

> This is in contrast with the traditional development strategies that
> advocate developing the compiler as a series of passes only the last
> of which gives the sense of accomplishment.

> With our approach of incremental development, where every step
> results in a fully working compiler for some subset of Scheme, the
> risk of not “completing” the compiler is minimized.

> This approach is useful for people learning about compilers on their
> own, where the amount of time they can dedicate constantly
> changes. It is also useful in time-limited settings such as an
> academic semester.

## 2.6 Development Methodology

> We advocate the following iterative development methodology:
>
> 1. Choose a small subset of the source language that we can compile
>    directly to assembly.
>
> 2. Write as many test cases as necessary to cover the chosen subset
>    of the language.
>
> 3. Write a compiler that accepts an expression (in the chosen subset
>    of the source language) and outputs the equivalent sequence of
>    assembly instructions.
>
> 4. Ensure that the compiler is functional, i.e. it passes all the
>    tests that are written beforehand.
>
> 5. Refactor the compiler, if necessary, making sure that none of the
>    tests are broken due to incorrect refactoring.
>
> 6. Enlarge the subset of the language in a very small step and
>    repeat the cycle by writing more tests and extending the compiler
>    to meet the newly-added requirements.

> Knowledge of compilation techniques as well as the target machine is
> built incrementally. The initial overhead of learning the assembly
> instructions of the target machine is eliminated—instructions are
> introduced only when they are needed. The compiler starts small and
> is well focused on translating the source language to assembly, and
> every incremental step reinforces that focus.

## 2.7 Testing Infrastructure

> The interface to the compiler is defined by one Scheme procedure,
> `compile-program`, that takes as input an s-expression representing
> a Scheme program. The output assembly is emitted using an emit form
> that routes the output of the compiler to an assembly file.

> There are two core components of the testing infrastructure:
> the test-cases and the test-driver.

> The test cases are made of sample programs and their expected
> output. For example, the test cases for the primitive `+` may be
> defined as follows:

```scheme
(test-section "Simple Addition")
(test-case ’(+ 10 15) "25")
(test-case ’(+ -10 15) "5")
...
```

> The test-driver iterates over the test cases performing the
> following actions:
>
> 1. The input expression is passed to compile-program to produce
>    assembly code.
>
> 2. The assembly code and a minimal run-time system (to support
>    printing) are assembled and linked to form an executable.
>
> 3. The executable is run and the output is compared to the expected
>    output string. An error is signaled if any of the previous steps
>    fails.

## 2.8 The End Goal

> For the purpose of this paper, we define the end goal to be writing
> a compiler powerful enough to compile an interactive evaluator.
> Building such a compiler forces us to solve many interesting
> problems.


# 3 Writing a Compiler in 24 Small Steps

TODO

# 4 Beyond the Basic Compiler

> There are several axes along which one can enhance the basic
> compiler. The two main axes of enhancements are the feature-axis and
> the performance-axis.

## 4.1 Enhancing Features

TODO

## 4.2 Enhancing Performance

TODO

# 5 Conclusion

> Compiler construction is not as complex as it is commonly perceived
> to be. In this paper, we showed that constructing a compiler for a
> large subset of Scheme that targets a real hardware is simple. The
> basic compiler is achieved by concentrating on the essential aspects
> of compilation and freeing the compiler from sophisticated analysis
> and optimization passes. This helps the novice compiler writers
> build the intuition for the inner-workings of compilers without
> being distracted by details. First-hand experience in implementing a
> basic compiler gives the implementor a better feel for the
> compiler’s shortcomings and thus provide the motivation for
> enhancing it. Once the basic compiler is mastered, the novice
> implementor is better equipped for tackling more ambitious tasks.
