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

## 2.1 The Run-time Model

这里所描述的 run-time model，
没有 forth 的 run-time model 简单，
但是支持 closure。

与 forth 相比的主要区别就是，
forth 用两个 stack，
而一般的 run-time 用一个 stack。

也可以基于这里所描述的 run-time model 写一个 vM，
作为 intermediate language 的 specification。

## 2.2 Code Generation

> The code generator produces code for the run-time model from the
> intermediate language of Figure 3. The language is similar to core
> Scheme despite several syntactic differences.  The principal
> difference is that variable references are subsumed by the `free`,
> `bound`, and `local` forms.

> Figure 3: The intermediate language:

```bnf
E = i | R | (begin E E) | (if E E E) | (E E ...)
  | (P E ...) | (closure (v ...) (R ...) E)
  | (let ((v E) ...) E)
R = (free n v) | (bound n v) | (local v)
P ∈ Primitives
i ∈ Immediates
v ∈ Variables
n ∈ N
```

> The free and bound forms each include an index indicating the offset
> from the cp or fp register at which the variable’s value can be
> found, while the local form includes only the name of a
> variable. Offsets from the fp for local are determined by the code
> generator. The closure form is like lambda, but the locations of the
> free variables are made explicit. Constants are restricted to
> immediate values.

```scheme
(let ((f (lambda (x)
           (let ((y (+ x 1)))
             (lambda (z)
               (cons y (cons z (quote (1 2)))))))))
  ((f 4) 5))
=>
(let ((t (cons 1 (cons 2 '()))))
  (let ((f (closure (x) ((local t))
                    (let ((y (+ (bound 0 x) 1)))
                      (closure (z) ((local y) (free 0 t))
                               (cons (free 0 y)
                                     (cons (bound 0 z)
                                           (free 1 t))))))))
    (((local f) 4) 5)))
```

## 2.3 Compiling to Intermediate Code

> The front-end of the compiler consists conceptually of three parts:
> a scanner and parser, a sequence of source-to-source translations,
> and a transformation that assigns locations to variables.

### 2.3.1 Scanning and Parsing

### 2.3.2 Code Transformation

### 2.3.3 Variable Addressing

> The last transformation before code generation assigns locations to
> variables and transforms the simplified Scheme program into the
> intermediate language. One analysis pass determines the free
> variables of each lambda expression. A second pass rewrites the
> program. Each bound and free variable reference is converted into a
> bound and free form respectively, and lambda expressions are
> converted into closure expressions.

> Also, expressions of the form
>
>     ((lambda (v0 ... vn) E) E0 ... En)
>
> are rewritten as let expressions, and let-bound variables are
> converted into local forms. The output is in the language defined by
> the grammar in Figure 3.

# 3 The Course

给学生提供处理 DFA 的工具 `declare-table` or `state-case`，
让学生实现 scanner。

关于使用 sexp 而不用 record（untyped ADT）：

> The third and fourth assignments involve implementing the
> transformations described in Sections 2.3.2 and 2.3.3. In order to
> ease the handling of such forms as the let form of the intermediate
> language, a macro package, `synlambda`, is made available to the
> students that extends Scheme with pattern-matching
> capabilities. Pattern matching over S-expressions does cause some
> execution overhead that can be avoided by first transforming the
> input into records. The source-to-source nature of the
> transformations can be obscured, however, if performed on records.

没有用 x86 构架，用的是 Alpha Architecture。

解释为什么没有用 CPS和 ANF（administrative normal form），因为不适合教学。
但是 jeremy siek 的课程就是用 ANF 的。

解释为什么用 stack 而不用 heap 处理函数调用。

# 4 Advanced Coursework

## 4.1 Compile-time Topics

> The following compile-time topics have been successfully covered
> in a follow-up course:
>
> - macro expansion [9],
> - destination-driven code generation [10],
> - copy propagation and constant folding [1],
> - register allocation [6], and
> - type check elimination by abstract interpretation [16, 4].

## 4.2 Run-time Topics

TODO

# 5 Conclusions

TODO
