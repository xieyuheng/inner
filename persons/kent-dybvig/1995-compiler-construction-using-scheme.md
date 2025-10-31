---
title: compiler construction using scheme
authors: [erik hilsdale, michael ashley, kent dybvig, daniel friedman]
year: 1995
---

# My Motive

[2025-06-07] 已经在看 jeremy siek
和 abdulaziz ghuloum 的编译器课程了，
而这篇论文介绍的是更早的 dan 风格的编译器课程。

[2025-10-31] 这里找到 free variable 的过程，
类似 EOC 的 `reveal-functions` pass。

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

> - the language is syntactically restricted so that the only numbers
>   accepted are integers in a bounded range,
>
> - all lambda expressions have a fixed arity, i.e., no rest
>   arguments.
>
> - programs cannot have free variables other than references to
>   primitives in operator position,
>
> - symbols cannot be interned at runtime,
>
> - first-class continuations and I/O are not supported,
>
> - derived syntax is not directly supported,
>
> - garbage-collection is not provided, and
>
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

这里的 run-time 与 EOC 课程中类似，
用的是类似 c 的 calling convention，
但是不完全一样。

注意，这里画图描述 stack 的时候使用了我认为正确的方向：

- address 从上到下递增。
- stack 从向低地址增长，因此像是真实世界的 stack of things。

> The `cp` register points to the closure of the active procedure, and
> the closure holds the values of the procedure’s free variables.

在 EOC 中，没有预留一个 register，
而是将 closure 作为 self（或者说 recur）参数，
传递给 lambda 所编译出来的 top-level function。

> The procedure call convention for non-tail calls is as follows. The
> caller first saves the closure pointer at the top of its frame. The
> callee’s frame is then built by pushing a return address and then
> evaluating each argument and pushing its value. The operator is
> evaluated last, and its value is placed in the `cp` register.
> Finally, the frame pointer is incremented to point to the base of
> the callee’s frame and control is transferred by a jump indirect
> through the closure pointer. On return, the callee places the return
> value in the accumulator `ac0` and jumps to the return address at
> the base of its frame. The caller restores the frame pointer to its
> old position and reloads the `cp` register with its old value.

这里与 c calling convention 的主要差异是，
stack 中的 return address 保存的是到 closure 的 pointer，
而不是到 function 的 pointer。

> The calling convention is simpler for tail calls. The arguments are
> evaluated and pushed, and the operator is then evaluated and stored
> in the `cp` register. The arguments are moved downwards to overwrite
> arguments of the caller’s frame, and control is transferred to the
> callee. The frame pointer does not move.

> Values are represented using 64-bit tagged pointers with the low
> three bits used for tag information [23]. Four of the nine
> data-types, booleans, characters, fixnums, and the empty list, are
> immediate data-types and are encoded directly in the pointer.
> Vectors, pairs, closures, strings, and symbols are allocated in the
> heap. Since the low three bits are used for the tag, allocation must
> proceed on eight-byte boundaries. A heap allocated object is tagged
> by subtracting eight from the pointer to the object and then adding
> the tag. Fields of the object can be referenced efficiently using a
> displacement operand. A type check is also efficient, requiring at
> worst a mask, compare, and branch.

接受了 fully tagged value 的效率。

这里的引用是：

- [23] Peter A. Steenkiste.
  The implementation of tags and run-time type checking.
  In Peter Lee, editor, Topics in Advanced Language Implementation,
  pages 3–24. MIT Press, 1991.

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

注意，这里的 `begin` 只有两个 sub-expressions。
并且 variable references 被分了一个独立的 ADT 出来。

> The `free` and `bound` forms each include an index indicating the
> offset from the `cp` or `fp` register at which the variable’s value
> can be found, while the `local` form includes only the name of a
> variable.

可以共用同一种 index，意味着 `cp` 和 `fp` 所指向的数据的结构是一样的！

> Offsets from the `fp` for `local` are determined by the
> code generator. The `closure` form is like `lambda`, but the
> locations of the free variables are made explicit. Constants are
> restricted to immediate values.

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

> Assignment is not part of the intermediate language, since variable
> assignment cannot be directly supported using the chosen run-time
> model.

> Assembly code can be generated from intermediate programs in one
> pass. Code is generated bottom-up with the invariant that the result
> of evaluating a subexpression is left in the accumulator `ac0`.
> Arguments to primitives are stored in temporary locations on the
> stack, and code for primitives is generated inline. The code
> generated for primitives is unsafe, i.e., no type checking is
> performed.

> Offsets for `free` and `bound` references are provided. Computing
> frame offsets for local references requires a lexical environment to
> be passed downwards. The environment maps local variable names to
> frame offsets. The environment is necessary since temporary
> locations used for primitive and procedure applications can be
> interspersed with local bindings.

> After code generation, the resulting assembly code is assembled
> using the system assembler `as` and linked against a C and assembly
> code stub using `ld`. The C stub obtains memory from the operating
> system for the stack and heap. The assembly code stub initializes
> the registers and places a return address back to C at the base of
> the stack. Upon return to C, the value left in the accumulator `ac0`
> is printed by a simple C-coded printer.

## 2.3 Compiling to Intermediate Code

> The front-end of the compiler consists conceptually of three parts:
> a scanner and parser, a sequence of source-to-source translations,
> and a transformation that assigns locations to variables.

### 2.3.1 Scanning and Parsing

### 2.3.2 Code Transformation

> Three source-to-source transformations are performed on the forms
> the parser produces.
>
> - The first transformation invokes the host system macro expander to
>   expand the input program and then regularizes the expanded
>   program.
>
> - The second transformation eliminates `set!` forms.
>
> - The third transformation eliminates complex quoted data.

TODO

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
>
> - destination-driven code generation [10],
>
> - copy propagation and constant folding [1],
>
> - register allocation [6], and
>
> - type check elimination by abstract interpretation [16, 4].

这里的 type check elimination 是比较重要的优化，其引用是：

- [4] J. Michael Ashley.
  A practical and flexible flow analysis for higher-order languages.
  To appear in Proceedings of the ACM Symposium
  on Principles of Programming Languages, 1996.

- [16] Suresh Jagannathan and Andrew Wright.
  Effective flow analysis for avoiding runtime checks.
  In Proceedings of the 1995 International Static Analysis Symposium, 1995.

原来所谓 abstract interpretation 就是干这个的。

> The discussion of copy propagation, procedure integration, and
> constant folding leads to (on-line) partial evaluation.

## 4.2 Run-time Topics

> Some of the topics covered in a past course have been:
>
> - separate compilation,
>
> - buffered I/O,
>
> - first-class continuations [14], and
>
> - garbage collection.

这里引用了一个：

- [15] Erik Hilsdale and Daniel P. Friedman.
  A Scheme-based course on compiler construction.
  In Preparation.

可惜这篇论文没发表。

> To support first-class continuations, the stack model is generalized
> to a series of linked stack segments. The model supports stack
> overflow gracefully by treating overflow as an implicit continuation
> capture in which a new stack is allocated and linked to the
> overflowed segment.

看来想要实现 first-class continuations
就必须使用有别于 c 的 calling convention。

> First-class continuations simplify memory management techniques. In
> lecture, various types of memory management are presented, including
> mark-sweep, reference counting, copying, and generational. The
> students implement a copying collector in Scheme as a procedure of
> no arguments that immediately reifies its current continuation.
> Since registers are caller-save and the collector is a procedure of
> no arguments, the continuation is the root of the collection.  The
> collector is an iterative Scheme program that performs a bounded
> amount of allocation. To detect when a garbage collection must
> occur, heap overflow checks are inserted by the compiler before each
> allocation.

有遇到了找 root 这个难题，
想要做到这里的「简化」，
必须做到完全避免使用 callee-saved registers 才行。
这不太实际。

> Buffered I/O is a straightforward topic. The implementation requires
> code in the C stub to interface to the operating system as well as
> primitives supported by the code generator that call the C routines
> in the stub.

没有用 C 的 calling convention，如何调用 C 的函数？
每个函数都要有所 wrap？
