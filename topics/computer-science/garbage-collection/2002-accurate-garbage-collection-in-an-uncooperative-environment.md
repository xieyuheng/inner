---
title: accurate garbage collection in an uncooperative environment
author: fergus henderson
year: 2002
---

# 动机

[2025-11-19] 在为实现 x-lisp 的 GC 而做调研。
fil-c 的 gc 中：https://fil-c.org/safepoints
提到了 henderson frame，就是这篇论文。

# ABSTRACT

> Previous attempts at garbage collection in uncooperative
> environments have generally used conservative or mostly-conservative
> approaches. We describe a technique for doing fully type-accurate
> garbage collection in an uncooperative environment, using a “shadow
> stack” to link structs of pointer-containing variables, together
> with the data or code needed to trace them. We have implemented this
> in the Mercury compiler, which generates C code, and present
> preliminary performance data on the overheads of this technique. We
> also show how this technique can be extended to handle multithreaded
> applications.

这里说的 Mercury 就是那个 logic + functional language：

- https://en.wikipedia.org/wiki/Mercury_(programming_language)

# 1. INTRODUCTION

> We propose an alternative approach that allows fully typeaccurate
> and liveness-accurate garbage collection, thus allowing the use of a
> normal copying collector, without requiring any support from the
> back-end target language, and while still generating code that uses
> the normal C function calling mechanism. We describe this approach
> in the context of compiling to C, although it would also work
> equally well when interfacing directly to a compiler back-end
> framework, such as the GCC back-end.

> Our technique is formulated as a transformation on the generated C
> code, which modifies the C code in such a way as to insert calls to
> perform garbage collection when necessary, and to provide the
> garbage collector with sufficient information to trace and if
> necessary update any pointers on the C stack. (The transformation is
> not entirely independent of the front-end language, however; it
> requires information from the source language front-end compiler
> about how to trace each local variable.)

由于是在生成的 C 代码上做文章，
所以不同考虑寄存器中保存 pointer 的问题。

# 2. THE GC TRANSFORMATION

> The basic idea is to put all local variables that might contain
> pointers in structs, with one struct for each stack frame, and chain
> these structs together as a linked list. We keep a global variable
> that points to the start of this chain.  At GC time, we traverse the
> chain of structs. This allows us to accurately scan the C stack.

shadow stack 中的每一个 frame 有一个 callback function `trace`，
用来报告这个 frame 中所用到的所有 pointer：

> The ‘trace’ field is the address of a function to trace
> everything pointed to by this stack frame.

这里的方案依赖于静态类型语言，
或者至少要能区分 pointer 与 non-pointer。
但是我想实现的 x-lisp 是纯动态类型语言，
在 basic-lisp 中，我没法区分 pointer 与 non-pointer。

但是我可以假设所有的 local variable 都是潜在的 pointer。

想要实现 henderson frame 就必须在翻译成 basic-lisp 的时候实现。
但是这也不太合理，因为 henderson frame 用到了太多的 c 的功能了。

