---
title: accurate garbage collection in uncooperative environments revisited
year: 2009
---

# 动机

[2025-11-20] 在为实现 x-lisp 的 GC 而做调研。

这篇论文是 EOC 所引用的三篇关于 shadow stack 的论文之一。

# 3. Previous Work: Explicit Pointer Stacks & Henderson’s Linked Frames

## 3.1. Explicit Pointer Stacks

这就是 EOC 中所用的方案。

## 3.2. Henderson’s Linked Frames

就是前一篇论文的方案。

# 4. Accuracy with Lazy Pointer Stacks

> The key to accurately obtaining references in the call stack is to
> force the compiler to place references in specific locations, which
> the approaches above do by segregating references to an explicit
> pointer stack, or in Henderson’s case, to a linked frame structure.

这算是把上面两个方案的本质说明白了。

> Both approaches are eager in the sense that the data structures
> describing live pointers are always up-to-date. In our work we
> investigate techniques that construct the equivalent of a pointer
> stack on demand.  We refer to this approach as lazy pointer
> stacks. The expected advantages of laziness are that accesses to
> references are direct and that the native compiler will have more
> opportunities for optimization; in particular, to allocate
> references in registers.

这里说的也是我在读 EOC 时所在意的问题。

这里的方案是把 push shadow frame 和保存 pointer 的代码放到一个 if 下面，
在 GC 的时候 copy 整个 stack，然后 unwind stack。

说是 return 就可以，但是如何找到那个 if？

我意识到，这个方案我也没法用，
假设我需要让 c 函数能调用 x-lisp 函数，
我可以给 x-lisp 函数生成一个 block 用来保存 pointer，
但是对于 c 函数我没法这么做。

这么看来我能用的唯一方案就是
1984-reference-count-garbage-collection
这种纯运行时方案。

这里的操作 stack 的方案还让我觉得，
对于带有 GC 但是没有静态类型系统的语言而言，
VM 是不错的选择，
否则 root scanning 问题太难了。
