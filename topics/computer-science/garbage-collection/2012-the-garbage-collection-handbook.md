---
title: the garbage collection handbook (2nd edition)
subtitle: the art of automatic memory management
authors: [richard jones, antony hosking, eliot moss]
years: 2023
---

# 动机

[2025-11-22] 在为 x-lisp 设计 GC 的时候，
我发现我处理不好 root scanning 的问题。
这导致我认为纯粹的动态类型语言 + GC 的设计，
不应该用编译器实现，而应该用 VM 实现。
因为在没有类型信息帮助的前提下，
在 native code 中很难处理 root 存在于 register 的情况。

看了一个基于 reference counting 的方案：
- 1984-reference-count-garbage-collection.md
但是想了一下所生成的汇编代码，感觉 overhead 太大了。

但是尝试读一下这本书，再看看还有什么别的方案。
或者我对之前的方案的理解还有什么不对的地方。

我目前关心的是：

- root scanning 问题。

  - chapter 11 Run-time interface

- 备选方案的 reference counting，因为可以完全避免 root scanning。

  - chapter 5 Reference counting
  - chapter 12 Language-specific concerns
    -- 关于 dead object 的 finalisation

# Preface

> In this book, we have tried to bring together the wealth of
> experience gathered by automatic memory management researchers and
> developers over the past fifty years. The literature is huge — our
> online bibliography contains 3,400 entries at the time of writing.
>
> - We discuss and compare the most important approaches and
>   state-of-the-art techniques in a single, accessible framework.
>
> - We have taken care to present algorithms and concepts using a
>   consistent style and terminology. These are described in detail,
>   often with pseudocode and illustrations.
>
> - Where it is critical to performance, we pay attention to low level
>   details, such as the choice of primitive operations for
>   synchronisation or how hardware components such as caches
>   influence algorithm design.

# 1 Introduction

> Developers are increasingly turning to managed languages and
> run-time systems for the many virtues they offer, from the increased
> security they bestow to code to the flexibility they provide by
> abstracting away from operating system and architecture. The
> benefits of managed code are widely accepted [Butters, 2007].
> Because many services are provided by the virtual machine,
> programmers have less code to write.

可能作者也假设了大多数带有 GC 的语言是用 VM 实现的。
如果是这样，那么我所关心的，和 register 相关的
root scanning 问题可能就讨论的不多。

## 1.6. TERMINOLOGY AND NOTATION

### Liveness, correctness and reachability

区分 liveness 和 reachability。
更重要的是，介绍如何用 digraph 理论作为模型，
来理解编程时所用的 object 与 pointer。

| digraph | memory management          |
|---------|----------------------------|
| digraph | object graph               |
| vertex  | object                     |
| edge    | address of field in object |

也就是说：

- 一个 object 中的 address，是一个 vertex 的潜在 edge。
- 当这个 address 保存了另一个 object 时，
  这两个 object 所代表的 vertex 之间才有了一个 edge。
- 也可以用 null 来把所有未连接的潜在 edge 理解为连接到 null。

这种理解 edge 的方式，是我之前没想到的。
我之前把 pointer 理解为 digraph 的方式是诉诸直觉，
而不是诉诸数学的（set theory 的），是不精确的。

注意，digraph 中没有所谓「潜在的 edge」，
如果需要，那就是 ported digraph 了。
在 inet 中，我已经大量使用 ported graph 了。

# 2 Mark-sweep garbage collection

TODO

# 5 Reference counting

TODO

# 11 Run-time interface

## 11.2 Finding pointers

### Accurate pointer finding in stacks and registers

#### Finding pointers in registers

> We now introduce an approach to the callee-save registers problem.

这正是我所面临的问题。

注意，这里给出的解决方案：

- 并不需要假设 tagged value。
- 也并不需要假设没有 pointer move，
  也就是说可以支持 copying GC。

对我们有 tagged value 的 mark-sweep GC 设计来说，
这个方案可以更简单。

> First, we add metadata that indicates for each function
> which callee-save registers it saves, and where in its frame it
> saves them.

> We assume the more common design where a function saves in one go,
> near the beginning of the function, all the callee-save registers
> that it will use. If the compiler is more sophisticated and this
> information varies from place to place within a function, then the
> compiler will need to emit per-location callee-save information.

TODO 实现到 GC 的 root scanning 的时候，再回来细读这里的方案。

**关于 x-lisp 中如何实现这里的方案**

我们其实没法假设 call stack 中只有 x-lisp 的函数，
因为操作系统需要调用 c 的 main，
而 c 的 main 需要调用 x-lisp 的 _main。

我们所能假设的只是：

- 在遇到 GC 时，call stack 中，
  从 top frame 开始，到 _main 的 frame 结束，
  都是 x-lisp 的 function 的 frame。

因此，我们不能在 safepoint 保存所有的 callee saved register，
而是需要以 _main 为重点，扫描整个 call stack，
来得到精确的 register 信息。

我们不能保存所有的 callee saved register，
并且我们需要的就是知道，哪些 register 需要保存，哪些不需要保存。

不对，这里的方案没有用 shadow stack，
所以这是真的从 call stack 中找 root。

**关于 primitive function 的限制**

其实既然已经在 C 代码中了，
就可以利用 henderson frame 的技巧，
来记录 C 函数所用到的局部变量。

对于 x-lisp 函数，用扫描 call frame 的方式来找 root，
对于 C 函数，用 henderson frame 中记录的信息来找 root。

只需要能够在 call stack 中区分 primitive 函数和 x-lisp 函数就可以。

# 12 Language-specific concerns

TODO
