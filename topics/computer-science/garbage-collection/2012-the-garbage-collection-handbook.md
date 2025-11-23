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

# 5 Reference counting

# 11 Run-time interface

# 12 Language-specific concerns
