---
title: compiler support for garbage collection in a statically typed language
authors: [amer diwan, eliot moss, richard hudson]
year: 1992
---

# My Motive

[2025-10-22] 在学习 EOC 的时候，需要实现 GC，
因此需要解决精确找到 roots 的问题。

想要学习如何在 value 没有 tag 的情况下找到 root，
尤其是寄存器中的 root。

# Abstract

> We cortsider the problem of supporting compacting garbage collection
> in the presence of modern compiler optimizations.  Since our
> colleetor may move any heap object, it must accurately locate,
> follow, and update all pointers and values derived from pointers.
> To assist the collector, we extend the compiler to emit tables
> describing live pointers, and values derived from pointers, at each
> program location where collection may occur.

这确实就是解决我所面临的问题的。
但是我目前想到的方案不是用 table，
而是直接生成代码，在进入 GC 前把寄存器中的 root
保存到一个专门保存 pointer 的 stack。

# 1 Introduction

> In a statically typed language, the compiler knows which global
> variables contain pointers. It also knows which stack locations and
> registers contain pointers at any point in a program. In the
> following sections we describe a technique that exploits this
> compile-time knowledge to assist the garbage collector in locating
> and updating pointers in the stack and in the registers, and at the
> same time meets our requirements: the alility to move objects,
> portability, and minimal impact on performance.  After describing
> the scheme, we present some experimental results.

# 2 Basic Problems

介绍 copying GC 所面临的问题。

由于作者是为 modula-3 设计 GC，
因此要考虑 pointer arithmetic 之类的
"values derived from pointers"，
这些都是我不需要考虑的。

我只关心 find root 的问题。

# 3 Solutions

为每个函数的每个可能出现的 GC 的地方，
都生成 table 描述 stack 和 register 中使用 pointer 的情况，
在运行时遇到 GC 就通过 frame 中的 return address 找到函数，
然后再找到相应的 table。

感觉不如直接编译出来处理 pointer 的代码。
