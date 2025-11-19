---
title: reference count garbage collection
author: thomas w christopher
year: 1984
---

# 动机

[2025-11-19] 在为实现 x-lisp 的 GC 而做调研。

2001-constant-time-root-scanning-for-deterministic-garbage-collection
中提到了这篇论文。

我现在关心的是如何找 root。

这篇论文所讲的应该是以引用计数为基础的内存管理，
如何利用 GC 处理 circle 引用的问题。
而不是专门关于 root scanning 的论文。

用引用计数来找 root 的方法是：
如果一个 object 在 heap 中被引用的次数，
比 object 本身所记录的引用次数少，
就代表有额外的引用来自 stack 或 register，
因此这个 object 就应该被作为 root。

# SUMMARY

TODO
