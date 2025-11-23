---
title: fil-c
author: Filip Pizlo
source: "https://fil-c.org"
---

项目起源于发现 LLVM 的中间语言的某个阶段，
是有安全的 atom vs pointer 的类型区分的。

以这个阶段为基础写 pass 就能实现内存安全的 c。

为了内存安全的 C 而使用 GC：

- https://fil-c.org/fugc
- https://fil-c.org/safepoints

这里的 GC 并不是为了避免让用户手动调用 free，
而是为了 free 的时候能有安全的报错。

我第一次见到这种使用 GC 的方式，
这大大拓宽了 GC 技术的应用领域。
比如，valgrind 和内存类 sanitizer 等工具，
都可以使用 GC。
