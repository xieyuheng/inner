---
title: The Zen Truth That Breaks Your Illusion of Control
author: Alan Watts
vedio: "https://www.youtube.com/watch?v=1qYe6_nOjEE"
backup: "https://www.bilibili.com/video/BV1sUyCByEU8"
date: 2025-10-29
---

天下皆知美之为美，斯恶已。
皆知善之为善，斯不善已。

正是因为看到了老师写的 OOP 代码，
还有老师实现过程中的缺陷，
我才能对比出来我的实现是更好的。

人们总是在对比中认识世界：

> 1. things
> 2. agents
> 3. rewards and punishments

graph theory 就是最典型的例子。

但是其实这些都是人类发明概念（concept）。

> 一珠映现一切珠，一切珠映现一珠。
>
> -- Indra's net / 因陀罗网

想到了在设计一个语言的过程中，其实在对比很多语言。
语言的不同特性，实现某个功能的不同方案，
都需要对比。

这个对比过程非常像是在用形式概念分析（FCA）来画 lattice 图。
形式概念分析的重点就在于形成概念，
而概念正是人们思想的主要工具，
就是这里所说的 things。

FCA 好像捕捉了人们认识世界的方式中的重点，
但是也要记住 FCA 的工具性：

> it is real like a raft that we use to cross a river.

想到了为形式概念分析设计工具的项目，
其实可以是一个表格 APP，
当填写好表格之后，就能生成 lattice 图。

想到了想要写这种 APP，其实 x-lisp 并不合适，
因为这种 APP 是 realtime 的，而 x-lisp 是带有 GC 的。
x-lisp 非常适合用来写编译器这种只需要最后计算结果的程序。

又想到了在我想设计的诸多 lisp 中，
可以有一个类似 c 的 lisp -- system-lisp，
践行 scalable-c 的编程风格。
语法方面，可以用 `=` 代表 local variable 的赋值，
用 `:` 声明 local variable 的类型。
在设计这个语言的过程中，
可以大量学习 jai 和 odin 还有 zig 之类的语言。
一旦有了这个 system-lisp，就可以用自己的语言写 runtime 而不必用 c。
