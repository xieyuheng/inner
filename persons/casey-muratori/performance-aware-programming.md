---
title: performance aware programming
author: casey muratori
website: "https://www.computerenhance.com/p/table-of-contents"
---

# My Motive

[2026-08-11] 这个系列课程中，有介绍 x86 的编码细节。
为了给 meta-lisp 实现汇编器 x86-lisp，来学习一下。

# Prologue: The Five Multipliers

性能优化中的五个因子：

- Waste
- Instructions Per Clock
- Single Instruction, Multiple Data
- Caching
- Multithreading

性能优化是一个因子相乘的过程，
类似小丑牌或 rouge 中每次数值增益（buff）都是一个因子。

## Waste

在 C 中 ADD 就被编译为 ADD 或 LEA 指令。

ADD 是 2 operand 加法，对 destination 有副作用：

    ADD RAX, RBX

LEA 可以作为到 3 operand：

    LEA RAX, [RBX + RCX]

展示为了做简单 ADD，
python 代码需要执行 181 个指令。

也就是只要使用了 python 这种 bytecode 解释器，
就是 100 倍以上的 waste 因子减益（debuff）。

## Instructions Per Clock

CPU 本身就能并行指令。
如果想要用到 CPU 并行指令的功能，
就需要避免指令之间的依赖。

这里的例子是，数组求和：

- 使用一个累加器，每次求和都是相互依赖的。
- 使用多个累加器，最后再把所有累加器相加，
  每次求和，都有很多指令是可以并行的。

实际测试，四个累加器，可以获得 2 倍增益。

## Single Instruction, Multiple Data

TODO

## Caching
## Multithreading
## Python Revisited
