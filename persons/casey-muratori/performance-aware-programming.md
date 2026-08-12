---
title: performance aware programming
author: casey muratori
website: "https://www.computerenhance.com/p/table-of-contents"
---

# My Motive

[2026-08-11] 这个系列课程中，有介绍 x86 的编码细节。
为了给 meta-lisp 实现汇编器 x86-lisp，来学习一下。

# Prologue: The Five Multipliers

**对于性能优化问题的认识**

performance aware programming
大概就是在写程序的时候，
知道其最终被编译成为什么样的汇编代码。
并且在设计数据结构的时候，知道它对 CPU cache 的影响。

也就是说，要了解现实硬件的属性。

**对于性能优化问题的分析**

性能优化只有两种可能：

- A 类：减少 CPU 需要执行的指令数量
- B 类：提升 CPU 执行指令的速度
  - 比如不要让 CPU 等待，和并行

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

- 之前学汇编的时候，没想到可以这样理解 LEA。
  也就是说 LEA 不只是可以用来做带有 scale 和 disp 的奇怪运算，
  而是可以直接被视为 x86 的 3 operand 加法。

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

例如 x86 的 SSE 和 AVX。
直接把小规模的并行计算用指令表达出来，
而不是让 CPU 分析指令，并找机会去做自动的并行。

可以理解为是操作向量的指令。

- 我们可以在 meta-lisp 中专门为 vector 设计内置函数，
  内置函数直接编译到 vector 指令集。

实际测试，增益倍数基本与向量的元数一致，分别是 4 和 8。

另外，这个增益因子，可以和上一个增益因子叠加。
实际测试，四个累加器，可以获得 2 倍增益。

## Caching

CPU 在 load 一个地址下的数据时，
如果这个地址不再 cache 内，
就需要先读入 cache，
然后才能 load。

但是 cache 很小，casey 的机器只有 32k 的 L1 cache。

所以，如何设计数据结构，
使得读取数据的模式能最大程度利用到 cache
就很重要。

这里的例子展示了，如果所处理的数据很多，
不在 L1 cache 时，所导致的就吞吐率而言的减益因子。
整套测试所记录的都是 "peak adds/clock"。

有吞吐量的减益，但是这来自数据量本身不能在一个 cache 内，
虽然数据本身是连续的，但是还是会有减益。

- 一般在 data-oriented design 中，
  所能做到的只是，让需要被批量处理的数据保持连续。
  但是这里的测试表明，当数据量很大，
  而不能在一个 cache 之内时，还是会有减益。

下一节的并行计算可以减缓 cache 的问题，
因为每个 core 都有自己的 cache。

## Multithreading

显然并行计算是性能优化中很重要的一个方案。
重点通常在于，问题本身能够被可并行的方案解决。

- 另外，erlang 这种直接从语言层面用到多核的设计也值得考虑。

由于每个 core 都有自己的 cache，
所以（当数据量足够多时）cache 增益因子和多核的增益因子会叠加。

也就是说，用 CPU 的多核做并行的同时，
也获得了额外增多的缓存增益。

这里测试的例子是，增加 core 时候，
所有的数据又可以都在缓存内了，
所以有很大的额外增益。

| #core | adds/clock | speed up |
|-------|------------|----------|
| 1     | 7.23       | 1        |
| 2     | 26.59      | 3.68     |
| 4     | 52.51      | 7.26     |

对于大量的数据，
由于多核不会带来额外的内存带宽，
内存带宽成了瓶颈，所以多核所带来的增益被大大削减了。

| #core | adds/clock | speed up |
|-------|------------|----------|
| 1     | 1.42       | 1        |
| 2     | 1.82       | 1.28     |
| 4     | 2.37       | 1.66     |

在尝试利用 CPU 实现多核的 interaction nets 时，
我遇到的多核增益削减，可能就来自于内存带宽这个瓶颈。

因为这里只是简单地在循环里做 array 的 sum。
这种简单的显然可并行的计算，已经会遇到很大的增益削减了。
更不要提 inet 的复杂 graph rewrite 了。

就算是用 GPU 实现 interaction nets，
GPU 的 CUDA 核心也没有独立的内存带宽，
共享的内存带宽也会成为瓶颈。

CUDA 的并行处理是为「计算密集型」任务而设计的，
而不是为「内存密集型」任务而设计的。

注意，「密集」与否，取决于内存访问与计算之间的比例。

对于 interaction nets 而言「内存密集型」
也可以理解为「通讯密集型」，
因为多核 + 共享的随机读写内存，
其实是一个小的分布式计算系统，
多个核对同一个地址的读写是需要被同步的。

真正的解决思路也许在于「面向现实的编程」，
每个 worker 所能读写的内存只能是自己的 core 内缓存大小，
而不能是随机读写整个内存。
以这个限制为基础来重新设计算法。

每个 core 以及它的缓存，
都构成一个小的 actor。
我们要为这种系统设计算法以及语言。

## Python Revisited

回顾我们对性能优化问题的分析。

两类有优化方法：

- A: reduce instructions
- B: increase instruction processing speed

五个优化因子：

- Waste -- A 类
- Instructions Per Clock -- B 类
- Single Instruction, Multiple Data A 类
- Caching -- B 类
- Multithreading -- B 类

这种「分析」是类似亚里士多德式的分析。

一般就写程序而言，我们会说实现的经验是最重要的。

但是关于性能优化问题而言，
重点不是在于实现某种具体的性能优化方案的知识，
而是在于完整地分析性能优化问题的能力。

这种分析的重点在于理解现实本身。
包括理解汇编，理解 cache，理解内存带宽，等等。

掌握了分析性能优化问题的一般方法，
就可以知道哪些地方是可以优化的。
因此大多数代码还是可以用慢速语言比如 python 写，
只需要在性能攸关的地方改用 C。
