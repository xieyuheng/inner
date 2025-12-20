---
title: Digital Design and Computer Architecture (Spring 2025)
teacher: onur mutlu
---

# My Motive

[2025-12-20]

- 设计自己的 ISA，以支持动态语言的使用 tagged value 的编译器。
- 为 interaction nets 设计专门的 microarchitecture。
- 理解 GPU 与 TPU 的差异，理解 tinygrad。

CPU 会进行很多 speculative execution，
因为硬件本质上是并行的，而每次只运行一个指令太浪费资源了。

但是可否有新的计算模型，可以避免这个问题？
让新的计算单元本身就能用到所有的计算资源，
而不是每次执行一条指令。

我必须学会为什么硬件本质上是并行的，
因为想要在现有的，基于 CPU 的技术栈上，
从软件层次高效地实现 inet 几乎是不可能的。

# L1: Intro: Fundamentals, Transistors, Gates (Spring 2025)

[2025-12-20]

| problem           |                    |
| algorithm         |                    |
| program/language  |                    |
| system software   | OS, VM             |
|-------------------|--------------------|
| SW/HW interface   | ISA (architecture) |
|-------------------|--------------------|
| microarchitecture |                    |
| logic             | gates              |
| devices           |                    |
| electrons         |                    |

不同的 architecture 或 microarchitecture 之间的差异，
在于不同的 architecture 是为了不同的 workload 而设计的。

TODO
