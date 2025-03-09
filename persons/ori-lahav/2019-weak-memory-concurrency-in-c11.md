---
title: weak memory concurrency in c11
author: Ori Lahav
author-homepage: "https://www.cs.tau.ac.il/~orilahav"
source: "https://www.youtube.com/watch?v=mOqu8vGSysc"
---

# My motive

[2025-03-08] 在学习并行编程时，
发现如果想要实现无锁的算法，
就需要学习 weak memory model。
Ori Lahav 这个演讲是我找到的对这个主题最好的介绍。

# Example: Dekker's mutual exclusion

[Dekker's algorithm](https://en.wikipedia.org/wiki/Dekker's_algorithm)
是一个用 shared-memory + control flow 解决 mutual exclusion 问题的巧妙算法，
可以想象有两个病人问诊但是只有一个医生的场景。

由于这个算法中使用 shared-memory 的方式，
刚好可以展示 weak memory model 中反直觉的现象。

```scheme
(begin
  (store x 0)
  (store y 0)

  (begin-thread
   (store x 1)
   (store a (load y)) ;; a = 0
   (if (eq? a 0)
     (critical-section)))

  (begin-thread
   (store y 1)
   (store b (load x)) ;; b = 0
   (if (eq? b 0)
     (critical-section))))
```

想要回答「为什么」的问题，就需要了解底层的模型，即 weak memory model。
与 weak memory model 相对的是 sequential consistency (SC)，
在 SC 中并不会出现这种现象。

所谓 sequential consistency，
就是 CPU 直接访问内存，
中间没有 cache 的简单构架。

> No existing hardware implements SC!
> - SC is very expensive (memory ∼100 times slower than CPU).
> - SC does not scale to many processors.

下面要介绍的 weak memory model，
每一个都对应一种芯片设计构架。

# Store buffering in x86-TSO

TSO 是 totel store order 的缩写。

这种构架是：

- load -- 直接从内存取；
- store -- 先保存在一个 store buffer 中，以免阻塞 CPU 运行，然后批量写入内存。

这种模型可以解释 load 被换到了 store 前面的现象。

# Load buffering in ARM

这种构架是 load 和 store 都有多级的 cache。

这种模型可以解释 store 被换到了 load 前面的现象。

# Challenge 2: Compilers stir the pot

介绍了一个编译器优化的例子，
即编译器优化掉一个 store，改为从已有的寄存器中复制。
当有多线程时，这种优化只有在 sequential consistency 下才合法。
在 weak memory model 下不合法。

# Challenge 3: Transformations do not suffice

这里的 transformation 指的是用代码的变换，
来解释 weak memory model 中的现象。
比如，上面的例子都可以用 instruction 的顺序改变来解释。

但是这一章介绍了一个不能用顺序改变来解释的 weak memory model 现象。

# Weak memory model desiderata

所谓 Weak memory model 就是要在，
芯片（指令集）的具体构架和编译器的优化上层，
给出一个好用的数学模型。

Desiderata：

1. Formal and comprehensive
2. Not too weak (good for programmers)
3. Not too strong (good for hardware)
4. Admits optimizations (good for compilers)

**Implementability v.s. Programmability**

# The C11 memory model

> - Introduced by the ISO C/C++ 2011 standards.
> - Defines the semantics of _concurrent_ memory accesses.

> Two types of accesses:
>
> - Ordinary (Non-Atomic) -- Races are **errors**
> - Atomic -- Welcome to the expert mode

就是说，不用 atomic 相关的 API 的话，根本不允许 data race。

- 此时只能用 lock 来避免 data race。
- 如果 data race 就是想要的行为，
  那就必须要用 atomic 加 explicit 声明 `memory_order_relaxed`。

> **DRF (data race freedom) guarantee:**
>
> no data races under SC => only SC behaviors

# A spectrum of access modes

TODO 这一章画了一个格，还是看图比较好。

# C11: a declarative memory model

> _Declarative semantics_ abstracts away from implementation details.
>
> 1. a program -> a set of directed _graphs_.
> 2. The model defines what executions are _consistent_.
> 3. C/C++11 also has _catch-fire_ semantics (forbidden data races).

一个多线程的程序运行起来之后，
把每个 thread 的 event 序列都带上时间戳记录下来，
所有的 event 按照时间排序有多种可能。

- 由于 thread 之间的相互影响，不混合所有 event，
  逐个检查 event 序列，也会发现有多种可能。

因此一个 program 会产生一个 execution graph 的集合。
graph 以 event 为节点，以 event 之间的关系为边。

- 注意，一般的关系产生的是 hypergraph，
  这里可能会限制在 graph 内，以方便使用数学工具。

# Execution graphs

多线程程序的例子：

```scheme
(begin
  (store x 0)
  (store y 0)

  (begin-thread
   (store-relax x 1)
   (load-relax y))

  (begin-thread
   (store-relax y 1)
   (load-relax x)))
```

关系：

- `po` -- program order
- `rf` -- read-from

```scheme
(graph
 (nodes
  :0 (store x 0)
  :1 (store y 0)
  :2 (store-relax x 1)
  :3 (load-relax y)
  :4 (store-relax y 1)
  :5 (load-relax x))
 (relations
  (po* :0 :2 :3)
  (po* :0 :4 :5)
  (po* :1 :2 :3)
  (po* :1 :4 :5)
  (rf :0 :3)
  (rf :1 :5)))

(graph
 (nodes
  :0 (store x 0)
  :1 (store y 0)
  :2 (store-relax x 1)
  :3 (load-relax y)
  :4 (store-relax y 1)
  :5 (load-relax x))
 (relations
  (po* :0 :2 :3)
  (po* :0 :4 :5)
  (po* :1 :2 :3)
  (po* :1 :4 :5)
  (rf :0 :4)
  (rf :1 :2)))
```

# My references

## memory order in C standards

[cppreference / memory_order](https://en.cppreference.com/w/c/atomic/memory_order)

```c
// Defined in header <stdatomic.h> (since C11)
enum memory_order {
    memory_order_relaxed,
    memory_order_consume,
    memory_order_acquire,
    memory_order_release,
    memory_order_acq_rel,
    memory_order_seq_cst
};
```

Example usage:

```c
// `memory_order_seq_cst` is the default.
atomic_load(pointer) =
    atomic_load_explicit(pointer, memory_order_seq_cst)
atomic_store(pointer, value) =
    atomic_store_explicit(pointer, value, memory_order_seq_cst)

// `volatile` means simply avoid compiler optimizations.
#define volatile_load(pointer) \
    atomic_load_explicit(pointer, memory_order_relaxed)
#define volatile_store(pointer, value) \
    atomic_store_explicit(pointer, value, memory_order_relaxed)
```

TODO
