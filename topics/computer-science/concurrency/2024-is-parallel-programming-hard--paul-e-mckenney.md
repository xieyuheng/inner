---
title: Is Parallel Programming Hard, And, If So, What Can You Do About It?
author: Paul E. McKenney
created-year: 2005
updated-year: 2024
source: "https://www.kernel.org/pub/linux/kernel/people/paulmck/perfbook/perfbook.html"
---

# My Motive

[2025-02-23] 为了实现 inet-lisp 而学 parallel programming。
开始是读 2024-shared-memory-synchronization，
但是感觉读地有点累，感觉需要更多的例子才能更好理解。
因此看看这本看起来更轻松的书。

# Chapter 1 How To Use This Book

> The purpose of this book is to help you program shared-memory
> parallel systems without risking your sanity.
>
> - Or, perhaps more accurately, without much greater risk to your
>   sanity than that incurred by non-parallel programming. Which, come
>   to think of it, might not be saying all that much.

# Chapter 2 Introduction

> Parallel programming has earned a reputation as one of the most
> difficult areas a hacker can tackle. ... However, new technologies
> that are difficult to use at introduction invariably become easier
> over time.

## 2.1 Historic Parallel Programming Difficulties

> As indicated by its title, this book takes a different
> approach. Rather than complain about the difficulty of parallel
> programming, it instead examines the reasons why parallel
> programming is difficult, and then works to help the reader to
> overcome these difficulties.

> The earlier restricted availability of parallel hardware is the real
> reason that parallel programming is considered so difficult.

作者的观点是，复杂的技术只要有广泛的市场，
能被大众消费得起，就会在演化中变得简单。

这本书就是在演化的过程中，
推动 parallel programming 变简单的因素之一。

## 2.2 Parallel Programming Goals

> The three major goals of parallel programming (over and above those
> of sequential programming) are as follows:
>
> - 1. Performance.
> - 2. Productivity.
> - 3. Generality.
>
> Unfortunately, given the current state of the art, it is possible to
> achieve at best two of these three goals for any given parallel
> program. These three goals therefore form the _iron triangle of
> parallel programming_, a triangle upon which overly optimistic hopes
> all too often come to grief.

从 inet-lisp 的角度看这三个问题。
首先要保证 performance，
在这个前提下 inet-lisp 的主要特点是 generality，
需要解决的主要问题是 linear variable 带来的 productivity（易编程）问题。
注意，如果处理不好 linear variable 也会损失 performance。

### 2.2.1 Performance

> Performance is the primary goal behind most parallel-programming
> effort.  After all, if performance is not a concern, why not do
> yourself a favor: Just write sequential code, and be happy? It will
> very likely be easier and you will probably get done much more
> quickly.

### 2.2.2 Productivity

硬件贵的时候，程序员的工资不是花钱的大头；
硬件便宜，如果程序员的开发效率低下，很容易就被暴露出来了。

### 2.2.3 Generality

这里从经济学的角度对软件开发中的某些现象给出了不错的分析：

> One such tradeoff is depicted by the green “iron triangle” shown
> in Figure 2.3, which shows how productivity becomes increasingly
> important at the upper layers of the system stack, while performance
> and generality become increasingly important at the lower layers of
> the system stack.

> - The huge development costs incurred at the lower layers must be
>   spread over equally huge numbers of users (hence the importance of
>   generality), and performance lost in lower layers cannot easily be
>   recovered further up the stack.

在 stack 的底层，人们更关心 generality 和 performance。

- 因为越是底层越 general，花费可以被均摊。
- 越是底层 performance cost 也会被继承，
  所以 performance 更重要。

> - In the upper layers of the stack, there might be very few users
>   for a given specific application, in which case productivity
>   concerns are paramount. This explains the tendency towards
>   “bloatware” further up the stack: Extra hardware is often
>   cheaper than extra developers.

这里对 "bloatware" 现象的解释很令人信服。
在 stack 的上层，人们更关心 productivity。

- 因为用户少，花费没法被均摊。

> This book is intended for developers working near the bottom of the
> stack, where performance and generality are of greatest concern.

比如程序语言的设计者。

> It is important to note that a tradeoff between productivity and
> generality has existed for centuries in many fields. For but one
> example, a nailgun is more productive than a hammer for driving
> nails, but in contrast to the nailgun, a hammer can be used for many
> things besides driving nails.

对于 productivity 和 generality 之间的 tradeoff，
又给出了「铆钉枪和锤子」的例子。

关于 productivity 比 generality 重要的例子：

> This is eminently achievable. The cellphone is a computer that can
> be used to make phone calls and to send and receive text messages
> with little or no programming or configuration on the part of the
> end user.
>
> This might seem to be a trivial example at first glance, but if you
> consider it carefully you will see that it is both simple and
> profound. When we are willing to sacrifice generality, we can
> achieve truly astounding increases in productivity. Those who
> indulge in excessive generality will therefore fail to set the
> productivity bar high enough to succeed near the top of the software
> stack. This fact of life even has its own acronym: YAGNI, or “You
> Ain’t Gonna Need It.”

> Another option is to tailor a given programming language or
> environment to some abstraction (for example, Haskell, Prolog, or
> Snobol), as is shown by the circular region near the center of
> Figure 2.4.  These languages can be considered to be general in the
> sense that they are equally ill-suited to the jobs required by users
> 1, 2, 3, and 4. In other words, their generality comes at the
> expense of decreased productivity when compared to domain-specific
> languages and environments. Worse yet, a language that is tailored
> to a given abstraction is likely to suffer from performance and
> scalability problems unless and until it can be efficiently mapped
> to real hardware.

设计 general 的语言时要小心。

## 2.3 Alternatives to Parallel Programming

> In order to properly consider alternatives to parallel programming,
> you must first decide on what exactly you expect the parallelism to
> do for you.

正如 Bishop 所说：
Do not ask whether a statement is true
until you know what it means.

> As seen in Section 2.2, the primary goals of parallel programming
> are performance, productivity, and generality.

其实最主要的重点就是 performance。

所以这一章要聊的就是，为了提升 performance，
除了并行之外 alternative 的方案还有哪些。

### 2.3.1 Multiple Instances of a Sequential Application

这是最简单有效的方式。

### 2.3.2 Use Existing Parallel Software

> There is no longer any shortage of parallel software environments
> that can present a single-threaded programming environment,
> including relational databases [Dat82], web-application servers, and
> map-reduce environments.

### 2.3.3 Performance Optimization

这里最有趣的观点是，
并行优化会受到 CPU 核心个数的限制，
但是算法优化没有限制。

另外，优化最好是用科学的态度，
首先用测量来找到 bottleneck。

对于并行程序的 bottleneck：

> The fact is that every resource which is shared between multiple
> CPUs or threads is a potential bottleneck.

memory cache 其实缓减了这种 sharing，
完全解决 sharing 的问题，可能需要分布式系统与共识算法。

## 2.4 What Makes Parallel Programming Hard?

> ... consider the tasks that parallel programmers must undertake that
> are not required of sequential programmers.  We can then evaluate
> how well a given programming language or environment assists the
> developer with these tasks. These tasks fall into the four
> categories shown in Figure 2.5, each of which is covered in the
> following sections.

与串行编程相比，并行编程中程序员要面临的四类新问题：

- Work Partitioning
- Parallel Access Control
- Resource Partitioning and Replication
- Interacting With Hardware

### 2.4.1 Work Partitioning

在实现 inet-lisp 时，work partitioning 是主要问题之一。

- 背景：多个 worker threads 每个有一个 task queue，
  一个 worker 处理完一个 task 之后，
  可能会产生多个新的 tasks。

- 问题：需要平衡所有 worker 所处理的 task 数量。

- 方案 A：额外增加一个 scheduler thread，
  每次 worker 产生新 tasks 时都返回给 scheduler，
  scheduler 按照当前 worder 的 workload 情况，
  把所收到的 task 分配给各个 worker。

  - worker 新产生 tasks 时，
    也可以直接放到自己的 task queue 里，
    只有处理不过来的时候才返回给 scheduler。

  - 实现 A：每个 worker 有一个 lock-free task queue，
    scheduler 也有一个 lock-free task queue。

    - worker 从自己的 queue 前面取 task，
      返回 task 到 scheduler 的 queue 后面。
    - scheduler 从自己的 queue 前面取 task，
      返回 task 到某个 worker 的 queue 后面。

    也可以考虑在 scheduler 中给每个 worker 一个自己的 task queue，
    这样 worker 在返回 task 给 scheduler 时是 lock-free 的了。

  - 实现 B：让 worker 处理自己的 task queue，
    在这个过程中产生的新 task 都返回给 scheduler。
    当所有的 worker 都停下来之后，
    scheduler 把产生的新一批 tasks 重新分配给 worker
    （称作一个 batch）。

    这是受到简单并行（embarrassing parallelism）的启发，
    因为每个 batch 阶段都是一个简单并行问题，
    无需 worker 之间的协调与通讯，
    至少在这个 batch 阶段每个 worker 是独立的。

    这也模仿了垃圾回收器的 "stop the world" 阶段。

    - 也许以这种方式使用 CPU 中的多线程并不合理，
      但是如果迫于无奈，可以用这种方式来使用 GPU！

### 2.4.2 Parallel Access Control

就是并行的 thread 在访问公用的资源时，
需要 synchronization，比如 lock 之类的。

### 2.4.3 Resource Partitioning and Replication

> The most effective parallel algorithms and systems exploit resource
> parallelism, so much so that it is usually wise to begin
> parallelization by partitioning your write-intensive resources and
> replicating frequently accessed read-mostly resources.

分布式数据库和带有 memory cache 的多核 CPU，都是一个道理。

### 2.4.4 Interacting With Hardware

> Hardware interaction is normally the domain of the operating system,
> the compiler, libraries, or other software-environment
> infrastructure.

当需要考虑不同机器 memory model 的差异时，就已经很底层了。

### 2.4.5 Composite Capabilities

> Although these four capabilities are fundamental, good engineering
> practice uses composites of these capabilities.

### 2.4.6 How Do Languages and Environments Assist With These Tasks?

> Although many environments require the developer to deal manually
> with these tasks, there are long-standing environments that bring
> significant automation to bear.

更多的关于并行计算的语言和工具还有待研究，
inet-lisp 就是其中之一。

## 2.5 Discussion

总之，不要怕并行编程。
既然已经有很多人能做好并行编程，
比如操作系统、数据库等等，
那么我们也能。

# Chapter 3 Hardware and its Habits

## 3.1 Overview

总结那些导致 CPU 运行速度慢下来的各种阻碍。

### 3.1.1 Pipelined CPUs

为了优化运行速度，
CPU 在硬件设计上令人惊讶的地方不是 pipeline，
而是 out-of-order execution 和 speculative execution。

> Achieving full performance with a CPU having a long pipeline
> requires highly predictable control flow through the program.

pipeline flush 是阻碍之一，
而导致 pipeline flush 的因素又有很多，
比如错误预测 branch。

### 3.1.2 Memory References

memory reference 会阻碍 CPU 快速运行。
memory cache 可以缓解这个阻碍，
但是 memory cache 需要 CPU 预测访问内存的规律。

使用很多指针，比如用 link list 而不用 array，
基本就告别 memory cache 了。

### 3.1.3 Atomic Operations

atomic operation 阻碍 CPU 快速运行，
因为它们要求不同 core 的 cache 之间相互协调。

### 3.1.4 Memory Barriers

> Memory barriers prevent reorderings that the CPU (to say nothing of
> the compiler) would otherwise undertake in order to increase
> performance.

### 3.1.5 Functional Unit Failings

CPU 有大量的 functional units，
但是处理某种任务的时候，
可能只能用到少数 units。

> Modern superscalar CPUs have numerous functional units with varying
> purposes and capabilities. Each CPU is likely to have several
> arithmetic-logic units (ALUs) for integer and boolean arithmetic, a
> few vector units, a couple of floating-point units (FPUs), and at
> least one each branch unit, load unit, and store unit. Different
> CPUs will of course have different combinations of functional units.

### 3.1.6 Thermal Throttling

讲温度墙导致某些软件上的优化反而成了劣化。

### 3.1.7 Cache Misses

cache misses 阻碍 CPU 快速运行。
并行的程序会加剧 cache misses。

### 3.1.8 I/O Operations

I/O operations 阻碍 cpu 快速运行。

> A cache miss can be thought of as a CPU-to-CPU I/O operation, and as
> such is one of the cheapest I/O operations available.

> This is one of the differences between shared-memory and
> distributed- system parallelism: Shared-memory parallel programs
> must normally deal with no obstacle worse than a cache miss, while a
> distributed parallel program will typically incur the larger network
> communication latencies. In both cases, the relevant latencies can
> be thought of as a cost of communication -- a cost that would be
> absent in a sequential program. Therefore, the ratio between the
> overhead of the communication to that of the actual work being
> performed is a key design parameter.

## 3.2 Overheads

> Don’t design bridges in ignorance of materials, and don’t design
> low-level software in ignorance of the underlying hardware.

### 3.2.1 Hardware System Architecture

由于 memory cache，
个芯片本身就是一个带有多个 core 的分布式系统，
需要 cache-coherency protocol。

这个 cache-coherency protocol 本身就是一个研究领域，
看起来还挺有意思的。

### 3.2.2 Costs of Operations

> In the time required to do one CAS operation, the CPU could have
> executed more than _two hundred_ normal instructions.

这有点让人质疑，对 inet-lisp 的并行到底是不是优化了。

- 如果 inet-lisp 的正常运行本身，
  也带有很多内存访问和 cache misses，
  可能和 CAS 差不多了。

### 3.2.3 Hardware Optimizations

TODO

## 3.3 Hardware Free Lunch?

> It is only natural to ask how the hardware is helping, and the answer
> is “Quite a bit!”

> (1) One hardware optimization is large cachelines.
>
> - However, this optimization has a dark side, namely false sharing,
>   which happens when different variables in the same cacheline are
>   being updated by different CPUs, resulting in a high cache-miss
>   rate.

> (2) A second related hardware optimization is cache prefetching, in
> which the hardware reacts to consecutive accesses by prefetching
> subsequent cachelines. ... Of course, the hardware must use simple
> heuristics to determine when to prefetch, and these heuristics can
> be fooled by the complex data-access patterns in many applications.

> (3) A third hardware optimization is the store buffer, which allows
> a string of store instructions to execute quickly even when the
> stores are to non-consecutive addresses and when none of the needed
> cachelines are present in the CPU’s cache.
>
> - The dark side of this optimization is memory misordering, for
>   which see Chapter 15.

> (4) A fourth hardware optimization is speculative execution, which
> can allow the hardware to make good use of the store buffers without
> resulting in memory misordering.
>
> - The dark side of this optimization can be energy inefficiency and
>   lowered performance if the speculative execution goes awry and
>   must be rolled back and retried.

> (5) A fifth hardware optimization is large caches, allowing
> individual CPUs to operate on larger datasets without incurring
> expensive cache misses.  Although large caches can degrade both
> energy efficiency and cache-miss latency, the ever-growing cache
> sizes on production microprocessors attests to the power of this
> optimization.

注意，large cache 和 large cacheline 是两个维度的优化。

> (6) A final hardware optimization is read-mostly replication, in
> which data that is frequently read but rarely updated is present in
> all CPUs’ caches. This optimization allows the read-mostly data to
> be accessed exceedingly efficiently, and is the subject of Chapter
> 9.

## 3.3 Hardware Free Lunch?

TODO
