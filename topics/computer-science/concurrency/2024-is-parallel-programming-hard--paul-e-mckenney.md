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

> The major reason that concurrency has been receiving so much focus
> over the past few years is the end of Moore’s-Law induced
> single-threaded performance increases (or “free lunch” [Sut08]),
> as shown in Figure 2.1 on page 18. This section briefly surveys a
> few ways that hardware designers might bring back the “free
> lunch”.

### 3.3.1 Novel Materials and Processes

新材料与新工艺，
比如取代二氧化硅（SiO₂）。
但是感觉这里进展不是很多。

### 3.3.2 Light, Not Electrons

> ... electric waves in semiconductor materials move at between 3% and
> 30% of the speed of light in a vacuum.

> there have been some experiments with tiny optical fibers as
> interconnects within and between chips, based on the fact that the
> speed of light in glass is more than 60% of the speed of light in a
> vacuum.  One obstacle to such optical fibers is the inefficiency
> conversion between electricity and light and vice versa, resulting
> in both power-consumption and heat-dissipation problems.

### 3.3.3 3D Integration

> 3-dimensional integration (3DI) is the practice of bonding very thin
> silicon dies to each other in a vertical stack. This practice
> provides potential benefits, but also poses significant fabrication
> challenges [Kni08].

### 3.3.4 Special-Purpose Accelerators

> Software must be modified to take advantage of this specialized
> hardware, and this specialized hardware must be sufficiently
> generally useful that the high up-front hardware-design costs can be
> spread over enough users to make the specialized hardware
> affordable.

这是前面章节提到的 generality 和 performance 之间的 tradeoff 的例子。

> For example, in the mid-2020s, many are betting on special-purpose
> accelerators for artificial-intelligence and machine-learning
> workloads.

现在（2025）的感受是，人们在这方面的探索比较多。

### 3.3.5 Existing Parallel Software

又提到了 SQL，这与硬件无关但是与 "free lunch" 有关。

还提到了 parallel-software crisis，
难道只有保持 "free lunch" 才算是没有 crisis？

## 3.4 Software Design Implications

> The values of the ratios in Table 3.1 are critically important, as
> they limit the efficiency of a given parallel application. To see
> this, suppose that the parallel application uses CAS operations to
> communicate among threads.  These CAS operations will typically
> involve a cache miss, that is, assuming that the threads are
> communicating primarily with each other rather than with
> themselves. Suppose further that the unit of work corresponding to
> each CAS communication operation takes 300 ns, which is sufficient
> time to compute several floating-point transcendental
> functions. Then about half of the execution time will be consumed by
> the CAS communication operations!  This in turn means that a two-CPU
> system running such a parallel program would run no faster than a
> sequential implementation running on a single CPU.

"the unit of work corresponding to each CAS
communication operation takes 300 ns"

想要真的有效率上的优化，
就必增大 "the unit of work"，
直到超过 CAS 几个数量级。

注意这里的 "which is sufficient time to
compute several floating-point transcendental functions"，
但是其实只要 "the unit of work" 中涉及 memory references，
其用时一下就上来了。

> This illustrates how important it is for communications operations
> to be extremely infrequent and to enable very large quantities of
> processing.

> The lesson should be quite clear: Parallel algorithms must be
> explicitly designed with these hardware properties firmly in
> mind. One approach is to run nearly independent threads. The less
> frequently the threads communicate, whether by atomic operations,
> locks, or explicit messages, the better the application’s
> performance and scalability will be. This approach will be touched
> on in Chapter 5, explored in Chapter 6, and taken to its logical
> extreme in Chapter 8.

> Another approach is to make sure that any sharing be read-mostly,
> which allows the CPUs’ caches to replicate the read-mostly data, in
> turn allowing all CPUs fast access. This approach is touched on in
> Section 5.2.4, and explored more deeply in Chapter 9.

> In short, achieving excellent parallel performance and scalability
> means striving for embarrassingly parallel algorithms and
> implementations, whether by careful choice of data structures and
> algorithms, use of existing parallel applications and environments,
> or transforming the problem into an embarrassingly parallel form.

"embarrassingly parallel" 虽然有个尴尬的名字，
但是却是并行计算的核心 idea。
也许应该换个名字叫 "simply parallel"。

# Chapter 4 Tools of the Trade

## 4.1 Scripting Languages

```shell
compute_it 1 > compute_it.1.out &
compute_it 2 > compute_it.2.out &
wait
cat compute_it.1.out
cat compute_it.2.out
```

```shell
grep $pattern1 | sed -e 's/a/b/' | sort
```

## 4.2 POSIX Multiprocessing

### 4.2.1 POSIX Process Creation and Destruction

> Processes are created using the `fork()` primitive, they may be
> destroyed using the `kill()` primitive, they may destroy themselves
> using the `exit()` primitive. A process executing a `fork()`
> primitive is said to be the “parent” of the newly created
> process. A parent may wait on its children using the `wait()`
> primitive.

### 4.2.2 POSIX Thread Creation and Destruction

这里的例子展示了 threads 可以 share memory，
而前一章 process 的例子不可以 share memory。

有趣的是 process 和 thread 的术语取反了：

- process：fork & wait
- thread：create & join

应该 fork 和 join 配对：

- process：fork & join
- thread：create & wait

### 4.2.3 POSIX Locking

> The POSIX standard allows the programmer to avoid data races via
> “POSIX locking”.

```c
pthread_mutex_t
pthread_mutex_lock()
pthread_mutex_unlock()
```

这里有一个没有 lock 导致两个 threads 的 event 序列交织的情况。

### 4.2.4 POSIX Reader-Writer Locking

```c
pthread_rwlock_t
pthread_rwlock_rdlock()
pthread_rwlock_wrlock()
pthread_rwlock_unlock()
```

> ... the reader-writer lock permits an arbitrarily large number of
> readers to concurrently hold the lock.

这里有一个展示 Reader-Writer Lock 效率的例子
Listing 4.8: Measuring Reader-Writer Lock Scalability。
这种用实际代码来展示效率的方式很科学，值得学习。

- 这个例子也是 scheduler thread，
  通过全局变量来控制 worker threads 运行的例子。

> As can be seen in the figure, reader-writer locking scalability is
> decidedly non-ideal, especially for smaller sizes of critical
> sections. To see why read-acquisition can be so slow, consider that
> all the acquiring threads must update the `pthread_rwlock_t` data
> structure. Therefore, if all 448 executing threads attempt to
> read-acquire the reader-writer lock concurrently, they must update
> this underlying `pthread_rwlock_t` one at a time.

这基本上就是说，
对于 small critical sections，
带 lock 的多线程是不适用的。

> **Quick Quiz 4.21:**
>
> But one microsecond is not a particularly small size for a critical
> section.  What do I do if I need a much smaller critical section,
> for example, one containing only a few instructions?

> Answer:
>
> If the data being read _never_ changes, then you do not need to hold
> any locks while accessing it. If the data changes sufficiently
> infrequently, you might be able to checkpoint execution, terminate
> all threads, change the data, then restart at the checkpoint.
>
> ... Some other ways of efficiently handling very small critical
> sections are described in Chapter 9 [Deferred Processing].

我所想到的 inet-lisp 的「实现 B」
可能就是这里描述的 "checkpoint execution"，
也可能是 Chapter 9 的 "Deferred Processing"。

Deepseek 关于 "checkpoint execution" 的解释：

当需要修改极少变化的共享数据时，通过以下步骤避免锁的开销：

- 保存状态：记录所有线程当前的执行进度（即“检查点”）。
- 暂停线程：确保所有线程安全地停止在某个一致的状态
  （如不持有锁、未处于临界区）。
- 修改数据：在无并发访问的环境下更新数据。
- 恢复执行：从检查点重启线程，继续运行。

这么说来 inet-lisp 的「实现 B」还真实类似，
因为所有 worker threads 都处理完毕 task queue 的时候，
就是一个自然的 checkpoint。

其实根本没必要以这个「都处理完毕 task queue」为停下来的条件，
随时停下来都能获得一个安全的 checkpoint，
也许这是更好的方式，因为不同 worker 所分配到的 task 的任务量并不一样，
因此可能会产生有的 worker 先完成所有 task 的情况。
可以随时停下，就给了 scheduler 更多的发挥空间，
可以设计更好的算法来 re-balance worker 的 task queue。

### 4.2.5 Atomic Operations (GCC Classic)

小的 critical section 有问题，
而 atomic operations 就可以被认为是，
从底层出发的，用来来解决极小的 critical section 的问题的方案。

- TODO 这是否意味着所有的 atomic operations
  都可以用 lock + critical section 来实现？

> If a pair of threads concurrently execute `__sync_fetch_and_add()`
> on the same variable, the resulting value of the variable will
> include the result of both additions.

所谓 "atomic" 就在于，两个 threads 的 event 序列不会交织，
因此多个 threads 在执行多个 atomic operation 时也一定成功。

```c
// return the old value
__sync_fetch_and_add()
__sync_fetch_and_sub()
__sync_fetch_and_or()
__sync_fetch_and_and()
__sync_fetch_and_xor()
__sync_fetch_and_nand()

// return the new value
__sync_add_and_fetch()
__sync_sub_and_fetch()
__sync_or_and_fetch()
__sync_and_and_fetch()
__sync_xor_and_fetch()
__sync_nand_and_fetch()
```

`fetch-add-and` 和 `add-and-fetch` 可以相互实现：

```scheme
(define (add-and-fetch pointer value)
  (= old-value (fetch-add-and pointer value))
  (= new-value (add old-value value))
  new-value)

(define (fetch-add-and pointer value)
  (= new-value (add-and-fetch pointer value))
  (= old-value (sub new-value value))
  old-value)
```

> The classic compare-and-swap operation
> is provided by a pair of primitives:

```c
__sync_bool_compare_and_swap()
__sync_val_compare_and_swap()
```

> - The first variant returns 1 if the operation succeeded and 0 if it
>   failed, for example, if the prior value was not equal to the
>   specified old value.
>
> - The second variant returns the prior value of the location, which,
>   if equal to the specified old value, indicates that the operation
>   succeeded.

用 `compare-and-swap-value` 可以实现 `compare-and-swap?`：

```scheme
(define (compare-and-swap? pointer expected new-value)
  (= old-value (compare-and-swap-value pointer expected new-value))
  (eq? old-value expected))
```

反过是（我还不完全理解这里使用的 memory fence）：

```scheme
(define (compare-and-swap-value pointer expected new-value)
  (if (compare-and-swap? pointer expected new-value)
    expected
    (let ((result (load pointer)))
      ;; when compare-and-swap-value fails and returns result ≠ expected,
      ;; the (load pointer) of result gets its value
      ;; from a (store pointer result) by another thread.
      ;; the fence (synchronize) makes sure that
      ;; that store is also visible to all other threads
      ;; before compare-and-swap-value returns.
      (synchronize)
      ;; also, you might as well do the fence before you retry,
      ;; because having a more up-to-date view of pointer means
      ;; you’re less likely to retry again.
      (if (eq? result expected)
        ;; the result might be a new object created by another thread,
        ;; but reused the same pointer, so it is wrong to return it,
        ;; and we must retry.
        (compare-and-swap-value pointer expected new-value)
        result))))
```

> In some cases, it is only necessary to ensure that the compiler
> avoids optimizing away a given memory read, in which case the
> `READ_ONCE()` primitive may be used, as it was on line 20 of Listing
> 4.5. Similarly, the `WRITE_ONCE()` primitive may be used to prevent
> the compiler from optimizing away a given memory write. These last
> three primitives are not provided directly by GCC, but may be
> implemented straightforwardly as shown in Listing 4.9, and all three
> are discussed at length in Section 4.3.4.

> **Listing 4.9**: Compiler Barrier Primitive (for GCC)

```c
#define ACCESS_ONCE(x) (*(volatile typeof(x) *)&(x))
#define READ_ONCE(x) ({ typeof(x) ___x = ACCESS_ONCE(x); ___x; })
#define WRITE_ONCE(x, val) do { ACCESS_ONCE(x) = (val); } while (0)
#define barrier() __asm__ __volatile__("": : :"memory")
```

- 在 C 语言中，volatile 关键字用于告诉编译器：
  **该变量的值可能会在程序的控制之外被意外修改**，
  因此编译器不应对此变量进行某些优化。
  它的核心作用是 **强制编译器生成直接访问内存的代码**，
  而不是依赖寄存器缓存或进行其他优化。

C11 `<stdatomic.h>` 中对应的 API：

```c
READ_ONCE(var) → atomic_load_explicit(&var, memory_order_relaxed)
WRITE_ONCE(var, val) → atomic_store_explicit(&var, val, memory_order_relaxed)
```

我的用法是：

```c
#define volatile_load(pointer) atomic_load_explicit(pointer, memory_order_relaxed)
#define volatile_store(pointer, value) atomic_store_explicit(pointer, value, memory_order_relaxed)
```

### 4.2.6 Atomic Operations (C11)

> The C11 standard added atomic operations.

```c
// load & store
atomic_load()
atomic_store()

// barriers
atomic_thread_fence()
atomic_signal_fence()

// read-modify-write atomics
atomic_fetch_add()
atomic_fetch_sub()
atomic_fetch_and()
atomic_fetch_xor()
atomic_exchange()
atomic_compare_exchange_strong()
atomic_compare_exchange_weak()
```

> These operate in a manner similar to those described in Section
> 4.2.5, but with the addition of memory-order arguments to
> `_explicit` variants of all of the operations.

> Without memory-order arguments, all the atomic operations are fully
> ordered, and the arguments permit weaker orderings. For example,
> `atomic_load_explicit(&a, memory_order_relaxed)` is vaguely similar
> to the Linux kernel’s `READ_ONCE()`.

### 4.2.7 Atomic Operations (Modern GCC)

> One restriction of the C11 atomics is that they apply only to
> special atomic types, which can be problematic. The GNU C compiler
> therefore provides atomic intrinsics.

```c
__atomic_load()
__atomic_load_n()
__atomic_store()
__atomic_store_n()
__atomic_thread_fence()
```

> These intrinsics offer the same semantics as their C11 counterparts,
> but may be used on plain non-atomic objects. Some of these
> intrinsics may be passed a memory-order argument from this list:

```c
__ATOMIC_RELAXED
__ATOMIC_CONSUME
__ATOMIC_ACQUIRE
__ATOMIC_RELEASE
__ATOMIC_ACQ_REL
__ATOMIC_SEQ_CST
```

### 4.2.8 Per-Thread Variables

正常情况下，代码中声明的全局变量是所有 thread 共享的。

> Per-thread variables, also called thread-specific data, thread-local
> storage, and other less-polite names, are used extremely heavily in
> concurrent code, as will be explored in Chapters 5 and 8. POSIX
> supplies the `pthread_key_create()` function to create a per-thread
> variable (and return the corresponding key), `pthread_key_delete()`
> to delete the per-thread variable corresponding to key,
> `pthread_setspecific()` to set the value of the current thread’s
> variable corresponding to the specified key, and
> `pthread_getspecific()` to return that value.

> A number of compilers (including GCC) provide a `__thread` specifier
> that may be used in a variable definition to designate that variable
> as being per-thread. The name of the variable may then be used
> normally to access the value of the current thread’s instance of
> that variable. Of course, `__thread` is much easier to use than the
> POSIX thread-specific data, and so `__thread` is usually preferred
> for code that is to be built only with GCC or other compilers
> supporting `__thread`.

> Fortunately, the C11 standard introduced a `_Thread_local` keyword
> that can be used in place of `__thread`.

## 4.3 Alternatives to POSIX Operations

> Unfortunately, threading operations, locking primitives, and atomic
> operations were in reasonably wide use long before the various
> standards committees got around to them. As a result, there is
> considerable variation in how these operations are supported.

### 4.3.1 Organization and Initialization

> Although many environments do not require any special initialization
> code, the code samples in this book start with a call to `smp_init()`,
> which initializes a mapping from `pthread_t` to consecutive integers.

`smp_init()` 是 linux 内核中的函数。

### 4.3.2 Thread Creation, Destruction, and Control

这一章介绍了作者自己在例子代码中所用到的 thread API。
也是定义了 `thread_id_t`，
应该是在 `pthread_t` 之类的底层 API 上包了一层。

```c
int smp_thread_id(void)
thread_id_t create_thread(void *(*func)(void *), void *arg)
void *wait_thread(thread_id_t tid)
void wait_all_threads(void)

// macros
for_each_thread(t)
for_each_running_thread(t)
```

### 4.3.3 Locking

> A good starting subset of the Linux kernel’s locking API
> is shown in Listing 4.13.

```c
void spin_lock_init(spinlock_t *sp);
void spin_lock(spinlock_t *sp);
int spin_trylock(spinlock_t *sp);
void spin_unlock(spinlock_t *sp);
```

### 4.3.4 Accessing Shared Variables

在学习并行编程以前，我们可能认为，
下面这种 inline 一个 variable 之后代码是等价的：

```c
ptr = global_ptr;
if (ptr != NULL && ptr < high_address)
  do_low(ptr);

if (global_ptr != NULL && global_ptr < high_address)
  do_low(global_ptr);
```

但是其实不是的。

#### 4.3.4.1 Shared-Variable Shenanigans

> Given code that does plain loads and stores, the compiler is within
> its rights to assume that the affected variables are neither
> accessed nor modified by any other thread. This assumption allows
> the compiler to carry out a large number of transformations,
> including:
>
> - load tearing,
> - store tearing,
> - load fusing,
> - store fusing,
> - code reordering,
> - invented loads,
> - invented stores,
> - store-to-load transformations,
> - dead-code elimination,
>
> all of which work just fine in single-threaded code. But concurrent
> code can be broken by each of these transformations, or
> shared-variable shenanigans, as described below.

不加任何额外信息的话，
编译器默认代码是 single-threaded，
并且以此为依据来做优化。

> - **Load tearing** occurs when the compiler uses multiple load
>   instructions for a single access. For example, the compiler could
>   in theory compile the load from `global_ptr` (see line 1 of
>   Listing 4.14) as a series of one-byte loads.

> - **Store tearing** occurs when the compiler uses multiple store
>   instructions for a single access. For example, one thread might
>   store 0x12345678 to a four-byte integer variable at the same time
>   another thread stored 0xabcdef00. If the compiler used 16-bit
>   stores for either access, the result might well be 0x1234ef00,
>   which could come as quite a surprise to code loading from this
>   integer.

> - **Load fusing** occurs when the compiler uses the result of a
>   prior load from a given variable instead of repeating the load.

> - **Store fusing** can occur when the compiler notices a pair of
>   successive stores to a given variable with no intervening loads
>   from that variable. In this case, the compiler is within its
>   rights to omit the first store.

> - **Code reordering** is a common compilation technique used to
>   combine common subexpressions, reduce register pressure, and
>   improve utilization of the many functional units available on
>   modern superscalar microprocessors.

> - **Invented Load** TODO

> - **Invented stores**

```c
if (condition)
  a = 1;
else
  do_a_bunch_of_stuff(&a);

a = 1;
if (!condition) {
  a = 0;
  do_a_bunch_of_stuff(&a);
}
```

TODO

#### 4.3.4.2 A Volatile Solution

TODO

#### 4.3.4.3 Assembling the Rest of a Solution

TODO

#### 4.3.4.4 Avoiding Data Races

TODO

### 4.3.5 Atomic Operations

> The Linux kernel provides a wide variety of atomic operations,
> but those defined on type `atomic_t` provide a good start.
>
> - https://www.kernel.org/doc/Documentation/atomic_t.txt

### 4.3.6 Per-CPU Variables

TODO linux kernel 相关的 API，先跳过。

## 4.4 The Right Tool for the Job: How to Choose?

> As a rough rule of thumb,
> use the simplest tool that will get the job done.
> If you can, simply program sequentially.

> ... Furthermore, always remember that inter-process communication
> and message-passing can be good alternatives to shared-memory
> multithreaded execution, especially when your code makes good use of
> the design principles called out in Chapter 6.

> Whatever approach you take, please keep in mind that randomly
> hacking multi-threaded code is a spectacularly bad idea, especially
> given that shared-memory parallel systems use your own perceived
> intelligence against you: The smarter you think you are, the deeper
> a hole you will dig for yourself before you realize that you are in
> trouble [Pok16](https://deadlockempire.github.io).

因此测量与科学的态度很重要。

# Chapter 5 Counting

> Counting is perhaps the simplest and most natural thing a computer
> can do. However, counting efficiently and scalably on a large
> shared-memory multiprocessor can be quite challenging. Furthermore,
> the simplicity of the underlying concept of counting allows us to
> explore the fundamental issues of concurrency without the
> distractions of elaborate data structures or complex synchronization
> primitives. Counting therefore provides an excellent introduction to
> parallel programming.

感觉这将是最有趣的一章。

比如 inet-lisp 的 `worker_t` 中就有两个 count。

```c
struct worker_t {
    // ...
    size_t node_id_count;
    size_t fresh_name_count;
    // ...
};
```

## 5.1 Why Isn’t Concurrent Counting Trivial?

- just count -- loss 87% of the counts
- just count atomically -- multiple orders of magnitude slower

## 5.2 Statistical Counters

> This section covers the count that is updated extremely frequently
> and the value is read out rarely.

### 5.2.1 Design

> Statistical counting is typically handled by providing a counter per
> thread (or CPU, when running in the kernel), so that each thread
> updates its own counter. The aggregate value of the counters is read
> out by simply summing up all of the threads’ counters, relying on
> the commutative and associative properties of addition.  This is an
> example of the Data Ownership pattern that will be introduced in
> Section 6.3.4 on page 200.

Data Ownership pattern，第六章是专门讲 pattern 的！

### 5.2.2 Array-Based Implementation

> One way to provide per-thread variables is to allocate an array with
> one element per thread (presumably cache aligned and padded to avoid
> false sharing).

> Such an array can be wrapped into per-thread primitives, as shown in
> Listing 5.3 (count_stat.c).

注意，这里的代码没有直接用 array 而是做了一层抽象。

这里不用 atomic operation 了，
但是还是需要在局部避免编译器优化。
因此还是用到了 `WRITE_ONCE` 和 `READ_ONCE`。

### 5.2.3 Per-Thread-Variable-Based Implementation

> The C language, since C11, features a `_Thread_local` storage class
> that provides per-thread storage.

我应该会很少用 per-thread-variable，而是多用 global array。
因为如果 worker 直接使用 global array，scheduler 访问起来就更方便。

- 如果用的是 per-thread-variable：

  - 方案 A：还是要有 global array，并且在启动和退出时，
    将 per-thread-variable 的信息保存在 global array 中，
    让 scheduler 能够引用到。

  - 方案 B：将 per-thread-variable 全部封装在 thread 的接口函数中。
    这个方案更好一点。

### 5.2.4 Eventually Consistent Implementation

前两章的方案是每个 worker 读写起来很快，但是 scheduler 读起来较慢。

> One way to retain update-side scalability while greatly improving
> read-side performance is to weaken consistency requirements.

> The counting algorithm in the previous section is guaranteed to
> return a value between the value that an ideal counter would have
> taken on near the beginning of `read_count()`’s execution and that
> near the end of `read_count()`’s execution.

> _Eventual consistency_ [Vog09] provides a weaker guarantee: In
> absence of calls to `inc_count()`, calls to `read_count()` will
> eventually return an accurate count.

与之前的方案相比，eventual consistency 的差别应该是不保证收敛的时间。

> We exploit eventual consistency by maintaining a global
> counter. However, updaters only manipulate their per-thread
> counters. A separate thread is provided to transfer counts from the
> per-thread counters to the global counter. Readers simply access the
> value of the global counter. If updaters are active, the value used
> by the readers will be out of date, however, once updates cease, the
> global counter will eventually converge on the true value -- hence
> this approach qualifies as eventually consistent.

> **Quick Quiz 5.27**
>
> Given that in the eventually-consistent algorithm shown in Listing
> 5.5 both reads and updates have extremely low overhead and are
> extremely scalable, why would anyone bother with the implementation
> described in Section 5.2.2, given its costly read-side code?
>
> Answer:
>
> The thread executing `eventual()` consumes CPU time. As more of
> these eventually-consistent counters are added, the resulting
> `eventual()` threads will eventually consume all available
> CPUs. This implementation therefore suffers a different sort of
> scalability limitation, with the scalability limit being in terms of
> the number of eventually consistent counters rather than in terms of
> the number of threads or CPUs.
>
> Of course, it is possible to make other tradeoffs. For example, a
> single thread could be created to handle all eventually-consistent
> counters, which would limit the overhead to a single CPU, but would
> result in increasing update-to-read latencies as the number of
> counters increased. Alternatively, that single thread could track
> the update rates of the counters, visiting the frequently-updated
> counters more frequently. In addition, the number of threads
> handling the counters could be set to some fraction of the total
> number of CPUs, and perhaps also adjusted at runtime. Finally, each
> counter could specify its latency, and deadline-scheduling
> techniques could be used to provide the required latencies to each
> counter.

像是运行 `eventual()` 的这种 thread 可以称为 "side-running thread"，
当算法中引入这种 "side-running thread" 时，
就会有新的解题空间可以探索，
也会产生新的 tradeoffs。

### 5.2.5 Discussion

在多线程程序中，因为所有的线程都可以通过 shared-memory 来传递信息，
所以解题空间很广泛，不必局限于一个信息传递的方向。

## 5.3 Approximate Limit Counters

这里描述的是资源计数的问题。

有点像是内存分配的问题，但是这里只是计数。

计数问题的特点是每个 +1 中的 1 没有 identity，
而内存分配问题中的 object 有 identity。

如果在实现 inet-lisp 的过程中，
发现 malloc 是多线程的瓶颈，
就还需要自己处理 node 的内存分配问题。

### 5.3.1 Design

> But then what happens if a given thread needs to increment its
> `counter`, but `counter` is equal to its `countermax`? The trick
> here is to move half of that thread’s `counter` value to a
> `globalcount`, then increment `counter`.  For example, if a given
> thread’s `counter` and `countermax` variables were both equal to
> 10, we do the following:
>
> 1. Acquire a global lock.
> 2. Add five to globalcount.
> 3. To balance out the addition, subtract five from this thread’s counter.
> 4. Release the global lock.
> 5. Increment this thread’s counter, resulting in a value of six.
>
> Although this procedure still requires a global lock, that lock need
> only be acquired once for every five increment operations, greatly
> reducing that lock’s level of contention.  We can reduce this
> contention as low as we wish by increasing the value of
> `countermax`. However, the corresponding penalty for increasing the
> value of `countermax` is reduced accuracy of `globalcount`.

`countermax` 越高效率越高，
`countermax` 越低 `globalcount` 越准确。

> This design is an example of _parallel fastpath_, which is an
> important design pattern in which the common case executes with no
> expensive instructions and no interactions between threads, but
> where occasional use is also made of a more conservatively designed
> (and higher overhead) global algorithm. This design pattern is
> covered in more detail in Section 6.4.

### 5.3.2 Simple Limit Counter Implementation

注意，当 `add_count()` 在发现当前 thread 的 counter 余额不够增加 `delta` 时，

- 先 `globalize_count()` -- 清空当前的 counter；
- 再 直接把 `delta` 加到 `globalcount`；
- 再 `balance_count()` -- 根据 `globalcount`
  和 `globalcountmax` 还有 `globalreserve`，
  把一些 counts balance 到当前 thread 的 couter 中。

感觉看上面的描述，还不如直接看代码呢，
毕竟正如 sussman 所说，代码就是描述过程式知识的。

这里 balance counter 的方法是否也可以用来 balance task queue？

每个 thread 都有 `add_count` 和 `sub_count`：

- `add_count` 就是增加 task 到自己的 task queue 中；
- `sub_ccont` 就是处理自己 task queue 中的 task，
  如果自己的已经处理完了，可以从全局的 task queue 中取。

这么看来好像确实是可以用来 balance task queue，
可以在实验的时候把 count 改成 task queue 试试。

> Lines 1–7 show `globalize_count()`, which zeros the current
> thread’s per-thread counters, adjusting the global variables
> appropriately. It is important to note that this function does not
> change the aggregate value of the counter, but instead changes how
> the counter’s current value is represented.

用不变量来简化人们对算法的理解。

> By doing this, `balance_count()` maximizes
> use of `add_count()`’s and `sub_count()`’s low-overhead fastpaths.

这就是 "balance" 通常的意义，
比如 balanced tree 是为了让搜索的速度更快。

> As with `globalize_count()`, `balance_count()` is not permitted to
> change the aggregate value of the counter.

用不变量来简化人们对算法的理解。

TODO 为什么在 `balance_count()` 中要 `countermax /= num_online_threads();`？
这样会导致每个 thread 所能分到的 `countermax` 不同。
比如，考虑初始化的时候，每个 thread 都运行一下 `balance_count`，
假设有 3 个 threads，那么每个 thread 所分配到的 `countermax` 为：

- thread 0 -- 33.3% -- (100% / 3)
- thread 1 -- 22.2% -- ((100% - (100% / 3)) / 3)
- thread 2 -- 14.8% -- ((100% - (100% / 3) - ((100% - (100% / 3)) / 3)) / 3)

不过这个是动态分配的，所以运行起来应该还好。

注意，这里用的 thread local variables 外加：

- `count_register_thread()` -- 启动 thread 后用来初始化；
- `count_unregister_thread()` -- 退出 thread 前清理资源。

实现了动态挂载新 thread 的效果。
也就是说 thread 也有生命周期，而上面两个类似于 vue 的：

- `count_register_thread()` -- `mounted`；
- `count_unregister_thread()` -- `before-destroyed`。

我之前说我不用 thread local variables，
但是这里看来，使用的话可以保持 main thread 简单。
好像这样也方便内存管理，thread 退出时局部变量就被回收了。

### 5.3.3 Simple Limit Counter Discussion

> ... the use of a per-thread
> `countermax` reserve means that `add_count()` can fail even when the
> aggregate value of the counter is nowhere near `globalcountmax`. Similarly,
> `sub_count()` can fail even when the aggregate value of the counter is
> nowhere near zero.

这里说的 "fail" 只是 "fail to take the fast path"。

> In many cases, this is unacceptable. Even if the `globalcountmax` is
> intended to be an approximate limit, there is usually a limit to
> exactly how much approximation can be tolerated. One way to limit
> the degree of approximation is to impose an upper limit on the value
> of the per-thread `countermax` instances. This task is undertaken in
> the next section.

### 5.3.4 Approximate Limit Counter Implementation

和上一节算法的差异就是不让 `countermax` 超过 `MAX_COUNTERMAX`。

### 5.3.5 Approximate Limit Counter Discussion

> These changes greatly reduce the limit inaccuracy seen in the
> previous version, but present another problem: Any given value of
> `MAX_COUNTERMAX` will cause a workload-dependent fraction of
> accesses to fall off the fastpath.  As the number of threads
> increase, non-fastpath execution will become both a performance and
> a scalability problem. However, we will defer this problem and turn
> instead to counters with exact limits.

## 5.4 Exact Limit Counters

> To solve the exact structure-allocation limit problem noted in Quick
> Quiz 5.4, we need a limit counter that can tell exactly when its
> limits are exceeded.  One way of implementing such a limit counter
> is to cause threads that have reserved counts to give them up. One
> way to do this is to use atomic instructions. Of course, atomic
> instructions will slow down the fastpath, but on the other hand, it
> would be silly not to at least give them a try.

### 5.4.1 Atomic Limit Counter Implementation

> Unfortunately, if one thread is to safely remove counts from another
> thread, both threads will need to atomically manipulate that
> thread’s `counter` and `countermax` variables. The usual way to do
> this is to combine these two variables into a single variable, for
> example, given a 32-bit variable, using the high-order 16 bits to
> represent `counter` and the low-order 16 bits to represent
> `countermax`.

> The variables and access functions for a simple atomic limit counter
> are shown in Listing 5.12 (`count_lim_atomic.c`). The `counter` and
> `countermax` variables in earlier algorithms are combined into the
> single variable `counterandmax` shown on line 1, with `counter` in
> the upper half and `countermax` in the lower half. This variable is
> of type `atomic_t`, which has an underlying representation of `int`.

作者的代码风格很不好：

- 有很多真让人头疼的缩写 -- `int cami, int *c, int *cm`。
- 另外 thread local variable 其实也是全局变量，
  在很多没必要用全局变量的地方用了全局变量。

> **Quick Quiz 5.41**:
>
> Given that there is only one counterandmax variable, why bother
> passing in a pointer to it on line 18 of Listing 5.12?
>
> Answer:
>
> There is only one counterandmax variable per thread. Later, we will
> see code that needs to pass other threads’ `counterandmax`
> variables to `split_counterandmax()`.

本身就应该避免上面的这种问题，
在没有必要的时候不要用全局变量。
下面这个问题也是同理：

> **Quick Quiz 5.42**:
>
> Why does `merge_counterandmax()` in Listing 5.12 return an int
> rather than storing directly into an `atomic_t`?
>
> Answer:
>
> Later, we will see that we need the int return to pass to the
> `atomic_cmpxchg()` primitive.

> Listing 5.13 shows the `add_count()` and `sub_count()` functions.

> Lines 1–32 show `add_count()`, whose fastpath spans lines 8–15,
> with the remainder of the function being the slowpath. Lines 8–14
> of the fastpath form a compare-and-swap (CAS) loop, with the
> `atomic_cmpxchg()` primitive on lines 13–14 performing the actual
> CAS.

这是第一次在这本书中简单 CAS 的实际应用。

另外，注意这种先在栈中定义局部变量，
然后再调用带有副作用的函数的 c 代码风格：

```c
int add_count(unsigned long delta) {
  int c;
  int cm;
  int old;
  int new;

  // ...
  split_counterandmax(&counterandmax, &old, &c, &cm);
  // ...
}
```

这种风格不用 `malloc` 但是没法处理递归的数据类型。

这一节，点题的函数是 `flush_local_count`，
其中对每个 thread 的 counter 调用了 `atomic_xchg`，
而不用加 lock。

注意，`add_count` 和 `sub_count` 都会调用 `flush_local_count`，
也就是说在 worker thread 在做 update 的时候，
虽然不用加 lock，但是会调用很多 `atomic_xchg`
来修改所有 thread 的局部变量 -- `counterandmax`。

现在终于可以理解 5.4 开头的这句话了：

> To solve the exact structure-allocation limit problem noted in Quick
> Quiz 5.4, we need a limit counter that can tell exactly when its
> limits are exceeded.  One way of implementing such a limit counter
> is to cause threads that have reserved counts to give them up.

正是调用 `flush_local_count`，
使得 `add_count` 和 `sub_count`
在返回 `0` 来报告失败状态时是精确的报告。

问题：为什么在 Listing 5.15 的
`globalize_count` 和 `flush_local_count` 两个函数中，
修改下面两个全局变量的时候不用考虑 atomic 了？

```c
globalcount += c;
globalreserve -= cm;
```

回答：因为 `add_count` 是在加了 lock 之后，
才调用 `globalize_count` 和 `flush_local_count` 的，
这两个调用根本不在 fastpath 中。
但是这个 lock 只是对 global variables 的 lock，
所以在 `flush_local_count` 中清空所有 thread local variables
-- `counterandmax` 时还是需要 atomic。

注意，在上一个版本中（Listing 5.9: Simple Limit），
也没有考虑这两个变量的 atomic。
但是在上一个版本中，是加了 lock 之后才调用 `globalize_count` 的。

TODO 我每看懂下面这个 Quiz：

> **Quick Quiz 5.45:**
>
> TODO

下面这个 Quiz 讨论了 atomic 是如何解决各种情况的 data race 的：

> **Quick Quiz 5.46:**
>
> What prevents concurrent execution of the fastpath of either
> `add_count()` or `sub_count()` from interfering with the
> `counterandmax` variable while `flush_local_count()` is accessing it
> on line 27 of Listing 5.15?
>
> TODO

TODO

## 5.5 Parallel Counting Discussion

### 5.5.1 Parallel Counting Validation

> Many of the algorithms in this section are quite simple, so much so
> that it is tempting to declare them to be correct by construction or
> by inspection.  Unfortunately, it is all too easy for those carrying
> out the construction or the inspection to become overconfident,
> tired, confused, or just plain sloppy, all of which can result in
> bugs.

本章的测试方法可以看 `counttorture.h` 和 `limtorture.h`。
作者直接把这些头文件，拼到不同的 counter 算法的 `.c` 文件后面了。
太不讲究了。

> Although this level of validation is good and sufficient for
> textbook implementations such as these, it would be wise to apply
> additional validation before putting similar algorithms into
> production. Chapter 11 describes additional approaches to testing,
> and given the simplicity of most of these counting algorithms, most
> of the techniques described in Chapter 12 can also be quite helpful.

### 5.5.2 Parallel Counting Performance

shared-array 比 thread local variable 的 read 速度要快很多，
因为后者多了一层 pointer 的 indirect。

### 5.5.3 Parallel Counting Specializations

> Wouldn’t it be better to have a general algorithm that operated
> efficiently in all cases?

这一章的回答是没有这么好的事，和其他设计决策一样都需要 tradeoffs。
并行计算给诸多 tradeoffs 中增加了和 scalability 的效率相关的变量。

> That said, it would be good to automate this process, so that the
> software adapts to changes in hardware configuration and in
> workload.

作者这里说的是 online reconfiguration 相关的方案，
inet-lisp 是否也算是这个问题的决方案呢？
可以用 inet-lisp 来实现并行的 counter 吗？

- 但是在 inet-lisp 中 thread 的概念不是 explicit 的，
  所以应该没法直接表达这里的 parallel counter 问题。
  也许可以考虑多个 active 的函数 update 同一个局部变量的问题。
  如何在 inet-lisp 中 share 局部变量呢？

  - 比如 sum a list of number 的问题，
    朴素的 reduce 可能没法利用到多个 worker。

> In short, as discussed in Chapter 3, the laws of physics constrain
> parallel software just as surely as they constrain mechanical
> artifacts such as bridges.

> These constraints _force specialization_, though in the case of
> software it might be possible to automate the choice of
> specialization to fit the hardware and workload in question.

### 5.5.4 Parallel Counting Lessons

可以总结出的 ideas：

- partitioning
- parallel fastpath (partial parallelization)
- data ownership
- batching
- deferring activity

> The partially partitioned counting algorithms used locking to guard
> the global data, and locking is the subject of Chapter 7. In
> contrast, the partitioned data tended to be fully under the control
> of the corresponding thread, so that no synchronization whatsoever
> was required. This _data ownership_ will be introduced in Section
> 6.3.4 and discussed in more detail in Chapter 8.

> Because integer addition and subtraction are extremely cheap
> compared to typical synchronization operations, achieving reasonable
> scalability requires synchronization operations be used
> sparingly. One way of achieving this is to _batch_ the addition and
> subtraction operations, so that a great many of these cheap
> operations are handled by a single synchronization operation.

> Finally, the eventually consistent statistical counter discussed in
> Section 5.2.4 showed how _deferring activity_ (in that case,
> updating the global counter) can provide substantial performance and
> scalability benefits. This approach allows common case code to use
> much cheaper synchronization operations than would otherwise be
> possible. Chapter 9 will examine a number of additional ways that
> deferral can improve performance, scalability, and even real-time
> response.

这一节结合具体例子整体总结了各种 ideas，值得反复读。

# Chapter 6 Partitioning and Synchronization Design

TODO

# Chapter 15 Advanced Synchronization: Memory Ordering

> ... this chapter will help you gain an understanding of memory
> ordering, that, with practice, will be sufficient to implement
> synchronization primitives and performance-critical fast paths.

这一章对于理解 memory-model 很重要。

## 15.1 Memory-Model Intuitions

> The key point is that although loads and stores are conceptually
> simple, on real multicore hardware significant periods of time are
> required for their effects to become visible to all other
> threads.

最简单的直觉是：
不同 thread 的 load 和 store
不需要时间就能相互同步。

正是这个简答的直觉是需要被纠正的。

> Strange though it might seem, this means that:
>
> - a given store’s value might not be returned by a load that
>   happens later in wall-clock time,
>
> - and it also means that a given store’s value might be overwritten
>   by a store that happens earlier in wall-clock time.

### 15.1.1 Transitive Intuitions

#### 15.1.1.1 Singular Intuitive Bliss

> A program that has only one variable or only one thread
> will see all accesses in order.

"only one thread" 的情况我能理解，
"only one variable"  的情况是什么？

#### 15.1.1.2 Locking Intuitions

> The graphical description is shown in Figure 15.1, which shows a
> lock being acquired and released by CPUs 0, 1, and 2 in that order.

> In short, lock-based ordering is transitive through CPUs 0, 1,
> and 2. A key point is that this ordering extends beyond the critical
> sections, so that everything before an earlier lock release is seen
> by everything after a later lock acquisition.

这里 "transitive" 指的是传递性。
就像代数结构中的直觉是被运算律相关的公理捕捉的一样。

> For those who prefer words to diagrams, code holding a given lock
> will see the accesses in all prior critical sections for that same
> lock, transitively. ... In other words, when a CPU releases a given
> lock, all of that lock’s subsequent critical sections will see the
> accesses in all of that CPU’s code preceding that lock release.

> Inversely, code holding a given lock will be protected from seeing
> the accesses in any subsequent critical sections for that same lock,
> again, transitively. ...  In other words, when a CPU acquires a
> given lock, all of that lock’s previous critical sections will be
> protected from seeing the accesses in all of that CPU’s code
> following that lock acquisition.

> Locking is strongly intuitive, which is one reason why it has
> survived so many attempts to eliminate it. This is also one reason
> why you should use it where it applies.

#### 15.1.1.3 Release-Acquire Intuitions

> Release-acquire chains also behave in a transitively intuitive
> manner not unlike that of locking, except that release-acquire
> chains do not provide mutual exclusion.

说这里的 acquire 和 release 应该是，
传递给 `atomic_` 函数的 memory-order 参数。

#### 15.1.1.4 RCU Intuitions

TODO 先略过，因为还没看到 Chapter 9 的 RCU。

#### 15.1.1.5 Fully Ordered Intuitions

> A more extreme example of transitivity places at least one
> `smp_mb()` between each pair of accesses. All accesses seen by any
> given access will also be seen by all later accesses.

### 15.1.2 Rules of Thumb

> The first rule of thumb is that memory-ordering operations are only
> required where there is a possibility of interaction between at
> least two variables shared among at least two threads, which
> underlies the singular intuitive bliss presented in Section
> 15.1.1.1.

TODO 为什么 one variable many threads 不会有问题呢？

下面的术语指的是 thread 之间，
由于 memory accesses 而产生的 dependency：

- load-to-store
- store-to-store
- store-to-load

> 1. Memory-ordering operations are required only if at least two
>    variables are shared by at least two threads.
>
> 2. If all links in a cycle are store-to-load links, then minimal
>    ordering suffices.
>
> 3. If all but one of the links in a cycle are store-to-load links,
>    then each store-to-load link may use a release-acquire pair.
>
> 4. Otherwise, at least one full barrier is required between each
>    pair of non-store-to-load links.

> One final word of advice: Use of raw memory-ordering primitives is a
> last resort. It is almost always better to use existing primitives,
> such as locking or RCU, thus letting those primitives do the memory
> ordering for you.

## 15.2 Ordering: Why and How?

这里举的反直觉的例子，
和 Michael Scott 在 2024-shared-memory-synchronization 中举的一个例子类似，
反直觉的结果是两个 thread 上的事件产生了循环的 ordering。

- 注意，Michael Scott 是这本书的编者的导师。

### 15.2.1 Why Hardware Misordering?

解释为什么 多核 + cache + store-buffer 的设计会产生 misordering。

- CPU 运行一般指令的速度比访问内存要快两个数量级。
- 因此需要有 cache 来解决这个速度差异。
- 如果 CPU 所 store 的内存不在 cache 中，
  就需要先记录到 store-buffer 中。

> When a given CPU stores to a variable not present in that CPU’s
> cache, then the new value is instead placed in that CPU’s store
> buffer. The CPU can then proceed immediately, without having to wait
> for the store to do something about all the old values of that
> variable residing in other CPUs’ caches.

用 event table 逐步解释上面的反直觉的例子。

> In summary, store buffers are needed to allow CPUs to handle store
> instructions efficiently, but they can result in counter-intuitive
> memory misordering.

### 15.2.2 How to Force Ordering?

可惜没有讲 pthread API。

TODO

### 15.2.3 Basic Rules of Thumb

TODO

# Appendix A Important Questions

TODO

# Appendix C Why Memory Barriers?

TODO
