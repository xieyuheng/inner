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

> - Answer: If the data being read _never_ changes, then you do not
>   need to hold any locks while accessing it. If the data changes
>   sufficiently infrequently, you might be able to checkpoint
>   execution, terminate all threads, change the data, then restart at
>   the checkpoint.
>
>   ... Some other ways of efficiently handling very small critical
>   sections are described in Chapter 9 [Deferred Processing].

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

用 `compare-and-swap-value` 可以实现 `compare-and-swap?`
（反过来好像不行）：

```scheme
(define (compare-and-swap? pointer expected new-value)
  (= old-value (compare-and-swap-value pointer expected new-value))
  (eq? old-value expected))
```

linux kernel 有定义自己的 macro：

```c
READ_ONCE(x)
WRITE_ONCE(x, val)
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

TODO linux kernel 相关的 API，先跳过。

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

TODO
