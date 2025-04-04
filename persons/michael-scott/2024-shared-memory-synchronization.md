---
title: shared memory synchronization
author: michael scott
year: 2024
---

# My Motive

[2025-02-21] inet-lisp 的项目已经进行到了需要验证并行计算的阶段。
初步的计划是先用 CPU thread，在 linux 上就是 pthread。
看起来需要用 thread pool 和 queue 来实现。
看这本书是为了学会如何设计相关的数据结构与算法。

这本书系统性很强，可以补全我关于并行计算的知识。

# 1 Introduction

并发（concurrency）之所以难，
在于两个并发的 event 序列，
其中的 event 可能相互交替。

- 开篇就建立了并发的模型与理论工具。

比如 [E1, E2, E3] 和 [F1, F2, F3] 这两个 event 序列，
可能交替出来 C(6, 3) = 20 种情况。

同步（synchronization）就在于排除
event 序列之间错误的交替情况。

> In a distributed (i.e., message-passing) system, synchronization is
> subsumed in communication: if thread T2 receives a message from T1,
> then in all possible execution interleavings, all the events
> performed by T 1 prior to its send will occur before any of the
> events performed by T2 after its receive. In a shared-memory system,
> however, things are not so simple. Instead of exchanging messages,
> threads with shared memory communicate implicitly through loads and
> stores. Implicit communication gives the programmer substantially
> more flexibility in algorithm design, but it requires separate
> mechanisms for explicit synchronization. Those mechanisms are the
> subject of this monograph.

> Significantly, the need for synchronization arises whenever
> operations are concurrent, regardless of whether they actually run
> in parallel. ... If a single processor core context-switches among
> concurrent operations at arbitrary times, then all interleavings are
> still possible.

> A few languages and systems guarantee that only one thread will run
> at a time, and that context switches will occur only at well defined
> points in the code. The resulting execution model is sometimes
> referred to as "cooperative" multithreading.

> As it turns out, almost all synchronization patterns in real-world
> programs (i.e., all conceptually appealing constraints on acceptable
> execution interleaving) can be seen as instances of either
> _atomicity_ or _condition synchronization_.
>
> - Atomicity ensures that a specified sequence of instructions
>   participates in any possible interleavings as a single,
>   indivisible unit -- that nothing else appears to occur in the
>   middle of its execution. (Note that the very concept of
>   interleaving is based on the assumption that underlying machine
>   instructions are themselves atomic.)
>
> - Condition synchronization ensures that a specified operation does
>   not occur until some necessary precondition is true. Often, this
>   precondition is the completion of some other operation in some
>   other thread.

如何证明所有的 synchronization patterns 都能被化归为上面两类的组合？
作者所说的，并不是所有的 synchronization patterns，
而是 real-world synchronization patterns，
这样就能避免演绎证明了，但是如何归纳证明？

> **Distribution**
>
> At the level of hardware devices, the distinction between shared
> memory and message passing disappears: we can think of a memory cell
> as a simple process that receives `load` and `store` messages from
> more complicated processes, and sends `value` and `ok` messages,
> respectively, in response. While theoreticians often think of things
> this way (the annual PODC [Symposium on Principles of Distributed
> Computing] and DISC [International Symposium on Distributed
> Computing] conferences routinely publish shared-memory algorithms),
> systems programmers tend to regard shared memory and message passing
> as fundamentally distinct. This monograph covers only the
> shared-memory case.

## 1.1 Atomicity

Atomicity 可以解决上面提到的 counter 问题。

Atomicity 可以用 lock 来实现。

> **Concurrency and Parallelism**
>
> Sadly, the adjectives “concurrent” and “parallel” are used in
> different ways by different authors.  For some authors (including
> the current ones), two operations are concurrent if both have
> started and neither has completed; two operations are parallel if
> they may actually execute at the same time. Parallelism is thus an
> implementation of concurrency. For other authors, two operations are
> concurrent if there is no correct way to assign them an order in
> advance; they are parallel if their executions are independent of
> one another, so that any order is acceptable. An interactive program
> and its event handlers, for example, are concurrent with one
> another, but not parallel. For yet other authors, two operations
> that may run at the same time are considered concurrent (also called
> task parallel) if they execute different code; they are parallel if
> they execute the same code using different data (also called data
> parallel).

介绍 fine-grain locking 与 deadlock 的例子。

- transactional memory 可以解决这里的 deadlock 问题。

> From the programmer’s perspective, fine-grain locking is a means of
> implementing atomicity for large, complex operations using smaller
> (possibly overlapping) critical sections.  The burden of ensuring
> that the implementation is correct (that it does, indeed, achieve
> deadlock-free atomicity for the large operations) is entirely the
> programmer’s responsibility. The appeal of transactional memory is
> that it raises the level of abstraction, allowing the programmer to
> delegate this responsibility to some underlying system.

也就是说，用 lock 在 composition 的过程中，有产生 deadlock 的危险。

## 1.2 Condition Synchronization

> In some cases, atomicity is not enough for correctness. Consider,
> for example, a program containing a work queue, into which
> “producer” threads place tasks they wish to have performed, and
> from which “consumer” threads remove tasks they plan to perform.
> To preserve the structural integrity of the queue, we shall need
> each insert or remove operation to execute atomically. More than
> this, however, we shall need to ensure that a remove operation
> executes only when the queue is nonempty and (if the size of the
> queue is bounded) an insert operation executes only when the queue
> is nonfull.

- 初步看来，这里的 work queue 和 producer threads 与 consumer threads，
  正是我需要用来解决 inet-lisp 中遇到的问题的 pattern。

看来这里的 Condition Synchronization 是用来实现 await 的：

```
Q.insert(data d):
  atomic
    await ¬Q.full()
    // put d in next empty slot

data Q.remove():
  atomic
    await ¬Q.empty()
    // return data from next full slot
```

atomicity 比 condition synchronization 更复杂，
因为它要求所有 threads 之间达成相互不干扰的共识。

## 1.3 Spinning Versus Blocking

> Just as synchronization patterns tend to fall into two main camps
> (atomicity and condition synchronization), so too do their
> implementations: they all employ _spinning_ or _blocking_.

> For isolated condition synchronization,
> it takes the form of a trivial loop:

```
while ¬condition
  // do nothing (spin)
```

> For mutual exclusion, the simplest implementation employs a special
> hardware instruc-tion known as `test_and_set` (TAS). The TAS
> instruction, available on almost every modern machine, sets a
> specified Boolean variable to true and returns the previous
> value. Using TAS, we can implement a trivial _spin lock_:

```scheme
(define-class lock-t ()
  :locked? bool-t)

(define (lock-acquire lock)
  (while (test-and-set lock:locked?)
    (spin)))

(define (lock-release lock)
  (assign lock :locked? false))
```

> The obvious objection to spinning (also known as _busy-waiting_) is
> that it wastes processor cycles. In a multiprocessor system it is
> often preferable to _block_ -- to yield the processor core to some
> other, runnable thread. The prior thread may then be run again later
> -- either after some suitable interval of time (at which point it
> will check its condition, and possibly yield, again), or at some
> particular time when another thread has determined that the
> condition is finally true.

> The software responsible for choosing which thread to execute when
> is known as a _scheduler_.

为了解决我在实现 inet-lisp 中遇到的问题，
我可能也需要实现一个 scheduler 来协调 worker threads 的工作。

> While blocking saves cycles that would otherwise be wasted on
> fruitless re-checks of a condition or lock, it _spends_ cycles on
> the context switching overhead required to change the running
> thread. If the average time that a thread expects to wait is less
> than twice the context switch time, spinning will actually be faster
> than blocking. ... Finally, as we shall see in Chapter 7, blocking
> (otherwise known as _scheduler-based synchronization_) must be built
> _on top_ of spinning, because the data structures used by the
> scheduler itself require synchronization.

> **Processes, Threads, and Tasks**
>
> Like “concurrent” and “parallel,” the terms “process,”
> “thread,” and “task” are used in different ways by different
> authors. In the most common usage (adopted here), a thread is an
> active computation that has the potential to share variables with
> other, concurrent threads. A process is a set of threads, together
> with the address space and other resources (e.g., open files) that
> they share. A task is a well-defined (typically small) unit of work
> to be accomplished -- most often the closure of a subroutine with
> its parameters and referencing environment. Tasks are passive
> entities that may be executed by threads. They are invariably
> implemented at user level. The reader should beware, however, that
> this terminology is not universal. Many papers (particularly in
> theory) use “process” where we use “thread.” Ada uses “task”
> where we use “thread.” The Mach operating system uses “task”
> where we use “process.” And some systems introduce additional
> words -- e.g., “activation,” “fiber,” “filament,” or “hart.”

在这里所采纳的 "thread 所执行的 task" 这种对 "task" 的用法之外，
我的 inet-lisp 实现中对 "task" 还有一个用法，
即 `worker_t` has a queue of `task_t`。
而 thread 所执行的 task 是 `run(worker)`。

## 1.4 Safety and Liveness

> Whether based on spinning or blocking, a correct implementation of
> synchronization requires both _safety_ and _liveness_.
>
> - Informally, safety means that bad things never happen:
>
>   - we never have two threads in a critical section for the same
>     lock at the same time;
>
>   - we never have all of the threads in the system blocked.
>
> - Liveness means that good things eventually happen:
>
>   - if lock L is free and at least one thread is waiting for it,
>     some thread eventually acquires it;
>
>   - if queue Q is nonempty and at least one thread is waiting to
>     remove an element, some thread eventually does.

> A bit more formally, for a given program and input,
> running on a given system,

> - safety properties
>   can always be expressed as predicates P on reachable system states S
>   -- that is, `∀S[P(S)]`.

```cicada
forall (S: ReachableState) P(S)
```

> - liveness properties require at least one extra level of quantification:
>   `∀S[P(S) → ∃T[Q(T)]]`,
>   where T is a subsequent state in the _same execution_ as S,
>   and Q is some other predicate on states.

```cicada
forall (S: ReachableState)
  (P(S)) -> exists (T: SubsequentState(S)) Q(T)
```

> From a practical perspective, liveness properties tend to be harder
> than safety to ensure -- or even to define; from a formal
> perspective, they tend to be harder to prove.

注意，这里的 `ReachableState` 和 `SubsequentState`，
都可以用开篇提出的 "多个 event 序列，外加全局状态" 来理解。

这里提出了三种 liveness：

- livelock freedom
- starvation freedom
- fairness

TODO 结合 lock 的例子，以及后续的例子充分理解这三种 liveness。
TODO 按照 liveness 的格式，定义这三种 liveness。

deadlock freedom 是一种 safety （而不是 liveness）。

TODO 按照 safety 的格式给出具体定义。

> **Multiple Meanings of “Blocking”**
>
> “Blocking” is another word with more than one meaning. In this
> chapter, we are using it in an implementation-oriented sense, as a
> synonym for “de-scheduling” (giving the underlying kernel thread
> or hardware core to another user or kernel thread). In a similar
> vein, it is sometimes used in a “systems” context to refer to an
> operation (e.g., a “blocking” I/O request) that waits for a
> response from some other system component. In Chapter 3, we will use
> it in a more formal sense, as a synonym for “unable to make forward
> progress on its own.” To a theoretician, a thread that is spinning
> on a condition that must be made true by some other thread is just
> as “blocked” as one that has given up its kernel thread or
> hardware core, and will not run again until some other thread tells
> the scheduler to resume it.  Which definition we have in mind should
> usually be clear from context.

# 2 Architectural Background

## 2.1 Cores and Caches: Basic Shared-Memory Architecture

> In a so-called symmetric machine, all memory banks are equally
> distant from every processor core. Symmetric machines are sometimes
> said to have a _uniform memory access_ (UMA) architecture. More
> common today are _nonuniform memory access_ (NUMA) machines, in
> which each memory bank is associated with a processor (or in some
> cases with a multi-processor node), and can be accessed by cores of
> the local processor more quickly than by cores of other processors.

也就是说，两个 core 访问同一块内存的时候速度可能不同。

### 2.1.1 Temporal and Spatial Locality

> In both sequential and parallel programs, performance can usually be
> expected to correlate with the temporal and spatial locality of
> memory references. If a given location `l` is accessed more than
> once by the same thread (or perhaps by different threads on the same
> core or cluster), performance is likely to be better if the two
> references are close together in time (temporal locality). The
> benefit stems from the fact that `l` is likely still to be in cache,
> and the second reference will be a hit instead of a miss.

> Similarly, if a thread accesses location `l2` shortly after `l1`,
> performance is likely to be better if the two locations have nearby
> addresses (spatial locality). Here the benefit stems from the fact
> that `l1` and `l2` are likely to lie in the same _cache line_, so
> `l2` will have been loaded into cache as a side effect of loading
> `l1`.

无奈的 abstraction leak。

### 2.1.2 Cache Coherence

> On a shared-memory parallel system, by contrast -- unless we do
> something special -- data in upper levels of the memory hierarchy
> may no longer be up-to-date if they have been modified by some
> thread on another core.

这么说来，memory cache 看起来是为 single-core 设计的，
当使用 multi-core 时，memory cache 会引发新的 cache coherence 问题。

> A _cache-coherent_ parallel system is one in which
>
> - (1) changes to data, even when cached, are guaranteed to become
>    visible to all threads, on all cores, within a bounded (and
>    typically small) amount of time;
>
> - (2) changes to the same location are seen in the same order by all
>    threads.

> On almost all modern machines, coherence is achieved by means of
> an _invalidation-based cache coherence protocol_.

### 2.1.3 Processor (Core) Locality

对于 inet-lisp 而言，thread locality 可能意味着，
worker threads 不应该每次都把新产生的 tasks 返回给 scheduler，
而是应该尽量自己处理自己产生的新 tasks，
只有在需要 balance 的时候才把多余的 tasks 返回给 scheduler。

> For busy-wait synchronization algorithms, it is particularly
> important to minimize the extent to which different threads may spin
> on the same location -- or locations in the same cache
> block. Spinning with a write on a shared location -- as we did in
> the `test_and_set` lock of Sec. 1.3, is particularly deadly: ...

一旦要考虑 cache，需要考虑的因素就太多了。

## 2.2 Memory Consistency

这一章介绍了重要的关于伪代码的术语，没法跳过。

> On a single-core machine, it is relatively straightforward to ensure
> that instructions appear to complete in execution order. Ideally,
> one might hope that a similar guarantee would apply to parallel
> machines -- that memory accesses, system-wide, would appear to
> constitute an interleaving (in execution order) of the accesses of
> the various cores.  For several reasons, this sort of _sequential
> consistency_ (Lamport 1979) imposes nontrivial constraints on
> performance.

这里的 "execution order" 可以理解为 "given order"。

> Most real machines implement a more _relaxed_ (i.e., potentially
> inconsistent) memory model, in which accesses by different threads,
> or to different locations by the same thread, may appear to occur
> “out of order” from the perspective of threads on other cores.

比如由于需要将某个 core 的 memory cache 的信息同步给其他 cores。

> When consistency is required, programmers (or compilers) must employ
> special _synchronizing instructions_ that are more strongly ordered
> than other, “ordinary” instructions, forcing the local core to
> wait for various classes of potentially in-flight
> events. Synchronizing instructions are an essential part of
> synchronization algorithms on any non-sequentially consistent
> machine.

### 2.2.1 Sources of Inconsistency

> Inconsistency is a natural result of common architectural features.

> - In an _out-of-order_ processor, for example -- one that can
>   execute instructions in any order consistent with (thread-local)
>   data dependences -- a write must be held in the _reorder buffer_
>   until all instructions that precede it in program order have
>   completed.

> - Likewise, since almost any modern processor can generate a burst
>   of `store` instructions faster than the underlying memory system
>   can absorb them, even writes that are logically ready to commit
>   may need to be buffered for many cycles. The structure that holds
>   these writes is known as a _store buffer_.

两个产生表面 ordering loop 的例子。

- 理解指令执行过程的模型需要更复杂，
  需要考虑到指令的效果传播到不同 core 的时间。

### 2.2.2 Special Instructions to Order Memory Access

本书的开篇提到了并发程序的难点在于：
两个 thread 的 event 序列可能相互 interleaving。

而这一章提到的需要程序员指明的 local order 在于：
processor 可能会不按顺序运行某个 thread 中的 instructions。

- 这一章很重要，因为规定了后面要频繁使用的 pseudocode 的语法。

  主要是一种「局部不可交换性」的标记。

  在一个 thread 中，由于没有用到 shared variable，也没有 conditional，
  所以表面上没有依赖关系的 instructions，
  在另外的 thread 中可能会由于 conditional 而产生依赖关系。

  - 由 conditional 导致的依赖关系，
    也可能会因为 processor 猜测 conditional 的结果而失效。

TODO Figure 2.5 -- local order 影响多线程程序语义的例子。

> Identifying a minimal set of ordering instructions to ensure the
> correctness of a given algorithm on a given machine is a difficult
> and error-prone task.4 In fact, the minimal fencing problem has been
> shown to be NP hard even for straightline programs when there are
> only two types of fences (Taheri et al. 2019).

> We have made a good-faith effort -- especially in Chapters 4 through
> 6 -- to explicitly specify near-minimal ordering constraints. For
> more complex algorithms -- including most of those presented in
> Chapter 8 -- we revert to unlabeled `load` and `store` instructions,
> which are fully ordered by default.

注意，minimal fencing 是一种优化，
默认用 sequentially consistent 就可以。

- C 关于 memory order 的文档：

  - https://en.cppreference.com/w/c/atomic/memory_order
  - https://en.cppreference.com/w/c/atomic
  - https://en.cppreference.com/w/c/header/stdatomic

### 2.2.3 Example Architectures

> Hill (1998) has argued that the overhead of sequential consistency
> need not be onerous, particularly in comparison to its conceptual
> benefits.

- Mark D. Hill.
  Multiprocessors should support simple memory-consistency models.
  Computer, 31(8):28–34, 1998.

> Most machines today, however, fall into two broad classes of more
> relaxed alternatives.

> - On the SPARC, x86 (both 32- and 64-bit), and IBM z Series, reads
>   are allowed to bypass writes, but `R||R`, `R||W`, and `W||W`
>   orderings are all guaranteed to be respected by the hardware, and
>   writes are always globally ordered (write atomic). Special
>   instructions -- synchronizing accesses or fences -- are required
>   only when the programmer must ensure that a write and a subsequent
>   read complete in program order.

> - On Arm, Power, and IA-64 (Itanium) machines, all four combinations
>   of local bypassing are possible: special instructions must be used
>   whenever ordering is required. Moreover, on Arm v8 and Power,
>   ordinary writes are not guaranteed to be write atomic.

> We will refer to hardware-level memory models in the SPARC/x86/z
> camp using the SPARC term TSO (Total Store Order). We will refer to
> the other machines as “more relaxed.”

> - On TSO machines, `W||R` orderings (`W||` on a `load`, `||R` on a
>   `store`, or a `W||R` `fence`) should be enforced with appropriate
>   machine instructions; other annotations can be elided from our
>   code.

> - On more relaxed machines, all annotations (explicit and implicit)
>   must be enforced with appropriate machine instructions.

> One particular form of local ordering is worthy of special mention.

> - As noted in Sec. 2.2.2, a lock `acquire` operation must ensure
>   that a thread cannot read or write shared data until it has
>   actually acquired the lock. The appropriate guarantee will
>   typically be provided by a `||RW` `load` or a `load` that is
>   followed by a `R||RW` fence.

> - In a similar vein, a lock `release` must ensure that all reads and
>   writes within the critical section have completed before the lock
>   is actually released. The appropriate guarantee will typically be
>   provided by a `RW||` `store` or a `store` that is preceded by a
>   `RW||W` fence.

> These combinations are common enough that they are sometimes
> referred to as _acquire_ and _release_ orderings.

正如 `<stdatomic.h>` 中所定义的：

```c
enum memory_order
{
    memory_order_relaxed,
    memory_order_consume,
    memory_order_acquire,
    memory_order_release,
    memory_order_acq_rel,
    memory_order_seq_cst
};
```

> They are used not only for mutual exclusion, but for most forms of
> condition synchronization as well. The IA-64 (Itanium), AArch64, and
> several research machines -- notably the Stanford Dash (Lenoski et
> al. 1992) -- support acquire and release orderings directly in
> hardware. In the mutual exclusion case, particularly when using
> synchronizing accesses rather than fences, acquire and release
> orderings allow work to “migrate” into a critical section both
> from above (prior to the lock acquire) and from below (after the
> lock release). They do not allow work to migrate out of a critical
> section in either direction.

## 2.3 Atomic Primitives

TODO

### 2.3.1 The ABA Problem
### 2.3.2 The Value of FAA
### 2.3.3 Other Synchronization Hardware

# 3 Essential Theory

> Concurrent algorithms and synchronization techniques have a long and
> very rich history of formalization -- far too much to even survey
> adequately here.
>
> - Arguably the most accessible resource for practitioners is the
>   text of Herlihy et al. (2021).
>
> - Deeper, more mathematical coverage can be found in the text of
>   Schneider (1997).

> - On the broader topic of distributed computing (which as noted in
>   the box on Chapter 1 is viewed by theoreticians as a superset of
>   shared-memory concurrency), interested readers may wish to consult
>   the classic text of Lynch (1996).

1997-on-concurrent-programming--fred-b-schneider.djvu
这本书介绍了逻辑相关的形式化方法，
值得看一看。

## 3.1 Safety

TODO 想要看懂这一章，需要先能看懂伪代码中的 `||` 参数标记。

# 4 Practica Spin Locks
# 5 Busy-Wait Synchronization with Conditions
# 6 Read-Mostly Atomicity
# 7 Synchronization and Scheduling

> So far in this monograph, we have emphasized busy-wait
> synchronization. In the current chapter we turn to mechanisms built
> on top of a _scheduler_, which multiplexes some collection of cores
> among a (typically larger) set of threads, switching among them from
> time to time and -- in particular -- when the current thread needs
> to wait for synchronization.

## 7.1 Scheduling

TODO

# 8 Nonblocking Algorithms

TODO 想要看懂这一章，需要先能看懂伪代码中的 `||` 参数标记。

# 9 Transactional Memory
