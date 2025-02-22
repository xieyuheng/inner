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
> as a simple process that receives load and store messages from more
> complicated processes, and sends value and ok messages,
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

TODO 先略过。

### 2.2.1 Sources of Inconsistency
### 2.2.2 Special Instructions to Order Memory Access
### 2.2.3 Example Architectures

## 2.3 Atomic Primitives

TODO 先略过。

### 2.3.1 The ABA Problem
### 2.3.2 The Value of FAA
### 2.3.3 Other Synchronization Hardware

# 3 Essential Theory

TODO

# 4 Practica Spin Locks
# 5 Busy-Wait Synchronization with Conditions
# 6 Read-Mostly Atomicity
# 7 Synchronization and Scheduling
# 8 Nonblocking Algorithms
# 9 Transactional Memory
