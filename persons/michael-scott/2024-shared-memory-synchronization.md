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

TODO

## 1.4 Safety and Liveness

TODO

# 2 Architectural Background
# 3 Essential Theory
# 4 Practica Spin Locks
# 5 Busy-Wait Synchronization with Conditions
# 6 Read-Mostly Atomicity
# 7 Synchronization and Scheduling
# 8 Nonblocking Algorithms
# 9 Transactional Memory
