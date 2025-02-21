---
title: shared memory synchronization
author: michael scott
year: 2024
---

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
> programs (i.e., all concep- tually appealing constraints on
> acceptable execution interleaving) can be seen as instances of
> either _atomicity_ or _condition synchronization_.
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

## 1.1 Atomicity

TODO

## 1.2 Condition Synchronization

TODO

# 2 Architectural Background
# 3 Essential Theory
# 4 Practica Spin Locks
# 5 Busy-Wait Synchronization with Conditions
# 6 Read-Mostly Atomicity
# 7 Synchronization and Scheduling
# 8 Nonblocking Algorithms
# 9 Transactional Memory
