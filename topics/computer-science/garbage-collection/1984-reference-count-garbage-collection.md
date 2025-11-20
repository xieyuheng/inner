---
title: reference count garbage collection
author: thomas w christopher
year: 1984
---

# 动机

[2025-11-19] 在为实现 x-lisp 的 GC 而做调研。

2001-constant-time-root-scanning-for-deterministic-garbage-collection
中提到了这篇论文。

我现在关心的是如何找 root。

这篇论文所讲的应该是以引用计数为基础的内存管理，
如何利用 GC 处理 circle 引用的问题。
而不是专门关于 root scanning 的论文。

用引用计数来找 root 的方法是：
如果一个 object 在 heap 中被引用的次数，
比 object 本身所记录的引用次数少，
就代表有额外的引用来自 stack 或 register，
因此这个 object 就应该被作为 root。

# SUMMARY

> A garbage collection algorithm that permits a reference count
> storage reclamation scheme to collect circularly linked inaccessible
> structures is presented. The algorithm requires no additional
> information beyond that required by a reference count scheme. In
> particular, it does not require the garbage collector to be able to
> find pointers outside the heap. The algorithm is most useful for
> augmenting reference count storage reclamation systems and for
> implementing storage management systems on top of languages that do
> not provide their own. It is, however, considerably less efficient
> in space and time than conventional garbage collection systems.

# INTRODUCTION

> The essential idea is this: from the reference counts of objects in
> the heap, one may subtract the number of references from other
> objects in the heap. A reference count that remains non-zero
> indicates that an object is pointed to from outside the heap and may
> serve as a root of a marking phase.

核心思想极为简单，问题就是用什么算法来高效地实现了。

如果把 root scanning 问题和 mark and sweep 问题分开，
假设已经有了一个 reference counting system，
那么显然可以用 O(n^2) 来完成 root scanning，其中 n 是 heap size。
但是这也显然是没法接受的时间复杂度。

# THE ALGORITHM

算法的核心是在 GC 阶段通过递减 object 的 reference count，
来发现 root，也就是那些 reference count 没有被递减到 0 的 object。

与普通的 mark-and-sweep GC 的复杂度对比来看：

| stage | complexity |
|-------|------------|
| mark  | O(R * C)   |
| sweep | O(R + N)   |

其中 R 是 reachable object 的个数，
N 是 non-reachable garbage object 的个数，
C 是 object direct children 的个数，
这依赖于具体的 object，但是我们还是用一个常数标记出来。

论文所描述的算法的复杂度：

| stage    | complexity     |
|----------|----------------|
| de-count | O((R + N) * C) |
| mark     | O(R * C)       |
| sweep    | O(R + N)       |
| re-count | O(R * C)       |

> At the end of step one [de-count], the reference counts indicate the
> number of references to the objects from outside the heap. A zero
> reference count indicates that the object is pointed to only from
> within the heap, if at all.

其中 R 的重复其实还好，因为在 reference counting 系统中，
只有当大部分 object 都是带有循环引用的 garbage 时才需要 GC。
因此 R 相比 N 应该很小。

论文所描述的算法重用了 reference count field，
来作为 intrusive linked list 的 link field，
这个 link list 就是普通 GC 中 mark 阶段保存 gray object 的 queue。

这个方案显然是不必要的，可以被普通的 queue 来代替。
object 本身除了 reference count field 之外，也可以带上 mark field。
这个 mark field 其实是必要的，否则就没法标记 gray，
来避免将一个 object 反复加入 object queue。

另外，为了避免 re-count 中的 C，
也可以给 object 加上一个专门用于 de-count 的 shadow count field。

| stage      | complexity     |
|------------|----------------|
| copy-count | O(R + N)       |
| de-count   | O((R + N) * C) |
| mark       | O(R * C)       |
| sweep      | O(R + N)       |

sweep 过程可以 clear shadow count，
或者也可以不管，反正下一个 GC 周期开始时会 copy-count。

---
title: reference count garbage collection
author: thomas w christopher
year: 1984
---

# 动机

[2025-11-19] 在为实现 x-lisp 的 GC 而做调研。

2001-constant-time-root-scanning-for-deterministic-garbage-collection
中提到了这篇论文。

我现在关心的是如何找 root。

这篇论文所讲的应该是以引用计数为基础的内存管理，
如何利用 GC 处理 circle 引用的问题。
而不是专门关于 root scanning 的论文。

用引用计数来找 root 的方法是：
如果一个 object 在 heap 中被引用的次数，
比 object 本身所记录的引用次数少，
就代表有额外的引用来自 stack 或 register，
因此这个 object 就应该被作为 root。

# SUMMARY

> A garbage collection algorithm that permits a reference count
> storage reclamation scheme to collect circularly linked inaccessible
> structures is presented. The algorithm requires no additional
> information beyond that required by a reference count scheme. In
> particular, it does not require the garbage collector to be able to
> find pointers outside the heap. The algorithm is most useful for
> augmenting reference count storage reclamation systems and for
> implementing storage management systems on top of languages that do
> not provide their own. It is, however, considerably less efficient
> in space and time than conventional garbage collection systems.

# INTRODUCTION

> The essential idea is this: from the reference counts of objects in
> the heap, one may subtract the number of references from other
> objects in the heap. A reference count that remains non-zero
> indicates that an object is pointed to from outside the heap and may
> serve as a root of a marking phase.

核心思想极为简单，问题就是用什么算法来高效地实现了。

如果把 root scanning 问题和 mark and sweep 问题分开，
假设已经有了一个 reference counting system，
那么显然可以用 O(n^2) 来完成 root scanning，其中 n 是 heap size。
但是这也显然是没法接受的时间复杂度。

# THE ALGORITHM

算法的核心是在 GC 阶段通过递减 object 的 reference count，
来发现 root，也就是那些 reference count 没有被递减到 0 的 object。

与普通的 mark-and-sweep GC 的复杂度对比来看：

| stage | complexity |
|-------|------------|
| mark  | O(R * C)   |
| sweep | O(R + N)   |

其中 R 是 reachable object 的个数，
N 是 non-reachable garbage object 的个数，
C 是 object direct children 的个数，
这依赖于具体的 object，但是我们还是用一个常数标记出来。

论文所描述的算法的复杂度：

| stage    | complexity     |
|----------|----------------|
| de-count | O((R + N) * C) |
| mark     | O(R * C)       |
| sweep    | O(R + N)       |
| re-count | O(R * C)       |

> At the end of step one [de-count], the reference counts indicate the
> number of references to the objects from outside the heap. A zero
> reference count indicates that the object is pointed to only from
> within the heap, if at all.

其中 R 的重复其实还好，因为在 reference counting 系统中，
只有当大部分 object 都是带有循环引用的 garbage 时才需要 GC。
因此 R 相比 N 应该很小。

论文所描述的算法重用了 reference count field，
来作为 intrusive linked list 的 link field，
这个 link list 就是普通 GC 中 mark 阶段保存 gray object 的 queue。

这个方案显然是不必要的，可以被普通的 queue 来代替。
object 本身除了 reference count field 之外，也可以带上 mark field。
这个 mark field 其实是必要的，否则就没法标记 gray，
来避免将一个 object 反复加入 object queue。

另外，为了避免 re-count 中的 C，
也可以给 object 加上一个专门用于 de-count 的 shadow count field。

| stage      | complexity     |
|------------|----------------|
| copy-count | O(R + N)       |
| de-count   | O((R + N) * C) |
| mark       | O(R * C)       |
| sweep      | O(R + N)       |

sweep 过程可以 clear shadow count，
或者也可以不管，反正下一个 GC 周期开始时会 copy-count。
