---
title: the garbage collection handbook (2nd edition)
subtitle: the art of automatic memory management
authors: [richard jones, antony hosking, eliot moss]
years: 2023
---

# 动机

[2025-11-22] 在为 x-lisp 设计 GC 的时候，
我发现我处理不好 root scanning 的问题。
这导致我认为纯粹的动态类型语言 + GC 的设计，
不应该用编译器实现，而应该用 VM 实现。
因为在没有类型信息帮助的前提下，
在 native code 中很难处理 root 存在于 register 的情况。

看了一个基于 reference counting 的方案：
- 1984-reference-count-garbage-collection.md
但是想了一下所生成的汇编代码，感觉 overhead 太大了。

但是尝试读一下这本书，再看看还有什么别的方案。
或者我对之前的方案的理解还有什么不对的地方。

我目前关心的是：

- root scanning 问题。

  - chapter 11 Run-time interface

- 备选方案的 reference counting，因为可以完全避免 root scanning。

  - chapter 5 Reference counting
  - chapter 12 Language-specific concerns
    -- 关于 dead object 的 finalisation

[2025-12-20] 感觉这里对大量的解决同一个问题的方案的比较，
非常适合 FCA。

我发现不止是 GC 这个领域有这种现象，
计算机科学的所有领域的很多问题都有这种现象，
即大量解决方案之间相互竞争的现象。

一些例子：

- 排序：有许多排序算法（快速排序、归并排序、堆排序、冒泡排序、插入排序等）。
- 哈希：解决冲突的不同方法（链地址法、开放地址法、线性探测、二次探测、双重哈希等）。
- 并发控制：在数据库中，有锁、时间戳、乐观并发控制等。
- 内存管理：除了GC，还有分页策略（FIFO、LRU、时钟算法等）。
- 图遍历：DFS、BFS等。
- 最短路径算法：Dijkstra、Bellman-Ford、Floyd-Warshall等。
- 字符串匹配：朴素算法、KMP、Boyer-Moore、Rabin-Karp等。
- 数据压缩：霍夫曼编码、LZ77、LZ78、LZW等。
- 公钥密码学：RSA、椭圆曲线密码、Diffie-Hellman等。
- 共识算法：Paxos、Raft、拜占庭容错算法等。
- 数值积分：梯形法则、辛普森法则、蒙特卡洛方法等。
- 机器学习中的优化：梯度下降、随机梯度下降、Adam、RMSprop等。
- 线性规划：单纯形法、内点法等。
- 自动微分：前向模式、反向模式。
- 解析：LL、LR、LALR等。
- 虚拟内存：页面置换算法。
- 缓存替换策略：LRU、LFU、FIFO等。
- 调度算法：在操作系统中，先来先服务、最短作业优先、轮转等。
- 负载均衡：轮询、最少连接、IP哈希等。
- 图像渲染：光线追踪、光栅化等。

可谓百舸争流，千帆竞渡。
因此可能 FCA 在「对比方案」这个方向有广阔应用是有可能的。
这个 FCA 项目可以叫做「竞渡」 -- jingdu analytics。

# Preface

> In this book, we have tried to bring together the wealth of
> experience gathered by automatic memory management researchers and
> developers over the past fifty years. The literature is huge — our
> online bibliography contains 3,400 entries at the time of writing.
>
> - We discuss and compare the most important approaches and
>   state-of-the-art techniques in a single, accessible framework.
>
> - We have taken care to present algorithms and concepts using a
>   consistent style and terminology. These are described in detail,
>   often with pseudocode and illustrations.
>
> - Where it is critical to performance, we pay attention to low level
>   details, such as the choice of primitive operations for
>   synchronisation or how hardware components such as caches
>   influence algorithm design.

# 1 Introduction

> Developers are increasingly turning to managed languages and
> run-time systems for the many virtues they offer, from the increased
> security they bestow to code to the flexibility they provide by
> abstracting away from operating system and architecture. The
> benefits of managed code are widely accepted [Butters, 2007].
> Because many services are provided by the virtual machine,
> programmers have less code to write.

可能作者也假设了大多数带有 GC 的语言是用 VM 实现的。
如果是这样，那么我所关心的，和 register 相关的
root scanning 问题可能就讨论的不多。

## 1.6. TERMINOLOGY AND NOTATION

### Liveness, correctness and reachability

区分 liveness 和 reachability。

更重要的是 object graph 这个概念，
也就是如何用 digraph 理论作为模型，
来理解编程时所用的 object 与 pointer。

| digraph | memory management          |
|---------|----------------------------|
| digraph | object graph               |
| vertex  | object                     |
| edge    | address of field in object |

在这个模型下，就可以在 CG 中区分出来 mutator 和 collector 两种职责：

> - The mutator executes application code, which allocates new objects
>   and mutates the object graph by changing reference fields so that
>   they refer to different destination objects.

所谓 reference fields 就是 edges。

> - The collector executes garbage collection code, which discovers
>   unreachable objects and reclaims their storage.

# 2 Mark-sweep garbage collection

> All garbage collection schemes are based on one of four fundamental
> approaches:
>
> - mark-sweep collection
> - copying collection
> - mark-compact collection
> - reference counting
>
> or some combination of these.

> The next four chapters focus on these four basic styles of
> collection. In Chapter 6 we compare their characteristics.

## 2.1 The mark-sweep algorithm

## 2.2 The tricolour abstraction

> The _tricolour_ abstraction [Dijkstra et al., 1976, 1978] is a
> useful characterisation of tracing collectors that permits reasoning
> about collector correctness in terms of invariants that the
> collector must preserve.

| color | meaning                     | during mark-sweep       |
|-------|-----------------------------|-------------------------|
| white | 尚未搜索的                  | not marked              |
| gray  | 当前搜索的边界（wavefront） | marked and in work list |
| black | 搜索过的                    | marked                  |

> The algorithm preserves an important invariant: at the end of each
> iteration of the marking loop, there are no references from black to
> white objects. Thus any white object that is reachable must be
> reachable from a grey object.

## 2.3 Improving mark-sweep

> Unfortunately, garbage collectors do not behave like typical
> applications. The temporal locality of mark-sweep collection is
> poor.

## 2.4 Bitmap marking

mark 信息保存在 object 之外。

除非能够自己定制 allocator，
否则就需要 object address 到 mark 的 hash map。

这是一个重要的思路（不是说 bitmap，而是说使用额外的 hash map），
因为 object 的 gc 相关的信息不用保存在 object 之内，
不需要 header object，所以是 scalable 的。

作者使用这个方案的目的是让 mark 保存在连续的内存中，
以利用 CPU cache。

> With a bitmap, marking will not modify any object, but will only
> read pointer fields of live objects. Other than loading the type
> descriptor field, no other part of pointer-free objects will be
> accessed.

因为 metadata 保存在额外的 hash map 中，
所以不用 wrap c native object：

> Bitmap marking dates to at least Lisp 1.5 but was adopted for a
> conservative collector designed to provide automatic memory
> management for uncooperative languages like C [Boehm and Weiser,
> 1988].

## 2.5 Lazy sweeping

mutator 显然可以在 sweep 的时候就恢复运行。

> The sweeper (or sweepers) could be executed as separate threads,
> running concurrently with the mutator threads, but a simple solution
> is to use _lazy sweeping_ [Hughes, 1982]. Lazy sweeping amortises
> the cost of sweeping by having the _allocator_ perform the
> sweep. Rather than a separate sweep phase, the responsibility for
> finding free space is devolved to `allocate`. At its simplest,
> `allocate` advances the sweep pointer until it finds sufficient
> space in a sequence of unmarked objects. However, it is more
> practical to sweep a block of several objects at a time.

这要求定制 allocator，
而 x-lisp 为了方便 c 扩展，
要保持 allocator 与 gc 无关。

## 2.6 Cache misses in the marking loop

> However, cache misses will be incurred as the fields of an unmarked
> object are read as part of the traversal. Thus, much of the
> potential cache advantage of using mark bitmaps in the mark phase
> will be lost as object fields are loaded.

也就是说，bitmap 对于 mark 阶段减少 cache miss 来说没什么大用。

> Boehm [2000] observes that marking dominates collection time, with
> the cost of fetching the first pointer from an object accounting for
> a third of the time spent marking on an Intel Pentium III system.

在具体测量之前，我还是不要考虑这方面的优化。

> Garner et al. observe that tracing edges rather than nodes can
> improve performance even without software prefetching, speculating
> that the structure of the loop and the first-in, first-out queue
> enables more aggressive hardware speculation through more
> predictable access patterns.

这也是一个有趣的思路，只有有了 object graph 这个思维模型之后，才容易想
到这个思路。

## 2.7 Issues to consider

> Despite its antiquity as the first algorithm developed for garbage
> collection [McCarthy, 1960], there are many reasons why mark-sweep
> collection remains an attractive option for developers and users
> today.

### Mutator overhead

> Mark-sweep in its simplest form imposes no overhead on mutator read
> and write operations. In contrast, reference counting (which we
> introduce in Chapter 5) imposes a significant overhead on the
> mutator.

> However, note that mark-sweep is also commonly used as a base
> algorithm for more sophisticated collectors which do require some
> synchronisation between mutator and collector.

> Both generational collectors (Chapter 9), and concurrent and
> incremental collectors (Chapter 15), require the mutator to inform
> the collector when they modify pointers. However, the overhead of
> doing so is typically small, a few percent of overall execution
> time.

### Throughput

> Combined with lazy sweeping, mark-sweep offers good throughput. The
> mark phase is cheap compared to other collection methods, and is
> dominated by the cost of pointer chasing. It simply needs to set a
> bit or byte for each live object discovered, in contrast to
> algorithms like semispace copying collection (Chapter 4) or
> mark-compact (Chapter 3) which must copy or move objects. On the
> other hand, like all the tracing collectors in these initial
> chapters, mark-sweep requires that all mutators be stopped while the
> collector runs.  The pause time for collection depends on the
> program being run and its input, but can easily extend to several
> seconds or worse for large systems.

对于 compiler 一类的应用来说，
所需要优化的就是 throughput，
而不用在乎 latency。

### Space usage

> Mark-sweep has significantly better space usage than approaches
> based on semispace copying. It also potentially has better space
> usage than reference counting algorithms.  Mark bits can often be
> stored at no cost in spare bits in object headers.

> On the debit side, non-compacting collectors, like mark-sweep and
> reference counting, require more complex allocators, such as
> segregated fits free-lists. The structures needed to support such
> collectors impose a further, non-negligible overhead. Furthermore,
> non-compacting collectors can suffer from fragmentation, thus
> increasing their effective space usage.

这是缺点，但是 more complex allocators 也可以直接用 c 标准库的。
相反 compacting collectors 必须要定制 allocator。

### To move or not to move?

non-moving 可以把 GC 和 allocator 完全解耦。

这里提到的 fragmentation 问题，
可以完全交给 allocator 去解决。

# 3 Mark-compact garbage collection

这么看来分类应该是：

- mark-sweep
- mark-compact
- mark-compact by copying

与 mark-sweep 相比 mark-compact 的主要难点是，
在移动 object 在内存中的位置之后，
需要更新引用了这个 object 的所有指针。

注意，命名中的 mark 和 sweep/compact 是在描述每个阶段的目的，
完成每个目的可能有不同的方法，
比如 mark 阶段一般都以 mark by traversing 来完成。
而 compact 阶段有很多方法：

- compact by filling and updating
- compact by sliding
- compact by copying

> The compaction order has locality implications. Any moving collector
> may rearrange objects in the heap in one of three ways.
>
> - Arbitrary: objects are relocated without regard for their original
>   order or whether they point to one another.

> - Linearising: objects are relocated so that they are adjacent to
>   related objects, such as ones to which they refer, which refer to
>   them, which are siblings in a data structure, and so on, as far as
>   this is possible.
>
> - Sliding: objects are slid to one end of the heap, squeezing out
>   garbage, thereby maintaining their order of placement in the heap.

## 3.1 Two-finger compaction

> Edwards’s Two-Finger algorithm [Saunders, 1974] is a two-pass,
> arbitrary order algorithm, designed to compact regions containing
> objects of a fixed size. The idea is simple: given the volume of
> live data in the region to be compacted, we know where the
> high-water mark of the region will be after compaction. Live objects
> above this threshold are moved into gaps below the threshold.

注意这里需要 "objects of a fixed size"。
也可以通过 "one level of indirect" 来处理任意 size 的 object。

既然已经假设了 "objects of a fixed size"，
为什么还需要 compact，而不是直接在 allocate 时找到下一个空位？

> The second pass updates the old values of pointers that referred to
> locations beyond the high-water mark with the forwarding addresses
> found in those locations, that is, with the objects’ new locations.

> Unfortunately, the order of objects in the heap that results from
> this style of compaction is arbitrary, and this tends to harm the
> mutator’s locality.

看来这个算法可以被命名为 compact by filling and updating。

这里的 two-finger 很形象地描述了程序员设计算法的过程，
下一节还会用到。

## 3.2 The Lisp 2 algorithm

GC 这种明显分 stage 的算法，很方便用 forth 来描述：

- mark
  - traverse
- compact
  - compute forwarding address
  - slide (relocate)
  - update reference

slide 和 update reference 两个阶段的顺序可以互换。

这个方案可以用来 compact 同一个 heap，
也可以用来把一个 heap compact 到一个新的 heap，类似 copy。

- 但是后面要介绍的 mark-compact by copying，
  直接融合了 mark 和 copy 两个阶段。

## 3.3 Threaded compaction

> The goal of threading is to allow all references to a node N to be
> found from N. It does so by temporarily reversing the direction of
> pointers.

```
A -> N
B -> N
C -> N
```

变成：

```
A <- B
B <- C
C <- N
```

我没看懂这样做意义何在。

## 3.4 One-pass algorithms

> If we are to reduce the number of passes a sliding collector makes
> over the heap to two (one to mark and one to slide objects), and
> avoid the expense of threading, then we must store forwarding
> addresses in a side table that is preserved throughout compaction.

好像只是为了不在 object header 中保存 forwarding address。

确实，问题在于保存 metadata，而方案有两种：

- 保存在 header 中。
- 保存在额外的以 object address 为 key 的 table 中。

## 3.5 Issues to consider

总结来看，这一章只有 "3.2 The Lisp 2 algorithm" 所描述的，
mark-compact by sliding 值得一学。

而 "3.1 Two-finger compaction" 所描述的算法，
只能用于 fixed sized object，根本没法接受。

### Throughput costs of compaction

> Sequential allocation in a compacted heap is fast.

这是 compacting GC 的最大优点，
-- allocation 就是简单地 increment a pointer。

因为我们在同时解决 GC 与 allocation 问题，
所以才能做到这一点。

mark-sweep 解耦了两个问题，
让实现变简单了，但是让整体的效率变低了。
但是其实也没那么糟糕，分离出来的，
专门解决 fragmentation 问题的 allocator 效率也可以很高。
想象用这里提到的 segregated-fits 来实现 allocator，
可以完全避免 fragmentation，而 allocation 就是找到合适大小的 chunk，
然后 increment a pointer，速度也可以很快。

# 4 Copying garbage collection

应该称为 mark-compact by copying。

> Its chief disadvantage is that it reduces the size of the available
> heap by half. It also requires processing all the bytes of each
> object rather than only the pointers.

## 4.3 Issues to consider

> Copying collection offers two immediately attractive advantages over
> non-moving garbage collectors like mark-sweep: it provides fast
> allocation and elimination of fragmentation.

> Simple copying collectors are also easier to implement than
> mark-sweep or mark-compact collectors.

我认为就实现之简单性而言，mark-sweep 最简单。

### Allocation

> Allocation in a compacted heap is fast because it is simple. In the
> common case, it only requires a test against a heap or block limit
> and that a free pointer be incremented. If a block-structured rather
> than a contiguous heap is used, occasionally the test will fail and
> a new block must be acquired.

> Sequential allocation also works well with multithreaded
> applications since each mutator can be given its own local
> allocation buffer in which to allocate without needing to
> synchronise with other threads. This arrangement is simpler and
> requires little metadata, in contrast with local allocation schemes
> for non-moving collectors where each thread might need its own size
> class data structures for segregated-fits allocation.

> The code sequence for such bump pointer allocation is short but,
> even better, it is well behaved with respect to the cache as
> allocation advances linearly through the heap.

> Blackburn et al. [2004a] found that although sequential allocation
> had an 11% advantage over free-list allocation in a micro-benchmark
> limit study, allocation itself accounted for less than 10% of
> overall running time in real applications. Thus, the difference in
> cost between bump pointer allocation and free-list allocation may
> not be significant.

对极限 advantage 的研究，可以告诉我们哪些优化的方向根本不值得去实现。

下面是解释，为什么 allocation 只占有 10%。

> However, allocation is only part of the picture for the mutator
> since the cost of creating a new object is likely to be dominated by
> its initialisation, certainly in systems that distinguish these
> actions.

> Furthermore, objects share similar life-cycles in many applications.
> The mutator creates some semantically related objects at around the
> same time, uses them, and finally tends to abandon them all at
> once. Here, compacted heaps offer good spatial locality, with
> related objects typically allocated on the same page and maybe in
> the same cache line if they are small. Such a layout is likely to
> lead to fewer cache misses than if related objects are allocated
> from different free-lists.

这可能是 mark-sweep GC + 独立的 allocator 的设计中，
allocator 怎么也做不到的，做不到 sequential allocation 这么好的 locality。

### Space and locality

一次 copy 之后 locality 属性就反转了：

> So, sequential allocation tends to lay out contemporaneously
> accessed objects contiguously, which helps to improve the mutator’s
> cache miss rate. But copying collection then reorders surviving
> objects in the heap. Although Cheney-style collectors need no
> auxiliary stack to guide the trace, their breadth-first traversal
> tends to separate parents and children.

但是有补救方案：

> Hierarchical decomposition offers a compromise between paying the
> costs of a tracing stack and improving the layout of objects in the
> heap.

又反转了，补救方案没什么效果：

> However, although careful reordering has benefits for some programs,
> it often has negligible effects. Why is this?  Most objects have
> short lifetimes and do not survive a collection. Moreover, many
> applications concentrate accesses, and especially writes, on these
> young objects [Blackburn and McKinley, 2003].

### Moving objects

有时候，因为没法 move objects，
所以没得选，只能用 mark-sweep。

> It is expensive to copy some objects. Although copying even a small
> object is likely to be more expensive than marking it, the cost and
> latency of doing so is often absorbed by the costs of chasing
> pointers and discovering type information. On the other hand,
> repeatedly copying large, pointer-free objects will lead to poor
> performance. One solution is simply not to copy them but instead
> devolve the management of large objects to a non-moving collector.
> Another is to copy them virtually but not physically.  This can be
> done either by holding such objects on a linked list maintained by
> the collector, or by allocating large objects on their own virtual
> memory pages which can be remapped. We consider such techniques in
> Chapters 8 to 10.

# 5 Reference counting

TODO

# 11 Run-time interface

## 11.2 Finding pointers

### Accurate pointer finding in stacks and registers

#### Finding pointers in registers

> We now introduce an approach to the callee-save registers problem.

这正是我所面临的问题。

注意，这里给出的解决方案：

- 并不需要假设 tagged value。
- 也并不需要假设没有 pointer move，
  也就是说可以支持 copying GC。

对我们有 tagged value 的 mark-sweep GC 设计来说，
这个方案可以更简单。

> First, we add metadata that indicates for each function
> which callee-save registers it saves, and where in its frame it
> saves them.

> We assume the more common design where a function saves in one go,
> near the beginning of the function, all the callee-save registers
> that it will use. If the compiler is more sophisticated and this
> information varies from place to place within a function, then the
> compiler will need to emit per-location callee-save information.

TODO 实现到 GC 的 root scanning 的时候，再回来细读这里的方案。

**关于 x-lisp 中如何实现这里的方案**

我们其实没法假设 call stack 中只有 x-lisp 的函数，
因为操作系统需要调用 c 的 main，
而 c 的 main 需要调用 x-lisp 的 _main。

我们所能假设的只是：

- 在遇到 GC 时，call stack 中，
  从 top frame 开始，到 _main 的 frame 结束，
  都是 x-lisp 的 function 的 frame。

因此，我们不能在 safepoint 保存所有的 callee saved register，
而是需要以 _main 为重点，扫描整个 call stack，
来得到精确的 register 信息。

我们不能保存所有的 callee saved register，
并且我们需要的就是知道，哪些 register 需要保存，哪些不需要保存。

不对，这里的方案没有用 shadow stack，
所以这是真的从 call stack 中找 root。

**关于 primitive function 的限制**

其实既然已经在 C 代码中了，
就可以利用 henderson frame 的技巧，
来记录 C 函数所用到的局部变量。

对于 x-lisp 函数，用扫描 call frame 的方式来找 root，
对于 C 函数，用 henderson frame 中记录的信息来找 root。

只需要能够在 call stack 中区分 primitive 函数和 x-lisp 函数就可以。

# 12 Language-specific concerns

TODO
