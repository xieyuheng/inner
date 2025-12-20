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

TODO

# 4 Copying garbage collection

TODO

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
