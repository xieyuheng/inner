---
title: Parallel iNet
date: 2023-11-30
---

# Problem

To implement parallel interaction nets,
we need to let many threads share the same memory.

To allocate memory of cells of the same size,
we can use an array, and a stack of unused indexes
(let's call this free-stack).

But the stack will still be the part where multi-thread is not safe
right? If any part of the implementation is not lock-free, the fine
grain parallel advantage of inet might be lost.

# Solution A

Maybe i can just use 8 "free-stacks" for 8 threads.

When a used index is to be freed, i can see which thread is having the
least free indexes, and give the new freed index to it.

- The query about "the least free indexes" is just heuristic,
  thus can be lock-free.

And a "free-stack" can be viewed as a ring,
i give back to the front instead of the end,
thus lock-free.

[2024-11-06] `malloc` 已经实现类似的功能了：
- [How does malloc work in a multithreaded environment?](https://stackoverflow.com/questions/10706466/how-does-malloc-work-in-a-multithreaded-environment)
实在不行还有 [jemalloc](https://jemalloc.net)：
- [How does jemalloc work? What are the benefits?](https://stackoverflow.com/questions/1624726/how-does-jemalloc-work-what-are-the-benefits)
所以，不用可以先不考虑 malloc 的并行性能。
这样可以让实现简化很多。

# Datatypes

I need to allocate the following kinds of datatypes:

```
Node {
  definition: NodeDefinition
  ports: List<Port>
}

Port {
  node: Node
  halfEdge?: HalfEdge
}

HalfEdge {
  otherHalfEdge: HalfEdge
  port?: Port
}
```

[2024-11-06] 上面的定义以及 inet-js 的带有 `HalfEdge` 的实现是错误的，
[对 free port 的新认识](2024-11-06-free-port.md)，
使得我们可以避免 `HalfEdge`。
