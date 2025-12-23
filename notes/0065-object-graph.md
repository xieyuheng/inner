---
title: object graph
date: 2025-12-19
---

在学习 "the garbage collection handbook" 时，
为了处理 liveness 和 reachability，
书中提出了 object graph 的概念。

也就是如何用 digraph 理论作为模型，
来理解编程时所用的 object 与 pointer。

| digraph | memory management          | implementation          | c             |
|---------|----------------------------|-------------------------|---------------|
| digraph | object graph               | heap + stack + register |               |
| vertex  | object                     | pointer                 | `object_t *`  |
| edge    | address of field in object | pointer field           | `object_t **` |

也就是说：

- 一个 object 中的 address，是一个 vertex 的潜在 edge。
- 当这个 address 保存了另一个 object 时，
  这两个 object 所代表的 vertex 之间才有了一个 edge。
- 也可以用 null 来把所有未连接的潜在 edge 理解为连接到 null。

这种理解 edge 的方式，是我之前没想到的。
我之前把 pointer 理解为 digraph 的方式是诉诸直觉，
而不是诉诸数学的（set theory 的），是不精确的。

- 注意，digraph 中没有所谓「潜在的 edge」，
  如果需要，那就是 ported digraph 了。

  - 在 inet 中，我已经大量使用 ported graph 了。

object graph 的理论意味着，
所有能够被计算机实现的复杂拓扑结构，
都是通过转化为 digraph 来实现的。

从计算机的角度看，digraph 其实是最基础的数据结构。

推论：所有看似是 digraph 推广的拓扑类数据结构，
比如 cell complex，都可以转化为 digraph 的理论。

在具体实现时，可以参考将 hypergraph
转化为 bipartite graph（二分图）来实现的方式。
所有高维的拓扑元素，也将被转化为某一类点。
