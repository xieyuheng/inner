---
title: automated lattice drawing
author: ralph freese
year: 2004
---

# My Motive

**动机 A：**

我想要为我所写的 FCA library，
制作一个渲染 concept lattice 的 web 前端。

有两类方案：

- 在二维空间中渲染：可以基于 svg 或 canvas 来实现；
- 在三维空间中渲染：可以基于 three.js 来实现。

这篇论文的方案是在三维空间中渲染。

**动机 B：**

学习一些 good drawing 的原则。

# 略读

本论文前几章的例子由于不是 FCA 所以我兴趣不大。
还有关于历史叙述，作者都说了是构拟的历史，而且很枯燥。
需要读的主要是算法部分。

# 4 Our Algorithm

> Our algorithm is based on a combination of a mathematical rank
> function to determine the height and a modification of the
> “forces” method of graph theory.

算法的类型是 layer-based drawing，
所以也许可以找类似的 graph drawing 的论文。

- layer-based drawing 最简单的情形是
  [graded poset](https://en.wikipedia.org/wiki/Graded_poset)，
  也许别的 layer-based drawing 都是 graded poset 情形的变体。
  - 因为 lattice 经常带有不能被 graded 的部分，所以需要变体。

> The program first calculates the rank function on the ordered set
> and uses this to determine the height of the elements. It then
> places the points in three space using the rank for the height,
> i.e., for the z-coordinate. The points of the same rank are arranged
> around a circle on a plane parallel to the x–y plane. We now place
> imaginary forces on the elements. Comparable elements are attracted
> to each other while incomparable elements are repulsed.

> These forces are applied several times in three phases. In the first
> phase the repulsive force is set to be very strong; in the second
> phase the attractive force is strong; and in the final phase the
> forces are balanced. Then an optimal projection to two space is
> chosen (either by the program or the user).

有了上面这个对方案的概览之后，
也可能有多种具体实现方式。

如果我们对作者的具体方案不满意，
也可以按照这个方案概览来修改。

## 4.1 The Rank Function

作者给出的 rank 公式是：

```
rank(a) = height(a) − depth(a) + M
```

其中：

- `height(a)` 是到底的最长 chain 的长度；
- `depth(a)` 是到顶的最长 chain 的长度；
- `M` 是任意常数，通常选为整个 lattice 的最长 chain 的长度。

启发作者获得这个公式的典型例子是，
最典型的不能被 graded 的例子：

```
     e
    / \
   /   c
  d    |
   \   b
    \ /
     a
```

按照作者的 rank 公式有：

```
rank(a) = 0 - 3 + 3 = 0
rank(b) = 1 - 2 + 3 = 2
rank(c) = 2 - 1 + 3 = 4
rank(d) = 1 - 1 + 3 = 3
rank(e) = 3 - 0 + 3 = 6
```

```
     6
    / \
   /   4
  3    |
   \   2
    \ /
     0
```

这个公式为什么有效呢？
其实只是对高度（最长 chain 长度）比较低的 lattice 有效。

假设最长链的长度是 `n`，这个公式有如下特点：

- `rank(bottom) = 0`
- `rank(top) = 2n`
- 最长链上的点的 rank 是 `[0, 2, 4, ..., 2n]`
- 中间点的 rank 是 `n`
- 中间两个点的 rank 分别是 `n-1` 和 `n+1`
- 所有非最长链上的点，
  不考虑 bottom 和 top 的话，
  间距都是 2

也就是说，对于高度比较高的 lattice，
这样的 rank 会让所有的点以上下间距 2 挤在中间。

我想，也许给出就最长链而言均匀的 rank 更好。
算法也很简单，直接均分最长链就行了。

注意，是这样的基于最长链的 rank，
都是以最长链的骨架为基础的，
只要这个骨架不变，
纵向的 rank 就不变。

- 之前我还不理解，
  为什么 ordered set 中的 chain
  要单独拿出来研究，
  现在理解了。

  - 是时候回顾 the order book 中之前掠过的部分了。

## 4.2 Initialization and Force Scheme

TODO

## 4.3 Iteration

TODO

# 5 What is a ‘Nice’ Diagram?

TODO
