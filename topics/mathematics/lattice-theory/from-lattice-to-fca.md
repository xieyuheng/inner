---
title: 从格到形式概念分析
date: 2024-07-07
---

# 题解

在介绍形式概念分析（FCA）的时候，
需要一些格论知识才能说清楚概念格的属性。
反过来，FCA 可以成为学习这些格论知识的动机，
因为如果只是单纯地去学这些知识，
而脱离了应用，是很难记住的。

# FCA 需要哪些格论知识？

可以从 Rudolf 对 FCA 的简要总结出发。

首先是如何计算 join 与 meet。
这需要 lattice 的 intersection-structure：
- meet 用到 extent 之间的 intersection；
- join 用到 intent 之间的 intersection。

其次是如何在已有 objects 和 attributes 的情况下，
生成 concept lattice 中的有 concepts。
这需了解 join-danse 与 meet-danse 的概念，
lattice 的这些子集可以用来生成 lattice 中的所有元素。

能从 lattice 的公理开始，
推导出这两点知识基本就够了。
这也正是 the order book 中在介绍 FCA 之前所介绍的概念。

## intersection-structure

TODO

## join-danse 与 meet-danse

TODO

# 画出概念格需要哪些格论知识？

首先想到的是给 lattice 中的点以 rank，来确定点的纵向坐标。
而最基础的给 rank 的方式都是基于 chain 的。
所以 chain 相关的知识就是所需要的。

TODO
