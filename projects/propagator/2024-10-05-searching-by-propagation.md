---
title: Searching by Propagation
date: 2024-10-05
---

论文是如何用 propagation networks 来实现搜索的？
并且还支持了 dependency-directed backtracking。

首先，每个解决 constraint processing 问题的系统，
都需要两个功能：searching 和 propagation。

将 constraint processing 嵌入在 prolog 一类的逻辑式语言中，
就是在已有 searching 功能的系统中加入 propagation。

将 constraint processing 嵌入在 propagation networks 中，
也需要在已有的 propagation 功能之外支持 searching。

我觉得这在于用 TMS 记录一个 domain 中所有的选择，
然后 filter 所有的 domain 所组成的笛卡尔积的集合。

首先 `(amb cell)` 可以实现 binary choice，
其次用 `amb` 实现的 `(one-of input ... output)`
可以实现在多个 input cells 中的选择，
每个选择会生成有独立 identity 的 reason。

发现 contradiction 就可以记录一个 nogood set
（可能带有 one-of 所生成的 reason），
在 propagation 的过程中，遇到每次遇到 TMS，
都有机会用 nogood set 来 filter TMS 中的 contingent objects。

这样就实现了带有 dependency-directed backtracking 的 searching。
