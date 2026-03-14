---
title: a gentle introduction to semantic subtyping
authors: [giuseppe castagna, alain frisch]
year: 2005
---

# My Motive

[2026-03-14] 2008-semantic-subtyping--dealing-set-theoretically-with-function-union-intersection-and-negation-types.md
这篇论文写得让人看不出来 subtyping 判断算法。
所以看这篇简单的。

# 3 A taste of formalism

## 3.1 Universal model

所构造的 model 里只有有限函数，
就是只对有限个参数适用的函数。

但是其实一个计算机可能处理的，都是有限函数。

## 3.2 Type representation and subtyping algorithm

本质重要的还是算法本身，
model 只是为了证明算法的正确性。

算法的重点在于计算类型的 disjunction normal form，
在 arrow 和 product 中出现的 union 和 intersection 需要被提取出来：

- product 的提取方式很简单，两个参数位置都是协变的。
- arrow 和 product 完全类似，只不过第一个参数位置是逆变的。

model 就是为了确保整个 normal formal 是由意义的。
