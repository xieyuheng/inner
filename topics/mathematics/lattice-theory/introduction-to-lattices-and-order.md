---
title: Introduction to lattices and order
subtitle: Second Edition
authors: [B. A. Davey, H. A. Priestley]
year: 2002
---

# 动机

读这本书的目标有两个：

- 一个是熟悉 FCA；
- 一个是看 propagator 中 cell 保存的东西，
  能否以某种 lattice 为 interface。

# 1. Ordered sets

## Ordered sets

一个 ordered set 就在于 reflexivity，antisymmetry 与 transitivity。

- (1) x == y -> x <= y & y <= x
- (2) x <= y & y <= x -> x == y
- (3) x <= y & y <= z -> x <= z

与 ordered set 等价的名字有 partially ordered set 和缩写 poset。

不要求 antisymmetry 称为 pre-order 或 quasi-order。

等价关系是退化的 order 称为 discrete order。

如何命名这个关系是个问题，
less than or equal to 太长了，也许 lteq 可以，
单个的英文词有 below 或 under，
这两个词暗示了画图描述 lattice 而方式，
也许 below 好一些。

最典型的 order：

- 数字而小于等于 x is less than or equal to y
- 集合的包含关系 x is included in y
- 命题的蕴含关系 x implies y

## Examples from social science and computer science

注意这里定义 interval order 的方式不是 interval 之间的包含关系，
而是两个 interval 的数字之间的大小关系：

```
x <= y := x.right <= y.left
```

- 后面也会提到用 interval 之间的包含关系来定义的 order。

这一章提到的 social choice function 非常有趣。
这是关于投票，或者集体意志和集体偏好的。

假设有 n 个人，给 m 个东西排序，
每个排序代表一个人对这 m 个东西的喜好程度，
如何获得这个集体对这个 m 个东西的喜好程度呢？

可能人们的喜好一样，但是不同的标准可以给出不同的集体喜好。
在社会学意义上，这个结果就很有意思了，
因为可以解释很多社会现象。

另外举了组织工业生产和组织会议等例子，
在设计解决方案的过程中，要求解 constraints，
而 constraints 通常是用 order 关系表达的。

可以说哪里有排序，哪里就有比较，也就有序理论。

> ..., concept analysis builds an ordered set which reveals inherent
> hierarchical structure and thence natural groupings and dependencies
> among the objects and the attributes.

formal concept analysis 也被算到 humanities 这一节了，
其实将其看作是计算机科学，甚至是纯数学也可以。

- 在 propagator 的 cell 中，保存 supported value 时，
  support 的集合越小，代表信息越多，
  也许应该被理解为 FCA 中的 attributes。

将要讲序理论在计算机领域的应用了，
这与我们的目的 propagator，息息相关了。

> In each case, a relation ⩾ serves to capture the notion of "is at
> least as informative as", with the precise interpretation depending
> on the context.

也就是说，就在 propagator 的 cell 中保存 interval 而言，
interval A 被包含在 B 中，反而代表 A 更精确，包含的信息更多，
因此 A ⩾ B -- A is at least as informative as b。

propagator 用的正是这种意义上的序关系，
merge 两份信息，会得到更多的信息，
及在 ⩾ 的意义上更大。

这里定义程序的时候，其实只要以 record 为 value，
就可以将处理输入 state 和输出 state 的 program，
理解为处理输入 record 和输出 record 的 function 了。

这里 partial maps 显然是关于递归函数的，
因为我们可以将递归函数视作是由无穷多个 partial maps 的极限。

## Diagrams: the art of drawing ordered sets
## Constructing and de-constructing ordered sets
## Down-sets and up-sets
## Maps between ordered sets
## Exercises

# 2. Lattices and complete lattices
# 3. Formal concept analysis
# 4. Modular, distributive and Boolean lattices
# 5. Representation: the finite case
# 6. Congruences
# 7. Complete lattices and Galois connections
# 8. CPOs and fixpoint theorems
# 9. Domains and information systems
# 10. Maximality principles
# 11. Representation: the general case
# Appendix A: a topological toolkit
# Appendix B: further reading
