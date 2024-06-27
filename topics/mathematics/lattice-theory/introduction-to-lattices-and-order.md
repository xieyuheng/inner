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

## 1.7 Ordered sets in the humanities and social sciences

注意这里定义 interval order 的方式不是 interval 之间的包含关系，
而是两个 interval 的数字之间的大小关系：

```
x <= y := x.right <= y.left
```

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

TODO

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
