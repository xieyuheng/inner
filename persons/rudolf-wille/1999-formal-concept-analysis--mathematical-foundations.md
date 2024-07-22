---
title: Formal Concept Analysis
subtitle: Mathematical Foundations
authors: [Bernhard Ganter, Rudolf Wille]
year: 1999
---

# O. Order-theoretic Foundations

## 0.1 Ordered Sets

二元关系（binary relation）。

序关系（order relation）。

下邻居（lower neighbour），上邻居（upper neighbour）。

层级图（line diagram，Hasse diagram）。

可比较（comparable），不可比较（incomparable）。

链（chain），反链（anitchain），长度（length），宽度（width）。

区间（interval），主理想（principal ideal），主滤子（principal filter）。

保序映射（order-preserving map），
保序嵌入（order-embedding），
序同构（order-isomorphism）。

直接积（direct product），不交并（disjoint union，tagged union）。

对偶序（dual order），对偶陈述（dual statement）。

下界（lower bound），上界（upper bound），
最大下界infimum），最小上界（supremum），
取下（meet），取上（join）。

## 0.2 Complete Lattices

格（lattice），完全格（complete lattice）。

顶（top，largest element，unit element），
底（bottom，smallest element，zero element）。

用取上的等式定义序关系，
用取下的等式定义序关系。

取上与取下满足结合律与广义结合律。

对偶格（dual lattice）。

为了证明一个序集是完全格，只需要证明下确界存在，
因为一个子集的上界的集合的下确界，就是这个子集的上确界。
反之证明上确界存在也可以。

取上不可约（join-irreducible）：不可被分解为另外两个元素的取上，有唯一下邻居；
取下不可约（meet-irreducible）：不可被分解为另外两个元素的取下，有唯一上邻居。

取上稠密（join-dense），
取下稠密（meet-dense）。

一个格的所有取上不可约元素的集合，是取上稠密的，
也许可以称作取上基，可以通过取上来生成所有元素；
一个格的所有取下不可约元素的集合，是取下稠密的，
也许可以称作取下基，可以通过取下来生成所有元素。

总结来说：
取上不可约集是取上基：最小取上稠密集；
取下不可约集是取下基：最小取下稠密集。

底的上邻居都是取上不可约的，称为格的原子（atom）；
顶的下邻居都是取下不可约的，称为格的对偶原子（coatom）。

一个格的原子构成取上基，称这个格具有原子性（atomistic）。

取上子半格（join-subsemilattice），
取下子半格（meet-subsemilattice），
子格（sublattice）；
上确界子半格（supremum-subsemilattice），
下确界子半格（infimum-subsemilattice），
完全子格（complete sublattice）。

保取上映射（join-preserving map），
保取下映射（meet-preserving map），
格同态（lattice homomorphism）；
保上确界映射（supremum-preserving map），
保下确界映射（infimum-preserving map），
完全格同态（complete lattice homomorphism）。

问题：
一个序集上如果可以定义取上与取下而成为一个格，这个格是唯一的吗？
也就是说，一个序集可以有不同的格结构吗？
假设我们按照元素个数不同来分类，画出所有序集的 Hasse 图，
可以看出，所有满足格条件的 Hasse 图，是所有的序集的子集。
那么对于有限情况而言，一个序集的格结构就是唯一的了。

## 0.3 Closure Operators

注意，这里定义的闭包系统（closure system），
就是 The Order Book 中定义的交集结构（intersection-structure），
但是在那本书里没有强调闭包算子。

闭包结构（closure structure）也是一个十分重要的数学结构，
它出现在拓扑、代数、逻辑等各个领域。

常见的闭包算子（closure operator），
是就某个集合 A 的幂集 PowerSet(A) 而言的。

```cicada
claim A: Set

closure: (PowerSet(A)) -> PowerSet(A)
extensive: (X: PowerSet(A)) -> SubsetOf(X, closure(X))
increasing: (X, Y: PowerSet(A), SubsetOf(X, Y)) -> SubsetOf(closure(X), closure(Y))
idempotent: (X: PowerSet(A)) -> Equal(closure(closure(X)), closure(X))
```

PowerSet(A) 是一个特殊的 OrderedSet，
closure 的定义也可以推广到一般的 OrderedSet。

```cicada
claim order: OrderedSet

closure: order.Element -> order.Element
extensive: (X: order.Element) -> order.Below(X, closure(X))
increasing: (X, Y: order.Element, order.Below(X, Y)) -> order.Below(closure(X), closure(Y))
idempotent: (X: order.Element) -> Equal(closure(closure(X)), closure(X))
```

闭包系统是说一个集合的某个子集
-- 称为闭集（closed set）的集合对于交封闭，
而闭包算子是一个集合的幂到自身的映射。

闭包系统与闭包算子之间的联系在于如下定理：

- 如果已有一个闭包算子，它的像空间就是闭包系统的闭集的集合；

- 如果已有一个闭包系统，对于任意一个子集，
  取所有包含它的闭集，然后求交（按闭包系统的定义封闭），
  就得到一个新的子集，这样就可以定义闭包算子。

TODO 证明这里的断言。

我们已经知道闭包系统对于形式概念分析非常重要，
因为在概念格中定义取上和取下运算的时候，
分别要用到内涵的交与外延的交。

乍一看，这好像是不对称的，为什么用到的是交而不是并？
也许闭包算子可以帮助理解这一点。

这方面最重要的定理是：

- 任意一个闭包系统都是一个完全格，
  其中，序关系是集合的包含关系
  （看参数的位置，应该说是「包含于」关系），
  取下就定义为集合的交，
  取上就定义为集合的并的闭包。

- 任意一个完全格，它的上闭包（或下闭包）的集合，
  可以看成是一个闭包系统。

  - 序集的上闭包（up closure）与下闭包（down closure），
    就是在 The Order Book 中提到的 up-set 和 down-set，
    它可以把对任意序关系的研究，
    转化为对某个集合的特殊子集之间的包含关系的研究。
    特殊的序关系被转化为了特殊的子集加一般的序关系（集合的包含关系）。

TODO 这里有一个问题，
一个闭包系统给出一个完全格，
但是一个完全格却可以分别用上闭包与下闭包给出两个闭包系统！

- 注意，上闭包与下闭包是可以对任意序集定义的，
  但是书中的定理的对象却不是序集，而是完全格，
  也许这个问题的答案就在这里。

很多重要的数学结构的子结构的集合，都是闭包系统。
也就是说，对于这样的每个具体的数学结构，
都可以获得一个子结构的完全格：

- 向量空间的子空间
- 群的子群和正规子群
- 拓扑空间中的闭集
- 欧氏空间中子集的凸包
- 多面体的面的集合 TODO ！？
- 等价关系 TODO ！？

注意，对于这些子空间构成的完全格来说，取下就是交，
但是，取上的计算带有闭包算子，与具体数学结构有关，
是值得仔细考察的东西。

TODO 看看群论中取上如何计算。

TODO 关于 distributive

## 0.4 Galois Connections

TODO

## 0.5 Hints and References

介绍与格论相关的教科书与文献。

# 1. Concept Lattices of Contexts
# 2. Determination and Representation
# 3. Parts and Factors
# 4. Decompositions of Concept Lattices
# 5. Constructions of Concept Lattices
# 6. Properties of Concept Lattices
# 7. Context Comparison and Conceptual Measurability
