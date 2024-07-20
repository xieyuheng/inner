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

TODO

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
