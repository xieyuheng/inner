---
title: Propagator Model and Lattice
date: 2024-09-16
---

# Structural Subtyping 作为 Lattice

Structural Subtyping 作为 Lattice，
可以理解为是一种结构化的构造新的 Lattice 的方式，
已知一些 lattice，然后生成 record lattice。

可以理解为带有名字（record 的 key）的 product lattice，
但是与 product 不同的是，key 出现与否可以给出额外的序关系。

例如：

```
{ a: A, b: B, c: C } <= { a: A, b: B }
{ a: A, b: { x: X, y: Y } } <= { a: A, b: { x: X } }
```

一般的集合有：

```
{a, b} <= {a, b, c}
```

但是当把集合的元素理解为 record 的 key 时，就有（相反的）：

```
{a: Unit, b: Unit, c: Unit} <= {a: Unit, b: Unit}
```

# Propagator Model 中的 Lattice

论文中有 Number、Interval、Belief、BeliefSystem 四种 Lattice。

其中：

- Number 是平凡的。
- Interval 可以看作是一个 primitive lattice。
- Belief 和 BeliefSystem 不是简单的 record lattice，
  但是可以看成是以 record lattice 为基础来定义的，
  就像是以 free group 为基础来定义 presentation of a group。

Belief 与 BeliefSystem 作为 lattice 之间的关系非常复杂，
要看具体的 merge 函数的定义。
