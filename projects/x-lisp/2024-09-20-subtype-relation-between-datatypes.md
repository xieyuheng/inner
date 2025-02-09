---
title: Subtype relation between datatypes
date: 2024-09-20
---

ADT 之间也可以有子类型关系，只不过和 class 的子类型关系是对偶的。
也就是说，和 `define-class` extend 已有的 class 一样，
`define-datatype` 也可以 extend 已有的 datatype。
注意，还是 nominal 的，而不是 structural 的。

- 需要从 cicada-solo 中迁移一些 `define-datatype` 的例子。

- 但是也许我们不应该实现这种子类型关系，
  因为返回值的范围不能被缩小到子类型。

- 这种子类型关系与 tagless finial 有关。
