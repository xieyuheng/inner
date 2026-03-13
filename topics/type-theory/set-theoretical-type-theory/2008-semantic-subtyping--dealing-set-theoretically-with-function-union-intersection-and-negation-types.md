---
title: semantic subtyping: dealing set theoretically with function union intersection and negation types
authors: [alain frisch, giuseppe castagna, véronique benzaken]
year: 2008
---

# My Motive

[2026-03-14] 发现 polymorphic function type 与集合论的直觉相矛盾，
所以来看看应该如何从集合论角度理解 function type。

初步看来，这里所描述的 set-theoretic type theory，
就是我想要在 lattice-lisp 中实现的类型系统，
但是我因为没能处理好带有 union 和 intersection 的递归类型而失败了。

# 1 Introduction

介绍了很多前人的工作。

# 2 Overview of the approach

介绍实现 subtype 关系的两种方式：

- syntactic -- 用推演规则来定义 subtype 关系。
- semantic -- 把 type 映射到一个 set-theoretic model，
  然后利用 model 中的 subset 关系来定义 subtype 关系。

semantic 是自然的处理方式，也是这篇论文的方式。

semantic 也可以被理解为，
有 decidable 的算法可以自动找到，
syntactic 中需要手动构造的 proof。

## 2.1 A five steps recipe

我很难想象如何实现，因为 set-theoretic model 中，
与 type 对应的 set 很多都是无穷集合，
如何实现算法来判断无穷集合之间的 subst 关系？

可能重点在于不是用最自然的集合，
即 value 的集合作为 model，
而是一 value 的集合作为基础，
寻求一个简化的，但是不改变 subtype 关系的 model。

## 2.2 Advantages of semantic subtyping

要知道，value 和 type 之间的属于关系，是一个二元关系，
所以一定能从一个 type 引导出来一个 value 的集合，
从而回到 set-theoretic 的 model 中来。

即便是那些没有 union type 和 intersection type 的类型系统，
比如 hott 也是如此。

## 2.3 A model of types

TODO
