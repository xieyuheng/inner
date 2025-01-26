---
title: a lazy logician s guide to linear logic
author: jennifer davoren
year: 1991
---

# 0 introduction

这里提到了 linear logic 和 relevance logic 相关。
值得学习一下。

linear logic 首先可以被想象成是 Hilbert 风格的公理系统，
有很多公理，只有 modus ponens 这一个推理规则。
与古典逻辑的差异在于，每个命题只能在 premise 中使用一次。

# 2 Phase space semantics

phase space 定义为 commutative monoid 外加一个选定的子集。
commutative monoid 其实就是 multiset rewriting system。
所选定的子集可以定义一个 dual 和 closure system，因此形成 lattice。
因此也可以而与 formal concept analysis 联系起来。

TODO 可否设计一个 typed multiset rewriting language，
以 linear logic 为其 type system？
