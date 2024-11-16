---
title: a calculus for interaction nets
authors: [Maribel Fernández, Ian Mackie]
year: 1999
---

# 学习动机

[2024-11-16]
HVM2 2023 的论文中说语法是从这篇论文来的。

# 1 Introduction

这里的语法设计是以 Lafont 原始论文中的语法设计为基础的。

我觉得这里的主要错误在于想要直接设计语法来表示 graph。
这不是不可以，只要用 reference 就行，
比如 lisp 的 `#1=(a . #1#)` 语法。
但是与我用 meta-language 作为 inet 语法思路相比，
这样绝对是不方便的。
