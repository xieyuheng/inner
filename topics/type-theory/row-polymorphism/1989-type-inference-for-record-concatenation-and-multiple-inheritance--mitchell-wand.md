---
title: type inference for record concatenation and multiple inheritance
author: mitchell wand
year: 1989
---

# My Motive

[2026-03-14] 要给 x-lisp/meta-lisp 增加 row-polymorphism 功能。
据说这篇论文是最早的关于 row-polymorphism 的论文。

# Abstract

> We show that the type inference problem for a lambda calculus with
> records, including a record concatenation operator, is decidable.

# 1 Introduction

# 2 Records: Basic Definitions

在 lambda calculus 的框架内，
用带有如下类型的有限函数来理解 record：

```scheme
(-> label-t (union V absen-t))
```

但是如果想要避免类型错误，
就要在类型中把有限的 key（label）表示出来。

也就是考虑 object 和 class。

# 3 Objects

# 4 Type Inference for Records

想要把 object 的 class 视为 lambda 的语法糖，
在只考虑 runtime 行为的时候是方便的，
但是考虑类型系统的时候，反而变复杂了，
不如直接考虑 object 与 class。

另外 object 和 class 中 key 的无序性，
是 lambda 没法在语法层面上处理的。
