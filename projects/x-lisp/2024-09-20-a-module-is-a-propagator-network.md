---
title: A Module is A Propagator Network
date: 2024-09-20
---

首先，显然在类型检查的过程中 context 是一个 propagator network，
其中 cell 对应类型检查的对象 -- 表达式。

考虑 generic functions，
就发现 module 显然也应该被实现为一个 propagator network。
一个 module 中，对每个名字的定义都是一个 cell，
如果 cell 保存的是 generic function，
定义多个 handler 就是 patch 这个 cell。

module 的概念会变得很简单：

- 首先一个 module 对应一个 file，会让理解变简单。
- 其次一个 module 中会记录它对于某些名字的理解，等待被 import。
  每个名字可以被理解为一个 propagator network 中的 cell。

这么看来，引用某个 module 中的名字，
可能也应该用 propagator 来实现，
而不是直接引用 cell，
-- 保持二分图的约束条件。
