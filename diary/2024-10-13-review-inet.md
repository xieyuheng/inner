---
title: review inet
date: 2024-10-13
---

回顾 inet 的文档。

更新 inet 的实现：

- 修复目前 require 的模块系统。

- 让 inet 可以和计划中的 pnet 一样，
  可以通过引用 JS 模块来扩展。

[2024-10-19] 开一个新项目：inet-cat

还是用 ts 来写，
但是这次从头开始重写，
并且尝试简化实现，
保留 HalfEdge 的概念以方便未来迁移到 C。
