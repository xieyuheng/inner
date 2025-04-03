---
title: concrete pointer graph
date: 2025-04-03
---

# 问题

在实现复杂的拓扑数据结构时
-- 比如 interaction nets 和 cell complex，
人们通常用高层的概念来画图
-- 比如 node、edge、face 等等。

但是这些高层的概念不能包含所有信息，
对程序具体行为的理解不充分会导致 bug。

# 方案

用具体的指针来画图就能观察到所有的信息，
我称之为 concrete pointer graph。

因为 pointer 代表着最底层的最具体的依赖关系，
因此所画的图也是 concrete dependancy graph。
