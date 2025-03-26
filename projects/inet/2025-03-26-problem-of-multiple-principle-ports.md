---
title: problem of multiple principle ports
date: 2025-03-26
---

问题不在于计算的不确定性，
而是在于 multiple principle ports
必定导致 subgraph pattern matching，
而后者很难并行：

- thread 1 -- 开始 match 一个子图 A
- thread 2 -- 开始 match 的子图 B 与 A 相交
- thread 2 -- match 到 B 与 A 相交部分的点 a（此时没有被标记为 match）
- thread 1 -- match 成功，并且标记 A 中的 node 为 matched
- thread 1 -- 把 match 的结果作为 task 返回
- thread 3 -- 接收到上面的 task，处理 task 的过程中删除 A
- thread 2 -- 继续处理点 a，但是其实 a 已经被删除了

一个补救的方案是使用 embarrassingly parallel 的方案：

- 分批给所有 worker thread 分配 task
- 每次所有 task 处理完之后再分配下一批 task

这样在处理某一批 task 的时候，
在同一批中被 matched 点不会被删除，
在处理下一批的时候，可能被删除的点都已经被标记为了 matched。
