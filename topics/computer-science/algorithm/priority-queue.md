---
title: priority queue
---

[2025-09-30] 看 "Essentials of Compilation" 课程，
老师说实现 graph-coloring 的 dsatur 算法，
需要 priority-queue。
所以来学习一下。

[Priority Queue Playlist / William Fiset](https://www.youtube.com/playlist?list=PLDV1Zeh2NRsCLFSHm1nYb9daYf60lCcag)

没想到 Priority Queue 这么简单。

学到了：

- priority queue -- 抽象数据类型
- heap -- 具体数据类型
- complete-binary-tree -- 用 array 表示 tree

但是其实实现 graph-coloring 的 dsatur 算法时，
不一定要用 priority-queue。
因为如果用了，每次更新 color 的时候，
都要重新 balance 中的很多 node。
