---
title: general graph rewriting
date: 2024-11-23
---

在这个演讲中：[[IFIP WG 1.6 - 2021] Graph Rewriting](https://www.youtube.com/watch?v=_gwz64o1eBQ&ab_channel=FSCD2021)

作者介绍了以往的 double pushout graph rewriting 有问题，
问题在于想要强行使用范畴论，并且实现自动重新连接，
而解决方式就在于人工标注如何重连。
所谓 Patch Graph Rewriting (2020)。

- 作者在 2021 的新论文中又想强行使用范畴论，感觉有点迂腐了。

在实现 interaction nets 时，我就是这样做的。
并且使用了一个底层的 meta-language，
通过 overload function application syntax 来描述如何重连。

# 一般的 hypergraph rewriting

可以用这种思路尝试实现一般的 hypergraph rewriting。

interaction nets in terms of general-graph-rewriting，
应该是一个有趣的练习。

注意，在 interaction nets 中，
node with ports 大大方便了重连的描述。

没有 port 的 general graph rewriting，
需要一些技巧来完成重连。

[question] 带有变量的 multi-set rewriting
是否其实是 hypergraph rewriting？
