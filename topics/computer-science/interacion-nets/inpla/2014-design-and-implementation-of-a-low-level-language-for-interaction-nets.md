---
title: design and implementation of a low level language for interaction nets
author: shinya sato
year: 2014
---

# My Motive

[2025-03-30] 在尝试给 inet-lisp 和 inet-forth 实现并行计算时失败了。
问题在于，虽然在一个网络中不同位置的 interaction 看似是相互独立的，
但是我实现 interaction 的方式是 disconnect + reconnect：

- disconnect -- 拿到一个可以反应的 node pair（redex），
  把相连的 wire 分开，删除这两个 node。

- reconnect -- 再生成新的 node 把上一步中暴露出来的 wire 重新连起来；
  重连过程中每一次形成连接，
  都要检查这个连接是否会引入新的可以反应的 node pair（redex）。

问题就在于这一步「检查」需要 read wire 另一端的 node 的信息。
而这个 read 和另一端可能的 write 会形成 data race。

在一般的介绍 inet 的论文中，
提到并行计算总是说 redex 是局部的切独立的，
但是在消除一个 redex 后，如果想要发现是否会引出更多 redex，
就需要读相邻 node 的信息，在并行计算中就会引起 data race。
这一点在一般的论文中是没有提到的。

读 inpla 相关的论文就是要看别人是如何处理这个 data race 的。

# Summary

> This thesis is about the implementation of interaction
> nets. Specifically, for the first contributions we define a
> low-level language as an object language for the compilation of
> interaction nets. We study the efficiency and properties of
> different data structures, and focus on the management of the
> rewriting process which is usually hidden in the graph rewriting
> system. We provide experimental data comparing the different choices
> of data structures and select one for further development. For the
> compilation of nets and rules into this language, we show an
> optimisation such that allocated memory for agents is reused, and
> thus we obtain optimal efficiency for the rewriting process.

> The second part of this thesis describes extensions of interaction
> nets so that they can be used as a programming language.

# Acknowledgements

本论文的作者 Shinya Sato 是 Ian Mackie 的学生。
另外这个小组的人还有 Abubaker Hassan 和 Maribel Fernández。

# 1 Introduction

## 1.1 Linear logic

介绍 interaction nets 是如何从
linear logic 的 proof nets 得来的，
但是感觉这里的介绍并不充分。

- Lafont 的论文 1995-from-proof-nets-to-interaction-nets，
  是对这段历史的更好的介绍。

## 1.2 Interaction nets

TODO

# 2 Background
# 3 Related works: evaluators towards efficient computation
# 4 Single link encoding method
# 5 Low-level language LL0
# 6 A language for programming in interaction nets
# 7 Results and future work
# 8 Conclusion
