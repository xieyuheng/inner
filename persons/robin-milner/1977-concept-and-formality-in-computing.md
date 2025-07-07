---
title: concept and formality in computing
author: robin milner
youtube: "https://www.youtube.com/watch?v=_hnMkvQpoJY"
bilibili: "https://www.bilibili.com/video/BV1LJ3QzDEDs"
year: 1977
---

这是我能找到的 robin milner 的唯一录像。

介绍 LCF 的由来，来自「定理自动搜索」与「定理证明检查」之间的折中。

介绍 process calculus 如何从 automata theory 发展而来。

认为 denotational semantics 比 operational semantics 更复杂，
前者适合有数学背景的人在设计语言的时候思考，
后者适合写给实现语言的人做参考。

对 denotational semantics 的批评：

- Stoy 在同一场会议的演讲中提到 Strachey 的研究小组的研究特点就是，
  使用奥卡姆的剃刀：「如无必要不增实体」。
  既然没有 denotation 就能用 operation 把语义解释清楚，
  那么根据奥卡姆的剃刀，是否应该抛弃那些数学结构呢？

  比如为了给一个语法上的 judgement `(check ctx exp type)` 做 denotation，
  Damas 在 1985-type-assignment-in-programming-languages 中，
  引入 domain 和 domain 的 ideal，把 exp 解释为 domain 中的元素，
  把 type 解释为 ideal，然后经过 ideal 上的高阶函数，
  间接地定义了 model theory 意义上的 `check`。
  相比之下，其实可以直接在语法层次，
  用 inference rule 定义 formal system 意义上的 `check`，
  这样会简单很多。

- 考虑 Curry 从形式主义对 denotational semantics 的批评。
  首先人们研究 formal system 就是为了给数学以坚实基础，
  但是 denotational semantics 又想在没有基础的数学中，
  给 formal system 找解释，这就与 formal system 的初衷相背了。

介绍 CSP 和 CCS 之间并行的发展，对比差异，促进理解。
CSP 和 CCS 没法被等同的事实，
可能预示着二者都属于一个更大的计算模型的集合。
这可能就是 Milner 研究 bigraph 的动机。
