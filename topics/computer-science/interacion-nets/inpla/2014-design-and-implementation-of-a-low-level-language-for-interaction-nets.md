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

> ... an interaction net computational step can be defined as a
> constant time operation, and the model allows for parallelism (many
> steps can take place at the same time).

在一般的介绍 inet 的论文中，
提到并行计算总是说 redex 是局部的切独立的，
但是在消除一个 redex 后，如果想要发现是否会引出更多 redex，
就需要读相邻 node 的信息，在并行计算中就会引起 data race。
这一点在一般的论文中是没有提到的。

这里在提到 inet 如何适合并行计算时也是如此。

- 但是，上面这种情况并不是无药可救，
  因为只是「读相邻 node 的信息」，
  也许可以有技巧来处理这一点。

> The model therefore is an interesting one if we are interested in
> cost models of computation, and also take advantage of possible
> parallelism.

这里所谓的 cost models of computation
就是 HVM 团队想用基于 inet 的语言，
来为区块链写智能合约的原因。

### 1.2.1 Interaction nets as an implementation language

> There have been a number of different encodings
> of the λ-calculus. We mention just a sample:

> - Gonthier, Abadi and Lévy [24] gave an optimal implementation
>   using an infinite set of (indexed) agents. In practice, this
>   system turns out to be very inefficient in time, but several
>   works (Asperti et al. [8] for example) have made significant
>   performance improvements.

> - In [41] an interaction system is given which uses a finite number
>   of agents, but it does not implement substitution through
>   λ-abstractions, which is essential to obtain sharing. Although
>   very little sharing is captured by this system it does better than
>   call-by-need. This system lead to a sequence of papers [43, 46]
>   delivering more efficient evaluators. These systems are the most
>   efficient to date.

> - Lippi [40]: has given an alternative approach based on encoding
>   the λ-calculus indirectly by implementing an environment machine.

### 1.2.2 Interaction nets as a programming language

我认为，把 interaction nets 直接当作一个程序语言，
和传统程序语言的的主要差异在于，
不是设计好数据结构再实现函数来操作数据，
而是把计算直接编码在数据的局部反应之中。

换了一个计算模型，
程序员应该如何编程也成了新的挑战。

## 1.3 Implementing and extending interaction nets

这里列举了很多 interaction nets 的实现，
需要的时候可以参考。

还有对 interaction nets 的扩展，比如：

- multiple principal ports:

  Vladimir Alexiev.
  Non-deterministic interaction nets.
  PhD thesis, University of Alberta, 1999.

- type system:

  Maribel Fernández.
  Type assignment and termination of interaction nets.
  Mathematical Structures in Computer Science, 8(6):593–636, 1998.

- equivalence:

  Maribel Fernández and Ian Mackie.
  Operational equivalence for interaction nets.
  Theoretical Computer Science, 297(1–3):157–181, February 2003.

- GPU:

  Eugen Jiresch.
  Towards a gpu-based implementation of interaction nets.
  In Benedikt Löwe and Glynn Winskel, editors, DCM, volume 143 of EPTCS,
  pages 41–53, 2014.

# 2 Background

## 2.1 Interaction nets

这一章将要介绍 Fernández 和 Mackie 设计的语法，
也就是 1999-a-calculus-for-interaction-nets。

### 2.1.1 Graph rewriting system

> Locality is a property of rewriting such that there is at most one
> interaction rule for each active pair and the interface is preserved
> during the rewriting. By the locality property, all rewritings are
> performed locally. In interaction nets, since strong confluence
> holds and all rewrites are local, rewriting can be performed in any
> order. Therefore interaction nets are inherently parallel.

这里有趣的现象是，乍一看确实可以并行，
但是实际实现时会发现有 overlooked data race。

### 2.1.2 A textual calculus for interaction nets

**Agents**：

类似于 inet-lisp 中的 node，
但是总是把 principal port 当作返回值（最后一个 port）。
注意这里的 arity 只是 auxiliary port 的个数，
而不包含 principal port。

**Names**：

对应于描述连接关系的 logic variables，
所描述的只能是 tree 的 auxiliary port 之间的连接关系。

**Terms**：

一个 term 是一个 tree，由于构造 tree 时，
一个 node 的 principal port 只能连接到 parent node 的 auxiliary port，
所以 tree 不包含 active pair。

这里的语法也是通过重载函数作用语法来构造网，
但是与 inet-lisp 相比有了更多的限制，
会导致 constructor 类的 node 很自然，
但是 eliminator 类的 node 不自然，
即 eliminator 的 target 参数，
会当成是函数作用语法中的返回值。

**Equations**：

An occurrence of an equation corresponds to
a connection between two ports。
即 active pair，或者说是 redex。

**Configurations**：

a sequence of terms, and a multiset of equations。
为什么是 multiset 而不是 set？
因为可能有很多 zero arity 的 nodes 相连。
只要说一个 equation 中所保存的是 term 的 reference
而不是 term 的 value 就可以了，
这样就是 set 而没必要说是 multiset。

变量名与类型之间的关系表：

| 变量名       | 类型                   |
|--------------|------------------------|
| α, β, ...  | agent                  |
| s, t, u, ... | term                   |
| x, y, z, ... | name (logic variables) |
| ∆, Θ, ...   | set of equations       |
| C, C', ...   | configuration          |


一个 configuration 就是一个 net。

我称作 inet-lisp 的语法为 natural applicative encoding，
而这里所介绍的语法为 principal applicative encoding。
相应的 inet-forth 的语法为 natural concatenative encoding。

principal applicative encoding 的好处是，
principal port 之间的连接 -- 也就是 active pair 或 redex，
总是被 equation 明显地表示出来了，
想要增加 principal port 之间的连接，用户需要明显地增加 equation。

而在 natural applicative encoding 中，
一个 net 的 active pair 是隐式的，
net 是拥有一个保存 active pair 的 container，
但是用户不直接操作这个 container，
用户只是用 applicative 语法来构造 net，
发现 active pair 之后会被自动保存在这个 container 中。

我们也可以为 principal applicative encoding
设计一个 lisp-like 语法，
以方便后面的讨论。

TODO

# 3 Related works: evaluators towards efficient computation
# 4 Single link encoding method
# 5 Low-level language LL0
# 6 A language for programming in interaction nets
# 7 Results and future work
# 8 Conclusion
