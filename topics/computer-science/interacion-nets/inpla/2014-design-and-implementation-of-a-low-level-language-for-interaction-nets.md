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
linear logic 的 proof nets 得来的。
Lafont 的论文 1995-from-proof-nets-to-interaction-nets，
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
注意，这里的 arity 只是 auxiliary port 的个数，
而不包含 principal port。

**Names**：

对应于描述连接关系的 logic variables，
所描述的只能是 tree 的 auxiliary port 之间的连接关系。

**Terms**：

一个 term 是一个 tree，由于构造 tree 时，
一个 node 的 principal port 只能连接到
parent node 的 auxiliary port，
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

- 后面可以发现如果一个 term 在
  configuration 的 equation set 中出现了，
  就不会在 term sequence 中再次出现了，
  因此 value 就有了 reference 的意义。
  另外 configuration 的 term sequence
  其实代表 free ports，即 net 的 interface。

变量名与类型之间的关系表：

| 变量名       | 类型                   |
|--------------|------------------------|
| α, β, ...  | agent                  |
| s, t, u, ... | term                   |
| x, y, z, ... | name (logic variables) |
| ∆, Θ, ...   | set of equations       |
| C, C', ...   | configuration          |

一个 configuration 就是一个 net。

我称 inet-lisp 的语法为 **natural applicative encoding**，
而这里所介绍的语法为 **principal applicative encoding**。
相应的 inet-forth 的语法为 **natural concatenative encoding**。

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
设计一个 lisp-like 语法，以方便后面的讨论。

configuration 是用来编码 net 的，
所以我们就用 `(net ...)` 来表示。
因此 configuration 的语法可以设计成：

```scheme
(net <term-sequence> . <equation-body>)
```

**Example 2.1.3**

```scheme
(net [(S Z)])
(net [r] (= (Add Z r) (S Z)))
```

**Example 2.1.4**

```scheme
(net [r] (= (Add Z r) (S w)) (= (Add Z w) Z))
```

下面沿用了 Lafont 所设计的 interaction rule 的语法。
这里可以发现与我的 natural encoding 相比，
这里的 principal encoding 在描述 interaction rule 时要难读很多。

我们不用 `⋈`（Bow Tie）而是直接写 `(rule)`：

**Example 2.1.5**

```scheme
(rule (Add y (S w)) (S (Add y w)))
(rule (Add y y) Z)
```

下面用 substitution 来描述
configuration 在 interaction 之后的变化。
类似 lambda calculus 的 beta-reduction rule。

这确实给出来了一个以 configuration 为 reduction 对象的 rewrite system。
我的在 inet-forth 和 inet-lisp 中的设计没法做到这一点。

- 因为只有完全 explicit 的语法才能做到这一点，
  而 active pair 在我的语法中是 implicit 的。

**Example 2.1.9**

```scheme
(net [r] (= (Add Z r) (S Z)))
(net [r] (= Z y') (= r (S w')) (= Z (Add y' w')))
(net [r] (= r (S w')) (= Z (Add Z w')))
(net [(S w')] (= Z (Add Z w')))
(net [(S w')] (= Z y'') (= w' y''))
(net [(S w')] (= w' Z))
(net [(S Z)])
```

> ... in the λ-calculus, there are two types of normal form: full
> normal form and weak normal form. In our framework, we can define
> interface normal form (INF) as a weak normal form paying attention
> to interfaces:

> Intuitively, a configuration is in INF when it is not expected to
> obtain new results that could be observed from the interface even if
> some reductions were applied.

> **Definition 2.1.12 (Weak reduction)**

看起来就是，只对与 interface 相连通的部分做 reduction，
我还需要更多的例子才能明白。

> By using weak reduction, we can evaluate only active pairs that are
> connected to the interface. In other words, we avoid evaluation of
> nets which are disconnected from the interface. This reduction
> strategy will be particularly useful when we have infinite lists,
> encodings of recursive functions, etc.

## 2.2 Examples

### 2.2.1 Arithmetic operations on unary natural numbers

这里介绍了另外一种定义 Add 和 S 之间反应规则的方式，
就是不要把 S 连接到返回值上，而是连接到被加数上。
说是在 Section 7.2.2 会解释这两种定义的差异。

原来的定义：

```scheme
(rule (Add y (S w)) (S (Add y w)))
(rule (Add y y) Z)
```

alternative 的定义：

```scheme
(rule (Add (S y) w) (S (Add y w)))
(rule (Add y y) Z)
```

用 Lafont 的 principal applicative encoding 来写 rule，
是非常不直观的，引入更多逻辑变量和等式可以缓解这种不直观。

原来的定义，引入更多的逻辑变量和等式：

```scheme
(rule (Add x1 x2) (S y1)
  (= y1 (Add x1 w))
  (= x2 (S w)))
```

将 rule body 中的等式带入 rule 的 head，才得到：

```scheme
(rule (Add x1 (S w)) (S (Add x1 w)))
(rule (Add y (S w)) (S (Add y w)))
```

alternative 的定义，引入更多的逻辑变量和等式：

```scheme
(rule (Add x1 x2) (S y1)
  (= y1 (Add y x2))
  (= x1 (S y)))
```

将 rule body 中的等式带入 rule 的 head，才得到：

```scheme
(rule (Add (S y) x2) (S (Add y x2)))
(rule (Add (S y) w) (S (Add y w)))
```

因此 `(rule)` 的一般语法为：

```scheme
(rule <left-term> <right-term> . <equation-body>)
```

principal applicative encoding 是如此不直观，
以至于论文的作者在第 20 页把 Add 和 S 之间的规则写错了，

应该是：

```scheme
(rule (Add x1 x2) (S y1)
  (= y1 (Add y x2))
  (= x1 (S y)))
```

作者错写成了：

```scheme
(rule (Add x1 x2) (S y1)
  (= y1 (Add (S x1) x2)))
```

就是 S 节点的方向错了。

在理解规则方面如此不直观，
我认为对于程序语言的语法设计而言，
principal applicative encoding 是完全不切实际的。

TODO 从 Fibonacci number 开始跳过例子，
因为我的目的是学习并行计算的实现方式。

# 3 Related works: evaluators towards efficient computation

## 3.1 Overview

介绍已有的 interaction nets 实现：

- 1991 -- Simon Gay
  - "Interaction nets"
  - with type system like in Lafont's first paper.

- 2000 -- AMINE -- Jorge Sousa Pinto
  - "Sequential and Concurrent Abstract Machines for Interaction Nets"
  - 用 substitution + search 来实现，
    就像用 substitution 来实现 lambda calculus 一样不切实际。
  - MPINE 是 AMINE 的并行版本。

- 2002 -- in^2 -- Sylvain Lippi
  - "in^2: A graphical interpreter for interaction nets"

- 2008 -- PIN -- Abubakar Hassan, Ian Mackie, and Shinya Sato
  - "Interaction nets: programming language design and implementation"
  - 避免用 substitution，编译到一个 VM 的 bytecode。

- 2009 -- INET -- Abubakar Hassan, Ian Mackie, and Shinya Sato
  - "Compilation of interaction nets"
  - 避免用 substitution，编译到 C。

- 2010 -- amineLight -- Abubakar Hassan, Ian Mackie, and Shinya Sato
  - "A lightweight abstract machine for interaction nets"
  - 据说改良了前面的语法和 calculus，称为 lightweight calculus。

- 2014 -- ingpu -- Eugen Jiresch
  - "Towards a gpu-based implementation of interaction nets"
  - ... re-wirings are realised by substitutions
    that require synchronised rewritings.
    - 可能这里说的 re-wirings 就是我遇到的问题。

| graph rewriting system | textual calculi                  |
|------------------------|----------------------------------|
| in^2, PIN, INET        | AMINE (MPINE), amineLight, ingpu |

## 3.2 Evaluators based on the graph rewriting system

TODO

# 4 Single link encoding method
# 5 Low-level language LL0
# 6 A language for programming in interaction nets
# 7 Results and future work
# 8 Conclusion
