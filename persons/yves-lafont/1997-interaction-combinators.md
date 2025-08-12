---
title: Interaction Combinators
author: Yves Lafont
year: 1997
---

# My Motive

[2024-11-15]
新的 c inet 实现，
消除了 input port 只能和 output port 相连这个限制，
因此可以用来实现 Interaction Combinators 了。
因此需要重新读一下这篇论文。

# [note] Drawing of nets

In the first paper "Interaction Nets",
drawing of a net is based on layered-tree,
input ports below a node, output ports above a node.

In this paper, there is no distinction
between input ports and output ports
(so that a node can connect to itself).

And drawing of node is changed, to highlight the principle port.

Without the input-output distinction,
the layered-tree meaning is losted in the drawing.

This makes the drawing much more hard to understand,
because based on layered-tree, a drawing of net
is similarly to a syntex tree,
and to highlight the principle,
we can just highlight the edge
without changing the layered-tree drawing.

# [note] Reduced the right side of rule

The author require the right side of a rule to be reduced.

This means when defining a rule,
we can (and we should) eager evaluate the right side,
and the evaluation must terminate.

We can let the compiler to do this partial evaluation
as an optimization and a terminate check.
Programmers do not need to care about this.

# Abstract

> It is shown that a very simple system of interaction combinators,
> with only three symbols and six rules, is a universal model of
> distributed computation, in a sense that will be made precise. This
> paper is the continuation of the author's work on interaction nets,
> inspired by Girard's proof nets for linear logic, but no preliminary
> knowledge of these topics is required for its reading.

# Introduction

> This paper addresses the following question: what are the
> fundamental laws of computation? Of course, the answer depends on
> the choice of a particular model of computation. Let us mention some
> of them:

> - _Turing machines_ imitate the mathematician writing symbols on
>   paper. There are many variants, for instance _register machines_
>   and _stack machines_. This notion has the advantage of being
>   simple and powerful at the same time, but it models only
>   sequential computation.

> - _Cellular automata_ can be seen as discrete approximations of
>   physical processes. This notion models distributed computation,
>   but with a global synchronization of transitions.

> - _Rewrite systems_ are closer to the algebraic tradition, since a
>   rewrite rule is just an oriented equation. An interesting example
>   is the _lambda-calculus_, with only one rewrite rule:
>
>       ((lambda (x) u) v) => v [u/x]

> This calculus is _Turing complete_ and it has a nice logical
> interpretation, at least in the typed case. However, the rule is
> more complicated than it seems: is it reasonable to consider
> substitution as an atomic operation? In this sense, a more primitive
> system is combinatory logic, with two rewrite rules:
>
>     Kxy => x,
>     Sxyz => xz(yz).
>
> But again, is it reasonable to consider erasing and duplication as
> atomic operations?

> Following this tradition of rewrite systems, interaction nets were
> introduced in [Laf 90] as a model of distributed computation with
> local synchronization (Section 1). These nets, which are related to
> the connection graphs of [Baw86], appeared as a generalization of
> Girard's proof nets for linear logic (see [Gir95, Laf95]).

> By "local synchronization," we mean that there is no need to
> consider a global time for computation. In other words, time is
> relativistic. By "distributed," we mean that the computation is
> performed at several places at the same time, whereas "parallel"
> sometimes refers to a kind of magical superposition, as in "parallel
> or":
>
>     T or x -> T,
>     x or T -> T,
>     F or F -> F.

> Our interaction nets are deterministic in a strong sense: not only
> the result, but also the computation is unique, up to trivial
> commutations. In particular, it is not possible to encode "parallel
> or." We shall not address the question of deciding whether this
> should be considered as a good or a bad point.

"parallel or" 是什么意思？
可能是说 `x or y` 可以同时计算两边，
然后把结果汇集起来。
在上篇论文中，
natural number 的 `max` 可能是一个类似的例子。

> From the viewpoint of computability, our interaction nets are
> equivalent to the Turing machines, but from the viewpoint of
> computation, there is something more, for instance parallelism (in
> the sense of distributed computation). To express this rigorously,
> we introduce a natural notion of _translation of interaction system_
> preserving the essential properties of computations, such as the
> complexity and the degree of parallelism.

> By definition, a _universal interaction system_ has the property
> that any other interaction system can be translated into it. Turing
> machines can be seen as particular interaction systems, but such
> systems are intrinsically sequential and cannot be universal in the
> above sense, even if they come from universal Turing machines. On
> the other hand, it is proved that a system of _interaction
> combinators_ is universal (Section 2). This suggests an answer to
> our original question, at least within the framework of interaction
> nets: the fundamental laws of computation are _commutation_ and
> _annihilation_.

如何理解这一点？
可能需要理解把任意 interaction net
翻译成 interaction combinators 的方式。

> Our system of interaction combinators has been obtained by a kind of
> distillation, starting from some more complicated system suggested
> by Samson Abramsky for implementing the proof boxes of linear logic
> (see [Mac94]). Independently, Simon Gay has also obtained a
> universal system, with eight symbols instead of three (see [Gay95]).

这个 [Gay95] 也值得看看，有时候形式上数量最少，并不是最容易理解的。

> Our system is simpler because it uses the same symbols for different
> purposes. Among the other related systems, let us mention the
> infinite one introduced by John Lamping for the optimal reduction of
> lambda-calculus (see [Lam90]) and the variants proposed by Georges
> Gonthier, Martin Abadi, and Jean-Jacques Levy, in connection with
> linear logic (see [GAL92a, GAL92b]).

为什么说 [Lam90] 的是 infinite one？
是因为有 infinite 个 combinators？

> Apart from its simplicity, the system of interaction combinators has
> an unexpected interpretation in terms of reversible 2-stack machines
> which seems to throw a bridge between distributed and sequential
> computation (Section 3). This section benefited from discussions
> with Vincent Danos and Laurent Regnier.

# 1 Interaction Nets

> The origin of our favorite model of computation is explained in [Laf 95].
> Here, it is introduced from scratch, without explicit reference to proof theory.

这里 [Laf 95] 引用的是 1995-from-proof-nets-to-interaction-nets，
而不是 1990-interaction-nets。

## 1.1 Nets

> From now on, a symbol will always be given with its arity n >= 0.
> An occurrence of such a symbol is called a _cell_.

换名字了，不管 node 叫 agent 了，叫 cell。

图像中用三角形的 node 来指出 principle port 的位置。

> Such a cell has one _principal port_ and n _auxiliary ports_. It is
> well understood that the latter are not interchangeable. For
> instance, one can number them from 1 to n, keeping 0 for the
> principal port.

> In practice, the ports will always be implicitly numbered in
> clockwise order.

这种规定不是好事，因为：

- 对于 constructor 而言，
  principal port 代表 value，
  此时 principal port 应该放到最后一个位置。

- 对于 eliminatior 而言，
  principal port 代表 target。
  此时 principal port 应该放到第一个位置。

在使用 concatenative 的语法时，
我们应该自然避免用 positional arguments，
而是用 named arguments -- 类似 record type。
这样就可以避免上面的约定。

在使用 applicative 语法时，
positional arguments 是自然的，
此时可以使用 pattern match 来取 positional arguments。

## 1.2 Interaction

这里可以明显看出来 interaction 应该被理解为 re-wiring。

> Note that cells can only interact pairwise,
> through their principal ports.

因此是一种带有特殊限制的 graph rewriting。

是否可以设计一种一个 node 带有多个 principal ports，
或者多个 node 组成的 pattern 作为 rewriting 的对象的计算模型？
在 build net 的过程中，每次 node 都可以检查自己是否和周边的 node
形成可 reduce 的 pattern。
好像这样也是完全可行的，但是会导致 rewriting 与顺序相关。
限制只有两个 node 的 pattern 作为 rewriting 的 target，
就是为了和顺序无关。

在这里描述了 strong confluence property。

> Moreover, if one abstracts from the irrelevant order of application
> of rules, there is only one possible reduction from A to B. So we
> can say interaction nets are a deterministic and asynchronous model
> of computation. In fact, we think that any computation of that kind
> can be modeled by means of interaction nets, but of course, an
> assertion of this kind cannot be proved.

## 1.3 Example: Turing Machines

> Classical models of sequential computation such as Turing machines,
> register machines and stack machines can be seen as special classes
> of interaction systems.

> For instance, a Turing machine is given by a triple `(Q, A, T)`,
> where `Q` is a finite set of states, `A` a finite set of letters,
> and `T` a (partially defined) map from `(Q, A)` to `(A, {+, -],
> Q)`. A configuration of the machine is given by a state `q`, an
> infinite tape filled with letters, and a current position in the
> tape:

```
                         q
                         |
  ... - a[-2] - a[-1] - a[0] - a[1] - a[2] - ...
```

> If `T(q, a) = (a', +, q')`, and being in state `q`, the machine
> reads `a` from the tape, then it writes `a'`, moves right, and goes
> to state `q'`: [picture]

> If `T(q, a) = (a', -, q')`, the machine has the same behavior,
> except that it moves left

> By doubling the number of states, one can always assume that the
> direction of the move depends only on `q'`. One says that `q'` comes
> from the left in the first case, and that it comes from the right in
> the second case.

我不理解这里的 doubling。
可能不应该说 depends on，
而是说 `T(q, a) = (a', +, q')`
可以理解为 `T(q, a) = (a', +q')`，
也许这样也不对。
我还是不理解。

可以尝试用这里提到的翻译方法，
把一些 turing machine 的程序翻译到 inet 试试。

> So it appears that interaction nets are complete from the viewpoint
> of computability. Similarly, the register machines and the stack
> machines can be simulated in a fairly obvious way.

如何实现呢？
尤其是如何实现 stack machines？
也许有众所周知的用 turing machine
实现 stack machine 的方法，
我还不知道。

## 1.4 Example: Cellular Automata

这里也需要一些 Cellular Automata 的例子和练习。

这里只处理的一维的，二维的如何？
会不会中间步骤太多了？

从上面两个例子可以体验到，
用 inet 编程的时候，
解决问题的方式是不同的，
可以直接用 net 来编码问题中的部分条件，
比如图的邻接关系。

- 用 propagator model 是否也会有类似的体验？

## 1.5 Example: Unary Arithmetics

这里类似函数式编程，是最简单的部分。

## 1.6 Reduced Nets
## 1.7 Translations

# 2 Interaction Combinators

## 2.1 The System
## 2.2 Multiplexors and Transpositors
## 2.3 Menus and Selectors
## 2.4 Translation (Restricted Case)
## 2.5 Duplication
## 2.6 Codes, Copiers, and Decoder
## 2.7 Translation (General Case)
## 2.8 Minimality of the System

# 3 Semantics of Combinators

## 3.1 Execution
## 3.2 Equivalence
## 3.3 Algebraic Formulation
## 3.4 Directed Combinators

# 4 Discussion
