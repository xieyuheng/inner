---
title: From Proof-Nets to Interaction Nets
author: Yves Lafont
year: 1995
---

# 学习动机

[2025-01-01] 这篇论文在 1997-interaction-combinators 之前。
需要看一下从 1990-interaction-nets 开始 inet 的发展。

[2025-01-05] 看了很多 linear logic 的文献，我还是不理解它的语义。
既然 interaction nets 和 proof nets 是 linear logic 的 term language，
也许反过来可以理解 linear logic。

# 1 introduction

> If we consider the interpretation of proofs as programs, say in
> intuitionistic logic, the question of equality between proofs
> becomes crucial: The syntax introduces meaningless distinctions
> whereas the (denotational) semantics makes excessive
> identifications. This question does not have a simple answer in
> general, but it leads to the notion of proof-net, which is one of
> the main novelties of linear logic. This has been already explained
> in [Gir87] and [GLT89].

> The notion of interaction net introduced in [Laf90] comes from an
> attempt to implement the reduction of these proof-nets. It happens
> to be a simple model of parallel computation, and so it can be
> presented independently of linear logic. However, we think that it
> is also useful to relate the exact origin of interaction nets,
> especially for readers with some knowledge in linear logic. We take
> this opportunity to give a survey of the theory of proof-nets,
> including a new proof of the sequentialization theorem.

"The notion of interaction net introduced in [Laf90] comes from an
attempt to implement the reduction of these proof-nets."

正如 untyped lambda calculus 可以从
lambda calculus + intuitionistic logic 中分离出来，
作为计算模型来研究。

# 2 Multiplicatives

对于 natural deduction 而言，
用来描述证明的 term language 就是 lambda term。

对于这里所描述的 sequent calculus 而言，
如何设计 term language 就成了难题，
因为需要忽略 sequent 左右两个 multisets 中元素的顺序。

这就是引入 graph 来描述 term 的动机。
用一个 meta language 来构造 graph，
就是我实现 inet 时主要的 idea。

linear logic 的四种 inference rules：

```
--------- link
|- A, ~A

|- A, Γ
|- ~A, Δ
--------- cut
|- Γ, Δ

|- A, Γ
|- B, Δ
------------------ conj-mul
|- (A ⨂ B), Γ, Δ

|- A, B, Γ
-------------- disj-mul
|- (A ⅋ B), Γ
```

TODO 给出 logic 等式，并且用 inference rule 证明这些等式。

# 3 Proof-nets

构造 well-formed nets 的四种方式，
对应于 linear logic 的四种 inference rules。

> We say that a net is well-formed if it is of one of the following forms:
>
> - a single wire,
> - two well-formed nets connected by a single wire,
> - two well-formed nets connected by a times cell through its auxiliary ports,
> - a well-formed net connected to itself by a par cell through its auxiliary ports.

注意，检查 well-formed 就是在连接 (times) 和 (par) 时，
检查 auxiliary port 所连接的 net 的拓扑性质 -- 连通性。
这会排除 circle。

每个 node 都有 typing 规则，限制接口的类型。
因此可以定义 typed net。

```scheme
(define-node times
  [left :A]
  [right :B]
  [value! (times-t :A :B)])

(define-node par
  [left :A]
  [right :B]
  [value! (par-t :A :B)])
```

proof-net 定义为 well-formed typed net。

注意只有类型相反的 wire 才能 connect。
可以认为有如下的定义（但是其实 wire 不是 node）：

```scheme
(define-node wire
  [start :A]
  [end (~ :A)])
```

在 linear logic 中：

```
proving =
weaving proof-net =
weaving well-formed typed net =
weaving net using (times) and (par) nodes
```

# 4 Reducing

cut-elimination 等价于 proof-net 的 rewrite rule：

```scheme
(define-rule
    [(times x1 x2 value!)
     (par y1 y2 value!)]
  (connect x1 y1)
  (connect x2 y2))
```

上面的 rule 也可以简写成：

```scheme
(define-rule (times x1 x2 (par y1 y2!))
  (connect x1 y1)
  (connect x2 y2))
```

> Lemma 2: An irreducible net can be classified to the following three kinds:

> - contains a clash:
>   (times) connected to (times) via principle ports,
>   or (par) connected to (par) via principle ports.

> - contains a vicious circle: principle path forms a circle.

> - in reduced form: trees + wiring (a partition of the remaining ports into pairs).

> It is clear that a typed net contains no clash,
> and it is easy to see that a well-formed net contains no vicious circle.
> Therefore, an irreducible proof-net is necessarily in reduced form.

> In fact, it also happens that a typed net contains no vicious circle
> of length n > 0, but it may contain closed wires.

我认为其实根本每必要排斥所谓 "clash" 与 "vicious circle"，
如果它们也是 interaction nets 合理的计算结果，
那么它们也应该被加入到逻辑中去。

# 5 Criterion

这一章想要找一个算法来判断一个 net 是否是 well-formed。
找出了一个基于 acyclic graph 的指数爆炸的算法，每什么用。

# 6 Parsing

这一章再次想要找一个算法来判断一个 net 是否是 well-formed。

> Typability of nets can be checked with a very simple unification
> algorithm. To check if a net is well-formed, there is of course a
> naive exponential algorithm which tries all possibilities, but Danos
> and Regnier noticed that this can be done in quadratic time (see
> also [Gal91]).

先把 net 简化为一个叫 parsing box 的东西，
这个过程类似于代数拓扑中的找不变量用的 functor。

# 7 Units

这里说加上 Units 之后，上面的判断 well-formed net 的算法又不好用了。

> In [LW94], it is shown indeed that the multiplicative fragment of
> linear logic with units but without proper atoms is NP-complete.

> [LW94] P. Lincoln & T. Winkler. Constant-Only Multiplicative Linear
> Logic is NP-Complete. To appear in Theoretical Computer
> Science. 1994.

说一个逻辑是 NP-complete 到底是什么意思？

> One can argue indeed that, in the criterion, acyclicity is more
> important than connectedness. For example, it is enough for
> preventing vicious circles.

> This suggests a weak version of the criterion, corresponding to a
> variant of linear logic with two extra deduction rules (mix and
> empty):

```
|- Γ
|- Δ
-------- mix
|- Γ, Λ
```

```
--- empty
|-
```

> This logic satisfies cut-elimination, but it looks a bit
> degenerate. For instance, `A ⨂ B` implies `A ⅋ B` and `one` is
> equivalent to `bottom` (see [FR90] for a more elaborate system).

> On the other side, the proof-boxes of [Gir87] introduce a kind of
> synchronization which is not justified from a purely computational
> viewpoint.  Maybe, a better understanding of linear logic is needed
> to settle this question.

Lafont 也看不懂 linear logic。

# 8 Exponentials

TODO

# 9 Interaction nets

TODO

# 10 Conclusion

> The notion of exponential proof-net is not completely convincing
> because of its hybrid character. The exponential box is indeed a
> direct translation of the promotion rule of sequent calculus, which
> is essentially nonlocal. There are two ways to cope with this
> problem. The first one is the geometry of interaction, which
> consists in replacing nets by more abstract objects (e.g. operators
> in an Hilbert space) in such a way that the cut-elimination can be
> localized in all cases. The second one has been sketched in the
> previous section. It consists in replacing the deduction rules of
> linear logic by interaction rules, in such a way that the concrete
> character of the computation is preserved, even if its logical
> status becomes less obvious. In fact, it is not necessary to
> consider arbitrary systems: In [1997-interaction-combinators], we
> prove that any interaction system can be translated into a very
> simple one with only three symbols that we call interaction
> combinators.  This means that any computation can be decomposed into
> elementary steps using a very small number of interaction rules.
