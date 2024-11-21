---
title: Interaction Nets
author: Yves Lafont
year: 1990
---

# 学习动机

[2024-11-06]
实现了 inet 之后，我以为自己早已经熟悉这篇论文了，
但是其实对 free port 的理解有大问题。
导致我实现 inet 的思路一直不对，
非常值得反思。
可能需要重读这篇论文。

[2024-11-16]
这次重读尤其是要看明白作者是如何设计 inet 的语法的。

# Abstract

> We propose a new kind of programming language,
> with the following features:
>
> - a simple graph rewriting semantics,
> - a complete symmetry between constructors and destructors,
> - a type discipline for deterministic
>   and deadlock-free (microscopic) parallelism.

> _Interaciion nets_ generalise Girard’s _proof nets_ of linear logic
> and illustrate the advantage of an _integrated logic_ approach, as
> opposed to the _external_ one.  In other words, we did not try to
> design a logic describing the behaviour of some given computational
> system, but a programming language for which the type discipline is
> already (almost) a logic.

> In fact, we shall scarcely refer to logic, because we adopt a naive
> and pragmatic style. A typical application we have in mind for this
> language is the design of interactive softwares such as editors or
> window managers.

# 1  Principles of Interaction

> Throughout this text, net means _undirected graph with labelled
> vertices_, also called _agents_.  For each label. also called
> _symbol_, a finite set of _ports_ has been fixed:

```
* (null) -- value!
* (cons) tail head -- value!
* (append) target! rest -- result
```

> We shall consider rewrite rules:

```
! (null)-(append)
  (append)-rest result-(append)

! (cons)-(append)
  (cons)-tail (append)-rest append
  (cons)-head cons result-(append)
```

> Here, rewriting is just a convenient language to express a very
> concrete notion of interaction, which we shall make precise by
> requiring some properties of rules. The first one is in fact imposed
> by our option of nets (as opposed to trees or directed graphs):

> 1. (linearity)
>    Inside a rule, each variable occurs exactly twice, once in the
>    left member and once in the right one.

> Consequently, explicit duplication and erasing symbols are required
> for algorithms such as unary multiplication (figure 1).

> To express our second constraint, we must first distinguish a
> _principal port_ for each symbol:

```
* (null) -- value!
* (cons) tail head -- value!
* (append) target! rest -- result
```

> 2. (binary interaction)
>    Agents interact through their principal port only.

append 相关的 rule 满足条件，
但是想用朴素的方式实现 max，
就发现需要一些技巧了。

> A pair of agents which are connected by their principal port is
> called _alive_, because some rule -- maybe several, maybe none -- is
> supposed to reduce it.  Clearly, a third constraint is necessary to
> ensure deterministic computation:

> 3. (no ambiguity)
>    There is al most one rule for each pair of distinct
>    symbols S, T, and no rule for S, S.

"no rule for S, S" 这个 constraint
会在下一篇论文 1997-interaction-combinators 中解除。

> The three conditions are enough to get the following (easy)
> property:

> Proposition 1 (strong confluence):
> If N reduces in one step to P and Q, with P != Q,
> then P and Q reduce in one step to a common R.

```
     N
   /   \
  P     Q
   \   /
     R
```

> Indeed, by conditions 2 and 3, rules apply to disjoint pairs of
> agents, and cannot interfere with each other.  Usual complications
> are avoided by condition 1. In fact, interactions are purely local
> and can be performed concurrently: proposition 1 expresses that the
> relative order of concurrent reductions is completely irrelevant.

> So far, nothing ensures that all alive pairs of agents are
> reducible, but it is a reasonable requirement, and indeed, it will
> be the case of typed nets.

> Consequently, if the right member of a rule contains some alive
> pair, we should be able to reduce it, and the following condition is
> natural:

> 4. (optimisation)
>    Right members of rules contain no alive pair.

在编写 rule 的时候其实不用满足这个条件，
optimisation 让机器来做就好了。

介绍两个编程的例子：

- concatenation of diflerence-lists
- polish parsing

polish parsing 处理的是纯前缀表达式，
是否也可以处理纯后缀表达式呢？

# 2 A Type Discipline

TODO
