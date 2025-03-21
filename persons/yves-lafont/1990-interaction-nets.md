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

> We are going to strengthen the conditions of section 1 so that for
> each alive pair of agents, some rule applies.  Introducing rules for
> all pairs of symbols is not conceivable: how the devil would Cons
> interact with Nil, or Parse with Append? Moreover this would be
> inconsistent with condition 3. So we are led to limit valid
> configurations by means of typing.

其实，找不到 rule 的 alive pairs 放着不管就可以了，
没必要因此引入类型系统。

> We introduce constant types
> -- `atom`, `list`, `nat`, `d-list`, `stream`, `tree`, ...
> For each symbol, ports must be typed as input or output.

在 1997-interaction-combinators 中将会消除 input 与 output 之分。

> A net is well typed if inputs are connected to outputs of the same
> type.  A rule is well typed if:
>
> - symbols in the left member match, which means
>   that their principal ports have opposite types,
> - the right member is well typed
>   (the types of variables being given by the left member).

其实不应该着急设计类型系统，
而是应该先尝试写程序，
来体验用这种模型编程的感觉。

> So we have new conditions for typed interaction:

> 5. (typing)
>    Rules are well typed.

> 6. (completeness)
>    There is a rule for each pair of matching symbols.

无类型时，inet 的 eliminatior
可以很好地支持 generic function，
但是加上类型反而不行了。

> All examples in section 1 are easily typed. The choice of an
> input/output denomination is purely conventional: it does not matter
> if you call input what I call output, and conversely, but we must
> agree on matching.  In other words, the notions of constructor
> (symbol with a positively typed principal port, like Cons) and
> destructor (symbol with a negatively typed principal port, like
> Append) are symmetrical in our system.

上面这段观察，已经说明区分 input 和 output 有问题了。
比如两个人可能写出来 library，
刚好只是因为 input 和 output 相反而不兼容。

> So far, typing ensures local correciness of computations, but we
> shall see that a notion of global correctness is necessary to
> prevent _deadlock_.

> Proposition 2 (stopping cases)
> If a net is irreducible,
> starting from any point,
> you can follow principal ports
> until you reach a variable, or you loop!

> Proposition 3 (deadlock)
> A circle like in proposition 2 stays forever.

论文里称其为 pathological vicious circle。
我觉得没必要用这种词，
因为这些 circle 可能就是我们想要让运算得到的结果。

也许应该叫做 principal circle，
因为是顺着 principal port 形成的 circle。

在图上看，好像形成 principal circle 就会失去引用了，需要被垃圾回收了。
但是其实图上所表达的引用，只是形成 graph 时所用的 pointer，
还可能有别的 pointer 引用 principal circle 中的 node 和 wire。

判断是否会出现 principal circle 是否和停机问题等价？

论文这一段通过给 auxiliary port 分组，
并且避免某些组之间相互连接，
来证明在某组 node 和 rule 的定义下，
写出的程序不会出现 principal circle。

> Proposition 6 (invariance)
> Simple nets are closed under reduction by simple rules.

我不明白分组的依据是什么，
可能要人工找到一个分组，
才能用来证明。
如果不能自动寻找分组，
那就没什么意义。

可能这个定理的意义在于，
如果一个 rule 没有故意引入 principal circle，
并且初始条件没有 principal circle，
那么整体的程序运行起来也不会产生 principal circle。
这样，针对小规模的数据做测试，
就可以保证没有 principal circle 了。

# 3 A Programming Language

这是我主要想批判的地方。

我觉得正确的方向不是直接给图设计具体语法，
而是用两层语言，一层构造图，一层以图为语法。

> For each symbol, the type of its principal port is given first:

```
symbol Cons: list+; atom-, list-
       Nil: list
       Append: list-; list-, list+
```

> Non-discrete partitions are specified by means of curly brackets.

```
symbol Dupl: nat-; {nat+, nat+}
       Erase: nat-; {}
```

> Notation for rules is a bit disconcerting, but very natural.
> Consider the following example: [picture]

> We join variables between left and right members: [picture]

> Putting principal ports up and auxiliary ones down,
> we obtain two trees with links between leafs: [picture]

> So interaction is written as follows:

```
Cons [x, Append(v, t)] >< Append [v, Cons(x, t)]
```

> Note that the left and right sides of `><` have nothing to do with
> the left and right members of the initial rule.  It requires a bit
> of training to become acquainted with this syntax.

disconcerting 和 natural 是相互矛盾的。

在我的设计中：

```
* (null) -- value!
* (cons) tail head -- value!
* (append) target! rest -- result

! (null)-(append)
  (append)-rest result-(append)

! (cons)-(append)
  (cons)-tail (append)-rest append
  (cons)-head cons result-(append)
```

一个 rule 的 body 就是构造 rule 的 right hand graph 的过程。
因此更直观。

> Here we described only the kernel of our language, but it should be
> interfaced with external devices such as keyboards or displays: for
> example, the output of a keyboard can be seen as an infinite stream
> of agents with characters as symbols.

> Furthermore some extensions such as polymorphic typing and
> modularity are certainly needed to get a high level programming
> language.

我就不抄 code listing 了。
但是也许为了看其他人基于这里的语法设计而写的论文，
还是必须熟悉这里的语法。

# Conclusion

> Our proposal can be compared with existing programming paradigms. As
> in functional programming, we have a strong type discipline and a
> deterministic semantics based on a Church-Rosser property, but the
> functional paradigm (like intuitionistic logic) assumes an essential
> asymmetry between inputs and outputs, which is incompatible with
> parallelism and inconvenient for writing interactive softwares.

> Our rules are clearly reminiscent of clauses in logic programming,
> especially in the use of variables (see the example of
> difference-lists), and our proposal could be related to PARLOG or
> GHC. There are also some similarities with data-flow languages and
> the CCS-CSP family, but as far as we know, the concepts of principal
> port (which is critical for determinism) and semi-simplicity (which
> prevents deadlock) has never been considered in such systems.

我觉得上面说的只有 logic programming 是值得对比的。
在 applicative 的语法下，一个 node 在作用时，
可以以 return value 为参数，
就像逻辑式语言中的关系一样。

> The first idea of generalising multiplicative connectors of linear
> logic appears in [Girard88] (partitions are considered in [Regnos])
> and led to the Geometry of inieruction [Girard89, Girard89a].

上面这些引用也许是值得一读的。

> We are now working on a true implementation of the language to
> develop real examples in a practical programming environment.

不知道作者的实现后来如何了。

# Appendix: Linear Logic

也就是为 inet 的运行时设计类型系统。
可以只考虑简单类型系统。

这一节的引用可能也值得一读。

问题是想要和 linear logic 联系起来，
就需要给 port 区分 input 和 output 以获得 atom 的 sign。

> Indeed, our proposal generalises the so-called multiplicative
> fragment of linear logic, for which the notion proof nel works very
> well, but with very iimited dynamics (everything reduces in linear
> time). On the contrary, our type system does not ensure termination,
> although it would be interesting to isolate terminating subsystems.
