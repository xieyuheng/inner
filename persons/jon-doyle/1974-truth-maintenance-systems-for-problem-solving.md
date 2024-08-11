---
title: Truth Maintenance Systems for Problem Solving
author: Jon Doyle
year: 1974
---

# 学习动机

这篇论文是 Jon Doyle 的学位论文，同时也是 TMS 的奠基论文。
Sussman 是作者的老师，也是这篇论文的导师。

- 动机 A：

  为了理解 propagator 中所用的 TMS。

- 动机 B：

  在 propagator 中，直接用 TMS 作为数据结构的名字，
  我不想用缩写，希望了解理论的背景之后能够想出更好的名字。

- 动机 C：

  经过初步的阅读发现，
  这里描述的语言是非常有趣的，
  值得单独（独立于 propagator）实现出来体验一下。

  - 也许这个项目可以叫做 maintainer。

# Abstract

> This report describes progress that has been made in the ability of
> a computer system to understand and reason about its own reasoning
> faculties. A new method for representing knowledge about beliefs has
> been developed. This representation, called a non-monotonic
> dependency system, extends several similar previous representation
> forms for such knowledge, and has been employed in developing new
> strategies for representing assumptions, describing hierarchical
> structures, backtracking, and controlling problem solving systems.

如果有 "new strategies for describing hierarchical structures"
那是不是还和形式概念分析（formal concept analysis）有关？

> This representation is employed by a set of computer programs called
> the Truth Maintenance System (TMS), which makes these abilities
> available as a domain-independent tool for problem solving.
> The TMS accepts as input justifications for belief in components of
> program knowledge, and uses these justifications to determine which
> beliefs endure the discovery of new information and the changing of
> hypotheses.

> The major points of the report are that appropriately recorded
> justifications for beliefs can be used to efficiently maintain the
> current set of program beliefs, and can further be used in a variety
> of tasks, including those of hypothetical reasoning, separating
> levels of detail in explanations, and dependency-directed
> backtracking.

# I. Introduction

## A. Overview of the Report

介绍论文的结构，其实目录已经够好了，这里大致重复了目录的信息。

## B. A Functional Description of Truth Maintenance

> A truth maintenance system is a combination of:
>
> - A representation for recording dependencies between program
>   beliefs;
>
> - And procedures for effecting any updating of beliefs necessary
>   upon the addition of new dependencies.

TODO 在看了 example 之后整理这里的描述。

## C. An Example

需要先看《仲夏夜之梦》，这是我没想到的。

```scheme
(assert (loves Hermia Lysander) (premise))
F-l (LOVES HERMIA LYSANDER) (PREMISE)

(assert (loves Helena Demitrius) (premise))
F-2 (LOVES HELENA DEMITRIUS) (PREMISE)
```

可以理解为，每次调用 assert，
会创造一个 id，并且插入一条数据到数据库。

> The information and rules of our example will be framed in a simple
> rule-based language called SCHPDS, developed by G. J. Sussman and
> J. de Kleer. Assertions, as above, are of the form
>
>     (ASSERT <assertion-pattern> <justification>)
>
> and should be read as
>
>     "belief in <assertion-pattern> is justified by <justification>"
>
> The justifications refer to functions which will accept the
> information transmitted in the justifications and implement the
> necessary TMS justifications between facts.

```scheme
(assume (loves Demitrius Hermia) (premise))
F-3 (ASSUMED (LOVES DEHITRIUS HERMIA)) (PREMISE)
F-4 (NOT (LOVES DEMTRIUS HERMIA)) ()
F-5 (LOVES DEMTRIUS HERMIA) (ASSUMPTION F-3 F-4)
```

> Assumptions are the fundamental use of non-monotonic justifications
> in the dependency system.  Thus the assumption of F-5 above is
> accomplished by asserting the reason for the assumption, F-3, and
> establishing belief in F-5 based on this reason and on, as will be
> explained further in the next chapter, the lack of belief in F-4.

可以理解为 assume 与 assert 的区别是，
assume 还会将代表否定的数据写入数据库吗？

```scheme
(assume (loves Lyeander Hermia) (premise))
F-6 (ASSUMED (LOVES LYSANDER HERMIA)) (PREMISE)
F-7 (NOT (LOVES LYSANDER HERMIA)) ()
F-8 (LOVES LYSANDER HERMIA) (ASSUMPTION F-6 F-7)
```

```scheme
(rule (:n (not (loves Demitrius Hermia)))
  (assert (loves Demitrius Helena) (quality-not-quantity :n)))
```

> This rule provides for Demitrius' love if he falls from love with
> Hermia by providing the alternative of Helena. The format of the
> rule is a pattern, which specifies both a variable (marked by the
> colon prefix) to be bound to the fact name of the matching
> assertion, and the pattern which assertions are to be matched by for
> the body to be executed. If a matching assertion is present, the
> rule will bind the variables of the pattern to the appropriate
> values and evaluate each expression of the body. In the rule above,
> if it becomes known that Demitrius does not love Hermia, the rule
> will justify the belief that Demitrius loves Helena.

可以想象，定义这些 rule 的时候，就扫描数据库中已有的 assertions，
然后按 rule 的 body 中的描述，增加新的 assertions 到数据库中
（类似 logic programming 中 forward-chaining 的情形）。
注意，由于 rule 中的 assertion 带有不同 justification 信息，
就算是写入数据库之后，还是能区分出来不同类型的 assertions。

```scheme
(rule (:n (not (loves Lysander Hernia)))
  (assert (loves Lysander Helena) (love-in-idleness :n)))
```

quality-not-quantity 与 love-in-idleness
算是对这两个男人的讽刺了，哈哈哈。

> Next, some of the more unfortunate aspects
> of the world are specified.

```scheme
(assert (jealous Lysander) (premise))
F-9 (JEALOUS LYSANDER) (PREMISE)
```

```scheme
(rule (:j (jealous :x))
  (rule (:l1 (loves :x :y))
    (rule (:l2 (loves :z :y))
      (if (not (equal :x :z))
        (assert (kills :x :z) (jealousy :j :l1 :l2))))))
```

> This rule embodies the knowledge that jealous people tend to react
> unpleasantly against others loving the object of their jealousy.
> The conditional of the rule body ensures that jealousy is not
> self-applicable.

```scheme
(rule (:l1 (loves :x :y))
  (rule (:l2 (loves :y :z))
    (if (not (equal :x :z))
      (assert (kills :x :z) (unrequited-love :ll :l2)))))
```

> This rule expresses the depression and consequent action
> resulting from unrequited love.

> The final rule provides the means by which the happy nature of this
> comedy is ensured.  This is accomplished by watching for killings,
> and a statement of contradiction implying that the set of
> assumptions about the loves of the characters which lead to such a
> tragedy must be changed.

```scheme
(rule (:k (Kills :x :y))
  (assert (tragedy :k) (contradiction :k)))
```

> With these assertions and rules we begin the analysis of the
> conflicts between the desires of the four lovers. For this example,
> we will choose an order for applying the rules to matching
> assertions which provides for maximal entertainment.

> The first derived assertion notes the conflict
> caused by Lysander's jealousy.

```scheme
F-10 (KILLS LYSANDER DEMTRIUS) (JEALOUSY F-9 F-8 F-5)
```

这么看来，这里的 justification 是对推理的记录，
对推理的记录就是证明。

在类型论的语境下：

- 满足某个类型的表达式就是证明；
- 所证明的命题就是所满足的类型。

回顾之前对 assertion 的定义：

> Assertions, as above, are of the form
>
>     (ASSERT <assertion-pattern> <justification>)
>
> and should be read as
>
>     "belief in <assertion-pattern> is justified by <justification>"
>
> The justifications refer to functions which will accept the
> information transmitted in the justifications and implement the
> necessary TMS justifications between facts.

在类型论的语境下：

- <assertion-pattern> 就是类型（命题）；
- <justification> 就是属于这个类型的表达式（这个命题的证明）。

与一般的类型论不同的是，这里有数据库的概念，
并且调用 ASSERT 时，会生成形如 F-<n> 的 id，
并且把相应的数据写入数据库。
但是在类型论的语境下，这些 id 可以视为是变量名，
因为在构造后面的 justification 表达式时，会用到它们。

- 在实现 dependent type 的时候，
  确实需要经常生成没有重复过的变量名。

- 在类型检查中用到的 context，
  就是变量名到类型（还有其他辅助信息）的对应。
  也许这种 context 就应该用类似数据库的模式来实现。

这里可以看出，所谓「类型论基本原则」是一个实用的想法，
即 "We should study **terms** and **types** together"。
它可以让我们发现 TMS 虽然看似形式不同，但是实际是满足类型论原则的。

和类型论对应起来之后，这篇论文中所声称的 TMS 的属性看起来就很神奇了。

- 一般的类型论是 monotonic 的，而 TMS 是 non-monotonic。

- 一般的类型论不能以直接的方式处理否定命题，
  而 TMS 可以直接处理否定命题。

但是要注意，与一般的类型论中手写证明不同，
这里的 rule 是自动去数据库里找符合类型的表达式，
然后生成新的表达式。

- 可能有点像是所谓「证明搜索」；
- 也可能只是为了找到矛盾，在一般的类型系统实现过程中也会用到类似技术。

回到论文中来。

> This, however, is noticed to be a tragedy,
> and so ruled out as a happy state of affairs.

```scheme
F-11 (TRAGEDY F-10) (CONTRADICTION F-10)
```

> The reaction of the system to contradictions is the invocation of
> dependency-directed backtracking. This process begins by examining
> the reasons for the contradiction in order to locate the
> inconsistent set of assumptions underlying the contradiction.  In
> this case, the contradiction F-11 depends upon F-10, which in turn
> depends upon F-9, F-8, and F-5. F-8 and F-5 are recognized as
> assumptions by the system, since the reasons for their beliefs
> include the lack of belief in the assertions F-7 and F-4
> respectively. Beliefs supported by a lack of knowledge in other
> assertions are suspect, since an inconsistency can be interpreted as
> indicating that some unbelieved assertion must be believed. Thus the
> backtracking system will use the support for the contradiction to
> justify belief in one of these unbelieved facts.

> Note at this point one of the efficiencies of dependency-directed
> backtracking relative to the traditional chronological backtracking
> schemes. In the above, the set of inconsistent assumptions
> underlying the contradiction is a subset of all extant assumptions.
> (I neglected to mention the assumed loves of Theseus, Hippolyta,
> Oberon, Titania, Bottom, Pyramus and Thisby, which may have been
> determined after the current choices for Lysander and Demitrius.)
> Thus where chronological systems for choosing alternatives might
> search through sets of choices involving these independent
> assumptions, the dependency-directed system will only consider those
> assumption actually affecting the discovered contradiction.

这里可以看出，chronological backtracking 确实有很大问题，
Kent Dybvig 说过所谓优化很多时候就是不要犯傻，
这里 chronological backtracking 就是犯傻了。

有趣的是，所提出的解决方案
-- dependency-directed backtracking
中所需要的信息，在 dependent type system 中也有！

> The next step in the backtracking procedure is the creation of a
> **nogood**, an assertion summarizing the support for the
> contradiction which is independent of the inconsistent set of
> assumptions.

```scheme
F-12 (NOGOOD F-11) (CP F-11 (F-8 F-5))
```

NOGOOD 记录的对象必须是 ASSUMPTION。

> This statement of independent support is made by means of a
> conditional proof justification, stating that F-12 should be
> believed if when F-8 and F-5 are believed, so is F-11.

注意，这里不是 `(-> [F-8 F-5] F-11)`，
因为这里要求 CP 所构造出来的是 value 级别的 expression，而不是 type。

更新已有的数据：

```scheme
F-7 (NOT (LOVES LYSANDER HERMIA)) (NOGOOD F-12 F-5)
TRUTH MAINTENANCE PROCESSING DUE TO F-7
F-4 (NOT (LOVES DEMTRIUS HERMIA)) (NOGOOD F-12 F-8)
```

这么看来 ASSUME 记录了一个搜索的分支点。

TODO 完成后面的推理，并检验后面的 TRUTH MAINTENANCE PROCESS 的效果。

# II. Truth Maintenance Systems Applied

## A. Historical Introduction

一般的逻辑式语言的 backtracking 机制，
其实假定的是时间上的先后关系就是依赖关系，
但是这显然是错误的，依赖关系来自具体的语义而不是时间。

## B. Representing Knowledge About Beliefs

> The basic components of a truth maintenance system are facts,
> representations for justifications of belief in facts, and processes
> for determining beliefs in facts consistent with these
> justifications.  The details of these representations and processes
> are not critical, and are discussed in Chapter III.  This section
> instead enumerates several basic forms of justifications for belief
> and their important properties and uses.

从类型论的角度看：

- facts -- 类型。

- representations for justifications
  of belief in facts -- 属于某类型的表达式。

- processes for determining beliefs in facts
  consistent with these justifications
  -- 也许对应于类型检查过程中的 constraint processing。

> Facts are not isolated objects, but are connected to each other by
> dependencies, the relationships of antecedence and consequence in
> which belief in a fact is related with belief in other facts by
> means of a justification.

"connected by dependencies" 类似 propagator network。

> Justifications for belief in a fact are predicates of other facts,
> predicates whose internal structure is accessible to the truth
> maintenance system to allow efficient processing and the
> determination of various dependency relationships.

在类型系统中，一个表达式通过变量引用到的，只是表达式而不是类型，
但是 "internal structure is accessible" 可能代表着，
通过 id 引用到的所有信息都可以使用。

- 注意，这里所说的 "justification for belief in a fact"，
  belief 是一个 meta 概念，如果非要在实现中使用这个词的话，
  可能可以理解为 justification + fact = belief。

另外一个重要的差异是 justification 是用 predicate 来实现的，而表达式不是。
表达式经过类型检查器的 check 函数解释之后，才能被理解为 predicate。

> Belief in a fact may or may not be supported by the existence of a
> valid (that is, **evaluating true**) justification for the fact.
> If a fact has at least one valid justification for belief, we say
> the fact is _in_; otherwise the fact is _out_.

> The distinction between in and out is not that of true and false;
> indeed, there is no imposed notion of falsity in a truth maintenance
> system. Instead, a support-status of _in_ for a fact denotes the
> existence of knowledge supporting belief in the truth of the fact,
> and _out_ denotes the lack of such knowledge. The function of the
> truth maintenance system is to mark each fact in or out in
> accordance with the validity of its justifications.

TODO justification 的分类。

> ... assumption justifications can be invalidated
> by the addition of new beliefs.

说 assumption 是 non-monottnic 的，
其实是不是等于说 assumption 节点，
在搜索空间中提供了可能的分支？

ASSUME 的语义是 "to assume a fact is true unless proven otherwise"，
因此，也许可以应用到推理小说或者法庭辩护游戏中！
因为 ASSUME 的语义是这些语境下经常出现的语义。

TODO 关于用 IN 和 OUT 来实现 beliefs 之间的各种不同关系，我没太看懂。

## C. Hypothetical Reasoning

> The making of hypotheses is a straightforward application of the
> justification forms described in the previous section.

作者之后的论文可能对这种，
justification form 方面的创新有更好的表达。

## D. Backtracking

> Systems engaging in hypothetical reasoning require mechanisms for
> reconciling beliefs upon the introduction of new hypotheses.

与数学辅助证明系统和朴素的逻辑式编程相比，
这种可以进入新假设的系统才更接近人们日常的推理。

> Two types of hypotheses can be distinguished; **speculative**
> hypotheses and **counterfactual** hypotheses.  Speculative
> hypotheses are those which are consistent with existing beliefs and
> justifications. Speculative hypotheses are useful when a lack of
> knowledge forces the making of an assumption for the purpose of
> exploration.  Counterfactual hypotheses, on the other hand,
> contradict previous beliefs. Such hypotheses are useful in exploring
> the results of actions and in deriving constraints existing in
> different worlds.

这里说的 counterfactual 是否就是 Judea Pearl 的 counterfactual 呢？
应该是的，因为 Judea Pearl 强调的是，
在用一个模型解释现象的时候，
可以利用模型，分析假设的现象。

在应用 dependency-directed backtracking 方面，
Stallman and Sussman 在 1976 论文中所解决的问题：
"transistor state-choices in electronic circuit analysis"，
作为一个典范问题也应该学习一下。

对 NOGOOD 的记录与使用，模仿了人类推理时所用的归谬法。

## E. Generalization and Levels of Detail

中间节点也有名字，而不是把中间节点实现为子节点的集合。

这样节点之间就能形成偏序关系，
而这个偏序关系代表了 levels of detail，
成了 backtracking 的依据，

其他系统中类似的设计决策：

- 实现编译器的时候，
  保持 function 与 inline 一个 function，
  与这里所描述的设计决策是类似的。

- 在实现 propagator 时，
  我把 `definePropagator` 实现为了去构建连接的函数，
  而不是一个存在于网络中的节点。

  这样做也许是不对的。

- 在实现 inet 时，
  我把形成抽象的机制 -- function statement，
  实现为了构造 network 的函数，
  而不是存在于系统中的节点。

  这样做也许是对的，
  因为系统中的节点都是有待反应的数据。

TODO 这里讲到了用到了 boundary 概念的实现技巧，我没理解。
是否就是简单的加一层 indirect？

TODO 略过这里关于电路的例子，关于电路我还看不懂。

## F. Comparison With Other Current Work

TODO 也许，在知道了 Sussman 想要达到的效果之后，
我应该遵循孔涅的教导，首先尝试自己实现类似的系统。

TODO

# III. Truth Maintenance Mechanisms

## A. Historical Introduction
## B. Facts and Dependencies
## C. Well-Founded Support Relations
## D. Truth Maintenance
## E. Truth Maintenance Efficiency
## F. Dependencies and Contexts
## G. Comparison With Other Current Work

# IV. Discussion

## A. Summary of the Key Ideas
## B. Future Work

# References

# Appendices

## 1. A TMS Glossary
## 2. Monotonic Truth Maintenance Systems
## 3. An Implementation of a TMS
