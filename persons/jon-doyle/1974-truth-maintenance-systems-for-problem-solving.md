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
F-ll (TRAGEDY F-10) (CONTRADICTION F-10)
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

TODO

# II. Truth Maintenance Systems Applied

## A. Historical Introduction
## B. Representing Knowledge About Beliefs
## C. Hypothetical Reasoning
## D. Backtracking
## E. Generalization and Levels of Detail
## F. Comparison With Other Current Work

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
