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
;; F-l (LOVES HERMIA LYSANDER) (PREMISE)

(assert (loves Helena Demitrius) (premise))
;; F-2 (LOVES HELENA DEMITRIUS) (PREMISE)
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
;; F-3 (ASSUMED (LOVES DEHITRIUS HERMIA)) (PREMISE)
;; F-4 (NOT (LOVES DEMTRIUS HERMIA)) ()
;; F-5 (LOVES DEMTRIUS HERMIA) (ASSUMPTION F-3 F-4)
```

> Assumptions are the fundamental use of non-monotonic Justifications
> in the dependency system.  Thus the assumption of F-5 above is
> accomplished by asserting the reason for the assumption, F-3, and
> establishing belief in F-5 based on this reason and on, as will be
> explained further in the next chapter, the lack of belief in F-4.

```scheme
(assume (loves Lyeander Hermia) (premise))
;; F-6 (ASSUMED (LOVES LYSANDER HERMIA)) (PREMISE)
;; F-7 (NOT (LOVES LYSANDER HERMIA)) ()
;; F-8 (LOVES LYSANDER HERMIA) (ASSUMPTION F-6 F-7)
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

```scheme
(rule (:n (not (loves Lysander Hernia)))
  (assert (loves Lysander Helena) (love-in-idleness :n)))
```

quality-not-quantity 与 love-in-idleness
算是对这两个男人的讽刺了，哈哈哈。

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
