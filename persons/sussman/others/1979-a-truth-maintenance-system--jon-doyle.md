---
title: A Truth Maintenance System
author: Jon Doyle
year: 1979
---

# 学习动机

这篇论文是 TMS 的奠基论文。

读这篇论文是因为 propagator 中用到了 TMS。

propagator 中，直接用 TMS 作为数据结构的名字，
我不想用缩写，所以读这篇论文也是为了获得更好的命名。

# ABSTRACT

> To choose their actions, reasoning programs must be able to make
> assumptions and subsequently revise their beliefs when discoveries
> contradict these assumptions. The Truth Maintenance System (TMS) is
> a problem solver subsystem for performing these functions by
> recording and maintaining the reasons for program beliefs.
> Such recorded reasons are useful in constructing explanations of
> program actions and in guiding the course of action of a problem
> solver.

# 1. Introduction

## 1.1. The essence of the theory

> Many treatments of formal and informal reasoning in mathematical
> logic and artificial intelligence have been shaped in large part by
> a seldom acknowledged view: the view that the process of reasoning
> is the process of deriving new knowledge from old, the process of
> discovering new truths contained in known truths.  This view, as it
> is simply understood, has several severe difficulties as a theory of
> reasoning. In this section, I propose another, quite different view
> about the nature of reasening. I incorporate some new concepts into
> this view, and the combination overcomes the problems exhibited by
> the conventional view.

> Briefly put, the problems with the conventional view of reasoning
> stem from the _monotonicity_ of the sequence of states of the
> reasoner's beliefs: his beliefs are true, and truths never change,
> so the only action of reasoning is to augment the current set of
> beliefs with more beliefs. This monotonicity leads to three closely
> related problems involving commonsense reasoning, the frame problem,
> and control. To some extent, my criticisms here of the conventional
> view of reasoning will be amplifications of Minsky's [36] criticisms
> of the logistic approach to problem solving.

考虑用 dependent type 语言来形式化数学定理，
这个过程确实是 monotonicity 的，
所证明的定理会慢慢增多而不会减少。

> Since we base our actions on what we currently believe, we must
> continually update our current set of beliefs. The problem of
> describing and performing this updating efficiently is sometimes
> called the _frame problem_.

> The problem of control is the problem of deciding what to do
> next. Rather man make this choice blindly, many have suggested that
> we might apply the reasoner to this task as well, to make inferences
> about which inferences to make. This approach to the problem of
> control has not been explored much, in part because such control
> inferences are useless in monotonic systems. In these systems,
> adding more inference rules or axioms just increases the number of
> inferences possible, rather than preventing some inferences from
> being made. One gets the unwanted inferences together with new
> conclusions confirming their undesirability.

比如，增加 facts 不会减少 logic programming 的搜索空间。

TODO

## 1.2. Basic terminology

# 2. Representation of Reasons for Beliefs

## 2.1. States of belief
## 2.2. Justifications
## 2.3. Support-list justifications
## 2.4. Terminology of dependency relationships
## 2.5. Conditional-proof justifications
## 2.6. Other types of justifications

# 3. Truth Maintenance Mechanisms

## 3.1. Circular arguments
## 3.2. The truth maintenance process
## 3.3. Analyzing conditional-proofs

# 4. Dependency-Directed Backtracking

# 5. Summarizing Arguments

# 6. Dialectical Arguments

# 7. Models of Others' Beliefs

# 8. Assumptions and the Problem of Control

## 8.1. Default assumptions
## 8.2. Sequences of alternatives
## 8.3. Equivalence class representatives

# 9. Experience and Extensions

# 10. Discussion
