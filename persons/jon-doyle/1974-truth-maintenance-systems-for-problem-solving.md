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

TODO 需要先看《仲夏夜之梦》，这是我没想到的。

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
