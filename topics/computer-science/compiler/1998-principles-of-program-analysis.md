---
title: principles of program analysis
authors: [Flemming Nielson, Hanne Riis Nielson, Chris Hankin]
year: 1998
---

# My Motive

[2026-06-22] 学习编译器优化过程中所解决的问题是什么。

# Preface

> This book concentrates on what we believe to be the four main
> approaches to program analysis:
>
> - Data Flow Analysis;
> - Constraint Based Analysis;
> - Abstract Interpretation;
> - and Type and Effect Systems.
>
> For each approach we aim at identifying and describing the important
> general principles, rather than presenting a cook-book of techniques
> and language constructs, with a view to how the principles scale up
> for more complex programming languages and analyses.

> As a consequence we have deliberately decided that this book should
> not treat a number of interesting approaches, some of which are dear
> to the heart of the authors, in order to be able to cover the four
> main approaches to some depth; the approaches not covered include
>
> - denotationally based program analysis,
> - projection analysis,
> - and logical formulations based on Stone dualities.
>
> For reasons of space, we have also had to omit material that would
> have had a natural place in the book; this includes
>
> - a deeper treatment of set constraints,
> - fast techniques for implementing type based analyses,
> - single static assignment form,
> - a broader treatment of pointer analyses,
> - and the interplay between analysis and transformation and how to
>   efficiently recompute analysis information invalidated by the
>   transformation.

# 1 Introduction

## 1.1 The Nature of Program Analysis

> Program analysis offers static compile-time techniques for
> predicting safe and computable approximations to the set of values
> or behaviours arising dynamically at run-time when executing a
> program on a computer.

由于停机问题，所以没法给出精确的预测，
所以要强调 computable approximations。

## 1.2 Setting the Scene

TODO 用 lisp 描述这里的例子语言。

# 2 Data Flow Analysis
# 3 Constraint Based Analysis
# 4 Abstract Interpretation
# 5 Type and Effect Systems
# 6 Algorithms
