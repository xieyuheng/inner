---
title: linear logic
author: frank pfenning
year: 2002
sources:
- "https://www.cs.cmu.edu/~fp/courses/15816-f01"
- "https://www.cs.cmu.edu/~fp/courses/15816-s12"
---

# My Motive

[2025-02-16] 在实现 inet-lisp 的时候，
图中 node 和 wire 之间的连接，
以及 wire 和 wire 之间的连接，
都是 double linked。

double linked 就意味着没法像 single linked 一样 share，
因此所有可能会保存 wire 的变量都必须使用线性变量，
即变量只能被引用一次。

由于所有的变量都可能会保存 wire，
所以所有的变量都必须是线性变量。

这里研究的不是 linear logic，
而是纯粹运行时的 linear variable。

这是否和这本书里的 linear lambda calculus 有关呢？

# 1 Introduction
# 2 Linear Natural Deduction
# 3 Sequent Calculus
# 4 Proof Search
# 5 Linear Logic Programming
# 6 Linear λ-Calculus
# 7 Linear Type Theory
