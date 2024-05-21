---
title: Two kinds of equalities in type theory
date: 2024-05-22
---

在实现类型系统的时候，
可以发现两种 equality，
一种被称作 definitional equality 或 computational equality，
是有 P 复杂度的算法可以直接判断的 equality；
另一种是 propositional equality，
需要人去找出其相等的证明，然后再被验证，
或者有 NP 复杂度的搜索算法可以判断 equality。

既然二者都是有算法的，
并且在 propositional equality 这种比较坏的情况，
搜索算法就类似于 logic programming
或者 constraint programming 中的搜索，
那么我们可以在设计类型系统时，
同时实现这两类算法，
作为工具为人所用。
