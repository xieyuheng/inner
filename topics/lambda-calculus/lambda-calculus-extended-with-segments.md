---
title: Lambda calculus extended with segments
author: H Balsters
year: 1986
---

# [note] 1. Introduction

论文算是对 AUTOMATH 所使用的语法的技术总结，
开篇有一些与 AUTOMATH 相关的引用文献，
在研究 AUTOMATH 的历史时可以参考。

先把 Lambda 演算的语法中的函数作用的函数与参数交换顺序，
介绍一下这个情况下的语法树的样子，
然后再更自然地引入后缀表达式的 de Bruijn 记法。

如果用带有局部变元的 Joy 来理解 segments 的语义，
书中所提到的下面这种 reduction 是有问题的：

```
[(x) (y) (z)] (s) s x
(x) (y) (z) x
```

在 quoted program 中的局部变元 `(x)`，被直接插入到外层了，
并且被外层的变元 `x` 引用，这样没有了 lexical scope。

在 de Bruijn 的原始关于 segments 的论文
“A namefree lambda calculus with facilities
for internal definition of expressions and segments”
中，以及在这本书中，作者都认为这是需要 de Bruijn index
而不能用 named variable 的原因。

我认为，其实应该以 lexical scope 为指导。
