---
title: the lambda calculus
subtitle: its syntax and semantics
author: henk barendregt
year: 1984
---

# My Motive

[2025-07-21] 在 lambda calculus 中，如果没有递归定义，
我知道如何用 partial evaluation 来判断两个函数是否等价。
如此定义的函数之间的等词可以作为基础等词。

在 lambda-lisp 这个项目中，
我尝试解决递归函数之间的基础等词问题，
及两个递归定义的函数等价，
当且仅当它们的无穷展开作为无穷的树是相等的。

在初步实现的算法在判断相等时用到了 trail，
为了逐步展开递归函数的作用，
我引入了 `DelayedApply` 这种 value，
这导致我的 evaluation strategy 变得很特殊了。

很久之前我就尝试读过这本书，但是没有坚持下来。
这次重读这本书就是为了理解，
我的算法中的 evaluation strategy 意味着什么。
