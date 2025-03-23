---
title: Sussman's language design ideas
date: 2024-05-19
---

Ideas 来自 Sussman 的新演讲 ["Artificial Intelligence: a Problem of Plumbing?"](https://www.youtube.com/watch?v=CGxbRJPCoAQ)

传统的机器学习是在全局地优化一个有很多参数的函数。
这样看来不够仿生，如何把传统的机器学习局部化？
可能在于使用类似 propagator 的 idea。

# 逻辑式编程的自我优化

逻辑式语言在搜索的时候，
可以根据 input 和当前的 solution，
来给出每个搜索分支一个优先级的估值。

可以用机器学习，
根据大量数据于试验学习到这个估值，
以实现逻辑式编程的自我优化。

solver 应该能意识到自己的状态，
并且当人们问 solver 它的状态的某些方面的时候，
它应该能够表达出来。

# macro

macro 可以让用户自定义语法，
与 lisp 相比，其他的没有良好 macro system 的语言，
相当于是将语法的定义权控制在了少数人手里，
比如语言的什么委员会，或者独裁者。

lisp 可以将这个权力分散给所有用户，
从而让语言能够更好更快地演化。

因此实现一个带有 macro 的 lisp 对人类来说也是很有必要的。

我们需要一个带有模块系统和包管理器的 scheme。

# implicit vs. explicit

语言设计的意义在与可以把 explicit 的事情变为 implicit 的，
这是一般的编程做不到的。

当我们把一个语言嵌入在 JS 或 scheme 中时，
我们就是在用 explicit 来实现某些新的计算 model 或 idea。
只有真的设计新语言才能把这些 idea 变成 implicit。

比如：

- 递归函数把 stack 变为 implicit。

- 垃圾回收器把内存管理从语言层面变为 implicit。

类似的语言设计 ideas：

- reactive programming

  在 Web 前端领域有很多使用 reactive programming 的例子，
  但是都是不是语言级别的，而是要用 explicit 的方式使用的，
  设计一个 reactive programming 的语言也是有意义的。

- propagator

  实现直接处理 propagator 的语言也是有意义的。
