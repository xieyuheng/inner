---
title: Human Problem Solving
authors: [Allen Newell, Herbert Simon]
year: 1972
---

# 学习动机

动机 A：

- 在读 "Building Problem Solvers" 的时候，
  发现整本书都是以 Simon 的 Problem Space Model 为基础的。
  还发现我之前写 constraint logic programming 时关于实现的笔记，
  其实也是这个 Problem Space Model。

动机 B：

- Simon 的 Problem Space Model 可以用来解释
  Polya 的 "How to Solve it" 中的技巧吗？
  毕竟想要回答 Why 就需要 Model，
  而我们正巧在开发一个 Model。

# 1 INTRODUCTION

> The aim of this book is to advance our understanding of how humans
> think. It seeks to do so by putting forth a theory of human problem
> solving, along with a body of empirical evidence that permits
> assessment of the theory.

首先界定本书所讨论的范围。

> The present study is concerned with the performance of intelligent
> adults in our own culture.  The tasks discussed are short
> (half-hour), moderately difficult problems of a symbolic nature.

说明本书将先研究例子，然后尝试给出理论。

> The three main tasks we use:
> - chess,
> - symbolic logic,
> - and algebra-like puzzles (called cryptarithmetic puzzles)
> typify this class of problems.

> The study is concerned primarily with performance,
> only a little with learning.

performance 和 learning 在人类看来好像是非常息息相关的，
但是假设我们实现程序去模拟 performance（problem solving）和 learning，
就会发现实现方式可能很不一样：

- problem solving 经常被实现为在某个 problem space 中的搜索问题。
- 而 learning 经常被实现为，为某个模式识别问题而设计的函数优化问题。

也许在搜索问题中，也可以增加符号型 learning 的元素，
比如，Sussman 常说的 dependency-directed backtracking，
和很多现代的 SAT Solver 所做的那样。

正如作者下面所提到的：

> ... the possibility that learning is taking place cannot be
> excluded in complex tasks that last tens of minutes.

> Thus, this study is concerned with thinking -- or that subspecies of
> it called problem solving -- but it approaches the subject in a
> definite way. It asserts specifically that thinking can be explained
> by means of an information processing theory. This assertion
> requires some explanation.

也就是说，在 problem solving 的方面，要实现 AI，
即用计算机来拟人类的 problem solving 过程。

> One may try to provide this meaning by saying that a computer is an
> instance of an information processor.  This would suggest that the
> phrase is a metaphor: that man is to be modeled as a digital
> computer.  Metaphors have their own good place in science, though
> there is neither terminology nor metatheory of science to explicate
> the roles of metaphors, analogs, models, theories and descriptions,
> or the passage from one category to another (Simon and Newell,
> 1956).

说没有这样的 metatheory，
但是 category theory 不就是这样的 metatheory 吗？

- metaphors, analogs -- Functor
- models -- Object
- theories and descriptions -- Category
- or the passage from one category to another -- Natural Transformation

作者不知道范畴论吗？

> But an alternative to metaphor is at hand. An abstract concept of an
> information processing system has emerged with the development of
> the digital computer.  In fact, a whole array of different abstract
> concepts has developed, as scientists have sought to capture the
> essence of the new technology in different ways (Minsky, 1967).

作者的意思是用 metaphor 之类的讨论（抽象废话），
而是去写程序实践自己的理论。

> Thus, in this book we will introduce a suitable abstract information
> processing system to describe how man processes task -- oriented
> symbolic information.  This is not the most abstract possible way to
> describe an information processing system, but it is tailored to our
> scientific needs.

具体做法是：

- 研究典型的例子；
- 总结出理论；
- 写程序实现理论。

这个紧紧跟随科学的方法论的流程太棒了。

TODO

# 2 INFORMATION PROCESSING SYSTEMS
# 3 TASK ENVIRONMENTS
# 4 PROBLEM SOLVING
# 5 CRYPTARITHMETIC: Task Analysis
# 6 CRYPTARITHMETIC: Behavior of a Single Subject
# 7 CRYPTARITHMETIC: A Broader View
# 8 LOGIC: Task Analysis
# 9 LOGIC: GPS and Human Behavior
# 10 LOGIC: A Broader View
# 11 CHESS: Task Analysis
# 12 CHESS: Behavior of a Single Subject
# 13 CHESS: A Broader View
# 14 THE THEORY OF HUMAN PROBLEM SOLVING
# EPILOGUE
# HISTORICAL ADDENDUM
# BIBLIOGRAPHY
