---
title: "computation: finite and infinite machines"
author: marvin minsky
year: 1967
---

# My Motive

[2025-02-01]

在学习 McCulloch-Pitts neuron 时：
- https://wiki.xxiivv.com/site/mcculloch_pitts.html
- https://git.sr.ht/~rabbits/neur
- https://www.jmeiners.com/neural-nets-sim/

发现 https://github.com/justinmeiners/neural-nets-sim
引用了 minsky 的这本书。

我之前知道神经网络来自 McCulloch-Pitts 的论文，
但是不知道作者设计了一个计算模型。
现在要从计算模型和语言设计的角度学习一下。

这本书不止介绍 McCulloch-Pitts 的计算模型，
还介绍了很多其他计算模型。

# preface

> Man has within a single generation found himself sharing the world
> with a strange new species: the computers and computer-like
> machines.

> It would indeed be reassuring to have a book that categorically and
> systematically described what all these machines can do and what
> they cannot do, giving sound theoretical or practical grounds for
> each judgment.

> The main goal of this book is to introduce the student to the
> concept of _effective procedure_. ... The theory of effectiveness is
> useful not only to prove things about complex systems, but is also
> necessary to prove things about proof itself!

> It would be well worth one's time perusing Davis' (1965) collection
> just to gain additional personal contact with the masters!

Martin Davis 收集了很多论文原著，编成了论文集。
值得进一步阅读。

# 1 physical machines and their abstract counterparts

就像希腊的古典学者一样，
从 machine 的定义开始。
说这个词很难定义，就像 living 很难定义一样。

Minsky 认为没法精确定义就在技术意义上是无用的。
没法精确定义的时候，就要反思一下所想的概念是否有助于理解，
毕竟人们发明概念就是为了理解。

在某些研究中，意识到自己没法给出精确定义也是重要的。

这里的 "machines as physical models of abstract processes"，
是说某个机器一定是某种理论模型的产物，
遇到问题时，可以改进理论模型，也可以改进机器的制造方式。
与物理学理论不同，物理学中只能根据实验结果改进理论模型。

> Some people believe that simple mathematical statements are
> "self-evident." Other people maintain that they are based on rather
> obscure but still empirical observations.  ... Perhaps one could
> even maintain the view that belief in an arithmetic statement is
> equivalent to the belief that certain machines, if properly built,
> will work.

这种经验主义的固定 belief 的方式，对于数学证明的形式化来说尤其重要。
长时间使用形式证明辅助系统的人，可能不可避免会产证这种态度。
前提是有好用的辅助证明系统。

# 2 finite-state machines

> ... To make this simple, we choose for our parts some very simple
> devices, the "neurons" of McCulloch and Pitts, and we eventually
> demonstrate that the theory of all finite automata is equivalent to
> the theory of these particularly simple elements.

也就是说，要用 neurons 来实现 finite-state machines！

如何从一般的带有输入输出的机器，
发明出来有限状态机。

- 带有输入输出的机器，
  完全可以想象成是一个带有副作用的 C 函数。

这里有个有趣的观点是，
一个 machine 的 internal state
压缩了 input history 中的信息。

有限状态机就是把可能无限的 input history
压缩到有限个状态。

TODO

# 3 neural networks. automata made up of parts

TODO

# 4 the memories of eventsin finite-state machines
# 5 computability, effective procedures, and algorithms. infinite machines
# 6 turing machines
# 7 universal turing machines
# 8 limitations of effective computability: some problems not solvable by instruction-obeying machines
# 9 the computable real numbers
# 10 the relations betweenturing machines and recursive functions
# 11 models similarto digital computers
# 12 the symbol-manipulationsystems of post
# 13 post's normal-form theorem
# 14 very simple bases for computability
# 15 solutions to selected problems
# 16 suggestions for further readingand descriptor-indexed bibliography
