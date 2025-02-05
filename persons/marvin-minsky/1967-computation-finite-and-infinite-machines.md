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

- t for discrete time
- H for history
- input channel S for "stimulus"
- output channel R for "response"

```
R(t + 1) = F(H(t), S(t))
```

带有输入输出的机器，
完全可以想象成是一个带有副作用的 C 函数。
此时 t 是调用次数，
S(t) 是第 t 次调用时的输入，
R(t) 是第 t 次调用时的输出。

这里有个有趣的观点是，
一个 machine 的 internal state
压缩了 input history 中的信息。

有限状态机就是把可能无限的 input history H(t)
压缩到有限个状态 Q(t)。

```
R(t + 1) = F(Q(t), S(t))
```

潜在无限的历史 event 的列表，
到有限集的映射（会形成等价类），
可能都可以这样实现。

> This brings us to the key postulate of the theory of finite
> automata. We assume that the machine can distinguish, by its present
> and future behavior, between only some finite number of classes of
> possible histories. These classes will be called the "internal
> states" of the machine.

在把 input history events H(t) 转化为有限内部状态 Q(t) 之后，
分析 Q(t) 的依赖关系可以得到状态转化函数 G：

```
Q(t + 1) = G(Q(t), S(t))
```

只把有限状态机当作一个 black box 来讨论，
有点像是范畴论中，用方程来讨论 object 和 arrow 的属性，
而不能看 object 和 arrow 的内部构造。

一般对有限状态机的定义其实是用内部构造来定义的。
即 state transition diagrams。

```scheme
(define-finite-automata <name>
  (<state> (<event> <new-state> <response>) ...)
  ...)

(define-finite-automata memory
  (q0 (s0 q0 r0) (s1 q1 r0))
  (q1 (s0 q0 r1) (s1 q1 r1)))

(define-finite-automata parity
  (even (0 even 0) (1 odd 1))
  (odd (0 odd 0) (1 even 1)))

;; TODO 2-memory
;; TODO 3-memory

(define-finite-automata binary-serial-adder
  ;; n for no curry
  (n (00 n 0) (01 n 1) (10 n 1) (11 c 0))
  ;; c for curry
  (c (00 n 1) (01 c 0) (10 c 0) (11 c 1)))
```

> What happens if a finite-state machine is left to itself -- that is,
> if it receives no information from the outside world? That would be
> the case if the machine receives only a constant input -- that is, a
> repetitive, unchanging signal.

想象出来这种机器，
就像是一个离散的随时间而变化的动力系统，
其运行轨迹一定是 chain (tree) + cycle。
像是很多分支河流汇入一个主要河流，然后进入一个漩涡。

> ..., different starting states may lead into the same cycle i.e.,
> different starting chains may merge. But two paths, once merged (by
> entering a common state) can never diverge again.

以这种想像为基础，
再进一步去想象如何构造可以接受多种 event 的 machine，
就获得了一种新的理解。

- 即便是可以接受多种 events，
  也可以逐个 event 来分析 machine 的结构，
  即每次假设输入只是一种 event。

到目前为止，对于 finite automata，
Minsky 已经给出了两种新颖的理解方式了。

真实的计算机也有有限多个状态，
只不过状态太多了，有 2^n 个状态，
其中 n 是机器 bit 的个数。

因此，虽然状态数很少时，有限状态机的表达能力很弱，
但是，只要我们在写规则的时候允许状态和事件带有参数，
就可以构造出来实用的有限状态机。

当数字大到这种宇宙级别时，
以有限性为前提对系统做的分析其实已经不适用了。

> THEOREM: No fixed finite-state machine can multiply arbitrarily
> large pairs of binary (or decimal) numbers.

# 3 neural networks. automata made up of parts

每个 neural 都可以被视为有限状态机，
或者说，上一章的结论其实是：
任何有有限个状态的机器，
都不可避免地是有限状态机。

注意，当 Minsky 把（被当作黑箱的）有限状态机组合起来，
形成 cellular automata 或 neural network 时，
要求了机器之间时间同步。
在物理仿真和游戏渲染中，这种要求是常用的，
但是作为计算模型这个要求是不好的。
也许换成 Petri nets 可以解决需要同步的问题。

- 注意，所画的 state transition diagram
  是作为黑箱的有限状态机内部的构造，
  以有限状态机为基本零件，
  组合成别的计算模型时，
  每个有限状态机都只是一个节点。
  对有限状态机而言，只有一个输入 channel 和一个输出 channel，
  多输入和多输出需要用笛卡尔积来编码。
  也许 Petri nets 可以直接处理多个输入与多输出。

在用有限状态机组成 neural network 时，
连接方式很自由，连线的输出可以有分支，
输出的断点可以选择是「激发」还是「抑制」这两种类型。
这些连接方式本身就可以利用不同的拓扑来编码很多信息，
比如用 circle 来实现 memory。

neural networks 不像是 interaction nets，
而是更像 propagator model。
因为 interaction nets 是 ported graph，
而另外两种 graph 不是。

neuron 与 propagator model 中的 cell 保存信息时的机制不同，
cell 的机制是 lattice 理论，
而 neuron 可能根本不保存信息（不依赖自身状态的 finite automata），
而是只能用 circle 所形成的 feedback 来保存信息。

尝试设计一个语言来描述 neural network：

```scheme
(define-neural-network
    (and a b -> c)
  (2 a b -> c))

(define-neural-network
    (or a b -> c)
  (1 a b -> c))

(define-neural-network
    (memory start (- stop) -> output)
  (2 input control -> output)
  (1 start stop feedback -> feedback control))

(define-neural-network
    (gate-with-memory input start (- stop) -> output)
  (2 input control -> output)
  (1 start stop feedback -> feedback control))
```

fizzbuzz in neur:

```
fizzbuzz* -> fizz* buzz*.
fizz* -> * -> * -> fizz*.
buzz* -> * -> * -> * -> * -> buzz*.

start fizzbuzz* program.
```

fizzbuzz in scheme:

```scheme
(define-neural-network (fizz in out)
  (1 (1 (1 in)) out
   :on (lambda () (println "fizz"))))

(define-neural-network (buzz in out)
  (1 (1 (1 (1 (1 in)))) out
   :on (lambda () (println "buzz"))))

(define-neural-network (fizzbuzz in out)
  (fizz in out)
  (buzz in out))
```

```scheme
(define-neural-network fizz
  (-> start * end)
  (-> end start))

(define-neural-network buzz
  (-> start * * * end)
  (-> end start))

(define-neural-network fizzbuzz
  (= f (fizz))
  (= b (buzz))
  (-> start f.start)
  (-> start b.start))

(= fb fizzbuzz)
(! fb.start)
```

neur 的语法问题在于，没法形成 abstraction，
因为 abstraction 必须就连接进行，而没法就 neuron 进行。
也许应该完全模仿 propagator model 的二分图。

neur 的一个启发在于，neuron firing 的 event 本身就是程序的输出。

TODO

# 4 the memories of events in finite-state machines
# 5 computability, effective procedures, and algorithms. infinite machines
# 6 turing machines
# 7 universal turing machines
# 8 limitations of effective computability: some problems not solvable by instruction-obeying machines
# 9 the computable real numbers
# 10 the relations between turing machines and recursive functions
# 11 models similar to digital computers
# 12 the symbol-manipulation systems of post
# 13 post's normal-form theorem
# 14 very simple bases for computability
# 15 solutions to selected problems
# 16 suggestions for further reading and descriptor-indexed bibliography
