---
title: Propagation Networks
subtitle: A Flexible and Expressive Substrate for Computation
author: Alexey Andreyevich Radul
year: 2009
---

# Abstract

> The propagation paradigm replaces computing by global effects on a
> large memory with computing by networks of local, independent,
> stateless machines interconnected with stateful storage cells.

> The novel insight that should finally permit computing with
> general-purpose propagation is that a cell should not be seen as
> storing a value, but as accumulating _information about_ a value.

propagation 在 constraint processing 中很常用，
但是在 constraint processing 中，每个 propagation system 都是特殊的。
而这里要设计一个语言，使得人们可以用它实现任何 propagation system。

# 1 Time for a Revolution

> This dissertation introduces, explains, illustrates, and implements
> _general-purpose propagation._

> The key idea of propagating _mergeable_, _partial information_
> allows propagation to be used for general-purpose computation.

> The second key idea of making the merging mechanism _generic_ offers
> a new kind of modularity and composability.

> Finally a more technical contribution comes in the form of a
> structure for carrying _dependencies_, and a presentation of the
> fantastic consequences of combining dependencies with propagation.

## 1.1 Expression Evaluation has been Wonderful

> A program is a series of expressions, evaluated either
> for their value or for their effect.

其实，在设计语言时，我们无法避免 expression evaluation，
即便是要构造 graph 也需要用 expression evaluation 来表达。
用 expression 来构造 graph，也就是这里所说的 evaluated for effect。

## 1.2 But we Want More

列举了其他作者在突破传统的 expression evaluation paradigm 方面的努力：

- 2009-constraint-propagation--models-techniques-implementation--guido-tack.pdf
- 1995-logic--programming-and-prolog.pdf
- 2008-integrating-dataflow-evaluation-into-a-practical-higher-order-call-by-value-language.pdf

## 1.3 We Want More Freedom from Time

> Why? Why is evaluation of expressions, which has served us so
> faithfully for half a century, not good enough for all these people?
> The fundamental problem is time.  Time in evaluation-land is
> fundamentally constrained.  The successful evaluation of an
> individual expression, or, equivalently, the successful production
> of an individual value, inescapably marks a _point_ in time. The
> work done to produce that value came _before_; the work that will be
> done using that value comes _after_.

在之前 inet 的设计中，正是因为 call-by-value 这种具有确定时间顺序的语义，
使得我们可以用 expression evaluation 来构造 graph。

另外，既然 inet 和 propagator 都使用 expression evaluation 来构造 graph，
可否有一个一般的底层 graph builder 语言，
然后 inet 和 propagator 分别为 graph 语法的解释器呢？

## 1.4 Propagation Promises Liberty

> Fortunately, there is a common theme in all these efforts to escape
> temporal tyranny.  The commonality is to organize computation as a
> network of interconnected machines of some kind, each of which is
> free to run when it pleases, propagating information around the
> network as proves possible.  The consequence of this freedom is that
> the structure of the aggregate does not impose an order of time.
> Instead the implementation, be it a constraint solver, or a logic
> programming system, or a functional reactive system, or what have
> you is free to attend to each conceptual machine as it pleases, and
> allow the order of operations to be determined by the needs of the
> _solution_ of the problem at hand, rather then the structure of the
> problem's _description_.

也就是构造图的过程与解释图的过程分离。
构造图的 expression 不是语义，
而是语法，或者说构造出来的图是具体语法，
而解释图的程序才是语义。

这个论文要设计一般的 propagation 语言，
来统一上面提到的几个领域：

- constraint processing
- logic programming
- reactive programming

> I believe that general-purpose propagation offers an opportunity of
> revolutionary magnitude. The move from instructions to expressions
> led to an immense gain in the expressive power of programming
> languages, and therefore in understanding of computers and
> productivity when building computing artifacts. The move from
> expressions to propagators is the next step in that path. This
> dissertation merely lifts one foot -- who knows what wonders lie
> just around the corner?

把 expression 到 propagator 的发展，
与 instruction 到 expression 的发展相比较。

可能有点牵强，哈哈。

# 2 Design Principles

前一章阐释发展 propagator 之必要，这一章讲如何发展。
从之前几十年 propagator 的发展中总结一些原则出来。

在总结准则的时候，还树立了一个明确的「敌人」，即「时间」这个概念。

## 2.1 Propagators are Asynchronous, Autonomous, and Stateless

> To be explicit about the events in the system, let us say that the
> machines are not connected to each other directly, but through
> shared locations that can remember things which interested machines
> can read and write. Such locations are tradition- ally called
> **cells**. As the machines communicate with each other and perform
> their various computations, information will propagate through the
> cells of the network.  The machines are for this reason
> traditionally called **propagators**.

cells 与 propagators，
和 petri net 中的 places 与 transitions
这种二分图类似。

> The purpose of all this is to avoid too early a commitment to the
> timing of events in our computers, so that the computers can decide
> for themselves in what order to do things, guided by what they
> discover when performing the tasks we assign to them.  We should
> consequently not build into our infrastructure any timing
> commitments we can possibly avoid.

首先是 propagator 的 stateless 这一原则：

> Let us therefore posit that our propagators are of themselves
> memoryless. The idea of memory is inextricably entwined with the
> idea of time, so let us push all memory out of our computing
> propagators, and into the cells, who are dedicated to dealing with
> memory anyway. We lose no generality by doing this, because we can
> always equip any machine that needs to appear stateful with a
> private cell to hold its state.

说记忆总与时间有关，看起来确实如此。有趣。

关于 propagator 的 autonomous 和 asynchronous 这两个原则：

> Let us likewise posit that our propagators are autonomous,
> asynchronous, and always on -- always ready to perform their
> respective computations. This way, there is no notion of time
> embedded in questions of which device might do something when, for
> they are all always free to do what they wish. Since the cells are
> the system's memory, it is the cells' responsibility to provide
> interlocks appropriately to prevent any nasty memory corruption
> problems, and to prevent propagators from seeing the contents of an
> individual cell in an inconsistent state.

但是 cell 还是要有自己的机制来保证自己的状态是 consistent 的，
比如，一种方案是，用 actor model 来实现 cell 和 propagator，
propagator 和 cell 之间也用 message passing 来沟通。

## 2.2 We Simulate the Network until Quiescence

Propagator 网络可以用实际的硬件构造，
也可以用软件来模拟。
初期当然是用软件模拟。

构建 Network 就是编程，
Network 的运行结果就是计算结果。

Network 的运行有三种情况：

- 最终达到稳定状态（quiescence），可以用来代表计算结果。
- 无限趋近于一个稳定状态。
- 没有稳定状态，网络一直活跃。

> By "quiescent" I mean "no propagator will add any more information
> to any cell without further input from outside the network."

如何 simulate？这里还没提到。
可能的方案是，cell 每次变化之后，
都把自己已经变化了这件事告知一个 scheduler，
可以用 actor model 实现，也可以是单线程程序中简单的一个 queue。

## 2.3 Cells Accumulate Information

> We should think of a cell as a thing that accumulates _information
> about_ a value.

这是这篇论文的主要贡献，即用 Lattice。

> The basic philosophical reason why cells must accumulate
> incrementally refinable information is that computation in
> propagation networks is essentially multidirectional.  Since several
> propagators can point into the same cell, there is no reason to
> demand that just one of them be responsible for producing the whole
> of that cell's contents.  In fact, the whole point of building
> propagation systems is to capitalize on the possibility of
> multidirectionality -- and to do that, each cell must be able to
> accept and merge all the contributions of all the propagators that
> might write to it.

### 2.3.1 Why don't cells store values?

如果一个 cell 已经有某个 value 了，
又有 propagator 向这个 cell 输出 value 的时候，
应该如何更新 cell？

最自然的方式就是用 lattice 的 join 或 meet。

也许在实现的时候，
这个 lattice 操作应该由 cell 自身来进行，
propagator 只负责把信息发过去。

- 也就是后文说的 making the cells a little smarter.

### 2.3.2 Because trying to store values causes trouble

如果没有 lattice，遇到上面的情况时，就有三种选项：

- A. Drop the second value on the floor,
- B. Overwrite the first value, or
- C. Forbid the event (signal an error).

这些方案的特点是都不看 value 是什么，
也不需要知道 value 的结构，
但是这些方案显然都有问题。

### 2.3.3 And accumulating information is better

> We solve this pile of problems at a stroke by changing the
> mindset. Instead of thinking of a cell as an object that stores a
> _value_, think of a cell as an object that stores _everything you
> know about a value_.

赋 value 以 lattice 结构才是正确的方案。

遇到 contradiction 的时候，可以分叉出来一个新的世界观。
但这一切还在 lattice 的范围内，
只是某些特殊的高阶 lattice 而已。

> The point of this dissertation is that propagating and accumulating
> partial information is enough for a complete, expressive, and
> flexible model of computation.

lattice 作为抽象的数学结构，
是一个 interface，
cell 只要实现这个 interface 就好。

# 3 Core Implementation
# 4 Dependencies
# 5 Expressive Power
# 6 Towards a Programming Language
# 7 Philosophical Insights
