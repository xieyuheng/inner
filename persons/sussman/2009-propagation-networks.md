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

时间的限制未必是坏处，
比如，在之前 inet 的设计中，
正是因为 call-by-value 这种具有确定时间顺序的语义，
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
> can read and write. Such locations are traditionally called
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

> We should think of a cell as a thing that
> accumulates _information about_ a value.

这是这篇论文的主要贡献，即用 lattice。

注意，如果把 merge 当成是 lattice 的 join 或 meet，
就要求 merge 满足交换律，
但是也许有实用的信息积累过程，
并不满足交换律。

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

## 3.1 Numbers are Easy to Propagate

注意，与 "The Art" 不同，这里没有用带有循环的 Heron 迭代作为第一个例子，
而是用了更简单的 `fahrenheitToCelsius`。
因为在迭代中逼近一个数字，需要用特殊的 lattice 来处理，
而不能用简单的平凡 lattice。

与 "The Art" 相比，这里的实现需要 scheduler，
因此在用户接口上，用户每次设置好 cell 中的初始值时，
需要调用 `run` 才能看 propagator network 的结果。
而在 "The Art" 中，每次调用 `add-content`
都会自动运行所有相关的 propagators。
修改很简单，只要把 `add-content` 中马上调用所有 propagators 的地方，
改成把 propagators 交给（submit） scheduler 就行了（比如保存到 scheduler 的队列里）。

- 在实现 inet 的时候，
  我所设计的用户接口也是先构造 network 再调用 `run`。

从图论的角度来考虑这里所描述的实现，
cells 和 propagators 形成一个（无向）二分图，
这个二分图中的边是通过 cells 和 propagators 之间的双向引用来实现的，

- 一个 cell 引用一个 propagator 的方式是，
  把它保存在一个这个 cell 所拥有的 propagator 的列表里。

- 而一个 propagator 是一个 unary closure，
  它引用一个 cell 的方式是在 closure 的 env 中，
  引用了某个保存这个 cell 的变量。

这与我的 inet 实现不同，
在 inet 中我是真的设计了代表 graph 的数据结构，
然后用来表示 inet。

在 inet 中，edge 是有具体存在的，
而在 propagator network 中，
edge 就是简单的用指针实现的双向连接了。

并且 inet 中的 edge 是用两个 half-edge 来实现的，
也就是说，在冯诺依曼构架下，所实现的东西，
自然的是 directed graph，
graph 是 directed graph + 双向连接。

这里作者给出的，具有教学意义的实现顺序是：

- 先实现 cell。

  - 推迟 scheduler 的实现，
    让 `add-content` 立刻运行所有的 propagator，
    或者用最简单的 scheduler + queue。

- 再实现 propagator。

与 "The Art" 中使用 Heron 迭代，而需要在初期就实现 `if` 不同，
这里可以把 `if` 的实现推迟到后期再实现。

## 3.2 Propagation can Go in Any Direction

到这里我们只是用 propagator network 模拟了 expression，
还没有处理的重要 feature 有：

- Recursion (Section 6.2)
- Compound data (Section 6.2)
- Higher order function (Future work)
  - 看来作者也没有明白如何处理高阶函数，
    我在 inet 的实现中也没有明白如何处理高阶函数。
    但是这也许并不难，
    只要让 propagator definition
    （或者说 propagator constructor）
    成为一种 value 就可以了，
    在探索的初期，
    这个 value 所属的 lattice 可以是平凡的 anti-chain。

> One of the original motivations for examining propagators is that
> multidirectional constraints are very easy to express in terms of
> unidirectional propagators.

> This does not yet constitute a full constraint solving system,
> because it will get confused by circularities in the constraints
> (which amount to simultaneous equations).  We will return to that
> problem later (Section 5.3), but for now even this facility can
> already be useful if it could integrate seamlessly with general
> programming, and it is already painful to try to build directly in
> expression-land.

这里说与 "a full constraint solving system" 相比还欠缺的属性，我还不理解。
也许到 (Section 5.3) 可以理解。

一个 `fahrenheit-celsius` 比分别写
`fahrenheit->celsius` 与 `celsius->fahrenheit` 要简单，
一个 `fahrenheit-celsius` 作为 API，使用起来也比两个函数要简单。
当我们再加一个 `celsius-kelvin` 的时候，
三种单位之间的换算需要六个独立的单向转换函数，
但是们只要把两个 propagator network 连接起来就行了。

看了这个例子，我想也许 propagator 可以在 Web 前端，
取代单方向的 reactive programming，
作为表达能力更强的范式，
来简化某些 Web 前端中遇到的问题。
但是注意，一定要有真实的案例支撑论点才行，
不能为了推广自己的方案而骗人。

如果像上面一样，从图论的角度分析 vue 的 reactive system，
可以发现 ref 就是 cell，而 watch 就是 propagator，

- TODO vue 的 API 是否与我目前的实现的 API 完全等价呢？
  需要好好检验一下这个问题。

## 3.3 We can Propagate Intervals Too

Cell 能够从任意多个方向接受信息，
那么下一步自然就想用 cell 来整合多个方向发来的部分信息。

这里没有急着做抽象，
而是直接加 if else 让实例代码能够跑过，
类似 Sandi Metz 的 "Shameless Green"。

## 3.4 Generic Operations let us Propagate Anything!

这里有必要用 generic function + dispatching，
而不能用简单的 interface，因为需要扩展的函数 merge 是二元函数，
并且应该根据两个参数的类型来 dispatch。

- 如果 generic dispatch 如此重要，是否在设计 cicada 的时候，
  也应该用 generic dispatch 而不应该用简单而 OOP 呢？
  这样，在使用 sexp 时，就没有 dot 语法上的设计难题了。

首先 `merge` 本身要被定义为 generic 函数。

其次在 define primitive propagator 的时候，
要以 generic 为基础来做定义。

- 注意这里的技巧，
  在用 `function->propagator-constructor` 时，
  套上了一个 lifting 函数 -- `nary-unpacking`：

  > ... to have a common mechanism to handle
  > sufficiently uniform types of partial information.

  以一般的方式处理，某些不用知道具体类型就可以处理的 partial information。
  在实现 cicada 的时候，我也经常用类似的技巧。
  `nary-unpacking` 的具体定义要看 A.5.1 一节。

# 4 Dependencies

人类可以处理相互冲突的信念（inconsistent beliefs）而不死机。
但是纯粹的古典逻辑不行。

> Our personal belief systems appear to be locally consistent, in that
> there are no contradictions apparent.  If we observe inconsistencies
> we do not crash -- we chuckle!

人类在思考中，
所能追求的，也是所应该追求的，
仅仅是局部的一致性，
而不是全局的一致性。

> Dependency decorates metadata to data.  The metadata records the
> justifications for the data.  Every piece of data (or procedure)
> came from somewhere.  Either it entered the computation as a premise
> that can be labeled with its external provenance, or it was created
> by combining other data. We can add methods to our primitive
> operations which, when processing or combining data that is
> decorated with justifications, can decorate the results with
> appropriate justifications.

首先是简单地，在函数作用的过程中，
用 metadata 来追踪 external provenance 的集合（称为 premises）。

以加法为例子，与 data 的相加平行的是作为集合的 premises 的 union。

这只是 dependency 所能引出的最简单的概念，
论文一共有三层 dependency 所引出的概念：

- dependencies for provenance
- dependencies for alternate worldviews
- dependencies for implicit search

如果这些概念都能通过扩展 merge 来实现，
并且这些扩展还是渐进的，
那将是极好的。

关于 alternate worldviews：

> By decorating data with dependencies a system can manage and
> usefully compute with multiple, possibly inconsistent world views.
> A world view is a subset of the data that is supported by a given
> set of explicit assumptions. Each computational process may restrict
> itself to working with some consistent world view.  Dependencies
> allow a system to separate the potentially contradictory
> consequences of different assumptions, and make useful progress by
> exercising controlled incredulity.

关于 implicit search：

> If a contradiction is discovered, a process can determine the
> particular **nogood set** of inconsistent premises. The system can
> then "chuckle", realizing that no computations supported by any
> superset of those premises can be believed; computations can proceed
> in worldviews that do not include the nogood set. This chuckling
> process, **dependency-directed backtracking**, can be used to
> optimize a complex search process, allowing a search to make the
> best use of its mistakes.

关于 dependency-directed backtracking 的文献：

- [Stallman and Sussman, 1977]
- [Lieberherr, 1977]
- [Zabih et al., 1987]

即使没有 implicit search 这个重要的应用，
在 propagator 中支持 alternate worldviews 这个 idea 也很有价值：

> But enabling a process to simultaneously hold beliefs based on
> mutually inconsistent sets of premises, without logical disaster, is
> itself revolutionary.

> ... dependency tracking is extremely useful in its own right, for
> provenance information, world-view separation, and search.

关于 dependency tracking，
我想到了在 web 前端中实现 reactive programming 时，
用到的自动的 dependency tracking，
即 `watchEffect(fn)` 时，会把 `fn` 保存到某个全局变量中，
在读 `ref` （类似 cell），
或经过 proxy 来读 reactive object 的 attribute 时，
会在 `ref` 中记录对这个 `fn` 的依赖
（类似 cell 要保存依赖了自己的 propagators）。

> Observe that as we evolve our dependency tracking system below, we
> need make no changes to any of the code already presented, whether
> to cells, basic propagators, or the scheduler.  All our changes are
> just new partial information structures.  The core propagation
> system is as modular and flexible as promised in the beginning.

> As a case in point, when the system needs to change its world-view,
> as may happen in search, it can do so directly through a partial
> information structure—no provision for this is necessary in the
> toplevel controller of the propagation system. This stands in stark
> contrast to traditional constraint satisfaction, where search is an
> additional, special-purpose external control loop commanding the
> propagation proper.

实现神经网络时所做的 back propagation，也可以说是 "a special-purpose
external control loop commanding the propagation proper"。

## 4.1 Dependencies Track Provenance

我想这里用 supported value 来命名，
意味着 value supported by evidence (provenance information)，
另外又用 premise 来命名，让人想到 premise 的集合，作为命题的集合在逻辑意义上的且。

直接用 lisp symbol 来代表 premise，
也让人想到命题逻辑中代表命题的 atom。

同时，premises 作为 symbol 的集合，
也让人想到 formal concept analysis 中对偶的 object 与 attribute。

将 `merge` 扩展到 supported value，
其实是结构化地将 `merge` 扩展到了，
能作用于平行的两部分数据 -- interval 和 set。
也许我们要先将 `merge` 扩展到 `Set`。

A justified-intervals anomaly:

```
A:     [           ]
B:           [        ]
-----------------------
A,B:         [     ]
C:         [   ]
-----------------------
A,B,C:       [ ]
```

即 supports 的集合可能与 supported value put 进来的顺序有关，

> The deep reason why this happens is that an interval is really a
> compound of the upper and lower bounds, and it is profitable to
> track those dependencies separately; we will study that question, in
> its general form, in Section 6.3. This anomaly will also become less
> severe as a consequence of the worldview support we will add in
> Section 4.2.

> The important thing is to describe how to merge the information
> contained in two such data structures; see Figure 4-2. The value
> contained in the answer must of course be the merge of the values
> contained in the two inputs, but sometimes we may get away with
> using only some of the supporting premises. There are three cases:
> if neither the new nor the old values are redundant, then we need
> both their supports; if either is strictly redundant, we needn’t
> include its support; and if they are equivalent, we can choose which
> support to use. In this case, we use the support of the value
> already present unless the support of the new one is strictly more
> informative (i.e., is a strict subset of the same premises).

也就是说，这里用的并不是集合意义上的 lattice，
或者说根本就不是 lattice，
因为集合在一般情况下做了并，
但是在某个特殊情况下做了交。

但是其实在某种情况下用并，
但是在另外特定的情况下用交，
可能还是可以构成一个 lattice。

TODO

## 4.2 Dependencies Support Alternate Worldviews

关于这里的命名，我不想用 TMS 这个所写，而是用 `BeliefSystem`。

```typescript
type BeliefSystem = {
  beliefs: Array<Supported>
}
```

关于 TMS：

- [Doyle, 1978]
- [McAllester, 1978]
- [Forbus and de Kleer, 1993]

可以理解为是 cell 的 belief system，
每个 cell 可以相信一些信息，
这些信息以 supported 的形式存在，
并且 promises 之间可能存在冲突。

这里论文中的命名有些乱：

- supported.support 也被称作 promises 和 worldview。

注意，supported 是 cell 可以保存的一种信息，
而不是 put （或 add-content）接口函数的一部分。

注意，cell 保存的信息可能会升级，
Number <= Interval <= Supported <= BeliefSystem，
保存的信息越来越多，并且只能递增不能减少。
当一个 cell 里已经保存着 BeliefSystem 了，
再往里加 Supported，得到的还是 BeliefSystem。
也就是说 cell 所保存的值所在的偏序关系，是跨类型的。

- BeliefSystem 中和「或」相关的语义，
  可能和分配格的理论类似。

如何避免这里所用的 global worldview？

## 4.3 Dependencies Explain Contradictions

> As promised, contradictory beliefs are not traumatic.
> The deep reason contradiction handling works so well is that
> `the-contradiction` is just another partial information state,
> and our truth maintenance machinery operates on partial information
> by design.

## 4.4 Dependencies Improve Search

> Implicit generate-and-test can be viewed as a way of making systems
> that are modular and independently evolvable.

generate-and-test 是 Sussman 经常提到的一个关键词（一个关键 idea）。

> A better way to handle this issue is to build backtracking into the
> infrastructure.

这就是 Sussman 所说的，
设计程序语言的意义在与，
能够把一个东西变成 implicit。

- 这不一定要通过设计新的语法来完成，
  也可以以函数为语法，在函数复合的限制下完成。

  - 浅嵌入就是如此。

- 但是函数有的时候确实是不够的，
  虽然图灵完备，但是写起来太啰嗦。

> It is, and should be, the receiver’s responsibility to determine if
> the ingredients to its computation are appropriate and acceptable.

比如，在实现 web app 的时候，
前端作为 receiver，要能接入任何后端，
即使后端的 API 是错的，
前端也要给出有意义的交互。

> We can reap the benefit of this modular flexibility by burying
> search into the already-implicit control flow of the propagator
> network.

我想，这和 datalog 的 forward chaining 应该很像，
在 datalog 中，每个关系作为一个数据库，
保存着 clauses（限于 facts），
这个数据库在 forward chaining 一直在演化和递增。
而在 propagator 中，cell 就是这样的数据库，
当里面保存的是带有「或」语义的 `BeliefSystem` 时，
就和 datalog 的一个关系中保存 clauses 的情况类似。

TODO

# 5 Expressive Power

TODO

# 6 Towards a Programming Language

## 6.1 Conditionals Just Work

TODO

# 7 Philosophical Insights

## 7.1 On Concurrency

> We are used to thinking, both in our everyday lives and in our
> computer programs, of things as happening in a sequence.

这是 concurrency 成问题的原因。

> In point of fact, concurrency is the natural state of affairs, and
> synchronicity is what's difficult.

大自然是非串行的，只不过计算机是串行才好 debug 的。

> The physical world in which we live is perfectly concurrent: every
> little patch of universe evolves on its own, according to local
> rules, and physical effects travel from one patch to another at a
> finite speed (which we even know: 299,792,458 meters per second).

> As a matter of physics, time in the universe is partially ordered: A
> occurs before B if and only if A has any chance of influencing B, to
> wit if and only if light emitted at A can reach the location of B
> before B occurs.  Over sufficiently large spaces or sufficiently
> small times this entails pairs of events neither of which occurred
> before the other, because light hasn't the time to reach in either
> direction.

尽管可以用宏观和高速的物理学来讨论时间，
但是在人类大部分时候所观察的尺度中，
时间还是全序的，而不是偏序的。

> Our experience of time appears as a linear stream of events only
> because our memory imposes an order on them, which is the order in
> which events are remembered to have occurred.

这就像是说，一个命题正确与否，取决于我是否倾向于相信这个命题。
这显然是错误的，甚至相信这种准则，将是不利于我在世间生存的。

下面讨论电子电路中的时间问题，算是回到了实用的范畴。
解释了为什么计算机必然向多核发展，
因此类似 propagator 的计算模型必将大行其道。

> The obvious thing to do is to make some variation on multicore
> computers, which consist of many complete locally-synchronous computer
> blocks. Unfortunately, the standard rendition of this idea is fatally
> flawed because it provides for a large memory that the cores all
> share. This large memory implicitly imposes a linear order on the
> reads and writes that it experiences. Since synchronizing concurrent
> things well is hard, the implicit synchronization done by such a
> memory is terrible. As a consequence of the pervasive, terrible
> synchronization, it appears that all concurrent programming is hard.

批判了多核共享内存的方案。
也许我可以以「制作非共享内存的 CPU」为目标项目，
来学习电子电路。

也许作者这里对共享内存的批判是错误的
（因此上面的项目根本就是不切合实际的），
因为 Forth 的创造者设计过多核，局部内存的芯片，
但是芯片中 cell 的拓扑是固定的，
每个 cell 只能直接与周围的邻居通讯，
而 propagator 要求 cell 的拓扑可变。

> Propagators don't magically solve the hard problem of synchronizing
> concurrent systems, they merely move it where it belongs -- away
> from the "concurrent" and to the "synchronizing".

呼应前文。

## 7.2 On Time and Space

> At the bottom of modern computation lie the highly parallel laws of
> electromagnetism, and the solid state physics of devices, that
> describe the behavior of electrical circuits.

> Thus we are come full-circle: from asynchronous devices with local
> communication; through computers that forcibly synchronize them; to
> simulations of asynchronous devices with local communication.

指在传统计算模型中模拟 propagator networks 的行为。

## 7.3 On Side Effects

想要让一个语言实用，就必须处理 side effects。

如何在 propagator model 中处理 side effects，
我在实现 inet 的时候也遇到过这个问题。

在 inet 中，不处理，也是一种选择，
因为可以返回表达式，被另一个带有 side effects 的语言解释。

在 propagator model 中，
用 actor model 的消息传递来处理 side effects
看来是自然而然的选择。

> The essential reason why side effects tend to mess up models of
> computation is that side effects inescapably introduce time.

其实并非如此，
因为消息传递作为重要且通用的 side effects，
是异步的。

# A Details

## A.1 The Generic Operations System

这里描述的是一个简单的运行时的 predicate dispatch system。

我们在 JavaScript 中实现的 API 如下：

```javascript
// const generic = defineGeneric({ default?: ... })
// defineHandler(generic, predicates, (...args) => ...)
```

例如：

```javascript
export const merge = defineGeneric({
  default: (content, increment) =>
    increment === content ? content : theContradiction,
})

defineHandler(merge, [isAnything, isNothing], (content, increment) => content)
defineHandler(merge, [isNothing, isAnything], (content, increment) => increment)
```

## A.2 The Scheduler

> ... it is, in fact, especially important that the scheduler not make
> too many promises, because a helpful promise that supports one
> application is a shackle that restrains another.

实现一个极简的 scheduler，
只为了在单线程的情况下模拟 propagator 网络。

有 scheduler 之后，需要 explicit use `run`，
而不是在每次 `addContent` 时候自动 `run` 了。

抽出来这个显示的 `run` 之后，可以支持：

- inspect scheduler 的状态。
- 让 scheduler 在运行的时候打 log。
- scheduler 在运行时的 profiling。

TODO 读论文中 scheduler 的代码。

- 论文要求 `schedule` 函数是 idempotent 的，
  这要求我们不能在 `definePrimitive` 中创建 closure。
- 如何做错误处理，也可以模仿论文中的实现。

## A.3 Primitives for Section 3.1

TODO

## A.4 Data Structure Definitions

TODO

## A.5 Generic Primitives

这里用 generic 实现了 monad，
需要复习一下 monad。

```cicada
given M: (Type) -> Type,
given A: Type,
claim unit: (A) -> M(A)

given M: (Type) -> Type
given A: Type
given B: Type
claim bind: (M(A), (A) -> M(B)) -> M(B)

given M: (Type) -> Type
given A: Type
claim flatten: M(M(A)) -> M(A)
```

如果 `bind` 的类型是（交换上面的参数顺序的）
`((A) -> M(B), M(A)) -> M(B)`，
并且假设 `a |> f = f(a)` 那么：

```cicada
bind(g, bind(f, a)) =
a
|> bind(f)
|> bind(g)
```

这可以代替 do notation 来形成易读的语法。

注意如果 `bind` 是中缀表达式的 `>>=`
（用 monad 在前的参数顺序），
也可以达到上面的效果。

```cicada
a >>= f
  >>= g
```

可以说 `|>` 是想用一个中缀表达式，
代替诸多中缀表达式。
但是代价是传统的 `bind` 的参数要交换位置。
也许应该更名叫 `chain`。

要为了这一节复习 monad，
其实只要知道：

```haskell
ma >>= f <-> (join ∘ (map f)) ma
bind(ma, f) <-> join(map(f, ma))
```

也就是：

```scheme
(define (generic-bind thing function)
  (generic-flatten (generic-unpack thing function)))
```

`generic-unpack` 其实是 `map`，
或者用 haskell 的 `fmap` 命名更好一些。

在 JS 实现中，我们的定义如下 generic function：

- fmap
- join
- bind
- pure

`nary-unpacking` 应该是：

```cicada
naryFmap: ((A1, A2, ...) -> B) -> (M(A1), M(A2), ...) -> M(B)
```

但是按照文章中的定义，所定义的可能是 `naryBind`。

TODO

## A.6 Miscellaneous Utilities

TODO

# Bibliography

[Abdelmeged et al., 2007] Ahmed Abdelmeged, Christine Hang, Daniel Rinehart, and Karl Lieberherr (2007). Superresolution and P-Optimality in Boolean MAX-CSP Solvers. Transition.

[Abelson et al., 1996] Harold Abelson, Gerald Jay Sussman, and Julie Sussman (1984, 1996). Structure and Interpretation of Computer Programs. MIT Press, Cambridge, MA.

[Allen et al., 2005] E. Allen, D. Chase, V. Luchangco, J.W. Maessen, S. Ryu, G.L. Steele Jr, S. Tobin-Hochstadt, J. Dias, C. Eastlund, J. Hallett, et al. (2005). The Fortress Language Specification. Sun Microsystems.

[Andersen, 1837] Hans Christian Andersen (1837). The Emperor’s New Clothes. In Tales for Children.

[Apt, 2003] Krzysztof R. Apt (2003). Principles of Constraint Programming. Cambridge University Press, Cambridge, UK.

[Apt et al., 1999] Krzysztof R. Apt, Victor W. Marek, Miroslaw Truszczynski, and David S. Warren, editors (1999). The Logic Programming Paradigm: a 25-Year Perspective. Springer, Berlin; New York.

[Athanassakis, 1983] Apostolos N. Athanassakis (1983). Hesiod: Theogony, Works and Days and The Shield of Heracles. Johns Hopkins University Press, Baltimore and London. p.90.

[Bacchus et al., 2003] Fahiem Bacchus, Shannon Dalmao, and Toniann Pitassi (2003). Value Elimination: Bayesian Inference via Backtracking Search. In Uncertainty in Artificial Intelligence, pages 20–28.

[Bonawitz, 2008] Keith A. Bonawitz (2008). Composable Probabilistic Inference with Blaise. CSAIL Tech Report MIT-CSAIL-TR-2008-044, MIT Computer Science and Artificial Intelligence Laboratory, Cambridge, MA.

[Borning, 1979] Alan H. Borning (1979). ThingLab–a Constraint-Oriented Simulation Laboratory. PhD thesis, Stanford University, Stanford, CA, USA.

[Borning, 1981] Alan H. Borning (1981). The Programming Language Aspects of ThingLab, a Constraint-Oriented Simulation Laboratory. ACM Transactions on Programming Languages and Systems, 3(4):353–387.

[Bricklin and Frankston, 1999] Dan Bricklin and Bob Frankston (1999). VisiCalc: Information from its Creators, Dan Bricklin and Bob Frankston. http://bricklin.com/visicalc.htm.

[Calandra, 1961] Alexander Calandra (1961). The Teaching of Elementary Science and Mathematics. Washington University Press, St. Louis.

[Calandra, 1968] Alexander Calandra (1968). Angels on a Pin. Saturday Review.

[Cooper, 2008] Gregory H. Cooper (2008). Integrating Dataflow Evaluation into a Practical Higher-Order Call-by-Value Language. PhD thesis, Brown University.

[Cooper and Krishnamurthi, 2004] Gregory H. Cooper and Shriram Krishnamurthi (2004). FrTime: Functional Reactive Programming in PLT Scheme. Computer science technical report CS-03-20, Brown University.

[Cooper and Krishnamurthi, 2006] Gregory H. Cooper and Shriram Krishnamurthi (2006). Embedding Dynamic Dataflow in a Call-by-Value Language. Lecture Notes in Computer Science, 3924:294.

[Cormen et al., 2001] T.H. Cormen, C.E. Leiserson, R.L. Rivest, and C. Stein (2001). Introduction to Algorithms. MIT Press, Cambridge, MA.

[de Kleer, 1976] Johan de Kleer (1976). Local Methods for Localizing Faults in Electronic Circuits. AI Memo 394, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[de Kleer and Brown, 1992] Johan de Kleer and John Seely Brown (1992). Model-Based Diagnosis in SOPHIE III. Readings in Model-Based Diagnosis.

[Dinesman, 1968] Howard P. Dinesman (1968). Superior Mathematical Puzzles, with Detailed Solutions. Simon and Schuster, New York, NY.

[Doyle, 1978] Jon Doyle (1978). Truth Maintenance Systems for Problem Solving. AI Memo 419, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[Doyle, 1890] Sir Arthur Conan Doyle (1890). The Sign of the Four. Lippincott’s Monthly Magazine. Available online at http://www.gutenberg.org/files/2097/2097-h/2097-h.htm.

[Driscoll et al., 1989] James R. Driscoll, Neil Sarnak, Daniel D. Sleator, and Robert E. Tarjan (1989). Making Data Structures Persistent. Journal of Computer and System Sciences, 38(1).

[Elliott and Hudak, 1997] Conal Elliott and Paul Hudak (1997). Functional Reactive Animation. In Proceedings of the second ACM SIGPLAN International Conference on Functional Programming, pages 263–273. Association for Computing Machinery, New York, NY.

[Erman et al., 1980] L.D. Erman, F. Hayes-Roth, V.R. Lesser, and D.R. Reddy (1980). The Hearsay-II Speech-Understanding System: Integrating Knowledge to Resolve Uncertainty. ACM Computing Surveys (CSUR), 12(2):213–253.

[Ernst et al., 1998] M. Ernst, C. Kaplan, and C. Chambers (1998). Predicate Dispatching: A Unified Theory of Dispatch. Lecture Notes in Computer Science, pages 186–211.

[Floyd, 1967] Robert W. Floyd (1967). Nondeterministic Algorithms. Journal of the ACM (JACM), 14(4):636–644.

[Forbus and de Kleer, 1993] Kenneth D. Forbus and Johan de Kleer (1993). Building Problem Solvers. MIT Press, Cambridge, MA.

[Friedman and Wand, 2007] Daniel P. Friedman and Mitchell Wand (2007). Essentials of Programming Languages. MIT Press, Cambridge, MA, 3rd edition.

[Goodman et al., 2008] N.D. Goodman, V.K. Mansinghka, D. Roy, K. Bonawitz, and J.B. Tenenbaum (2008). Church: a Language for Generative Models. In Uncertainty in Artificial Intelligence.

[Gosling et al., 2005] J. Gosling, B. Joy, G. Steele, and G. Bracha (2005). The Java (TM) Language Specification. Addison-Wesley Professional.

[Gupta et al., 1996] Vineet Gupta, Radha Jagadeesan, and Vijay Saraswat (1996). Models for Concurrent Constraint Programming. Lecture Notes in Computer Science, pages 66–83.

[Hand, 2009] Linda Hand (2009). Have you found all three easter eggs in this document? In Look Carefully, page 7. MIT Press, Cambridge, MA.

[Hanson, 2007] Chris Hanson (2007). Personal communication.

[Hanson et al., 2005] Chris Hanson et al. (2005). MIT/GNU Scheme Reference Manual. Massachusetts Institute of Technology, Cambridge, MA. http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/index.html. The code presented in this dissertation was run specifically on MIT/GNU Scheme Release 7.7.90.+, Snapshot 20080401, under a GNU/Linux operating system.

[Hayes-Roth, 1985] Frederick Hayes-Roth (1985). Rule-Based Systems. Communications of the ACM, 28(9):921–932.

[Heath, 1921] Thomas Little Heath (1921). A History of Greek Mathematics. Clarendon Press, Oxford.

[Hewitt, 1969] Carl E. Hewitt (1969). Planner: A Language for Proving Theorems in Robots. In Proceedings of the International Joint Conference on Artificial Intelligence, pages 295–301.

[Hudak et al., 1999] Paul Hudak, John Peterson, and Joseph H. Fasel (1999). A Gentle Introduction to Haskell 98. Online tutorial.

[Jaynes, 2003] Edwin T. Jaynes (2003). Probability Theory: The Logic of Science. Cambridge University Press, Cambridge, UK.

[Johnston et al., 2004] W.M. Johnston, J.R.P. Hanna, and R.J. Millar (2004). Advances in Dataflow Programming Languages. ACM Computing Surveys, 36(1):1–34.

[Jones, 2002] Simon L. Peyton Jones (2002). Tackling the Awkward Squad: Monadic Input/Output, Concurrency, Exceptions, and Foreign-Language Calls in Haskell. Engineering theories of software construction.

[Kelsey et al., 1998] Richard Kelsey, William D. Clinger, Jonathan Rees, et al. (1998). Revised5 Report on the Algorithmic Language Scheme. SIGPLAN Notices, 33(9):26–76.

[Kiczales et al., 1999] Gregor Kiczales, Daniel G. Bobrow, and Jim des Rivières (1999). The Art of the Metaobject Protocol. MIT Press, Cambridge, MA.

[Konopasek and Jayaraman, 1984] Milos Konopasek and Sundaresan Jayaraman (1984). The TK! Solver Book: a Guide to Problem-Solving in Science, Engineering, Business, and Education. Osborne/McGraw-Hill.

[Liang et al., 1995] S. Liang, P. Hudak, and M. Jones (1995). Monad Transformers and Modular Interpreters. In Proceedings of the 22nd ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, pages 333–343. Association for Computing Machinery, New York, NY.

[Lieberherr, 1977] Karl Lieberherr (1977). Information Condensation of Models in the Propositional Calculus and the P=NP Problem. PhD thesis, ETH Zurich. 145 pages, in German.

[Lloyd, 1987] J.W. Lloyd (1987). Foundations of Logic Programming. SpringerVerlag, New York, NY.

[McAllester, 1978] David Allen McAllester (1978). A Three Valued Truth Maintenance System. AI Memo 473, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[McAllester, 1990] David Allen McAllester (1990). Truth Maintenance. In Proceedings of the Eighth National Conference on Artificial Intelligence, volume 2, pages 1109–1116. AAAI Press.

[McCarthy, 1963] John McCarthy (1963). A Basis for a Mathematical Theory of Computation. In P. Braffort and D. Hirschberg, editors, Computer Programming and Formal Systems, pages 33–70. North-Holland, Amsterdam.

[Mikkelson and Mikkelson, 2007] Barbara Mikkelson and David Mikkelson (2007). The Barometer Problem. http://www.snopes.com/college/exam/barometer.asp.

[Milch et al., 2005] B. Milch, B. Marthi, S. Russell, D. Sontag, D.L. Ong, and A. Kolobov (2005). BLOG: Probabilistic Models with Unknown Objects. In Proceedings of the Nineteenth Joint Conference on Artificial Intelligence.

[Müller, 2001] Tobias Müller (2001). Constraint Propagation in Mozart. PhD thesis, Universität des Saarlandes, Saarbrücken.

[Nilsson et al., 2002] H. Nilsson, A. Courtney, and J. Peterson (2002). Functional Reactive Programming, Continued. In Proceedings of the 2002 ACM SIGPLAN workshop on Haskell, pages 51–64. Association for Computing Machinery, New York, NY.

[Nilsson and Małuszyński, 1995] Ulf Nilsson and Jan Małuszyński (1995). Logic, Programming and Prolog. Wiley, second edition.

[Norvig, 2004] Peter Norvig (2004). Paradigms of Artificial Intelligence Programming: Case Studies in Common LISP. Morgan Kaufmann.

[Ockham, 1340] William of Ockham (ca. 1340). Ockham’s razor.

[Park et al., 2005] S. Park, F. Pfenning, and S. Thrun (2005). A Probabilistic Language Based upon Sampling Functions. In Proceedings of the 32nd ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, pages 171–182. Association for Computing Machinery, New York, NY.

[Pfeffer, 2001] Avi Pfeffer (2001). IBAL: A Probabilistic Rational Programming Language. In International Joint Conferences on Artificial Intelligence, pages 733–740.

[Pfeffer, 2007] Avi Pfeffer (2007). The Design and Implementation of IBAL: A General-Purpose Probabilistic Language. In An Introduction to Statistical Relational Learning, pages 399–432. MIT Press, Cambridge, MA.

[Piponi, 2006] Dan Piponi (2006). You Could Have Invented Monads! (And Maybe You Already Have.). Weblog post. http://sigfpe.blogspot.com/2006/08/you-could-have-invented-monads-and.html.

[Puget, 1998] J.F. Puget (1998). A Fast Algorithm for the Bound Consistency of Alldiff Constraints. In Proceedings of the National Conference on Artificial Intelligence, pages 359–366. John Wiley & Sons Ltd.

[Radul, 2007] Alexey Radul (2007). Report on the Probabilistic Language Scheme. In DLS ’07: Proceedings of the 2007 symposium on Dynamic languages, pages 2–10. Association for Computing Machinery, New York, NY. http://hdl.handle.net/1721.1/39831.

[Radul and Sussman, 2009a] Alexey Radul and Gerald Jay Sussman (2009a). The (Abridged) Art of the Propagator. In Proceedings of the 2009 International Lisp Conference, pages 41–56, Cambridge, MA. Association of Lisp Users, Sterling, Virginia, USA.

[Radul and Sussman, 2009b] Alexey Radul and Gerald Jay Sussman (2009b). The Art of the Propagator. CSAIL Tech Report MIT-CSAIL-TR-2009-002, MIT Computer Science and Artificial Intelligence Laboratory, Cambridge, MA. http://hdl.handle.net/1721.1/44215.

[Ramsey and Pfeffer, 2002] Norman Ramsey and Avi Pfeffer (2002). Stochastic Lambda Calculus and Monads of Probability Distributions. Proceedings of the 29th ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, pages 154–165.

[Royce, 1970] Winston W. Royce (1970). Managing the Development of Large Software Systems. In Proceedings, IEEE WESCON, pages 1–9.

[Russell and Norvig, 1995] Stuart J. Russell and Peter Norvig (1995). Artificial Intelligence: a Modern Approach. Prentice-Hall, Inc. Upper Saddle River, NJ, USA.

[Schulte and Stuckey, 2008] C. Schulte and P.J. Stuckey (2008). Efficient Constraint Propagation Engines. Transactions on Programming Languages and Systems, 31(1).

[Shivers, 1991] Olin Shivers (1991). Control-Flow Analysis of Higher-Order Languages. PhD thesis, Carnegie Mellon University, Pittsburgh, PA. CMU-CS-91-145.

[Shivers, 1999] Olin Shivers (1999). SRFI 1: List Library. Scheme Requests for Implementation. http://srfi.schemers.org/srfi-1/srfi-1.html.

[Siskind and McAllester, 1993] Jeffrey Mark Siskind and David Allen McAllester (1993). Screamer: A Portable Efficient Implementation of Nondeterministic Common Lisp. Technical report IRCS-93-03, University of Pennsylvania Institute for Research in Cognitive Science.

[Sitaram, 2004] Dorai Sitaram (2004). Teach Yourself Scheme in Fixnum Days. Online at http://www.ccs.neu.edu/home/dorai/ty-scheme/ty-scheme.html.

[Smolka, 1995] Gert Smolka (1995). The Oz Programming Model. Lecture Notes in Computer Science, 1000:324–343.

[Stallman and Sussman, 1977] Richard Matthew Stallman and Gerald Jay Sussman (1977). Forward Reasoning and Dependency-Directed Backtracking in a System for Computer-Aided Circuit Analysis. Artificial Intelligence, 9:135–196.

[Steele Jr., 1980] Guy L. Steele Jr. (1980). The Definition and Implementation of a Computer Programming Language Based on Constraints. AI Memo 595, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[Steele Jr. and Sussman, 1980] Guy L. Steele Jr. and Gerald Jay Sussman (1980). Constraints-A Language for Expressing Almost-Hierarchical Descriptions. Artificial Intelligence, 14(1):1–39.

[Strachey, 1967] Christopher Strachey (1967). Fundamental Concepts in Programming Languages. Lecture notes for International Summer School in Computer Programming.

[Strachey, 2000] Christopher Strachey (2000). Fundamental Concepts in Programming Languages. Higher-Order and Symbolic Computation, 13(1):11–49.

[Stroustrup et al., 1991] Bjarne Stroustrup et al. (1991). The C++ Programming Language. Addison-Wesley Reading, MA.

[Sulzmann and Stuckey, 2007] M. Sulzmann and P.J. Stuckey (2007). HM (X) Type Inference is CLP (X) Solving. Journal of Functional Programming, 18(02):251–283.

[Sussman et al., 2001] Gerald Jay Sussman, Jack Wisdom, and M.E. Mayer (2001). Structure and Interpretation of Classical Mechanics. MIT Press, Cambridge, MA.

[Sutherland, 1963] Ivan E. Sutherland (1963). SketchPad: A Man-Machine Graphical Communication System. PhD thesis, Massachusetts Institute of Technology, Cambridge, MA.

[Tack, 2009] Guido Tack (2009). Constraint Propagation - Models, Techniques, Implementation. PhD thesis, Saarland University, Germany.

[Van Hentenryck, 1989] Pascal Van Hentenryck (1989). Constraint Satisfaction in Logic Programming. MIT Press, Cambridge, MA.

[Van Roy, 2005] Peter Van Roy (2005). Multiparadigm Programming in Mozart/Oz. Springer.

[Van Roy and Haridi, 2004] Peter Van Roy and Seif Haridi (2004). Concepts, Techniques, and Models of Computer Programming. MIT Press, Cambridge, MA.

[Waltz, 1972] David L. Waltz (1972). Generating Semantic Description from Drawings of Scenes with Shadows. PhD thesis, Massachusetts Institute of Technology, Cambridge, MA.

[Waltz, 1975] David L. Waltz (1975). Understanding Line Drawings of Scenes with Shadows. The Psychology of Computer Vision, pages 19–91.

[Wan and Hudak, 2000] Z. Wan and Paul Hudak (2000). Functional Reactive Programming from First Principles. ACM SIGPLAN Notices, 35(5):242–252.

[Zabih, 1987] Ramin Zabih (1987). Dependency-Directed Backtracking in Non-Deterministic Scheme. Master’s thesis, Massachusetts Institute of Technology, Cambridge, MA.

[Zabih, 1998] Ramin Zabih (1998). Dependency-Directed Backtracking in Non-Deterministic Scheme. AI Memo 956, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[Zabih et al., 1987] Ramin Zabih, David Allen McAllester, and David Chapman (1987). Non-Deterministic Lisp with Dependency-Directed Backtracking. In Proceedings of AAAI 87, pages 59–64.

# 后记

## 想象运行

Imagine propagator networks run.

想象 propagator networks 运行起来的样子。
形成计算结果的方式与之前不同了。
尝试类比类似的过程。
类似构造蛋白质的过程？

再想象。

## back propagation 适合用 propagator network 来理解

用 propagator network 来实现 back propagation 将是很自然的事情，
在一般的 expression 语言中，需要非常复杂技巧才能完成的计算，
在 propagator network 中几乎是平凡的。

用 "The Little Learner" 中的技巧去实现 back propagation，
看起来还是非常难理解的。

但是如果画出 propagator network，
就容易理解多了。

可以说 propagator model 是一种理解计算的方式，
在其中，相邻的节点之间，可以沿任意方向，以任意的方式，传播信息，
而不是像 expression tree 中那样，只能沿着一个方向传播信息。

## inet v.s. pnet

- inet -- interaction nets
- pnet -- propagation nets
