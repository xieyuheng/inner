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
> problem’s _description_.

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

TODO

# 3 Core Implementation
# 4 Dependencies
# 5 Expressive Power
# 6 Towards a Programming Language
# 7 Philosophical Insights
