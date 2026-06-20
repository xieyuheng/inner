---
title: SSA-based compiler design
year: 2022
---

# My Motive

[2026-02-05] 为 x-lisp 重新设计了一个，
专门用于动态类型语言的中间语言 -- basic-lisp。
其设计大部分来自 EOC 和 SICP。
为了了解以 basic-lisp 为基础做优化的可能，
需要学习一下 SSA。

[2026-06-19] x-lisp 已经改名为 meta-lisp，改为了Hindley-Milner 类型系统。
并且语言设计相对完善了，目前编译到 stack based 虚拟机。
在实现编译器 self-hosting 的过程中，发现速度太慢了，现在要编译到 native x86。

因此现在需要重新设计 basic-lisp，增加类型系统，
并且同时支持动态类型语言和静态类型语言的编译。

现在我知道了，SSA 和 propagator model 基本上是同样的东西，
只不过 SSA 中 side-effect 还是 implicit 的，
需要被完全处理为 explicit（比如 LLVM 的 MemorySSA -- MemoryDef 和 MemoryUse）
才能被完全视为是 propagator network。

另外一个区别是 Sussman 在实现 propagator model 时，
大量使用了 generic function，
即同一个函数通过 generic dispatch 来处理不同类型的 cell value。
而在 SSA 中处理 propagator network 时使用类型参数，
即每次所有 cell 保存同类型的 value，
propagator network 中的 propagator 原本只是 symbol，
需要被重新解释为这些 value 的处理函数。

这次学习的过程中，可以主要思考使用 propagator model 实现 SSA 的方式。

# Foreword (Kenneth Zadeck, Mark N. Wegman)

> Our initial motivation for SSA form was to build a sparse
> representation that avoided the cost of proving every fact at every
> location. Our primary idea was to encode the data and control
> dependencies into a naming scheme for the variables.  We chose to
> solve the extended form of the constant propagation problem which
> had been well studied by Wegbreit. We presented the algorithm at the
> 12th POPL conference. The algorithm was much more efficient than
> Wegbreit’s version, but it also had one very interesting and unique
> property that was not appreciated at the time: the transformation
> did not adversely effect the correctness of the representation.

> The first work was incomplete. It didn’t cover the semantics of
> many common storage classes, and the construction algorithm was not
> well thought through. But the surprising lesson was that by
> constructing a uniform representation in which both transformations
> and analysis could be represented, we could simplify and improve the
> efficiency of both.

> Over the next few years, we teamed up with others within IBM to
> develop not only an efficient method to enter SSA form but also a
> suite of techniques that each used SSA form and left the
> representation intact when finished. These techniques included dead
> code elimination, value numbering, and invariant code motion.

> By today’s standards, our original work was not really that useful:
> there were only a handful of techniques and the SSA form only worked
> for unaliased variables.  But that original work defined the
> framework for how programming language transformation was to be
> performed. This book is an expression of how far SSA form has come
> and how far it needs to go to fully displace the older techniques
> with more efficient and powerful replacements. We are grateful and
> humbled that our work has led to such a powerful set of directions
> by others.

# [2026-06-20] SSA 在于优化 propagator network

注意 SSA 和 propagator network 的差异在于，
SSA 在于利用 propagator network 对某些信息的 propagation 结果，
来优化 propagator network，
把 propagator network 转化为在某种意义上等价的更优的 propagator network。

这看来是比 propagator network 本身更高一级别的视角。
除非我们去研究 propagator network 在运行的时候，
如何利用 propagation 的结果来优化自身。
