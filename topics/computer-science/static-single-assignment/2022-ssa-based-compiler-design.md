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

# Part I Vanilla SSA

## 1 Introduction (Jeremy Singer)

> In computer programming, as in real life, names are useful handles
> for concrete entities. The key message of this book is that having
> _unique names_ for _distinct entities_ reduces uncertainty and
> imprecision.

> This book is about the _Static Single Assignment form_ (SSA), which
> is a naming convention for storage locations (variables) in
> low-level representations of computer programs.

### 1.1 Definition of SSA

TODO

# Part II Analysis

# Part III Extensions

# Part IV Machine Code Generation and Optimization
