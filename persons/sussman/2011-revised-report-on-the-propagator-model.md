---
title: Revised Report on the Propagator Model
authors: [Alexey Andreyevich Radul, Gerald Jay Sussman]
year: 2011
source: "https://groups.csail.mit.edu/mac/users/gjs/propagators"
---

# Abstract

> In the past year we have made serious progress on elaborating the
> propagator programming model [^2][^3]. Things have gotten serious
> enough to build a system that can be used for real experiments.

> The most important problem facing a programmer is the revision of an
> existing program to extend it for some new situation. Unfortunately,
> the traditional models of programming provide little support for
> this activity. The programmer often finds that commitments made in
> the existing code impede the extension, but the costs of reversing
> those commitments are excessive.

这篇论文可以看作是 "Software Design for Flexibility" 的前兆了。

> Such commitments tend to take the form of choices of strategy. In
> the design of any significant system there are many implementation
> plans proposed for every component at every level of
> detail. However, in the system that is finally delivered this
> diversity of plans is lost and usually only one unified plan is
> adopted and implemented. As in an ecological system, the loss of
> diversity in the traditional engineering process has serious
> consequences.

> The Propagator Programming Model is an attempt to mitigate this
> problem. It is a model that supports the expression and integration
> of multiple viewpoints on a design. It incorporates explicit
> structure to support the integration of redundant pieces and
> subsystems that solve problems in several different ways. It will
> help us integrate the diversity that was inherent in the design
> process into the delivered operational product.

> The Propagator Programming Model is built on the idea that the basic
> computational elements are autonomous machines interconnected by
> shared cells through which they communicate. Each machine
> continuously examines the cells it is interested in, and adds
> information to some based on computations it can make from
> information from the others. Cells accumulate information from the
> propagators that produce that information. The key idea here is
> additivity. New ways to make contributions can be added just by
> adding new propagators; if an approach to a problem doesn't turn out
> to work well, it can be identified by its premises and ignored,
> dynamically and without disruption.

> This work was supported in part by the MIT Mind Machine Project.

这篇论文的 abstract，
对「propagator model 有什么用？」这个问题，
给出了新的软件工程方面的答案。

先知道了一个新计算的存在，
然后慢慢发现应用领域
（毕竟项目的时间跨越了 30 多年）。

可以感到，作者在为 propagator model 寻找应用场景。
我认为基于 propagator model 的类型检查器就是很有趣的应用场景之一，
因为每种逻辑就对应一种类型系统，因此有各种各样的类型检查器有待开发。
Sussman 也曾经说过，类型检查不过是 constraint solving 而已。

# 1 Propagator System

> Although most of this document introduces you to the
> Scheme-Propagator system that we have developed in MIT Scheme, the
> Propagator Model is really independent of the language. You should
> be able to write propagators in any language you choose, and others
> should be able to write subsystems in their favorite language that
> cooperate with your subsystems. What is necessary is that all users
> agree on the protocol by which propagators communicate with the
> cells that are shared among subsystems. These rules are very simple
> and we can enumerate them right here:

> Cells must support three operations:
>
> - add some content
> - collect the content currently accumulated
> - register a propagator to be notified
>   when the accumulated content changes

> When new content is added to a cell, the cell must merge the
> addition with the content already present. When a propagator asks
> for the content of a cell, the cell must deliver a complete summary
> of the information that has been added to it.

> The merging of content must be commutative, associative, and
> idempotent. The behavior of propagators must be monotonic with
> respect to the lattice induced by the merge operation.

# 2 Getting Started

```scheme
(define-cell a)
(define-cell b)
(add-content a 3)
(add-content b 2)
(define-cell answer (e:+ a b))
(run)
(content answer) ==> 5
```

```typescript
const a = Cell()
const b = Cell()
patch(a, 3)
patch(b, 2)
const answer = adder(a, b)
await run()
answer.content // ==> 5
```

# 3 The Details

> Now that you know how to play around with our propagators we have to
> tell you what we actually provide. In every coherent system for
> building stuff there are primitive parts, the means by which they
> can be combined, and means by which combinations can be abstracted
> so that they can be named and treated as if they are primitive.

# 4 Making Propagator Networks

> The two basic operations when making a propagator network are making
> cells and attaching propagators to cells.

为了简化语法，"attaching propagators to cells" 可以用函数作用来表示。

## Attaching Basic Propagators: d@

> The name d@ is mnemonic for "diagram apply".

```scheme
(d@ propagator boundary-cell ...)
```

> Attaches a propagator to the given boundary cells. By convention,
> cells used as outputs go last. As a Scheme procedure, d@ does not
> return a useful value.

例子：

```scheme
(d@ p:+ foo bar baz)
```

> As in Scheme, p:+ is actually the name of a cell that contains a
> propagator constructor for attaching propagators that do
> addition. The first argument to d@ can be any cell that contains any
> desired partial information (see Section 6) about a propagator
> constructor. Actual attachment of propagators will occur as the
> propagator constructor becomes sufficiently well constrained.

p:+ 不是 propagator constructor，
而是一个保存了 propagator constructor 的 cell。
这里与 "Propagation Networks" 论文中的实现方式不一样了。

## Propagator Expressions: e@

> The d@ style is the right underlying way to think about the
> construction of propagator networks. However, it has the unfortunate
> feature that it requires the naming of cells for holding all
> intermediate values in a computation, and in that sense programming
> with d@ feels a lot like writing assembly language.

这也与逻辑式编程中使用的 logic variables 相似。

> The Scheme procedure e@ attaches propagators in expression
> style. The name e@ is mnemonic for "expression apply". The e@
> procedure is just like d@, except it synthesizes an extra cell to
> serve as the last argument to d@, and returns it from the e@
> expression (whereas the return value of d@ is unspecified).

在我的 JS 实现中，我直接重载了函数作用，
让函数作用可以同时表示这里的 d@ 和 e@，
只要不是像 e@ 一样只是为最后一个参数 synthesizes 一个 cell 作为返回值，
而是对所有没有提供的参数，
都 synthesizes 一个 cell 出来作为返回值（可能要返回一个 list）。

```scheme
(e@ propagator boundary-cell ...)
```

> Attaches the given propagator to a boundary consisting of the given
> boundary cells augmented with an additional, synthesized cell. The
> synthesized cell goes last, because that is the conventional
> position for an output cell. Returns the synthesized cell as the
> Scheme return value of e@.

> For example, here are two ways to do the same thing:

```scheme
(define-cell x)
(define-cell y)
(define-cell z)
(d@ p:* x y z)

(define-cell x)
(define-cell y)
(define-cell z (e@ p:* x y))
```

> Generally the e@ style is convenient because it chains in the familiar way

```scheme
(e@ p:- w (e@ p:* (e@ p:+ x y) z))
```

在我们直接重载函数作用的实现中，可以直接做到：

```scheme
(subtractor w (multiplier (adder x y) z))
```

用 p: 做前缀可以避免去为和函数相关的 propagator 取新的名字。

```scheme
(p:- w (p:* (p:+ x y) z))
```

Sussman 的设计也重载了函数作用，
但是还是用 p: 和 e: 前缀，
区分了参数充足和参数不足（差一个参数）的情况。

> To save typing when the propagator being attached is known at
> network construction time, the p:foo objects are also themselves
> applicable in Scheme, defaulting to applying themselves in the d@
> style. Each also has an e:foo variant that defaults to the e@
> style. So the following also works:

```scheme
(define-cell x)
(define-cell y)
(define-cell z (e@ p:* x y))
(d@ p:/ z x y)
(d@ p:/ z y x)
```

> The following also works:

```scheme
(define-cell x)
(define-cell y)
(define-cell z (e:* x y))
(p:/ z x y)
(p:/ z y x)
```

## Late Binding of Application

propagator is first class citizen，
这意味着 propagator 也可以保存在 cell 中
（可能需要定义相关的 partial information）。

成为了 first class citizen 之后，
就可能有 high order propagator，
diagram apply -- d@ 就是最典型的 high order propagator。

```scheme
(define-cell operation)
(define-cell answer)
(d@ operation 3 4 answer)
(run)
(content answer)  ==>  nothing
```

可以在 diagram apply 构造好 propagator 之后，
再向 operation cell 中添加 propagator。

```scheme
(p:id p:* operation)
(run)
(content answer)  ==>  12
```

为什么要用 `(p:id p:* operation)`
而不是直接 `(add-content (content p:*) operation)`，
因为前者是 reactive 的，
而后者会导致 p:* 的 content 更新之后，
operation 的 content 不会跟着更新。

`(operation 3 4 answer)`
也可以作为 `(d@ operation 3 4 answer)` 的缩写。
就是对于 cell 也可以 overload function application syntax。

## Provided Primitives: p:foo and e:foo
## Cells are Data Too
## Compound Data
## Propagator Constraints: c:foo and ce:foo
## Constants and Literal Values
## Constant Conversion
## Making Cells
## Conditional Network Construction

# 5 Making New Compound Propagators

## Lexical Scope
## Recursion

# 6 Using Partial Information

# 7 Built-in Partial Information Structures

## Nothing
## Just a Value
## Numerical Intervals
## Propagator Cells as Partial Information
## Compound Data
## Closures
## Truth Maintenance Systems
## Contradiction
## Implicit Dependency-Directed Search

# 8 Making New Kinds of Partial Information

## An Example: Adding Interval Arithmetic
## Generic Coercions
## The Partial Information Generics
## Individual Propagator Generics
## Uniform Applicative Extension of Propagators
## Interoperation with Existing Partial Information Types

# 9 Making New Primitive Propagators

## Direct Construction from Functions
## Propagatify
## Compound Cell Carrier Construction
## Fully-manual Low-level Propagator Construction

# 10 Debugging

# 11 Miscellany

## Macrology
## Reboots
## Compiling
## Scmutils
## Editing
## Hacking
## Arbitrary Choices

# 12 How this supports the goal

# Bibliography

[1]: Applicative programming with effects

- Conor McBride and Ross Paterson.
- Journal of Functional Programming, 18(1):1-13.
- 2008.

[2]: Propagation Networks: A Flexible and Expressive Substrate for Computation

- Alexey Radul.
- PhD thesis, Massachusetts Institute of Technology,
  Cambridge, MA.
- September 2009.
- http://hdl.handle.net/1721.1/49525

[3]: The Art of the Propagator

- Alexey Radul and Gerald Jay Sussman.
- CSAIL Tech Report MIT-CSAIL-TR-2009-002,
  MIT Computer Science and Artificial Intelligence Laboratory,
  Cambridge, MA.
- 2009.
- http://hdl.handle.net/1721.1/44215
