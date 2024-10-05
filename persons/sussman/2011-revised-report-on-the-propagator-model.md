---
title: Revised Report on the Propagator Model
authors: [Alexey Andreyevich Radul, Gerald Jay Sussman]
year: 2011
source: "https://groups.csail.mit.edu/mac/users/gjs/propagators"
---

# 学习目标

与 "Propagation Networks" 那篇论文相比，
这篇论文在处理 cell 和 cons 时有很多新的见解，
在之前很多设计决策还悬而未决，
这篇论文已经做出了选择。

"Cells are Data Too" 之类的章节，
延续了 Sussman 的研究风格，
比如在设计 scheme 之初，
很多设计决策都来自概念之间的融合与简化，这个指导思想。

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

cell 保存 propagator 相关的 partial information 之后，
是否可以直接用这种 partial information
来实现 `define-generic` 和 `define-handler` 的效果呢？
我想应该是可以的！

## Provided Primitives: p:foo and e:foo

除非设计纯粹的 propagator system，
其中没有正常意义上的函数，
所有的行为都由 propagator 完成；
否则总是要让 function 与 propagator 共存，
不同类型的东西共存有命名问题，
何况这两类东西都占用 function application syntax。

也许应该设计 pure propagator system，
因为 pure 意味着简单，没有很多相互掺杂的概念，
比如 syntax overload，
还有 `add` v.s. `adder` 的 naming 的问题。

假设我们 JS 来实现 pure propagator system，
所有的 primitive 都要用 JS 实现。
这在实现的时候确实很麻烦，
但是使用的时候简单，
而且可以帮助我们提炼出来合理的 primitive propagator 集合。

假设我们要实现 pure propagator 语言，
类型系统应该如何处理？
其实就是两层计算，是否可以直接用 partial information 来处理呢？
我想也是可以的！

```scheme
(p:foo input ... output)
```

> Attaches a propagator that does the foo job to the given input and
> output cells. p:abs, p:square, p:sqrt, p:not, p:pair?, and p:null?
> accept one input cell and one output cell. p:+, p:-, p:*, p:/, p:=,
> p:<, p:>, p:<=, p:>=, p:and, p:or, p:eq?, p:eqv?, p:atan2, and
> p:expt, accept two input cells and one output cell.

```scheme
(e:foo input ...)
```

> The e:foo equivalents of all the p:foo propagator constructors are
> all available and accept the same number of input cells (and make
> their own output cell).

```scheme
(p:id input output)
(e:id input)
```

> Attaches an identity propagator to the given cells. The identity
> propagator will continuously copy the contents of the input cell to
> the output cell.

```scheme
(p:== input ... output)
(e:== input ...)
```

> These are variadic versions of p:id. The result is a star topology,
> with every input feeding into the one output.

```scheme
(p:switch control input output)
(e:switch control input)
```

> Conditional propagation. The propagator made by switch copies its
> input to its output if and only if its control is "true". The
> presence of partial information (see Section 6) makes this
> interesting. For example, a #t contingent on some premise will cause
> switch to propagate, but the result written to the output will be
> contingent on that premise (in addition to any other premises the
> input may already be contingent on).

```scheme
(p:conditional control consequent alternate output)
(e:conditional control consequent alternate)
```

> Two-armed conditional propagation. May be defined by use of two
> switch propagators and a not propagator.

```scheme
(p:conditional-router control input consequent alternate)
(e:conditional-router control input consequent)
```

> Two-output-armed conditional propagation. This is symmetric with
> conditional; the consequent and alternate are possible output
> destinations.

"Propagation Networks" 论文中
`conditional-router` 叫 `conditional-writer`。

## Cells are Data Too

> Cells, and structures thereof, are perfectly good partial
> information (see Section 6) and are therefore perfectly legitimate
> contents of other cells. The event that two different cells A and B
> find themselves held in the same third cell C means that A and B are
> now known to contain information about the same thing. The two cells
> are therefore merged by attaching c:id propagators to them so as to
> keep their contents in sync in the future.

这些想法在 Propagation Networks 那篇论文中，
只是在 6.3.2 Carrying cells 的章节稍有提及。

```scheme
(p:deposit cell place-cell)
(e:deposit cell)
```

>  Grabs the given cell and deposits it into place-cell. The rule for
>  merging cells has the effect that the given cell will be identified
>  with any other cells that place-cell may come to hold.

```scheme
(p:examine place-cell cell)
(e:examine place-cell)
```

> Grabs the given cell and deposits it into place-cell. The rule for
> merging cells has the effect that the given cell will be identified
> with any other cells that place-cell may come to hold.
>
> In fact, p:deposit and p:examine are the same operation, except with
> the arguments reversed.
>
> The e:examine variant includes an optimization: if the place-cell
> already contains a cell, e:examine will just Scheme-return that cell
> instead of synthesizing a new one and identifying it with the cell
> present.

## Compound Data

```scheme
(p:cons car-cell cdr-cell output)
(e:cons car-cell cdr-cell)
```

> Constructs a propagator that collects the car-cell and the cdr-cell,
> makes a pair of them, and writes that pair into the output
> cell. This is like a binary p:deposit.

```scheme
(p:pair? input output)
(e:pair? input)
```

> Attaches a propaagtor that tests whether input cell contains a pair.

```scheme
(p:null? input output)
(e:null? input)
```

> Attaches a propaagtor that tests whether input cell contains the empty list.

```scheme
(p:car input output)
(e:car input)
```

> Makes a propagator that identifies the given output with the cell in
> the car of the pair in the given input. This is like a p:examine of
> that field. Note that using p:car on an input implies the
> expectation that said input contains a pair. That wish is treated as
> a command, and a pair appears. If fact, (p:car input output) is
> equivalent to (p:cons output nothing input).

```scheme
(p:cdr input output)
(e:cdr input)
```

> Same as p:car and e:car, except the other field of the pair.

> Scheme pairs created by p:cons and company are partial information
> structures, and they merge by recursively merging their
> corresponding fields. Together with the rule for merging cells, the
> emergent behavior is unification (with a merge delay instead of the
> occurs check).

这里的 merge delay 是什么意思？
也许是说构造 network 的时候，并不会发生 merge，
只有当 run network 的时候才会发生 merge，
而在 run 的过程中，如果发现两个 cell 已经保存了相同的 pair，
就不会再有进一步的 propagation。

## Propagator Constraints: c:foo and ce:foo

combined 应该可以被视作是 primitive，
但是这里却用了不同的名字前缀来表示 combined propagator。

可能是因为取名字的压力太大了。
也可能是因为要区分相似的但是方向不同的 propagator。
- 也许可以在类型中带上信息来区分 propagator 的方向。

## Constants and Literal Values

```scheme
(define-cell thing)
((constant 5) thing)
(content thing) ==> #(*the-nothing*)
(run)
(content thing) ==> 5
```

> There is also an expression-oriented version,
> called, naturally, e:constant:

```scheme
(define-cell thing (e:constant 5))
(run)
(content thing) ==> 5
```

## Constant Conversion

> In fact, inserting constants is so important, that there is one more
> nicification of this: whenever possible, the system will convert a
> raw constant (i.e. a non-cell Scheme object) into a cell, using
> e:constant.

> Some examples:

```scheme
(e:+ x 2)          ==   (e:+ x (e:constant 2))
(define-cell x 4)  ==   (define-cell x (e:constant 4))
(c:+ x y 0)        ==   (c:+ x y (e:constant 0))
```

这个 implicit conversion 未必是好事。

## Making Cells

使用 macro：

```scheme
(define-cell x)
```

而不是：

```scheme
(define x (make-cell))
```

就可以获得名字信息，达到类似下面的效果：

```scheme
(define x (make-cell :name 'x))
```

这种 `define-cell` 的设计，
就是为什么用 lisp/scheme 可以更快速地做一些语言设计实验。

> Just as Scheme has several mechanisms of making variables, so
> Scheme-Propagators has corresponding ones. Corresponding to Scheme's
> let, Scheme-Propagators has let-cells:

```scheme
(let-cells ((foo (e:+ x y))
            (bar (e:* x y)))
  ...)
```

> will create the Scheme bindings foo and bar, and bind them to the
> cells made by (e:+ x y) and (e:* x y), respectively (this code is
> only sensible if x and y are already bound to cells (or subject to
> constant conversion)). The new bindings will only be visible inside
> the scope of the let-cells, just like in Scheme; but if you attach
> propagators to them, the cells themselves will continue to exist and
> function as part of your propagator network.

> One notable difference from Scheme: a cell in a propagator network,
> unlike a variable in Scheme, has a perfectly good "initial
> state“. Every cell starts life knowing nothing about its intended
> contents; where Scheme variables have to start life in a weird
> "unassigned" state, nothing is a perfectly good partial information
> structure. This means that it's perfectly reasonable for let-cells
> to make cells with no initialization forms:

```scheme
(let-cells (x y (foo (some thing))) ...)
(let-cells ((x) (y) (foo (some thing))) ...)
```

> Moreover, since an "uninitialized" propagator cell can still start
> life in a perfectly sensible state, namely the state of containing
> nothing, let-cells-rec removes a restriction that Scheme's letrec
> enforced; namely, you may use the names defined by a given
> let-cells-rec directly in the defining forms, without any explicit
> intermediate delay in evaluation. For example:

```scheme
(let-cells-rec ((z (e:+ x y))
                (x (e:- z y))
                (y (e:- z x)))
  ...)
```

上面这个例子很有趣，
看起来递归和相互递归的处理都更简单了。

## Conditional Network Construction

> The switch propagator does conditional propagation -- it only
> forwards its input to its output if its control is "true". As such,
> it serves the purpose of controlling the flow of data through an
> existing propagator network. However, it is also appropriate to
> control the construction of more network, for example to design
> recursive networks that expand themselves no further than
> needed. The basic idea here is to delay the construction of some
> chunk of network until some information appears on its boundary, and
> control whether said information appears by judicious use of switch
> propagators. The low-level tools for accomplishing this effect are
> delayed-propagator-constructor and switch. The supported user
> interface is:

```scheme
(p:when internal-cells condition-cell body ...)
```

> Delays the construction of the body until sufficiently "true" (in
> the sense of switch) partial information appears in the
> condition-cell. The condition-cell argument is an expression to
> evaluate to produce the cell controlling whether construction of the
> body takes place. The body is an arbitrary collection of code,
> defining some amount of propagator network that will not be built
> until the controlling cell indicates that it should. The
> internal-cells argument is a list of the free variables in
> body. This is the same kind of kludge as the import clause in
> define-propagator (see Section 5.1).

```scheme
(p:if internal-cells condition-cell consequent alternate)
```

> Two-armed conditional construction. Just like a p:when and a
> p:unless: constructs the network indicated by the consequent form
> when the condition-cell becomes sufficiently "true", and constructs
> the network indicated by the alternate form when the condition-cell
> becomes sufficiently "false". Note that both can occur for the same
> p:if over the life of a single computation, for example if the
> condition-cell comes to have a TMS that includes a #t contingent on
> some premises and later a #f contingent on others.

# 5 Making New Compound Propagators

```scheme
(define-propagator (foo ...) ...)     defines  p:foo and e:foo
(define-propagator (p:foo ...) ...)   defines  p:foo and e:foo
(define-propagator (e:foo ...) ...)   defines  p:foo and e:foo
(define-propagator (c:foo ...) ...)   defines  c:foo and ce:foo
(define-propagator (ce:foo ...) ...)  defines  c:foo and ce:foo
```

## Lexical Scope

> Compound propagator definitions can be closed over cells available
> in their lexical environment:

```scheme
(define-e:propagator (addn n)
  (define-e:propagator (the-adder x)
    (import n)
    (e:+ n x))
  e:the-adder)
```

> import is a kludge, which is a consequence of the embedding of
> Scheme-Propagators into Scheme. Without enough access to the Scheme
> interpreter, or enough macrological wizardry, we cannot detect the
> free variables in an expression, so they must be listed explicitly
> by the user. Globally bound objects like e:+ (and p:addn and e:addn
> if the above were evaluated at the top level) need not be mentioned.

embedding 的坏处竟然是很难处理 lexical scope。

- 也可能是实现方式导致的，而不是 embedding 的限制，
  因为在我的 embedding 实现中，
  我想象不到为什么会需要这样处理 lexical scope，
  直接用 hosting language 的 closure 就可以了。

- 也许 Sussman 这么做是为了垃圾回收？

## Recursion

```scheme
(define-propagator (p:factorial n n!)
 (p:if (n n!) (e:= 0 n)
   (p:== 1 n!)
   (p:== (e:* n (e:factorial (e:- n 1))) n!)))
```

> p:if needs to be told the names of the non-global variables that are
> free in its branches, just like the import clause of a propagator
> definition (and for the same kludgerous reason).

```scheme
(define-e:propagator (e:factorial n)
  (e:if (n) (e:= 0 n)
    1
    (e:* n (e:factorial (e:- n 1)))))
```

# 6 Using Partial Information

> Partial, cumulative information is essential to multidirectional,
> non-sequential programming. Each "memory location" of
> Scheme-Propagators, that is each cell, maintains not "a value", but
> "all the information it has about a value". Such information may be
> as little as "I know absolutely nothing about my value", as much as
> "I know everything there is to know about my value, and it is 42",
> and many possible variations in between; and also one not-in-between
> variation, which is "Stop the presses! I know there is a
> contradiction!"

> The key thing about partial information is that it's cumulative.  So
> if you also added some other knowledge to a cell, it would need to
> merge with the content that's there to represent the cumulated
> knowledge.

> If incoming knowledge hopelessly contradicts the knowledge a cell
> already has, it will complain, and stop the network mid-stride, and
> give you a chance to examine the situation so you can debug the
> program that led to it, using the standard MIT Scheme debugging
> facilities.

> The partial information types are defined by a suite of Scheme
> procedures. The ones defining the actual partial information types
> are `equivalent?`, `merge`, and `contradictory?`, which test whether
> two information structures represent the same information, merge
> given information structures, and test whether a given information
> structure represents an impossible state, respectively. Each partial
> information structure also defines the way various propagators treat
> it. The behavior in the control position of a `switch` propagator
> and in the operator position of an apply propagator are particularly
> important.

# 7 Built-in Partial Information Structures

The following partial information structures are provided with
Scheme-Propagators:

- nothing
- just a value
- intervals
- propagator cells
- compound data
- closures
- supported values
  - 和 TMS 一起介绍，
    并且已经改名为了 contingent values。
- truth maintenance systems
- contradiction

从 Propagation Networks 论文实现到这里，我还差：

- propagator cells
- compound data
- closures

## Nothing

```scheme
nothing
```

> A single Scheme object that represents the complete absence of information.

```scheme
(nothing? thing)
```

> A predicate that tests whether a given Scheme object is the nothing object.

> nothing is equivalent? only to itself.

> nothing never contributes anything to a merge -- the merge of
> anything with nothing is the anything.

> nothing is not contradictory?.

> Strict propagators, such as ones made by p:+, output nothing if any
> of their inputs are nothing.

> A switch whose control cell contains nothing will emit nothing.

> An apply propagator whose operator cell contains nothing will not do
> anything.

这里的说明，其实描述了一个类型的 partial information
所需要满足的 interface，只不过这里 interface
是以 generic function 的方式实现的。

我们可以在结尾以 OOP interface 的形式总结这里所描述的 interface。

## Just a Value

> A Scheme object that is not otherwise defined as a partial
> information structure indicates that the content of the cell is
> completely known, and is exactly (by eqv?) that object. Note:
> floating point numbers are compared by approximate numerical
> equality; this is guaranteed to screw you eventually, but we don't
> know how to do better.

> Raw Scheme objects are equivalent? if they are eqv? (or are
> approximately equal floating point numbers).

> Non-equivalent? raw Scheme objects merge into the contradiction
> object.

> A raw Scheme object is never contradictory?.

> A switch interprets any non-#f raw Scheme object in its control cell
> as true and forwards its input cell to its output cell unmodified. A
> switch whose control cell is #f emits nothing to its output cell.

> An apply propagator whose operator cell contains a raw Scheme
> procedure will apply it to the boundary cells. It is an error for a
> raw Scheme object which is not a Scheme procedure to flow into the
> operator cell of an apply propagator.

## Numerical Intervals

> An object of type interval? has fields for a lower bound and an
> upper bound. Such an object represents the information "This value
> is between these bounds".

```scheme
(make-interval low high)
```

> Creates an interval with the given lower and upper bounds

```scheme
(interval-low interval)
```

> Extracts the lower bound of an interval

```scheme
(interval-high interval)
```

> Extracts the upper bound of an interval

```scheme
(interval? thing)
```

> Tests whether the given object is an interval

> Two interval objects are equivalent? if they are the same
> interval. An interval is equivalent? to a number if both the upper
> and lower bounds are that number.

> Arithmetic can be performed on intervals. They can be compared, and
> the comparison predicates will have a truth value only when no
> future shrinkage of the intervals can change that value. For
> example, (e:< int1 int2) will be true only if (e:< (interval-high
> int1) (interval-low int2)); it will be false only if (e:>=
> (interval-low int1) (interval-high int2)); otherwise the result of
> the comparison is nothing.

> Interval objects merge with each other by intersection. Interval
> object merge with numbers by treating the number as a degenerate
> interval and performing intersection (whose result will either be
> that number or an empty interval). Interval objects merge with other
> raw Scheme objects into the contradiction object.

> An interval object is contradictory? if and only if it represents a
> strictly empty interval (that is, if the upper bound is strictly
> less than the lower bound).

> The arithmetic propagators react to interval objects by performing
> interval arithmetic.

> A switch propagator treats any interval object in its control as a
> non-#f object and forwards its input to its output.

> It is an error for an interval object to appear in the operator
> position of an apply propagator.

> As an interval arithmetic facility, this one is very primitive. It
> cannot extract new information from division by an interval that
> contains zero, because that would require intervals around the point
> at infinity. The main purpose of including intervals is to have a
> partial information structure with an intuitive meaning, and that
> requires nontrivial operations on the information it is over.

## Propagator Cells as Partial Information

> A propagator cell interpreted as partial information is an
> indirection: it means ``I contain the structure that describes this
> value''. Cells can appear as the contents of cells or other
> structures via the deposit and examine propagators (see Section
> 4.5).

> Propagator cells are equivalent? if they are known to contain
> information about the same subject. This occurs only if they are
> identically the same cell, or if they have previously been
> unconditionally identified (by merging).

> Propagator cells merge with each other by attaching bidirectional
> identity propagators that keep the contents of the cells in
> sync. These identity propagators will cause the contents of the
> cells to merge, both now and in the future.

> A propagator cell is never contradictory?.

## Compound Data

TODO

## Closures

TODO

## Truth Maintenance Systems

> A Truth Maintenance System (TMS) is a set of contingent values. A
> contingent value is any partial information object that describes
> the "value" in the cell, together with a set of premises. The
> premises are Scheme objects that have no interesting properties
> except identity (by eq?). A worldview defines which premises are
> believed.

之前的 supported value 在这里被称为 contingent value。

> The meaning of a TMS as information is the logical **and** of the
> meanings of all of its contingent values. The meaning of each
> contingent value is an implication: The conjunction of the premises
> implies the contingent information. Therefore, given a worldview,
> some of the contingent information is believed and some is not. If
> the TMS is queried, it produces the best summary it can of the
> believed information, together with the full set of premises that
> information is contingent upon.

注意是 logical and 而不是 or！

TODO

> TMSes are equivalent? if they contain equivalent contingent
> objects. Contingent objects are equivalent if they have equivalent
> info and the same set of premises.

> TMSes merge by appending their lists of known contingencies (and
> sweeping out redundant ones).

上面的描述很简单，但是对 redundant 的定义，
涉及到 contingent objects 之间的序关系，
其实是比较复杂的。

> Strict propagators react to TMSes by querying them to obtain
> ingredients for computation. The result of a computation is
> contingent on the premises of the ingredients that contribute to
> that result.

上面尝试同时描述 contingent object 和 TMS 的 monad 的行为。

> If the input itself is a TMS, switch queries it and (possibly)
> forwards the result of the query, rather than forwarding the entire
> TMS. For example:

```scheme
(define-cell frob (make-tms (contingent 4 '(bill))))
(define-cell maybe-frob (e:switch (make-tms (contingent #t '(fred))) frob))
(run)
(tms-query (content maybe-frob))  ==>  #(contingent 4 (bill fred))
```

虽然不是 forwarding the entire TMS，
但是 maybe-frob 的 content 还是一个 TMS。

> If a TMS appears in the operator cell of an apply propagator, the
> apply propagator will query the TMS. If the result of the query is a
> contingent propagator constructor, the apply propagator will execute
> that constructor in a sandbox that ensures that the premises on
> which the constructor was contingent are both forwarded to the
> constructed propagator's inputs and attached to the constructed
> propagator's outputs. For example, suppose Bill wanted us to add 3
> to 4:

```scheme
(define-cell operation)
(define-cell answer)
(p:switch (make-tms (contingent #t '(bill))) p:+ operation)
(d@ operation 3 4 answer)
(run)
(tms-query (content answer))  ==>  #(contingent 7 (bill))
```

> The answer cell contains a 7 contingent on the Bill premise. This is
> the right thing, because that answer depends not only on the inputs
> to the operation being performed, but also on the identity of the
> operation itself.

## Contradiction

> The Scheme object the-contradiction represents a completely
> contradictory state of information. If a cell ever finds itself in
> such a contradictory state, it will signal an error. The explicit
> the-contradiction object is useful, however, for representing
> contradictory information in recursive contexts. For example, a
> truth maintenance system may discover that some collection of
> premises leads to a contradiction -- this is represented by a
> the-contradiction object contingent on those premises.

```scheme
the-contradiction
```

> A Scheme object representing a contradictory state of information
> with no further structure.

> the-contradiction is equivalent? only to itself.

> Any information state merges with the-contradiction to produce
> the-contradiction.

> the-contradiction is contradictory?.

带有 the-contradiction 的 contingent object 应该也是 contradictory?
下一章的 implicit dependency-directed search
就是利用这种信息来增加 nogood set 的。

> Propagators cannot operate on the-contradiction because any cell
> containing it will signal an error before any such propagator might
> run.

## Implicit Dependency-Directed Search

> If a cell discovers that it contains a TMS that harbors a contingent
> contradiction, the cell will signal that the premises of that
> contradiction form a nogood set, and that nogood set will be
> recorded. For the worldview to be consistent, at least one of those
> premises must be removed. The system maintains the invariant that
> the current worldview never has a subset which is a known nogood.

```scheme
(p:amb cell)
(e:amb)
```

> A propagator that emits a TMS consisting of a pair of contingencies.
> One contains the information #t contingent on one fresh hypothetical
> premise, and the other contains the information #f contingent on
> anther.  amb also tries to maintain the invariant that exactly one
> of those premises is believed. If doing so does not cause the
> current worldview to believe a known nogood set, amb can just
> bring-in! one premise or the other. If the current worldview is such
> that bringing either premise in will cause a known nogood set to be
> believed, then, by performing a cut, the amb discovers and signals a
> new nogood set that does not include either of them. Together with
> the reaction of the system to nogood sets, this induces an emergent
> satisfiability solver by the resolution principle.

只是看上面的描述我还是不能理解 amb 这种 propagator，
由于和全局变量相关，所以 TMS 和 amb 都很复杂。
要实际实现出来才能理解。

```scheme
(p:require cell)
(e:require)
```

> A propagator that requires its given cell to be true (that is to
> say, signals contradictions if it is not).

```scheme
(p:forbid cell)
(e:forbid)
```

> A propagator that forbids its given cell from being true (that is to
> say, signals contradictions if it is).

如何实现这里提到的 signals contradictions？
可能 cell 的行为（为了找到 nogood），
当 cell 发现自己 content 带有 the-contradiction + reasons 时，
就把 reasons 记录成 nogood set。

- 如果是 cell 的行为，
  就可以通过一个带有 local cell 的 propagator
  来实现 signals contradiction。

```scheme
(p:one-of input ... output)
(e:one-of input ...)
```

> An n-ary version of amb. Picks one of the objects in the given input
> cells using an appropriate collection of amb and switch propagators
> and puts it into its output cell.

amb 其实是为了实现这个 one-of，
而 one-of 在于用 propagation 来实现搜索功能。
这里 output 就是搜索中对 input cells 的选择。

```scheme
(require-distinct cells)
```

> Requires all of the objects in its list of input cells to be
> distinct (in the sense of eqv?)

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

> We started with the goal of making it easier for people to build
> systems that are additive. A system should not become so
> interdependent that it is difficult to extend its behavior to
> accommodate new requirements. Small changes to the behavior should
> entail only small changes to the implementation. These are tough
> goals to achieve.

> Systems built on the Propagator Model of computation can approach
> some of these goals.

> A key idea is to allow fan-in, merging partial results. A result may
> be computed in multiple ways, by complementary processes. There may
> be multiple ways to partially contribute to a result; these
> contributions are merged to make better approximations to the
> desired result. Partial results can be used as a base for further
> computations, which may further refine known values or partially
> determine new ones. So we can make effective use of methods that
> give only part of an answer, depending on other methods to fill in
> missing details. This ability to absorb redundant and partial
> computations contributes to additivity: it is easy to add new
> propagators that implement additional ways to compute any part of
> the information about a value in a cell.

> Dependency tracking and truth maintenance contribute to additivity
> in a different way. If you want to add a new pile of stuff, you
> don't need to worry too much about whether or not it will be
> compatible with the old: just make it contingent on a fresh
> premise. If the addition turns out to conflict with what was already
> there, it (or the offending old thing) can be ignored, locally and
> dynamically, by retracting a premise.

> A propagator program is analogous to an electrical circuit diagram,
> whereas a program written for a more traditional model is more like
> a system diagram: the intellectual viewpoint of the Propagator Model
> of computation is not the composition of functions, as in
> traditional models, but is rather the construction of
> mechanisms. The difference is profound:
>
> 1. circuit models are multidirectional; system diagrams compute from
>    inputs to outputs.
>
> 2. circuit models are abstractions of physics; system diagrams are
>    abstractions of process.

> The circuit diagram viewpoint gives us powerful ways to think. We
> can modify a circuit diagram by clipping out a part, or by
> installing a different version. We can temporarily carry information
> from one place to another using a clip lead. We have lots of places
> to connect various kinds of meters for monitoring and debugging.

> Now for something completely different! Scheme-Propagators is built
> with dynamically-extensible generic operations. The pervasive merge
> operation, as well as all the primitive propagators, are generic,
> and this makes it easier for us to add new forms of partial
> information. Adding new forms of partial information is a way to
> extend the capabilities of the propagation infrastructure to novel
> circumstances. With appropriate foresight, new partial information
> structures can interoperate with old, and additively extend the
> capabilities of existing systems -- a way to teach old dogs new
> tricks. Of course, such power is dangerous: if a network that
> depends on commutativity of multiplication of numbers meets an
> extension to square matrices, we will get wrong answers. But then,
> cross-checking across complementary methods, together with
> dependency tracking, simplifies the task of debugging such errors.

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
