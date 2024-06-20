---
title: The Art of the Propagator
authors: [Alexey Andreyevich Radul, Gerald Jay Sussman]
year: 2009
---

# 目标

目标：实现 Propagator。

- 方案 A：

  设计一个嵌入在 JS 中的 Propagator 实现，来做实验。

- 方案 B：

  为 Propagator 设计一个拥有具体语法的语言，
  为了流行可以用类似 JS 语法，
  为了 Sussman 和 Dan 可以用 Scheme 语法。

1979 年的论文也设计了一版 propagator 语言，
分析这篇四十年后的论文中的语言的演化方向也是有趣的。

# 结论

这个世界上绝对应该有一本 "The Little Propagator"。
最好是 Dan 和 Sussman 一起写的。

# 1 Introduction

> We use the idea of a propagator network as a computational metaphor
> for exploring the consequences of allowing places to accept information
> from multiple sources. We study the design choices available within this
> metaphor and their consequences for the resulting programming system.

# 2 Propagators

> Our computational model is a network of autonomous machines, each
> continuously examining its inputs and producing outputs when possible.
> The inputs and outputs of a machine are shared with other machines so
> that the outputs of one machine can be used as the inputs for another.
> The shared communication mechanism is called a _cell_ and the machines
> that they interconnect are called _propagators_. As a consequence of this
> viewpoint computational mechanisms are naturally seen as wiring diagrams.

简单的例子，用海伦迭代的方式求平方根。

```
h = (g + x/g) / 2
```

`g` 是猜测的 `x` 的平方根，
`h` 是下一次迭代后更精确的 `x` 的平方根。

> We can describe such a network in a conventional programming
> language (Scheme) by the following ugly code:

```scheme
(define (heron-step x g h)
  (compound-propagator
   (list x g)
   (lambda ()
     (let ((x/g (make-cell))
           (g+x/g (make-cell))
           (two (make-cell)))
       (divider x g x/g)
       (adder g x/g g+x/g)
       ((constant 2) two)
       (divider g+x/g two h)))))
```

作者提到了 ugly code，
也就是认为嵌入在 Scheme 中的 propagator 的语法是不够好的。

尝试设计类 Scheme 的具体语法：

```scheme
(define (heron-step x g h)
  (div (add g (div x g)) 2 h))

;; ugly 的版本也是可以兼容的：

(define (heron-step x g h)
  (let ((x/g (div x y))
        (g+x/g (add g x/g)))
    (div g+x/g 2 h)))
```

> To use this network we need to make cells to pass in to this network
> constructor; we need to give some cells values, and we need to access the
> values in other cells:

```scheme
(define x (make-cell))
(define guess (make-cell))
(define better-guess (make-cell))

(heron-step x guess better-guess)

(add-content x 2)
(add-content guess 1.4)

(content better-guess)
; 1.4142857142857141
```

> Things get considerably more complicated when we want to iterate the
> improvement to get a better answer. The propagator wiring diagram is
> more analogous to assembly language than the expression languages that
> we are used to.

可见 assembly language 的特点正是 assembly，比如构造 graph。
并且在 inet 的设计中，我们其实找到了一种，
利用 currying 和多返回值，把 expression language
解释为 graph assembly language 的方案。

> What is interesting is the mechanism of iteration (or recursion)
> that we use.  A propagator does something only if it has inputs
> worth working on.  A `compound-propagator` builds its body when
> presented with values in all of the cells declared as its inputs.
> The `sqrt-iter` propagator (see Figure 1) uses switches to connect
> inputs to the `heron-step` and the recursive call only if the
> `good-enuf?` test is not satisfied. This is not the only way to do
> recursion, but it is a good start.

用 `compound-propagator` 来实现递归的 propagator，
是一种 call-by-need，即在运行时动态构造出更大的 propagator network，
此时 network 的规模和递归的次数成正比，这显然是不可取方案。
因为与递归函数在栈中的展开不同，
函数可以通过 return 来回收栈的空间，
但是动态构造的 propagator network 并不会被回收。

- 这也许是 "Propagation Networks" 这篇论文，
  中不再以 heron 迭代为例子的原因。

正确的方案需要引入 lattice，
因为对于这里的逼近问题，
可以用 `good-enuf?` 来定义一个 lattice。

先描述一个假想的语言，
然后跟一个 "Making this work"
来实现这个语言，
这种格式也是我写 inet 文档时候的格式，
哈哈。

"Making this work" 放在前面，
有点类似 TDD 中先用测试来设计 API，
然后再实现。

# 3 Partial Information

以 interval 为例子，来介绍非平凡的 lattice 数据类型。

# 4 Multidirectional Computation

这里描述的多方向计算与 logic programming 中的反向计算不同，
这里并不能处理 relation 的多值性，
而是只有当反方向的计算也形成具有单值性的函数时才能进行。

```scheme
(define (product x y total)
  (multiplier x y total)
  (divider total x y)
  (divider total y x))

(define (quadratic x x^2)
  (squarer x x^2)
  (sqrter x^2 x))
```

这里可以思考一下 lattice 的表示论，
即用集合来表示 lattice。
因为 constraint processing 中有 domain 的概念，
一个变量的值是一个 domain，即一个集合。

这里也可以结合 formal concept analysis 的理论，
把集合单纯的 objects 推广成 objects + attributes。

# 5 Generic Operations

用对函数的所有参数的类型和值的 dispatching 来实现 generic operations。

# 6 Dependencies

记录依赖关系是为了处理 beliefs 之间的冲突。

> If we observe inconsistencies we do not crash -- we chuckle!

## 6.1 Dependencies for Provenance

> We now illustrate these ideas, and the manner in which they fit into
> the propagator network framework, by building a sequence of sample
> dependency tracking systems of increasing complexity. We start with
> a relatively simple system that only tracks and reports the
> provenance of its data.

和 "Making this work" 一样，
注意这里的教学法，或者说展示法。

> We now illustrate these ideas，
> by building a sequence of X
> of increasing complexity.

> How do we want our provenance system to work? We can make cells
> and define networks as usual, but if we add supported values as inputs,
> we get supported values as outputs.

也就是说，我们用 generic operations 的方式实现 dependency。
具体就是将 operations 扩展到 supported value 上。

所谓 supported value 就是给每个 value
加上一个 label 的集合，用来简单地（非递归地）标记 value 的来源。

## 6.2 Dependencies for Alternate Worldviews

> The anomaly shown above was a consequence of the loss of information
> about the derivation of a value due to its sequential accumulation.
> We can begin to generalize this by allowing a cell to hold more than
> one value at a time, each justified by its own justification.

是否可以直接用这种方式实现 Horn clause 中的「或」？
毕竟逻辑式编程中的 substitution 只是特殊的 constraint，
那么逻辑式编程中的 goal 也只是特殊的 propagator 吗？

这里描述的「未能获得最精确 dependencies」问题，
是通过更进一步的用户交互来解决的，
是否有更好的解法？

## 6.3 Dependencies for Implicit Search

> Implicit generate and test can be viewed as a way of making systems
> that are modular and independently evolvable.

Sussman 说过，设计语言就在于 "making something implicit"。

> Of course, this gets us into potentially deadly exponential searches
> through all possible assignments to all the choices that have been
> made in the program. As usual, modular flexibility can be dangerous.

比如说，如果真的用生成语法的规则生成所有可能的句子，
通过这种方式来识别某个句子是否属于某个语言，
这种方式实现起来确实简单，但是复杂度是爆炸的。

> The extension we need make to the system we already have is to add a
> propagator that makes guesses and manufactures new premises to
> support them, and modify the contradiction detection machinery to
> inform the guessers of their mistakes and give them the opportunity
> to change their minds.

这一章的 footnote 中，也设计了一种简化的基于表达式的构造图的语言。

这一章对搜索的优化太重要了。

TODO

# 7 There is More to Do

propagator 可以有两种模式 push 和 pull，
push 类比 applicative order lambda calculus，
pull 类比 normal order lambda calculus。

在 pull 的模式下，只有当查询某个 cell 的时候，
相关的 propagator 才会运行。

> In fact, maintaining separate justifications for
> the upper and lower bounds of an interval would have eliminated the
> anomaly of Figure 2.

处理上下界所形成的 constraint 时，
把上下界分解成上界和下界更方便。

> We come, at the end, to the eternal problem of time.  The structure
> of a propagator network is space.  To the extent that there is no
> "spooky" action at a distance, there is no reason to require time
> to pass uniformly and synchronously in all regions of the network.

我们目前对 propagator 的实现是单线程的，
可以想像用 actor model 之类的并发模型来实现 propagator。

可以尝试用 shared-memory multithreading 来实现 propagator，
看看有哪些地方是需要上锁的。
