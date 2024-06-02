---
title: The Art of the Propagator
authors: [Alexey Radul, Gerald Jay Sussman]
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

尝试设计类 JS 的具体语法：

```cicada
propagator add(x: Number, y: Number, z: Number) {
  // 带有多个运行方向的 primitive propagator。
  TODO
}

propagator div(x: Number, y: Number, z: Number) {
  TODO
}

// 还是用类似函数的语法，但是输入 cells，
// 然后 connect cells。

propagator heronStep(x: Number, g: Number, h: Number) {
  div(add(g, div(x, g)), 2，h)
}
```

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

TODO

先描述一个假想的语言，
然后跟一个 "Making this work"
来实现这个语言，
这种格式也是我写 inet 文档时候的格式，
哈哈。

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

# 5 Generic Operations

用对函数的所有参数的类型和值的 dispatching 来实现 generic operations。

# 6 Dependencies

记录依赖关系是为了处理 beliefs 之间的冲突。

> If we observe inconsistencies we do not crash -- we chuckle!

## 6.1 Dependencies for Provenance

TODO

## 6.2 Dependencies for Alternate Worldviews

TODO

## 6.3 Dependencies for Implicit Search

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
