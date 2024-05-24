---
title: The Little Learner
---

# 0: Are You Schemish?

复习一下 Scheme。

# 1: The Lines Sleep Tonight

介绍 line 这个简单的函数。

# 2: The More We Learn, the Tenser We Become

介绍 Tenser 的概念。

# Interlude I: The More We Extend, the Less Tensor We Get

将作用于标量的函数扩展成可以作用于张量的函数。

# 3: Running Down a Slippery Slope

介绍梯度。

# 4: Slip-slidin’ Away

递归下降法。

# Interlude II: Too Many Toys Make Us Hyperactive

介绍 dynamic scope，可以用来处理递归下降法中的参数。

# 5: Target Practice

之前用 line 测试了递归下降法，
line 被称为 target，
这章找更多的 target 函数来练习一下。

练习了一个非线性函数 -- 二次函数 quad，
还有一个多元函数 -- 平面 plane。

> **The Rule of Parameters**
>     (Final Version)
> Every parameter is a tensor.

注意 `Parameters` 不是 `Tensor` 而是 `Array<Tensor>`，
因为每个 parameter 的 shape 可能不一样。

> **The Rule of θ**
> θ is a list of parameters
> that can have different shapes.

# Interlude III: The Shape of Things to Come

介绍新的 Tensor 的缩写。

把 rank 为 2 的 tensor 写成矩阵，
然后在旁边标注上 shape。

比如 shape 为 `(3 1)` 的是 column matrix，
而 shape 为 `(1 3)` 的是 row matrix。

# 6: An Apple a Day

用 Sampling 来避免在每次调用 loss 函数的时候都使用整个数据集。

> Using a small random sample of a few points
> from the data set produces a good enough
> approximation of loss, which can be used to
> revise θ. We refer to this sample as a batch
> and its generation as sampling a batch from
> the data set.

注意这里函数复合式的设计，
实现 `samplingObjective` 之后，
并不用修改 `gradientDescent` 的实现，
只需要在使用 `gradientDescent` 的时候用 `samplingObjective` 做复合。
在 Sussman 关于如何灵活地编程的新书中，也有提到类似的技巧。

> This kind of gradient descent where the
> objective function uses sampling is known as
> _stochastic gradient descent_

> **The Law of Batch Sizes**
> Each revision in stochastic gradient descent uses only a batch of
> size batch-size from the data set and the ranks of the tensors in
> the batch are the same as the ranks of the tensors in the data set.

# 7: The Crazy “ates”

抽出来一个 interface，为下一步做准备。

```js
interface Representation { inflate, deflate, update }
```

# 8: The Nearer Your Destination, the Slower You Become

上一章和这一章，完美的展示了，如何先把代码打开，然后在修改代码。

> For each desired change,
> make the change easy (warning: this may be hard),
> then make the easy change.
> -- Kent Beck

由于接近极值点的时候梯度太小了，
这里的方案是去积累整个梯度下降路径上的梯度，
每次都乘上一个因子来减少积累的程度。

这种梯度下降称为 momentum gradient descent。

# Interlude IV: Smooth Operator

> So, smooth seems to be a way of
> incorporating historical information that
> is less relevant as we move forward.

# 9: Be Adamant

> Because α (learning rate) represents the fraction of the
> gradient we're going to use as our velocity,
> another approach to addressing this
> problem is to make this fraction _adaptive_.

> Adaptive here means that the fraction is
> decided based on the gradient and its
> historical values.

不用在纠结 `learningRate` 这个常数了，
因为我们讲用历史数据类计算 `adaptiveLearningRate`。

并且可以把这里修改 `learningRate` 的技巧，
和前一章的 momentum gradient descent 结合起来用，
得到 ADMA 算法。

> NOTE ADAM is short for adaptive moment estimation.
> The accompaniment `v` is known as
> the gradient's 1st moment and `r` its 2nd moment.

注意，
这里两个算法的复合并没有那么优雅了，
必须要修改算法定义的细节才能做到。

# Interlude V: Extensio Magnifico!

这里学习如何实现 extended functions。

我不理解为什么不用函数复合，
而是重新从头定义 `of-rank?` 之类的函数。
是为了优化？难道经过测试这里的函数是瓶颈？

我在实现这里的 `extend1` 和 `extend2` 时，
没有先看这章，但是我只是实现了对 Scalar 函数的 extend，
这里实现了对任意 base rank 的函数 extend，
技巧就是把 base rank 作为参数传进来。

```typescript
const mul21 = extend2(mul, 2, 1)
```

`mul21` 与 `mul` 作用于两个 rank 为 2 的 Tensor 的效果并不一样，
`mul21` 可以用来实现 matrix-vector multiplication。

# 10: Doing the Neuron Dance
# 11: In Love with the Shape of Relu
# 12: Rock Around the Block
# 13: An Eye for an Iris
# Interlude VI: How the Model Trains
# Interlude VII: Are Your Signals Crossed?
# 14: It's Really Not That Convoluted
# 15: …But It Is Correlated!
# Epilogue - We've Only Just Begun
# Appendix A: Ghost in the Machine

介绍了利用 Dual Number 进行自动微分的方法，
不用对函数复合而成的表达式做符号处理，
只需要扩展 Number 的定义，
并且再扩展 primitive function，
不需要改变 compound function 的定义，
就能完成自动微分。

据说这个方法是 Clifford 发明的，
Sussman 在讲经典力学的 Scheme 书中也用了这种方法。

在无类型语言中，
可以自由地扩展 primitive function
（需要小心避免递归定义）。
在带有类型检查的语言中，
可能需要重新定义一套 primitive function
（正如我们在 JS/TS 实现中做的那样）。

我们在什么地方可以用类似的技巧呢？

注意不用修改已有的定义就能做扩展，
也是 propagator 的主题。

# Appendix B: I Could Have Raced All Day
# References
