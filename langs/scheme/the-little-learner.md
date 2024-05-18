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
# Interlude IV: Smooth Operator
# 9: Be Adamant
# Interlude V: Extensio Magnifico!
# 10: Doing the Neuron Dance
# 11: In Love with the Shape of Relu
# 12: Rock Around the Block
# 13: An Eye for an Iris
# Interlude VI: How the Model Trains
# Interlude VII: Are Your Signals Crossed?
# 14: It's Really Not That Convoluted
# 15: …But It Is Correlated!
# Epilogue - We've Only Just Begun
# Appendix B: I Could Have Raced All Day
# References
