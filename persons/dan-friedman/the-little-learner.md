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

我们在练习实现的时候可以不用 dynamic scope，
因为这是一个程序语言级别的功能，
而一般的程序语言没有 dynamic scope 这个功能。

在一般的程序语言中，我们可以使用的备选方案是，
直接多带一个参数 `options`，
把需要的所有参数都传到函数里。

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
interface Representation {
  inflate,
  deflate,
  update,
}
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

注意，对于 `extend2` 之类的高阶 tensor 函数，
只有当考虑 tensor 的 shape 的时候，
其类型才能有效地约束函数的范围。

比如：

```cicada
mul21: ([m, n], [n]) -> [m, n]
mul21: ([a, m, n], [n]) -> [a, m, n]
mul21: ([m, n], [b, n]) -> [b, m, n]

// 由于 extend2 的定义中两个参数 rank 相等时，
// 被特殊处理为 zip + map 了，
// 这也许是不合理的，因为下面两个作用返回的 shape 一样了。
mul21: ([a, m, n], [a, b, n]) -> [a, b, m, n]
mul21: ([a, m, n], [b, n]) -> [a, b, m, n]
// 也许应该要求相对于两个 base rank 的 extra rank 相等时，
// 才使用 zip + map，从而达到下面的效果：
mul21: ([a, m, n], [a, n]) -> [a, m, n]
// 而按照当前的定义，我们将得到：
mul21: ([a, m, n], [a, n]) -> [a, a, m, n]
```

TODO 用 dependent type 描述 `extend2` 的类型将会是很有趣的挑战。

```cicada
claim extend2: (
  fn: (
    Tensor(firstShape),
    Tensor(secondShape),
  ) -> Tensor(resultShape)
) -> (
  Tensor(???),
  Tensor(???),
) -> Tensor(???)
```

# 10: Doing the Neuron Dance

如何构造更复杂的，模仿人类神经网络的函数，作为 target 函数呢？

用函数复合就可以吗？

称 activation functions 为 deciders，
activation 来自神经网络的比喻，
但是 decider 这样的命名更接地气。

> Functions like relu are known
> as artificial neurons. Each
> neuron has a linear part, like
> linear, and a nonlinear
> decider like rectify.

引入 relu 函数。

> **The Rule of Artificial Neurons**
> An artificial neuron is a parameterized linear function
> composed with a nonlinear decider function.

> Let's see an illustration of
> how multiple uses of relus
> can be combined to do more interesting things.

获得用 relu 的复合模拟更复杂的函数的直觉。

用 `halfStrip` 和 `fullStrip` 为例子，
因为根据勒贝格（Lebesgue）积分的方案，
任何函数都可以被分解为 `halfStrip` 和 `fullStrip`，
即 piecewise-linear approximation。

这类方案叫做 universal approximation。

# 11: In Love with the Shape of Relu

这一章学习神经网络的 layer，
layer function 是特殊的 target function，
有一样的类型声明。

有不同类型的 layer：

- dense layers (fully-connected layers)

  In general, the layer function of a dense
  layer with `m` neurons takes one input tensor of shape `[n]`,
  and parameters weight of shape `[m, n]` and bias of shape `[m]`,
  and invokes each of those `m` neurons on input,
  to produce output tensor of shape `[m]`.

  We say that the width of this layer is `m`.

> **The Law of Dense Layers**
>       (Initial Version)
> A dense layer function invokes m neurons on an n-element
> input tensor-1 and produces an m-element output tensor-1.

> **The Law of Dense Layers**
>       (Final Version)
> A dense layer function invokes m neurons on an n-element
> input tensor-1 that produces an m-element output tensor-1
> in a single invocation of *-2-1.

注意，这里对 `linear` 的定义要用 `matrixVactorMul`，而不能用 `dot`。

观察这里 `matrixVactorMul` 和 `dot` 的定义，
也许我们在处理 tensor 的函数的时候，
应该放弃传统线性代数中，只有行向量与列向量才能 `dot` 的限制，
也许行列向量的发明只是为了书写矩阵乘向量的时候方便。

> The list of shapes of the tensor2 and
> tensor1 parameters necessary for a layer is
> known as the _shape list_ of the layer.

在带有 dependent type 的语言中，
shape list 就是 layer 的类型参数。

这里很多篇幅用来处理 `k-relu` 的 currying，
看来带有自动 currying 的语言是必要的。

> The problem is that network functions, as
> in 1-relu, 2-relu, and 3-relu above, must
> take their arguments t and θ one at a time.

# 12: Rock Around the Block

前一章定义的 `k-relu`，
给出了 `k` 之后，
对所接受的 parameters 完全没有约束。

parameters 的 shape list 必须作为分离的 metadata 给出，
这一章我们改进这一点。

> We begin by introducing blocks.
> (A block is short for network building block.)
> A block associates a layer function with its shape list.

可以说是带有一些运行时的类型信息了，
也许可以在 compose block 的时候用来检查 shape 是否符合？

- 其实不是，在做 composition 的时候，
  shape list 只是简单地被 append 到一起，
  shape list 之间并没有什么相互的约束（constraints）。

> Now let's see the real magic in these blocks.
> A neural network is fully described by a
> network function and a shape list.

> Oh, that means a neural
> network can also be a block!
> So stacking blocks together
> also produces another block.

找到了！什么？
范畴的态射。
那是带有 shape list 的 layer functions。

有 composition 的地方，就有范畴与它的态射，
而上一章的 `k-relu` 显然是在做 composition，
因此要找范畴与态射。

但是，如果真的是范畴的话，
单位元是什么？
就是对 输入 tensor 而言的 identity function，
并且忽略 parameters。

想要成为范畴，
还需要检查复合是满足结合律的，
这是显然的。

> **The Law of Blocks**
> Blocks can be stacked to form bigger blocks
> and complete networks.

# 13: An Eye for an Iris

> Degrees of belief is the confidence we have
> about a certain statement.

> In general, the layers closer to the input are
> wider than the layers closer to the output.

> The layers closer to the input are responsible
> for learning some of the more primitive
> characteristics of the data set, and the layers
> closer to the output learn more advanced
> characteristics based on the output produced
> by the earlier layers.

Breaking the symmetry between neurons
-- 使用随即的初始 weights。

He initialization for networks that use rectify:

> ... that formula makes sure that in deep
> networks, the weights stay in a very tight
> cluster around 0.0 so that when a scalar from
> the input tensor is multiplied with weights in
> each layer, the result neither explodes nor
> vanishes.

> **The Rule of Layer Initialization**
>          (Initial Version)
> The bias tensor-1 of a layer is initialized to contain only 0.0
> The weight tensor-2 of a layer is initialized to random scalars
> with a central value of 0.0 and a variance of 2/n where n is the
> length of the input to the layer.

用部分数据来训练，用另一部分数据来测试。

> On larger data sets, the general guideline
> is to use 20% of the data set for testing.
> Since our data set here is quite small, we
> limit the test set to 10%.

> A model is an approximation of an
> _idealized_ function represented by the data
> set. This idealized function yields, for
> every x in the xs of the data set, the
> corresponding y from ys, but also
> produces a y for any given x, even if it is
> not in xs.

这一章的 `randomTensor` 需要用到正态分布。
要写一个能生成符合正态分布的随机数的函数。

引入 model 的概念，
每次 training 都会获得一个 model，
但是只是用函数来实现 model 可能是不合适的，
因为好的 model 的 parameters 应该被保存下来。

# Interlude VI: How the Model Trains

> [for classifiers]
> We say that the model is accurate for
> those inputs where the two classes are
> the same.

> When the classes are not the same,
> we call them _classification errors_.

> The ratio of the number of accurate
> classifications to the total number of
> test inputs we have is known as the
> _accuracy_ of the model.

当决定算法的合适参数（options）时，
可以尝试不同的参数组合，
然后用 accurate 来判断参数是否合适。

> This way of testing different
> combinations for the best one is known as
>     _grid search_.

尽管这里把 `grid-search` 定义为了一个语法关键词，
但是也有用函数实现 `grid-search` 的方案，
并且不依赖 dynamic scope。

这里像是 `grid-search` 这种基于实验的方案，
让我感到很不舒服，也许是因为与（可以被机器学习的） parameters 相比，
人们对这些 hyperparameters 的理解并没有那么透彻，
也许科学的探索就是需要这些实验。

# Interlude VII: Are Your Signals Crossed?

利用神经网络，来解码带有噪声的信号。

`tenserZip` -- `[d, n] -> [n, d]` -- 与矩阵转置类似。

> **The Law of Zipped Signals**
> A signal-2 is formed by zipping signal-1s, and the signal-2 as well
> as its constituent signal-1s all have the same number of segments.

由于信息开始的时间可能是任意的，
所以需要使用卷积来消除信号中开始时间的差异。

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
