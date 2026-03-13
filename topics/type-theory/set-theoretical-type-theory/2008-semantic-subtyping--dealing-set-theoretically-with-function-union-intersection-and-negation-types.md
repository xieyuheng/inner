---
title: semantic subtyping: dealing set theoretically with function union intersection and negation types
authors: [alain frisch, giuseppe castagna, véronique benzaken]
year: 2008
---

# My Motive

[2026-03-14] 发现 polymorphic function type 与集合论的直觉相矛盾，
所以来看看应该如何从集合论角度理解 function type。

初步看来，这里所描述的 set-theoretic type theory，
就是我想要在 lattice-lisp 中实现的类型系统，
但是我因为没能处理好带有 union 和 intersection 的递归类型而失败了。

# 1 Introduction

介绍了很多前人的工作。

# 2 Overview of the approach

介绍实现 subtype 关系的两种方式：

- syntactic -- 用推演规则来定义 subtype 关系。
- semantic -- 把 type 映射到一个 set-theoretic model，
  然后利用 model 中的 subset 关系来定义 subtype 关系。

semantic 是自然的处理方式，也是这篇论文的方式。

semantic 也可以被理解为，
有 decidable 的算法可以自动找到，
syntactic 中需要手动构造的 proof。

## 2.1 A five steps recipe

我很难想象如何实现，因为 set-theoretic model 中，
与 type 对应的 set 很多都是无穷集合，
如何实现算法来判断无穷集合之间的 subst 关系？

可能重点在于不是用最自然的集合，
即 value 的集合作为 model，
而是一 value 的集合作为基础，
寻求一个简化的，但是不改变 subtype 关系的 model。

## 2.2 Advantages of semantic subtyping

要知道，value 和 type 之间的属于关系，是一个二元关系，
所以一定能从一个 type 引导出来一个 value 的集合，
从而回到 set-theoretic 的 model 中来。

即便是那些没有 union type 和 intersection type 的类型系统，
比如 hott 也是如此。

## 2.3 A model of types

> Note however that in order to define semantic subtyping all we need
> is a set-theoretic _model of types_. The construction works even if
> we do not have a model of terms. To push it to the extreme, in order
> to define subtyping we do not need terms at all, since we could
> imagine to define type inclusion for types independently from the
> language we want to use these types for. More plainly, the
> definition of a semantic subtyping relation needs neither an
> interpretation for applications (that is an applicative model) nor,
> thus, the solution of complicated domain equations.

能够避免传统的 domain theory，正是我想要的。

这种思路可以用于 dependent type 吗？

- 如果 dependent type 所依赖的不是 expression 而是 value，那么也许是可能的。
- 对于 dependent type 的 model，我们所关心的不只是 subset 关系。
- dependent type 最明显的 model 就是拓扑空间，
  或者说是 cell complex 或 cobordism 一类的构造。

## 2.4 Types as sets of values

不考虑 expression 的集合，
而考虑 value 的集合，
看似是可行的。

但是 value 一旦包含了 lambda abstraction，
lambda 的 body 就又包含 expression 了。

expression 中又有 application，
又需要 domain theory 了。

这也回答了上面关于 dependent type 的问题。
依赖 value 是不行的，因为 value 包含 lambda abstraction。
比如：

```scheme
(the-equal-t (-> nat-t nat-t nat-t)
  nat-add
  (lambda (x y)
    (nat-add y x)))
```

也许可以尝试用基于图论的 denotational semantics，
来避免 domain theory。

## 2.5 A circularity to break

就是避免依赖 value。

## 2.6 Set-theoretic models

这里说明为什么函数类型所对应的集合，
不能被理解为笛卡尔积的子集。

因为 cardinality 上会有悖论，
这个悖论也是 domain theory 被提出的原因。

> Since this point is central to our model, let us explain it
> differently.  Recall that the only reason why we want to accurately
> state what the set-theoretic model of types is, is to precisely
> define the subtyping relation for syntactic types.  In other words,
> we do not define an interpretation of types in order to formally and
> mathematically state what the syntactic types _mean_ but, more
> simply, we define it in order to state how they are _related_.

> So, even if we would like to say that a type `t→s` must be
> interpreted in the model as [function image] as stated by (2), for
> what it concerns the goal we are aiming at, it is enough to require
> that a model must interpret functional types so that the induced
> subtyping relation is the same as the one the condition (2) would
> induce.

也就是说，就用子集关系导出子类型关系而言，
找出一个与「函数作为图像的集合」有同样效果的集合，
作为函数类型的 model。

这一节所推理出来的，
是一个对 model 的限制，
还不是具体的 model。

函数类型按照直觉，应该对应于所有函数的集合，
从集合论的角度看，也就是所有函数的图像的集合，
函数图像作为笛卡尔积的子集，
称其两个元素为「参数」和「返回值」，
应该满足条件：
如果参数在定义域中，那返回值必定在值域中。

从后面 6.8 A universal model 的定义看来，
具体的 model 就是把函数类型映射为，
满足上面条件的（全集的）笛卡尔积的有限子集。

所有有限子集的集合还是无限的，所以还是要找出一个，
就这种 model 定义而言的，子类型检查算法。

TODO
