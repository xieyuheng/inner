---
title: 形式概念分析
subtitle: Formal Concept Analysis
---

# 学习动机

**动机 A：**

首先要学会如何从数据构造 concept lattice。
因为这是非常实用的工具，可以让我们的思想变清晰。
回忆 Peirce 的 "How to make our ideas clear"。

我们要写算法实现这个构造，
并且要写实用的 app 来产品化相关的算法。

**动机 B：**

FCA 的形式定义就是 [EAV](https://en.wikipedia.org/wiki/entity-attribute-value_model) 但是没有 V。
与 FCA 的 EA 相比，EAV 多出的一个 V 在理论上有什么意义呢？
还能形成类似 complete lattice 的理论吗？

# 所解决的问题

当观察物体（Object，Gegenstand）时，我们总是想要去给分类，
而给我们物体分类的依据会被总结成物体的属性（Attribute，Merkmal）。

我们经常会发现某一组属性具有特殊意义，
我们可以给这一组属性，以及其所描述的所有物体，
取一个名字，并且称其为一个概念（Concept）。

- 新发现一个概念，是否与新增一个属性等价呢？
  显然可以，因为所发现的一个概念的 intent 就是一组 attributes，
  只要把这组 attributes 的交命名为一个新的 attribute 就可以了。

寻找有意义的属性组合以形成概念，
就是我们所要解决的问题。

形式概念分析（Formal concept analysis）正是解决上述问题的数学理论。
我们通过分析概念的一般性与特殊性，我们可以构造出概念格（Concept lattice）。
在概念格中我们可以找到所有概念（即有意义的属性组合）。

# 形式语境

一组属性，以及其所描述的所有物体，就是一个概念。
因此为了描述概念，我们先要明确什么是物体与属性。

先看物体与属性之间的关系，
我们可以说「物体具有属性」或「物体拥有属性」，
也可以说「属性描述物体」或「属性属于物体」。

想要明确什么是物体与属性，
就需要明确它们之间的「具有」或者「描述」关系。

我们通过形式语境（Formal context）来限定物体与属性以及其关系。

想要给出一个形式语境，
- 首先要限定其中的所有物体，
- 其次要限定其中的所有属性，
- 最后还要明确其中物体与属性之间的「具有」关系表。
  给出形式语境中的任意一个物体与属性，
  我们可以通过查找关系表来判断这个物体是否具有这个属性。

一个形式语境中的物体和属性的个数必须是有限的。

# 形式概念

我们所说的概念有别于一般意义上的认知概念（Cognitive concept），
我们进一步明确我们的术语，称其为形式概念（Formal concept）。

在某个形式语境中，一组属性，以及其所描述的所有物体，就是一个形式概念。
这组属性称为其内涵（Intent），这些物体称为其外延（Extent）。

# 概念层级

在某一语境下，概念的外延作为集合之间的包含关系，可以定义概念之间的包含关系。
这种偏序关系下，概念组成概念层级（Concept hierarchy）。

# 概念格

概念层级是一个完全格（Complete lattice），
称为概念格（Concept lattice）。

# 对比集合论

集合可以看成是没有内涵而只有外延的概念。

# 衍生算子

首先可以把「具有」与「描述」关系，
从元素与元素之间的关系，
拓展为元素与集合之间的关系：

- 说一个物体具有一组属性，就是说这个物体具有其中每一个属性。
- 说一个属性描述一组物体，就是说这个属性描述其中每一个物体。

然后可以定义两个衍生算子，
也算是把「具有」与「描述」关系，
拓展成了集合与集合之间的关系：

- 一组物体可以衍生出一组属性，即所有描述这组物体的属性。
- 一组属性可以衍生出一组物体，即所有具有这组属性的物体。

注意，形式概念定义中所说的
「一组属性，以及其所描述的所有物体，就是一个形式概念。」
只是显式地说出了，内涵衍生外延（或者说内涵描述外延）。
为了定义形式概念，我们还必须要求外延也衍生内涵（或者说外延具有内涵）。
即是说，概念的内涵与外延相互衍生。

# 闭包算子

衍生算子还可以给出两个闭包算子：

- 一组物体所衍生的属性所衍生的物体，就是这组物体的闭包。
- 一组属性所衍生的物体所衍生的属性，就是这组属性的闭包。

对比一般的[闭包算子](https://en.wikipedia.org/wiki/Closure_operator)，
以及[拓扑学中的闭包算子](https://en.wikipedia.org/wiki/Characterizations_of_the_category_of_topological_spaces)。

# 用物体与属性生成概念

闭集与它的衍生集，构成概念。

闭集都可以用来生成概念。
单一的元素的闭包是特殊的闭集。

# 概念格中的运算

在上面所定义的闭包算子下，闭集的交集还是闭集。

利用这一点我们可以定义完全格中的上确界（Supremum）与下确界（Infimum）。

# 形式定义

注意，FCA 的三元组形式定义，与二分图完全相同。

然而二分图的存在非常广泛，
比如 Petri net，propagator，cell complex（多分图），等等。
也就是说，可以用 FCA 的方法来分析二分图。

# 连续的数据如何处理？

可以根据例子观察一下 FCA 的局限性，
属性必须被离散化，才能被理解为 concept，比如：
- size 被离散化为 small、medium、large；
- distance 被离散化为 near、far。

那么数学中那些连续函数怎么办？
concept 可以有某种连续性吗？
可以通过带连续的参数来实现吗？
和 temporal logic 类似也有 temporal FCA，
是否和 TLA+ 这种建模语言中使用 temporal 参数类似呢？

# FCA 与 Peirce

Peirce 的实用主义格言：

> It appears, then, that the rule for attaining the third grade of
> clearness of apprehension is as follows: Consider what effects, that
> might conceivably have practical bearings, we conceive the object of
> our conception to have. Then, our conception of these effects is the
> whole of our conception of the object.

「物体所能拥有的所有效果。」

- 就可以理解为是 FCA 中物体的所有属性。
  例如在 FCA 中，两个 object 相等，
  就定义为它们的 attributes 完全相等。

- 这也与带有 structural subtyping 的类型系统类似，
  比如 TypeScript。

「效果指我们所能想到的，有实际影响的效果。」

- 强调「我们所能想到的实际影响」，
  因为在不同的语境下（情况下），
  我们所考虑的「实际影响」可能不同。
  可以理解为是 FCA 中的形式语境。

也许可以总结为：

> 我们在对一个物体所形成的概念，
> 就是，在我们所关心的情况下，
> 这个物体的具有实际影响的效果的总和。

一些自动翻译的结果蛮有趣的：

- Kimi: The concept we form of an object is, in the cases we care
  about, the sum of the practical effects that the object has.

- Google: The conception we form of an object is the sum of its
  effects, which have a practical influence on the case in which we
  are concerned.

- ChatGPT: The concept we form of an object is the sum of the effects
  it has in the situations we care about.

考虑 Peirce 的 "How to make our ideas clear?"，
也可以说我们能够回答 "When to use formal concept analysis?"，
答案是 "When we want to make our ideas clear"。

注意，我们并不是所有时候都想这样做，
比如读诗歌和欣赏风景的时候，
单纯只是在体验而不是在思考。
