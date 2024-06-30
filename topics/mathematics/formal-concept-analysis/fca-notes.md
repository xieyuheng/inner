---
title: 形式概念分析笔记
subtitle: Formal Concept Analysis Notes
---

# 所解决的问题

当观察物体（Object，Gegenstand）
我们可以总结出属性（Attribute，Merkmal），
我们经常会发现某一组属性具有特殊意义，
我们可以给这一组属性，以及其所描述的所有物体，
取一个名字，并且称其为一个概念（Concept）。

寻找有意义的属性组合以形成概念，
就是我们所要解决的问题。

# 形式定义

注意，FCA 的三元组形式定义，与二分图完全相同。

然而二分图的存在非常广泛，
比如 Petri net，propagator，cell complex（多分图），等等。
也就是说，可以用 FCA 的方法来分析二分图。

# 实现

```typescript
interface FormalContext {
  objects: Array<Object>
  attributes: Array<Attribute>
  incidence(object: Object, attribute: Attribute): boolean
}

function extent(
  ctx: FormalContext,
  attributes: Array<Attribute>,
): Array<Object>

function intent(
  ctx: FormalContext,
  objects: Array<Object>
): Array<Attribute>

function extentClosure(
  ctx: FormalContext,
  objects: Array<Object>,
): Array<Object> {
  return extent(intent(ctx, objects))
}

function intentClosure(
  ctx: FormalContext,
  attributes: Array<Attribute>,
): Array<Attribute> {
  return intent(extent(ctx, attributes))
}

interface FormalConcept {
  ctx: FormalContext
  objects: Array<Object>
  attributes: Array<Attribute>

  extent(ctx, attributes) == objects
  intent(ctx, objects) == attributes
}
```

# FCA 与 Peirce

Peirce 的实用主义格言：

> It appears, then, that the rule for attaining the third grade of
> clearness of apprehension is as follows: Consider what effects, that
> might conceivably have practical bearings, we conceive the object of
> our conception to have. Then, our conception of these effects is the
> whole of our conception of the object.

也许可以总结为：

```
object = all of it's practical effects
```

例如在 FCA 中，两个 object 相等，
就定义为它们的 attributes 完全相等
（与带有 structural subtyping 的类型系统类似，比如 TypeScript）。

# 形式概念分析

形式概念分析（Formal concept analysis）正是解决上述问题的数学理论。
我们通过分析概念的一般性与特殊性，我们可以构造出概念格（Concept lattice）。
在概念格中我们可以找到所有概念（即有意义的属性组合）。

# 形式语境

一组属性，以及其所描述的所有物体，就是一个概念。
因此为了描述概念，我们先要明确什么是物体与属性。

先看物体与属性之间的关系，我们可以说「物体具有属性」，也可以说「属性描述物体」。

想要明确什么是物体与属性，就需要明确它们之间的「具有」或者「描述」关系。

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

概念层级是一个完全格（Complete lattice）称为为概念格（Concept lattice）。

# 对比集合论

集合可以看成是没有内涵而只有外延的概念。

# 衍生算子

一组物体可以衍生出一组属性，即所有描述这组物体的属性。
（说一个属性描述一组物体，就是说这个属性描述其中每一个物体。）

一组属性可以衍生出一组物体，即所有具有这组属性的物体。
（说一个物体具有一组属性，就是说这个物体具有其中每一个属性。）

形式概念定义中所说的「一组属性，以及其所描述的所有物体，就是一个形式概念。」
只是显式地说明了，内涵衍生外延。
为了定义形式概念，我们还必须要求外延也衍生内涵。
即是说，概念的内涵与外延相互衍生。

# 闭包算子

一组物体所衍生的属性所衍生的物体，就是这组物体的闭包。

一组属性所衍生的物体所衍生的属性，就是这组属性的闭包。

TODO 对比一般的闭包算子，以及拓扑学的闭包算子。
- https://en.wikipedia.org/wiki/Closure_operator
- https://en.wikipedia.org/wiki/Characterizations_of_the_category_of_topological_spaces

# 用物体与属性生成概念

闭集与它的衍生集，构成概念。

闭集都可以用来生成概念。
单一的元素的闭包是特殊的闭集。

# 概念格中的运算

在上面所定义的闭包算子下，闭集的交集还是闭集。

利用这一点我们可以定义完全格中的上确界（Supremum）与下确界（Infimum）。
