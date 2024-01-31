---
title: Generic figures and their glueings
subtitle: A constructive approach to functor categories
authors: [Marie La Palme Reyes, Gonzalo E. Reyes,Houman Zolfaghari]
year: 2004
---

# INTRODUCTION

> A few years ago, Lawvere suggested another alternative: to introduce
> topos theory through presheaf toposes or, equivalently, C-sets.
>
> These are categories whose objects result from the glueing of simpler ones,
> the generic figures. These categories are Grothendieck toposes which
> do not involve the notion of a Grothendieck topology, making them
> much easier to understand and work with them.

这听起来很有可能可以用描述 Cell Complex。
这也是我对这本书感兴趣的原因。

> Incidence relations may be formulated in terms of right actions.

C 的 Morphism 从右边作用于 C 的 Object 所对应的集合。

Vertex 和 Edge 被视为 Category 的 Object，
Edge 有 start 和 end 两种 Vertex，被视为两种 Morphism。

如果考虑二维的 Cell Complex，其 Face 是 Polygon，
其到 Edge 的 Morphism 可以是代表 Polygon 的 side 的 index。

但是考虑三维的 Cell Complex，Body 是 Polyhedron，
此时就很难定义 Morphism 了。

# Chapter 1：THE CATEGORY OF C-Sets

Let `C` be a category whose objects are thought of as generic figures
and whose morphisms are thought of as changes of figures.

A C-Set -- `X` -- is a family sets `X(F)`
indexed by the objects of `C` -- `F: C.Object`,
which we call `F`-figures of `X`,
together with a right action -- `figChange`:

```cicada
C: Category
X: (F: C.Object) -> Type
F, F1: C.Object

figChange: (s: X(F), f: C.Morphism(F1, F)) -> X(F1)

  figChange(s, C.id(F)) = s
  figChange(figChange(s, f), g) = figChange(s, C.compose(f, g))
```

```cicada
(claim C Category)
(claim X (-> C.Object Type))
(claim [F F1] C.Object)

(claim fig-change
  (Pi ([s (X F)]
       [f (C.Morphism F1 F)])
    (X F1)))

(== (fig-change s (C.id F)) s)
(== (fig-change (fig-change s f) g) (fig-change s (C.compose f g)))
```
