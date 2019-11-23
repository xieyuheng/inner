---
title: The Catsters
---

# The Catsters

------
- Author: The Catsters
------

Notes taken from TheCatsters' lectures: http://www.simonwillerton.staff.shef.ac.uk/TheCatsters

## Natural transformations

A functor is a morphism between categories (in the category of categories).

A natural transformation is a morphism between functors (in the category of functors),
where the `dom` and `cod` of the functors must be the same.

Natural transformation is like homotopy,
given two functions `f, g : X -> Y`,
and homotopy `h : X * I -> Y` between `f` and `g`,
`(i) => h(x, i) : I -> Y` is a path from `f(x)` to `g(x)`.

Natural transformation == functor of type `C * I -> D`,
where `I` is the categorical interval (a little shape category).

- **[Exercise]** How the naturality of natural transformation
  can be derived from the above definition?

Since natural transformation is morphism in some category, we can compose them.
We have functor category, `C, D : category_t`,
`functor_category_t(C, D)` is a category,
where `object_t = functor_t(C, D)`
and `morphism_t(f, g) = transformation_t(C, D, f, g)`.

The above definition functor category compose natural transformations vertically,
We can alos compose natural transformations horizontally.

Middle four interchange law says,
when composing four natural transformations the order does not matter.

- **[Xie]** This law just the scratch the surface of higher dimensional algebra.

The category of categories is an instances of 2-category (`strict_two_category_t`),
`strict_two_category_t` is define as a structure with morphisms between morphisms.

As an enriched category, `strict_two_category_t` is enriched over `category_t`,
with the monoidal structure given by product of categories.

``` js
object_t = category_t
morphism_t(f, g) = functor_category_t(f, g)
two_morphism_t(f, g) = transformation_t(C, D, f, g)
```

- **[Xie]** Higher category theory is just special case of the theory of cell-complex.

## Representable Functors and Yoneda

``` js
class contravariant_representable_functor_t {
  cat : category_t
  a : cat.object_t
  // functor from `opposite(cat)` to `set_category`
  map : { x : cat.object_t -> homset(x, a) }
  fmap : {
    [ x, y : cat.object_t ]
    f : cat.morphism_t(y, x)
    g : homset(x, a)
    ---------
    homset(y, a)
  } = {
    compose(f, g)
  }
}

class covariant_representable_functor_t {
  cat : category_t
  a : cat.object_t
  // functor from `cat` to `set_category`
  map : { x : cat.object_t -> homset(a, x) }
  fmap : {
    f : cat.morphism_t(x, y)
    g : homset(a, x)
    ---------
    homset(a, y)
  } = {
    compose(g, f)
  }
}

yoneda_embedding : {
  cat : category_t
  a : cat.object_t
  ---------
  functor_category_t(opposite(cat), set_category)
}
```

## Adjunctions
