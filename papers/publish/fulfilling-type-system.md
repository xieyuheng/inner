---
title: Fulfilling Type System
author : Xie Yuheng
date : 2018-07-18
keywords : [type system, dependent type, categorical model, programming language design]
---

# Abstract

In this work, I am designing a language,
in which I use partly inhabited typed-record as type,
and fully inhabited typed-record to inhabits such type.
Thus I call it **Fulfilling Type System**.

The fields of a typed-record can naturally dependent on each other.
And one typed-record can be used in different ways, by omitting different fields.

When defining a type by giving a typed-record a name, it can inherit other types.
This inheritability introduces sub-type relation between types,
which makes the type system suitable to formalize mathematical structures.
- In this paper, I will formalize basic category theory as the main example.

# My Personal Vision

The future of programming language is mathematics.

Post-CS mathematics will be formalized in programming languages,
in which mathematical constructions will be tested and debugged,
and theorems will be proved with the help of the machines.

The modern mathematical language is category theory.

Thus I am designing a programming language here,
of which the main aim is to formalize category theory.

# The Design

## Fulfilling Type

In the fulfilling type system,
I use partly inhabited typed-record as type,
and fully inhabited typed-record to inhabits such type.

For example, cons-t can be defend as following :

``` scala
cons-t : type-tt
cons-t = data
  t : type-tt
  car : t
  cdr : (list-t t)
```

The record is named `cons-t`,
it has three fields, each with a name and a type.

``` scala
;; We can use `cons-t` as type-constructor :
(cons-t <t>)
(cons-t <t> <car>)
(cons-t t = <t>, cdr = <cdr>)

;; We can use `cons-c` as data-constructor,
;;   and the above types will be inhabited respectively as the following :
(cons-c <car> <cdr>)
(cons-c <cdr>)
(cons-c <car>)
```

## Sub-type Relation

It is natural to use sub-type relation to model relation between mathematical structures.
For example :
- equivalent-relation is preorder structure plus symmetric axiom.
- groupoid structure is category structure plus one more axiom about inverse of arrow.

To achieve this modeling,
I use inheritable record-type as the main datatype.
For example :
- eqv-relation-t <: preorder-t
- groupoid-t <: category-t

## Sum-type

The use of record-type and the introduction of sub-type,
will not be in conflict with algebraic datatype.

On the contrary, when record-type is used uniformly,
sum-type will be symmetric to sub-type :

| syntax | semantics              |
|--------|------------------------|
| `:>`   | summing sub-types      |
| `<:`   | inheriting super-types |

For example, list-t can be defined as following :

``` scala
list-t : type-tt
list-t = data :> [null-t cons-t]
  t : type-tt

null-t : type-tt
null-t = data
  t : type-tt

cons-t : type-tt
cons-t = data
  t : type-tt
  car : t
  cdr : (list-t t)
```

list-t is declared as sum-type of null-t and cons-t,
thus null-t and cons-t must have all the fields of list-t,
`list-t :> [null-t cons-t]` also means
`null-t <: list-t` and `cons-t <: list-t`.

## Partly Inhabited Record as Type

- A type is a partly inhabited record -- constructed by <name>-t.

- A data is a fully inhabited record -- constructed by <name>-c.

``` scala
list-append : -- (list-t t) (list-t t) -> (list-t t)
list-append ante succ =
  case ante
    null-t succ
    cons-t (cons-c ante.car (recur ante.cdr succ))
```

- In the type of `list-append`,
  (list-t t) means the first field of the record list-t
  is inhabited by a value of type `type-t`;

- To construct a data of the type (list-t t),
  we can use (cons-c <car> <cdr>),
  which inhabit the rest of the fields of the record cons-t.

## Naming Convention to make Levels of Universes explicit

- I also introduced a naming convention to make the level-ness of a type explicit.
  For examples :

| level | example                             |
|-------|-------------------------------------|
| 0     | zero-c, null-c, cons-c              |
|-------|-------------------------------------|
| 1     | nat-t, list-t, (-- list-t -> nat-t) |
|-------|-------------------------------------|
| 2     | type-tt, category-tt                |
|-------|-------------------------------------|
| 3     | type-ttt, category-ttt              |

## Summary

I am using partly inhabited typed-record as type,
and fully inhabited typed-record to inhabits such type.

I call it **fulfilling type system**.

This allows types to be used in a more flexible way,
and makes sub-type relation easily expressed.

To model mathematical structures
will be as intuitive as object-oriented programming.

To prove theorems about mathematical constructions
will be as intuitive as functional programming.

# Examples

## Functional Programming

### nat-t

``` scala
nat-t : type-tt
nat-t = data :> [zero-t succ-t]

zero-t : type-tt
zero-t = data

succ-t : type-tt
succ-t = data
  prev : nat-t
```

### nat-add

``` scala
nat-add : -- nat-t nat-t -> nat-t
nat-add x y =
  case x
    zero-t y
    succ-t (succ-c (recur x.prev y))
```

### list-t

``` scala
list-t : type-tt
list-t = data :> [null-t cons-t]
  t : type-tt

null-t : type-tt
null-t = data
  t : type-tt

cons-t : type-tt
cons-t = data
  t : type-tt
  car : t
  cdr : (list-t t)
```

### list-length

``` scala
list-length : -- (list-t t) -> nat-t
list-length list =
  case list
    null-t zero-c
    cons-t (succ-c (recur list.cdr))
```

### list-append

``` scala
list-append : -- (list-t t) (list-t t) -> (list-t t)
list-append ante succ =
  case ante
    null-t succ
    cons-t (cons-c ante.car (recur ante.cdr succ))
```

### list-map

``` scala
list-map : -- (-- a -> b) (list-t a) -> (list-t b)
list-map fun list =
  case list
    null-t list
    cons-t (cons-c (fun list.car) (recur fun list.cdr))
```

## Logic Programming

### list-length-t

``` scala
list-length-t : type-tt
list-length-t = data :> [zero-length-t succ-length-t]
  list : (list-t t)
  length : nat-t

zero-length-t : type-tt
zero-length-t = data
  list : (list-t t)
  length : nat-t
  list = null-c
  length = zero-c

succ-length-t : type-tt
succ-length-t = data
  list : (list-t t)
  length : nat-t
  prev : (list-length-t list length)
  list = (cons-c x list)
  length = (succ-c length)
```

### list-append-t

``` scala
list-append-t : type-tt
list-append-t = data :> [zero-append-t succ-append-t]
  [ante succ result] : (list-t t)

zero-append-t : type-tt
zero-append-t = data
  [ante succ result] : (list-t t)
  ante = null-c
  result = succ

succ-append-t : type-tt
succ-append-t = data
  [ante succ result] : (list-t t)
  prev : (list-append-t cdr succ result-cdr)
  ante = (cons-c car cdr)
  result = (cons-c car result-cdr)
```

## Mathematical Structures

### preorder-tt

``` scala
note
  preorder is a thin category
  with at most one morphism from an object to another.

preorder-tt : type-ttt
preorder-tt = data
  element-t : type-tt

  pre-t :
    -- element-t element-t
    -> type-tt

  pre-reflexive :
    -- a :: element-t
    -> (pre-t a a)

  pre-transitive :
    -- (pre-t a b)
       (pre-t b c)
    -> (pre-t a c)
```

### eqv-relation-tt

``` scala
eqv-relation-tt : type-ttt
eqv-relation-tt = data <: [preorder-tt]
  pre-symmetric :
    -- (pre-t a b)
    -> (pre-t b a)
```

## Category theory

### category-ttt

``` scala
category-ttt : type-tttt
category-ttt = data
  object-tt : type-ttt
  arrow-tt : -- object-tt object-tt -> type-ttt
  arrow-eqv-tt : -- (arrow-tt a b) (arrow-tt a b) -> type-ttt

  identity : -- a :: object-tt -> (arrow-tt a a)

  compose : -- (arrow-tt a b) (arrow-tt b c) -> (arrow-tt a c)

  identity-neutral-left :
    -- f : (arrow-tt a b)
    -> (arrow-eqv-tt f (compose identity f))

  identity-neutral-right :
    -- f : (arrow-tt a b)
    -> (arrow-eqv-tt f (compose f identity))

  compose-associative :
    -- f : (arrow-tt a b)
       g : (arrow-tt b c)
       h : (arrow-tt c d)
    -> (arrow-eqv-tt
         (compose f (compose g h))
         (compose (compose f g) h))

  arrow-eqv-relation :
    -- [a b] :: object-tt
    -> (eqv-relation-tt
         element-tt = (arrow-tt a b)
         pre-tt = arrow-eqv-tt)
```

### category.isomorphic-tt

``` scala
category.isomorphic-tt : type-ttt
category.isomorphic-tt = data
  [lhs rhs] : object-tt
  iso : (arrow-tt lhs rhs)
  inv : (arrow-tt rhs lhs)
  iso-inv-identity : (arrow-eqv-tt (compose iso inv) identity)
  inv-iso-identity : (arrow-eqv-tt (compose inv iso) identity)
```

### category.product-tt

``` scala
category.product-candidate-tt : type-ttt
category.product-candidate-tt = data
  fst : object-tt
  snd : object-tt
  product : object-tt
  fst-projection : (arrow-tt product fst)
  snd-projection : (arrow-tt product snd)

category.product-tt : type-ttt
category.product-tt = data <: [product-candidate-tt]
  factorizer :
    -- cand : (product-candidate-tt fst snd)
    -> factor : (arrow-tt cand.product product)
  unique-factor :
    -- cand : (product-candidate-tt fst snd)
    -> unique (factorizer cand)
       of (arrow-tt cand.product product)
       under arrow-eqv-tt
       such-that
         (arrow-eqv-tt
           cand.fst-projection
           (compose factor fst-projection))
         (arrow-eqv-tt
           cand.snd-projection
           (compose factor snd-projection))
```

### groupoid-tt

``` scala
groupoid-tt : type-ttt
groupoid-tt = data <: [category-tt]
  inverse : -- f : (arrow-tt a b) -> (isomorphic-tt a b f)
```
