---
title: An intuitionistic theory of types
author: Martin-Löf
year: 1972
---

# 1. informal explanation of the basic concepts

## 1.1 mathematical objects and their types

objects are always with their types

a type is defined by prescribing
what we have to do
in order to construct an object of that type.
[as Bishop's set]

- x -
  not necessarily be able to write a predicate
  to decide whether an arbitrary object
  is of that type?

## 1.2 propositions and proofs

proposition is type
thus,
a proposition is defined by prescribing
how we are allowed to prove it.
a proposition is true intuitionistically
if there is a proof of it.

## 1.3 cartesian product of a family of types

- dependent arrow

``` cicada old-design
B: (A) -> type
{ (x: A) -> B(x) } : type

b(x: A) : B(x)
b(x: A) = ...

a: A
--------
b(a) : B(a)
```

## [note] schema

the author use the term schema to specify definition,
which may be understood as
general patterns of function body.

whenever there are multiple equations in the schema,
branching in the function body must be used
for example : (case) (cond) (match)

## 1.4 disjoint union of a family of types

- dependent pair

``` jojo
(* (: :x A) :x B)
(: C (-> (* (: :x A) :x B) -- Type))

(note
  this C is to demonstrate
  how should we construct the elements of a type
  which dependents on (* (: :x A) :x B))

(: d (-> (: :x A) (: :y :x B) -- (* :x :y) C))
(+fun f (-> (* (: :x A) (: :y :x B)) -- (* :x :y) C)
  tuple-spread d)
(+fun f (-> (: (* :x :y) (* A :x B)) -- (* :x :y) C)
  tuple-spread d)

(+fun f (-> (* (: :x A) (: :y :x B)) -- (* :x :y) C)
  (* :x :y) {d} E)

(+fun E (-> (suppose (: :A Type) (: :B (-> :A -- Type)))
            (* (: :x :A) (: :y :x :B))
            (: :f (-> (: :x :A) (: :y :x :B) -- (* :x :y) :C))
         -- (* :x :y) :C)
  tuple-spread :f)

(+fun p (note left projection)
  (-> (^ (: :A Type) (: :B (-> :A -- Type)))
      (* (: :x :A) (: :y :x :B))
   -- (: :x :A)))

(+fun q (note right projection)
  (-> (^ (: :A Type) (: :B (-> :A -- Type)))
      (* (: :x :A) (: :y :x :B))
   -- (: :y :x :B)))

(note example
  R for real number
  (= R [(: :x (-> Z -- Q))
        (-> (: :m :n Z)
            -- :m :n add :x :m :x sub abs
            2 :m neg power LT)]))
```

## 1.5 disjoint union of two types

- disjoint
  thus can not be dependent

``` jojo
(+ A B)
(: C (-> (+ A B) -- Type))

(: d (-> (: :x A) -- :x C))
(: e (-> (: :y B) -- :y C))
(+fun f (-> (: :z (+ A B)) -- :z C)
  (case :z
    A [:z d]
    B [:z e]))

(+fun f (-> (: :z (+ A B)) -- :z C)
  :z {d} {e} D)

(+fun D (-> (: :z (+ A B))
            (: :d (-> (: :x A) -- :x C))
            (: :e (-> (: :y B) -- :y C))
         -- :z C)
  (case :z
    A [:z :d]
    B [:z :e]))
```

## 1.6 Finite types

``` jojo
:n Finite
3  Finite
(: C (-> 3 Finite -- Type))

(: c1 1 C)
(: c2 2 C)
(: c3 3 C)
(+fun f (-> (: :x 3 Finite) -- :x C)
  (cond [:x 1 eq?] c1
        [:x 2 eq?] c2
        [:x 3 eq?] c3))

(note
  (= [0 Finite] Falsehood)
  (= [1 Finite] Truth))
```

## 1.7 Natural numbers

``` jojo
(+type N : Type
  zero : N
  succ : (-> (. .ante N) -- N))

(: C (-> N -- Type))

(: d zero C)
(: e (-> (: :n N) :n C -- :n succ C))

(+fun f (-> (: :x N) -- :x C)
  (match :x
    zero d
    succ [:x :x .ante f e]))

(+fun f (-> (: :x N) -- :x C)
  :x d {e} R)

(+fun R (-> (: :x N)
            (: :d zero C)
            (: :e (-> (: :n N) :n C -- :n succ N))
         -- :x C)
  (match :x
    zero :d
    succ [:x .ante, :x .ante :d {:e} R, :e]))

(+fun R (-> zero C
            (-> (: :n N) :n C -- :n succ N)
            (: :x N)
            -- :x C)
  (match :x
    zero [drop]
    succ [tuck :x .ante R swap :x .ante swap apply]))
```

## [note] type constructor

- x -
  when defining a type-constructor,
  this type-constructor can be used to form dependent arrow,
  the author also shows carefully
  how to construct functions
  whose type is this dependent arrow.

  why?

## 1.8 Reflection principle

- x -
  this section is very important,
  for it shows what the author intend to achieve.

  and it shows that the use of type valued function
  is really flexible.

  and the so called Reflection principle
  is simply a closure principle.

``` jojo
(note
  equality between natural numbers)

(+fun E (-> [:m :n : N] -- Universe)
  (match [:m :n]
    [zero zero] Truth
    [zero succ] Falsehood
    [succ zero] Falsehood
    [succ succ] [:m .ante :n .ante E]))

(note
  the Universe seems specially ad hoc here
  because this equality is treated in a better way
  in the published paper)

:x : N -> :x F

(+fun F
  : (-> :x : N -- Type)
  (match :x
    zero N
    succ (-> :x.pre F -- N)))

zero F == N
zero succ F == (-> N -- N)
zero succ succ F == (-> (-> N -- N) -- N)
...
```

## 1.9 Girard's paradox

# 2 formalization of an intuitionistic theory of types
# 3 reduction of some other formal theories to the theory of type
# 4 the normalization theorem and its consequences
