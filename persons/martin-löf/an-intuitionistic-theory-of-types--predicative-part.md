---
title: "An intuitionistic theory of types: predicative part"
author: Martin-Löf
year: 1973
---

# 1. informal explanation of the primitive notions

## 1.1 mathematical objects and their types

## 1.2 propositions and proofs

proposition and datatype
are two view of the same construction

- when view a type as proposition
  we only care whether it is non empty

- when view a type as datatype
  we also care about what its objects are

## 1.4 cartesian product of a family of types

``` jojo
(: B (-> A -- Type))
(-> (: :x A) -- :x B)

(+fun b (-> (: :x A) -- :x B)
  :x ...
  (note
    the author introduce this
    by so called "explicit definition".
    it would be better be introduce by examples))

(: a A)
(: a b a B)
```

## 1.5 disjoint union of a family of types

``` jojo
(* (: :x A) :x B)
(: C (-> (* (: :x A) :x B) -- Type))

(note
  this C is to demonstrate
  how should we construct the elements of a type
  which dependents on (* (: :x A) :x B)
  (note
    why this is important?
    because [:x C] is the general form
    of non trivial propositions?

    thus C is to demonstrate
    how to prove a proposition
    for objects of type (* (: :x A) :x B)

    it seems that
    to define a type constructor
    it is not sufficent to only prescribe
    how to construct its elements !

    because type constructor is not a type?))

(note
  in this version of the paper
  the author does not use general combinators anymore)

(: g (-> (: :x A) (: :y :x B) -- (* :x :y) C))
(+fun f (-> (* (: :x A) (: :y :x B)) -- (* :x :y) C)
  tuple-spread g)

(note
  (= (* (: :x A) (: :y :x B))
     (: (* :x :y) (* A :x B))))

(+fun p (note left projection)
  (-> (^ (: :A Type) (: :B (-> :A -- Type)))
      (* (: :x :A) (: :y :x :B))
      -- (: :x :A))
  tuple-spread drop)

(+fun q (note right projection)
  (-> (^ (: :A Type) (: :B (-> :A -- Type)))
      (* (: :x :A) (: :y :x :B))
      -- (: :y :x :B))
  tuple-spread swap drop)

(note example
  R for real number
  (= R [(: :x (-> Z -- Q))
        (-> (: :m :n Z)
            -- :m :n add :x :m :x sub abs
            2 :m neg power LT)])
  here R is defined by Cauchy condition.
  the author is thinking about
  Bishop's constructive analysis.)
```

## 1.3 properties

- a proposition valued function
  is called a property,
  or, in intuitionistic terminology, a species.

- thus, we also call
  a type valued function
  a species.

- if (: B (-> A -- Type))
  then [a B] is the proposition that
  a belongs to the species B.

  we call B 'a species of objects of A'

- the term 'belongs to' is used here,
  but it is different from ':'.

- examples :
  3 : N   -- 3 is a natural number
  3 Prime -- 3 is a prime number

- x -
  classcally 'natural number' and 'prime number'
  seems both are set.
  but, in type theory,
  the different notions of 'belongs to' are explicit.

  - Bishop -
    meaningful distinctions deserve to be maintained.

## 1.6 disjoint union of two types

``` jojo
(+ A B)
(: C (-> (+ A B) -- Type))

(: d (-> (: :x A) -- :x C))
(: e (-> (: :y B) -- :y C))
(+fun f (-> (: :z (+ A B)) -- :z C)
  (case :z
    A [:z d]
    B [:z e]))
```

## [note] disjoint union of two types -- broken symmetry

- x -
  (* ...) is a type-constructor
  and it is also a data-constructor
  if (: a A) (: b B)
  then (: (* a b) (* A B))

  (+ A B) is a type-constructor
  but it is not a data-constructor
  [at least not in current semantic]

  we want to say
  if (: a A) (: b B)
  then (: (+ a b) (+ A B))

  but if we view (+ a b) as one value [object]
  we must introduce non-deterministic

- k -
  There are several ways
  an algorithm may behave differently
  from run to run.
  1. A concurrent algorithm
     can perform differently on different runs
     due to a race condition.
  2. A probabilistic algorithm's behaviors
     depends on a random number generator.

- and the nondeterministic algorithms
  are often used to find an approximation to a solution,
  when the exact solution would be too costly
  to obtain using a deterministic one.

- in nondeterministic programming
  at certain points in the program (called "choice points"),
  various alternatives for program flow.
  Unlike an if-then statement,
  the method of choice between these alternatives
  is not directly specified by the programmer;
  the program must decide at run time
  between the alternatives,
  via some general method applied to all choice points.

- some alternatives may "fail,"
  backtracking might be used.

## 1.7 identity

``` jojo
(: I (-> (: :x :y :A) -- Type))
(: refl (-> (: :x :A) -- :x :x I))

(: C (-> (: :x :y :A) :x :y I -- Type))

(: g (-> (: :x :A) -- :x :x :x refl C))
(+fun f (-> (: :x :y :A) (: :z :x :y I) -- :x :y :z C)
  :x g)

(+fun f (-> [:x :y] : :A, :z : :x :y I -- :x :y :z C)
  :x g)
```

## 1.8 Finite types

## 1.9 Natural numbers

- x -
  this section shows why the author uses 'C'.
  it means
  whenever we defined a type-constructor or a type,
  we also need to prescribe how can we proof
  'for all objects of such type, property C holds'
  (-> :x : [...] -- :x C)

  this is an implicit specification [or implicit aim]
  of the design of any prover,
  i.e. be able to capture mathematical induction.

- k -
  and such type-constructors
  can be introduced un-conditionally.

  why?

  and what is the meaning of those data-constructors
  of a type-constructor?

- x -
  can we reduce such user defined type-constructors
  to other fixed type-constructors?

  I sense broken symmetry here.

  if we view (-> ... -- ...) as type-constructor
  it will be so special.

``` jojo
(+type N : Type
  zero : N
  succ : (-> (. .ante N) -- N))

(: C (-> N -- Type))

(: c zero C)
(: g (-> :n : N, :n C -- :n succ C))

(+fun f (-> :x : N -- :x C)
  (match :x
    zero c
    succ [:x .ante dup f g]))
```

## 1.10 Universes

- the abstractions described so far
  still do not allow us to types and type valued functions.

``` jojo
(note
  the type of finie sequence of natural numbers)

(* :x : N, :x F)

(+fun F (-> :x : N -- Universe)
  (match :x
    zero N1
    succ (* :x .ante F, N)))

(note
  transfinite type)

(-> :x : N -- :x G)

(+fun G (-> :x : N -- Universe)
  (match :x
    zero N
    succ (-> :x .ante G -- N)))

(note
  if we use Universe to type
  the return value of above functions
  Universe must be close under type-constructors
  such as (* ...) and (-> ... -- ...))

(note
  although Universe is closed under many type-constructors
  but we can not have (: Universe Universe))
```

## [note] the use of Universe

- x -
  the use of Universe seems un-natural to me.
  because the function body here contain so much informations
  but all these informations are lost.

- k -
  the goal is 'every type is also an object of some type'

- x -
  we should get rid of those type valued function
  the type of whose return value is Universe.

  because all informations are sunk into this Universe.

## 1.11 definitional equality

- x -
  this means during unification
  we can and should do function call.

  or before pure unification
  we must try to reduce the term.

- k -
  although it is called 'definitional equality'
  the basic relation is actually directed.

- principle -

``` jojo
a : A, A = B
--------------
a : B
```

## [note] lambda of type

- x -
  we do not have lambda of type
  we can not do definition inductively without naming.

# 2 formalization of an intuitionistic theory of types

## 2.1 terms and type symbols

the formal system we shell setup
consists of a certain number of mechanical rules
for deriving symbolic expressions of forms :
1. a : A
2. a conv b -- 'conv' denotes 'converts to'
   i.e. term reduce, computation.

thus we also have two kinds of rules,
classified by conclusion type :
1. term formation
2. c

## 2.2 variables
## 2.3 constants
## 2.4 rules for Pi -- dependent arrow
## 2.5 rules for Sigma -- dependent product
## 2.6 rules for Plus -- sum type
## 2.7 rules for I
## 2.8 rules for Nn
## 2.9 rules for N

# 3 the model of closed normal terms

the normalization theorem (for closed terms) and its consequences
