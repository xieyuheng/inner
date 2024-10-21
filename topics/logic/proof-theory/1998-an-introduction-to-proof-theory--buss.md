---
title: an introduction to proof theory
author: buss
year: 1998
---

# proof theory of propositional logic

## intro

- propositional variable

- propositional formula formed by a set of connectives

- inputting true or false
  to a formula's variables,
  the formula can be viewed as a boolean function.

- so defined formal language is *complete*,
  if every boolean function can be defined by a formula.

- a propositional formula is a *tautology*
  or to be (classically) valid
  if its boolean function always return true.

- a propositional formula is a *satisfiable*
  if its boolean function can return true.

- if we want to decide whether a formula
  is a tautology,
  we can check if it returns true on all its input.
  this brute-force method is of O(n^2)

- propositional proof systems provide better methods.

## frege proof systems

### one inference rule

- modus ponens

``` jojo
:a : A, :f : (-> A -- B)
--------------------------
:a :f : B
```

### axiom schemes

``` jojo
(+fun * (-> :a1 : :P1 -- (-> :P2 -- :P1))
  {drop :a1})

(+fun * (-> :f1 : (-> :P1 -- :P2)
            :f2 : (-> :P1 -- (-> :P2 -- :P3))
         -- (-> :P1 -- :P3))
  {dup :f1 swap :f2 apply})

(+fun * (-> :P1 -- (+ :P1 :P2))
  nop)

(+fun * (-> :P2 -- (+ :P1 :P2))
  nop)

(+fun *
  (-> :f1 : (-> :P1 -- :P3)
      :f2 : (-> :P2 -- :P3)
   -- (-> (+ :P1 :P2) -- :P3))
  {(case dup
     :P1 :f1
     :P2 :f2)})

(+fun * (-> :P1 :P2 -- :P1)
  drop)

(+fun * (-> :P1 :P2 -- :P2)
  swap drop)

(+fun * (-> :P1 :P2 -- :P1 :P2)
  nop)
```

## the propositional sequent calculus

### inference rules

``` jojo
(-> X -- (+ Y A))
(-> A X -- Y)
------------------------ cut
(-> X -- Y)

(-> A B X -- Y)
------------------------ conj-left
(-> (* A B) X -- Y)

(-> X -- (+ Y A))
(-> X -- (+ Y B))
------------------------ conj-right
(-> X -- (+ Y (* A B)))

(-> A X -- Y)
(-> B X -- Y)
------------------------ disj-left
(-> (+ A B) X -- Y)

(-> X -- (+ Y A B))
------------------------ disj-right
(-> X -- (+ Y (+ A B)))

(-> X -- (+ Y A))
(-> B X -- Y)
------------------------ arrow-left
(-> (-> A -- B) X -- Y)

(-> A X -- (+ Y B))
------------------------ arrow-right
(-> X -- Y (-> A -- B))
```

### one axiom scheme

``` jojo
(-> :A -- :A)
```

## propositional resolution refutations

# proof theory of first-order logic

## syntax and semantics

``` jojo
(-> :x : A -- :x B)
[:x : A, :x B]
```

## hilbert-style proof systems

## the first-order sequent calculus

### inference rules

``` jojo
(-> t A, X -- Y)
------------------------ hypo-left
(-> (-> :x : :T -- :x A) X -- Y)
```

## cut elimination

## herbrand's theorem, interpolation and definability theorem

## first-order logic and resolution refutations

# proof theory of other logics

## intuitionistic logic

## linear logic
