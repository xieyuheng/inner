---
title: Warren's Abstract Machine
---

# [note]

## motivation

- I am reading this because I want to implement cicada
  and the new implementing strategy
  is to use the type system as a logic programming first

- I have the following understanding :
  - a [typical] dependent type system
    in lack of reification and logic programming interfaces
  - a [typical] logic programming
    in lack of the informations
    about the names of inference rules
    and in lack of the type system

- minikanren and ukanren are cool
  but they rely on lambda

- how to do it without lambda?
  will we inevitably re-implement
  all that lambda did for us with no gain
  and make things more complicated than them need to be?

## ukanren

```cicada
var-t = uuid-t
term-t = + (var-t ...)
subst-t = dict-t (var-t term-t)
walk : -- var-t subst-t -- term-t
deep-walk : -- term-t subst-t -- term-t
// acting subst-t on term-t
//   subst-t is compose-able
/    -- a category -- a preorder
extend : -- var-t term-t subst-t -- subst-t
goal-t = -- subst-t -> stream-t (subst-t)
// but I do not use the above alias
//   to be clear about the use of lambda
// note that we have a monad here
// also note that
//   if the language is not lazy
//   to use infinite stream
//   special lambda tricks are needed
unify : -- subst-t term-t term-t -> subst-t
eqo : -- term-t term-t -> -- subst-t -> stream-t (subst-t)
// and the stream-t returned by eqo
//   can either has 1 or 0 subst
//   eqo is the primitive goal constructor
disj : // logic or
  -- subst-t -> stream-t (subst-t)
     subst-t -> stream-t (subst-t)
  -> subst-t -> stream-t (subst-t)
conj : // logic and
  -- subst-t -> stream-t (subst-t)
     subst-t -> stream-t (subst-t)
  -> subst-t -> stream-t (subst-t)
mplus : // stream append
  // not really appending
  //   because we need to interleaving the stream
  //   to avoid depth first searching
  -- stream-t (subst-t) stream-t (subst-t)
  -> stream-t (subst-t)
bind : // stream map
  -- stream-t (subst-t)
     -- subst-t -> stream-t (subst-t)
  -> stream-t (subst-t)
```

## ukanren - yoneda

```cicada
var-t = uuid-t
term-t = + (var-t ...)
subst-t = dict-t (var-t term-t)
walk : -- var-t subst-t -- term-t
deep-walk : -- term-t subst-t -- term-t
extend : -- var-t term-t subst-t -- subst-t
unify : -- subst-t term-t term-t -> subst-t

goal-t = type {
   eqo-goal-t  : -- term-t term-t -> goal-t
   disj-goal-t : -- goal-t goal-t -> goal-t
   conj-goal-t : -- goal-t goal-t -> goal-t
}
goal-apply : -- goal-t subst-t -> stream-t (subst-t)

mplus :
  -- stream-t (subst-t) stream-t (subst-t)
  -> stream-t (subst-t)
bind :
  -- stream-t (subst-t)
     -- subst-t -> stream-t (subst-t)
  -> stream-t (subst-t)
```

# 1 Introduction

- the author wish to use
  a gradual presentation of partial Prolog machines
  as shown by the titles of the chapters
  - unification
  - resolution (without backtracking)
  - backtracking

- this approach is common sense for a good programmer

# 2 Unification - Pure and Simple

## intro

- L0 -- lang-0 of term and query

- M0 -- machine-0 consist of
  - data representation
  - machine instruction

- having defined a program term p
  one can submit any query?-q
  and execution either
  1. fails if p and q do not unify
  2. or succeeds with a binding of the variables in q
     obtained by unifying it with p

## 2.1 Term representation

## 2.2 Compiling L0 queries

- x -
  a query is not compiled to a term
  but to a sequence of instructions which will build the term

# 3 Flat Resolution - without backtracking

# 4 Prolog

# 5 Optimizing the Design

# A Prolog in a Nutshell

## terms and substitutions

- in the most general form, a substitution is

```cicada
subst-t : -- term-t -> term-t
```

- subst-t is obviously compose-able as function

- we can see that
  it is indeed good to use function types
  to model our semantics

- preorder struct of subst-t can be defined by
  - a is more general than b
    if there exist x
    such that a; x = b
  - there `;` is infix notation for function composition
  - note that eqv between functions is used
    to define this relation

- the relation might be important
  - for defining most general unifier (MGU)
    as result of unification algorithm
  - and if we only want to show
    the most general subst-t to user
    as search results

- for simple function type like subst-t
  it is easy to see how to struct-lize it
  to dict-t (var-t term-t)
  and use deep-walk as subst-t apply to term-t

- but for complicated function type like goal-t above
  the solution is not obvious

## unification algorithm

- with the abstract definition of subst-t and unify
  we can formalize other semantics of our language
  such as sub-type and type-check more easily

```cicada
unify : -- u : term-t v : term-t -> s : subst-t
such-that {
  check-same (term-t) (s (u) s (v))
}
```

- u and v in `unify` is called equation
  or we can say :

```cicada
equation-t = * (term-t term-t)
```

- solution preserving translation on equations
  - decomposition
  - variable elimination

- unification algorithm
  is based on solution preserving translation
  most general unifier (MGU) should be the result

## [note] unify in searching vs. unify in checking

```cicada
// unify in searching
unify : -- term-t term-t -> subst-t
// unify in checking
unify : -- term-t term-t -> result-t (subst-t error-t)
// because we need to do error report to user
```

## prolog

### [todo] datatype

```cicada
relation-t =?

// in a mexp syntax
list-append-t:zero (null-c succ succ)
list-append-t:succ (
  cons-c (car cdr)
  succ
  cons-c (car result-cdr)
) {
  list-append-t (cdr succ result-cdr)
}
```

### interface

```cicada
query : -- list-t (relation-t) -> stream-t (subst-t)

query ([
  list-append-t (x y z)
])

relation-t :?

query : -- relation-t list-t (term-t) -> stream-t (subst-t)

relation-t = -- list-t (term-t) -> goal-t
```

### low level semantics

- when try to implement prolog by a machine
  the machinery meens much like that of a parsing machine

- without backtracking
  during a query
  the car relation-t of list-t (relation-t)
  can be stepped to
  tuple (list-t (relation-t) subst-t)
  where the list-t (relation-t) comes from
  the car relation-t's choices
  query ([A, B, C]) => query ([A1, A2, ..., B, C])

- backtracking is needed
  when choices are made
  - x -
    so called "non-deterministic choices" which means
    choices will make the machine
    a non-deterministic automaton
    where non-deterministic means that the automaton
    does not need to do choices
    (beautiful tautology)

- two non-deterministic choices are made
  in the process of querying
  1. one of an relation to rewrite
  2. among the potentially many rules
     whose head to unify with this relation

### problem

- prolog uses depth first searching
  which we should avoid

# B The WAM at a glance
