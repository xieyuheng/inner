---
title: Do-it-yourself Type Theory
authors: [Roland Backhouse, Paul Chisholm, Erik Saaman, Grant Malcolm]
year: 1988
---

# Abstract

> This paper provides a tutorial introduction to a constructive theory
> of types based on, but incorporating some extensions to, that
> originally developed by Per Martin-L6f. The emphasis is on the
> relevance of the theory to the construction of computer programs and,
> in particular, on the formal relationship between program and data
> structure. Topics discussed include the principle of propositions as
> types, free types, congruence types, types with information loss and
> mutually recursive types. Several examples of program development
> within the theand mutually recursive types. Several examples of
> program development within the theory are also discussed in detail.

# 1 Introduction

Let us demystify Martin-Löf's type theory.

We achieve this by observing the pattern of inference rules,
and deriving *elimination rule* and *computation rule*
from *formation rule* and *introduction rule*,
thus reduce the rules to remember when study the theory.

- **Xie**: When designing or studying a formal language,
  one has to answer three questions,
  - What are the primitives?
  - How to do composition?
  - How define new from existing? (How to do abstraction?)

  Martin-Löf had not answer the third question in his type theory.
  He did not specify a way to define new type from existing types
  (abstraction over types).

  For normal programming language,
  the answer of the third question
  can be as simple as be able to define functions.
  But for type theory to be used as logic,
  when introducing new definitions,
  we need to maintain consistent of the logic.

  When implementing type theory,
  one starts from Martin-Löf's type theory,
  and extends it by adding new types.

  To add a new type into the theory,
  one has to specify its inference rules,
  and use the inference rules to implement the type checker.

  We can abstract over this process and answer the third question for Martin-Löf's type theory.

  This paper provide some good answers.

# 2 Propositions As Types

- **Xie**: Some preliminary remarks about the notation.

  | Expression type         | Example expression |
  |-------------------------|--------------------|
  | Abstraction             | `(x) => f(x)`      |
  | Application             | `f(x)`             |
  | Product type            | `A * B`            |
  | Sum type                | `A + B`            |
  | Arrow type              | `(A) -> B`         |
  | Dependent function type | `forall (P, Q[x])` |
  | Dependent product type  | `exists (P, Q[x])` |

Examples:

- Dependent product type

  ``` cicada
  the([T: Type | T], [Nat | 1])
  the([T: Type | (T) -> T], [Nat | (x) => x])
  ```

- Dependent function type

  ``` cicada
  the((T: Type) -> (T) -> T, (T) => (x) => x)

  the(
    (A: Type, B: Type) -> (A) -> (B) -> A,
    (A, B) => (x, y) => x
  )

  the(
    (implicit A: Type, A,
     implicit B: Type, B) -> A,
    (x, y) => x
  )
  ```

- Product type:

  ``` cicada
  the(
    (implicit A: Type, implicit B: Type, Pair(A, B)) -> A,
    (pair) => car(pair)
  )

  the(
    (implicit A: Type, implicit B: Type, Pair(A, B)) -> B,
    (pair) => cdr(pair)
  )

  function pair_elim(
    implicit A: Type,
    implicit B: Type,
    target: Pair(A, B),
    motive: (Pair(A, B)) -> Type,
    f: (A, B) -> motive(target),
  ): motive(target) {
    return f(car(target), cdr(target))
  }

  function fst(
    implicit A: Type,
    implicit B: Type,
    target: Pair(A, B),
  ): A {
    return pair_elim(target, (_) => A, (a, b) => a)
  }

  function snd(
    implicit A: Type,
    implicit B: Type,
    target: Pair(A, B),
  ): B {
    return pair_elim(target, (_) => B, (a, b) => b)
  }

  same_as_chart! (implicit A: Type, implicit B: Type, Pair(A, B)) -> A [
    (pair) => car(pair),
    (pair) => fst(pair),
    fst,
  ]

  same_as_chart! (implicit A: Type, implicit B: Type, Pair(A, B)) -> B [
    (pair) => cdr(pair),
    (pair) => snd(pair),
    snd,
  ]
  ```

- Sum type:

  ``` cicada
  function either_swap(
    implicit A: Type,
    implicit B: Type,
    target: Either(A, B),
  ): Either(B, A) {
    return either_ind(
      target,
      (_) => Either(B, A),
      (left) => inr(left),
      (right) => inl(right)
    )
  }
  ```

## 2.1 The Membership Judgement Form

``` js
P type
p : P
p == q : P
P == Q
```

- **Xie**: I also use the following notations for judgement,

  ``` js
  P: type
  p: P
  Equal(P, p, q)
  Equal(Type, P, Q)
  ```

## 2.2 An Example Derivation

About variable,

``` js
A type
-------------------- // assumption
{ x : A -> x : A }
```

``` cicada
function id(implicit A: Type, x: A): A {
  return x
}
```

About arrow,

``` js
{ x : A -> f(x) : B }
------------------------ // lambda-introduction
(x) => f(x) : (A) -> B

a : A
f : (A) -> B
---------------- // arrow-elimination
f(a) : B
```

About sum type,

``` js
a : A
---------------- // inl-introduction
inl(a) : Either(A, B)

b : B
---------------- // inr-introduction
inr(b) : Either(A, B)
```

Example proof:

``` js
example : ((Either(A, (A) -> B)) -> B) -> B
example = (f) => f(inr((x) => f(inl(x))))
```

Example proof, with more detailed deduction steps:

``` js
example : { { Either(A, { A -> B }) -> B } -> B }
example = {
  f : { Either(A, { A -> B }) -> B }
  --------
  g : { A -> B }
  g = {
    x : A
    ------------
    inl(x) : Either(A, { A -> B })
    f(inl(x)) : B
  }
  inr(g) : Either(A, { A -> B })
  f(inr(g)) : B
}
```

- **Xie**: Don't forget that proof is function.

  ``` cicada
  function example(
    implicit A: Type,
    implicit B: Type,
    f: (Either(A, (A) -> B)) -> B,
  ): B {
    function g(x: A): B {
      check! inl(x): Either(A, (A) -> B)
      check! f(inl(x)): B
      return f(inl(x))
    }

    check! inr(g): Either(A, (A) -> B)
    check! f(inr(g)): B
    return f(inr(g))
  }
  ```

- **Xie**:
  The author uses the syntax `|[ ... |> ... ]|`
  for both deduction (function) and judgement (pi type).

  We may use the syntax `{ ... -> ... }` for easy of writing,
  whose argument types are fully annotated (de Bruijn style typed lambda calculus).
  The last element in conclusion is return value.

If we replace `B` by `Absurd`,
we get a proof of `not not (A + not A)`,
because `not P` is defined to be `(P) -> Absurd` in constructive mathematics.

This means although the law of the excluded middle
is not valid in constructive mathematics,
but it can never be refuted in constructive mathematics.

Other examples of propositions that are classically valid but cannot be generally established in constructive mathematics are the following:

``` js
(A -> B) + (B -> A)
(A -> (B + C)) -> ((A -> B) + (A -> C))
(not B -> not A) -> A -> B
```

Indeed it is a theorem attributed by Kleene to Glivenko that
if `P` is any tautology of the classical propositional calculus
then the proposition `not not P` is always constructively valid.

# 3 The Structure of the Rules

On first encounter, however, the universal reaction among computing scientists appears to
be that the theory is formidable. Indeed, several have specifically referred to the overwhelming
number of rules in the theory. On closer examination, however, the theory betrays a rich structure
-- a structure that is much deeper than is suggested by the superficial observation that types
are defined by formation, introduction, elimination and computation rules. Once recognised, this
structure considerably reduces the burden of understanding. The aim of this section is, therefore,
to convey that structure to you.

The rules defining individual type constructors can be divided into five sets.
- A formation rule.
- The introduction rules.
- An elimination rule.
- The computation rules.
- The congruence rules.

The main contribution that we make here is to describe a scheme for inferring the elimination
rule and computation rules for a newly introduced type constructor. In other words, we show that
it suffices to provide the type formation rule and the introduction rules for a new type constructor;
together these provide sufficient information from which the remaining details can be deduced.

We have divided the discussion into three parts.
- Free types -- "free" of additional equalities.
- Congruence types.
  - Quotient type.
- Types with information loss,
  in which some information about proof objects is not recorded
  in the process of constructing the type or its elements.
  - Equality type.
  - Subtype.

- **Xie**: The classification of types is all about equational theory.

## 3.1 Free Type Structures

In a "free" type two canonical objects are equal
if they have the same constructor and they have equal components.

### 3.1.1 Lists

*Formation and Introduction Rules*

``` js
A type
---------------- // list-formation
List(A) type

A type
---------------- // nil-introduction
nil : List(A)

A type
a : A
l : List(A)
---------------------- // cons-introduction
cons(a, l) : List(A)
```

In cicada,

``` js
List : {
  A : type
  ---------
  type
} = {
  case nil : {
    [ A : type ]
    ---------
    List(A)
  }
  case cons : {
    [ A : type ]
    a : A
    l : List(A)
    ---------
    List(A)
  }
}
```

- **Xie**: We use

  ``` js
  {
    case <name> : { ... -> ... }
    case <name> : { ... -> ... }
    ...
  }
  ```

  to specify a list of data constructors.

It is normal to omit the premises of the formation rule from the premises of the introduction
rules. Thus the premise `A type` would normally be omitted from the nil- and cons-introduction
rules above. We shall follow this practice in the remainder of this discussion.

*Elimination Rule*

The return type of the eliminator for a type constructor `A`
involves a family of types -- `C`, indexed by objects of `A`.

``` js
w : A
------------
C(w) type
```

Given a type `A`, suppose we want prove a theorem about elements of `A`,
the elimination rule of `A` help us achieve this.

The arguments of the eliminator consist of a target to eliminate,
and one function for each case of the introduction rule of the type.

There are three kinds of premises in elimination rule,
- The type premises.
- A major premise, that correspond to the target.
- The minor premises, that correspond to each induction case.

The premises of an introduction rule become assumptions in the corresponding premise of the elimination rule.

For each recursive introduction variable (such as the `l` in `cons(a, l)`),
we need to add an induction hypothesis (such as `h : C(l)`)
to the assumptions of the corresponding minor premise.

``` js
{ w : List(A)
  ------------
  C(w) type }
x : List(A)
y : C(nil)
{ a : A, l : List(A), h : C(l)
  ------------------------
  z(a, l, h) : C(cons(a, l)) }
--------------------------- // list-elimination
list_elim(x, y, z) : C(x)
```

In cicada,

``` js
list_elim : {
  [ A : type
    C : { w : List(A) -> type } ]
  x : List(A)
  y : C(nil)
  z : {
    a : A
    l : List(A)
    h : C(l)
    ---------
    C(cons(a, l)) }
  ---------
  C(x)
} = #<elim>
```

``` js
list_append(l, m) = list_elim(l, m, (x, _, h) => cons(x, h))

list_append : (List(A), List(A)) -> List(A)
list_append = (l, m) => list_elim(l, m, (x, _, h) => cons(x, h))

proof {
  { l : List(A)
    m : List(A)
    ------------------
    { x : A
      _ : List(A)
      h : List(A)
      ------------------
      cons(x, h) : List(A)
    }
    (x, _, h) => cons(x, h) : (A, List(A), List(A)) -> List(A)
    list_elim(l, m, (x, _, h) => cons(x, h)) : List(A)
  }
  list_append : (List(A), List(A)) -> List(A)
}
```

- **Xie**: In JoJo,

  ``` js
  list_append : { (- A List) (- A List) A List }
  list_append = {
    [l, m]
    l m { [x, _, h] x h cons }
    list_elim
  }
  ```

- **Xie**: In cicada,

  ``` js
  list_append : {
    l, m : List(A)
    ---------
    List(A)
  }
  list_append = {
    l : List(A)
    m : List(A)
    -----------------------
    g : { A; List(A); List(A); -> List(A) }
    g = {
      x : A
      _ : List(A)
      h : List(A)
      ------------------
      cons(x, h) : List(A)
    }
    list_elim(l, m, g) : List(A)
  }

  // Or, in one definition:

  list_append : {
    l, m : List(A)
    ---------
    List(A)
  } = {
    g : {
      x : A
      _ : List(A)
      h : List(A)
      ---------
      List(A)
    } = {
      cons(x, h) : List(A)
    }
    list_elim(l, m, g) : List(A)
  }
  ```

*Computation Rules*

To express the computation rules we need to make use of
the third judgement form in the theory -- that is, the form

``` js
p == q : P
```

- **Xie**: When analysing or implementing the computation rules,
  sometimes we need to add direction to the equality judgement,
  and to view it as reduction.

The `nil-computation` rule is like the `list-elimination` rule.
Since `x` is replaced by `nil` in `list_elim(nil, y, z)`,
we replace `x : List(A)` in `list-elimination`
by the list of premises in `nil-introduction`.

The list of premises in `nil-introduction` is empty,
thus we simply delete `x : List(A)` from the `list-elimination` rule.

``` js
{ w : List(A) -> C(w) type }
y : C(nil)
{ a : A, l : List(A), h : C(l)
  ------------------------
  z(a, l, h) : C(cons(a, l)) }
---------------------------------- // nil-computation
list_elim(nil, y, z) == y : C(nil)
```

The `cons-computation` rule is like the `list-elimination` rule.
Since `x` is replaced by `cons(a, l)` in `list_elim(cons(a, l), y, z)`,
we replace `x : List(A)` in `list-elimination`
by the list of premises in `cons-introduction`.

The list of premises in `cons-introduction` is `{ a : A, l : List(A) }`.

We can just follow the type to get the right right hand side of the equality.

``` js
{ w : List(A) -> C(w) type }
a : A
l : List(A)
y : C(nil)
{ a : A, l : List(A), h : C(l)
  ------------------------
  z(a, l, h) : C(cons(a, l)) }
----------------------------------- // cons-computation
list_elim(cons(a, l), y, z) ==
z(a, l, list_elim(l, y, z)) : C(cons(a, l))
```

### 3.1.2 Natural Numbers

``` js
------------ // nat-formation
Nat type

------------ // zero-introduction
zero : Nat

n : Nat
------------ // succ-introduction
succ(n) : Nat
```

``` js
Nat : type
Nat = {
  case zero : Nat
  case succ : { Nat -> Nat }
}
```

### 3.1.3 Disjoint Sums

- **Xie**: We can just say "Sum" instead of "Disjoint Sum",
  and say "Sum" is "Disjoint Union".

``` js
A type
B type
-------------- // sum-formation
A + B type

a : A
----------------- // inl-introduction
inl(a) : A + B

b : B
----------------- // inr-introduction
inl(b) : A + B

{ w : A + B -> C(w) type }
d : A + B
{ a : A -> e(a) : C(inl(a)) }
{ b : B -> f(b) : C(inr(b)) }
------------------------------- // sum-elimination
sum_elim(d, e, f) : C(d)

{ w : A + B -> C(w) type }
a : A
{ a : A -> e(a) : C(inl(a)) }
{ b : B -> f(b) : C(inr(b)) }
--------------------------------------------- // inl-computation
sum_elim(inl(a), e, f) == e(a) : C(inl(a))

{ w : A + B -> C(w) type }
b : B
{ a : A -> e(a) : C(inl(a)) }
{ b : B -> f(b) : C(inr(b)) }
--------------------------------------------- // inr-computation
sum_elim(inr(b), e, f) == f(b) : C(inr(b))
```

### 3.1.4 The Empty Type

``` js
----------------- // absurd-formation
Absurd type

{ w : Absurd -> C(w) type }
r : Absurd
--------------------------- // absurd-elimination
absurd_elim(r) : C(r)
```

### 3.1.5 Arrow Type

``` js
A type
B type
--------------- // arrow-formation
(A) -> B type

{ x : A -> f(x) : B }
------------------------ // lambda-introduction
(x) => f(x) : (A) -> B

a : A
f : (A) -> B
-------------- // arrow-elimination
f(a) : B

a : A
{ x : A -> f(x) : B }
---------------------------- // lambda-computation (beta-reduction)
{ (x) => f(x) } (a) == f(a) : B
```

We observe that we can not follow the pattern of "Free Type Structures" any more.
The introduction rule of arrow type (`lambda-introduction`)
is different from that of `List` or `Nat`,
for the premise `{ x : A -> f(x) : B }` has a hypothesis (`x : A`).

- **Xie**: *F-algebra* and *F-coalgebra* generalize this.
  Would it be easier to describe them in jojo?

## 3.2 More on Equality and Type Judgements

### 3.2.1 Families of Types

### 3.2.2 The Equality Type

``` js
A type
a : A
b : A
--------------------- // eqv-formation
Equal(A, a, b) type
```

- **Xie**: Note that `a == b : A` is a judgement of the system,
  while `Equal(A, a, b)` is a inductively defined type.

### 3.2.3 General Rules

``` js
a == b : A
----------------------- // eqv-introduction
refl : Equal(A, a, b)

a == b : A
----------------------- // eqv-introduction
same(a) : Equal(A, a, b)
```

- **Xie**: The `replace` rule,

  ``` js
  { w : A -> C(w) type }
  p : Equal(A, x, y)
  base : C(x)
  --------------------------
  replace(p, base) : C(y)
  ```

  This rule clearly does not follow
  the pattern of elimination rule of "Free Type Structures",
  `C` does not apply on the eliminator `replace` 's first argument `p`,
  but applies on values `x` and `y` in `p`'s type.

- **Xie**: Another elimination rule for `Equal`,

  ``` js
  { x : A, y : A, p : Equal(A, x, y) -> C(x, y, p) }
  { x : A -> s(x) : C(x, x, same(x)) }
  ------------------------------------------- // eqv-elimination
  eqv_ind(x, y, p, s) : C(x, y, p)

  { x : A, y : A, p : Equal(A, x, y) -> C(x, y, p) }
  { x : A -> s(x) : C(x, x, same(x)) }
  ------------------------------------------- // eqv-computation
  eqv_ind(x, x, same(x), s) ==
  s(x, x, same(x)) : C(x, x, same(x))
  ```

  ``` js
  eqv_ind : {
    [ C : {
        x, y : A
        p : Equal(A, x, y)
        ------
        type } ]
    x, y : A
    p : Equal(A, x, y)
    s : { x : A -> C(x, x, same(x)) }
    --------
    C(x, y, p)
  } = {
    J(x, s, p)
  }

  J : {
    [ C : {
        x, y : A
        p : Equal(A, x, y)
        --------
        type } ]
    x : A
    [ y : A ]
    s : { x : A -> C(x, x, same(x)) }
    p : Equal(A, x, y)
    --------
    C(x, y, p)
  } = {
    let same(t) = p
    d(t)
  }
  ```

  ``` js
  function eqv_ind : {
    suppose C : {
      given x, y : A
      given p : Equal(A, x, y)
      conclude type }
    given x, y : A
    given p : Equal(A, x, y)
    given s : { given x : A conclude C(x, x, same(x)) }
    conclude C(x, y, p)
  } = {
    return J(x, s, p)
  }

  function J : {
    suppose C : {
      given x, y : A
      given p : Equal(A, x, y)
      conclude type }
    given x : A
    suppose y : A
    given s : { given x : A conclude C(x, x, same(x)) }
    given p : Equal(A, x, y)
    conclude C(x, y, p)
  } = {
    let same(t) = p
    return d(t)
  }
  ```

### 3.2.4 Closure and Individuality Properties

## 3.3 Congruence Types

The equalities are specified by extra introduction rules,
which we refer to as congruence rules.
We describe congruence types in this section
by defining finite bags (multisets) and finite sets.

Bags are constructed from lists by adding a congruence rule
which identifies lists which differ only in the order of elements.
Sets are constructed from bags by identifying those bags
which differ only in the number of occurrences of elements.

### 3.3.1 Finite Bags

``` js
A type
------------ // bag-formation
Bag(A)

------------ // bag_empty-introduction
bag_empty(A)

a : A
s : Bag(A)
-------------------------- // bag_cons-introduction
bag_cons(a, s) : Bag(A)

a : A
b : A
s : Bag(A)
-------------------------- // order-congruence
bag_cons(a, bag_cons(b, s)) ==
bag_cons(b, bag_cons(a, s)) : Bag(A)
```

When defining the elimination rule,
note that a function must give equal values
when applied to equal objects.

``` js
{ w : Bag(A) -> C(w) type }
t : A
c : C(bag_empty)
{ a : A, s : Bag(A), h : C(s)
  -------------------------------------
  d(a, s, h) : C(bag_cons(a, s)) }
{ a : A, b : A, s : Bag(A), h : C(s)
  --------------------------------------
  d(a, bag_cons(b, s), d(b, s, h)) ==
  d(b, bag_cons(a, s), d(a, s, h))
  : C(bag_cons(a, bag_cons(b, s))) }
----------------------------------------- // bag-elimination
bag_elim(t, c, d) : C(t)
```

### 3.3.2 Finite Sets

### 3.3.3 The NuPrl Quotient Type

## 3.4 Computational Redundancy and Types with Information Loss

### 3.4.1 Computational Redundancy

We shall say that a type `A` *exhibits computational redundancy*
if there exists `a0` in `A` such that forall `a` in `A`
we have `a == a0 : A`.

This means such types either has one element, or has no element.
We are interested only in whether they are inhabited.

Examples are
- `Equal(A, x, y)` only has element `refl`.
- `Absurd` has no element.
- `A -> Absurd` only has element `(x) => x`.

### 3.4.2 Information Loss: The Subset Type

Information loss from an existential type (dependent product type) to a subset type.

``` js
A type
{ x : A -> B(x) type }
------------------------- // exists-formation
exists(A, B)

a : A
b : B(a)
------------------------- // exists-introduction
pair(a, b) : exists(A, B)
```

``` js
A type
{ x : A -> B(x) type }
------------------------- // subset-formation
Subset(A, B)

a : A
b : B(a)
------------------------- // subset-introduction
a : Subset(A, B)

{ w : Subset(A, B) -> C(w) }
a : Subset(A, B)
{ x : A, y : B(x) -> c(x) : C(x) }
----------------------------------- // subset-elimination
c(a) : C(a)
```

Since `Subset` has no canonical constants,
it is unnecessary to have an elimination constant.
Likewise, there are no computation rules.

Instead of discarding the second component
we might choose to discard the first component.
This would give objects of a union type.

``` js
a : A
b : B(a)
------------------  // union-introduction
b : Union(A, B)
```

An object of `Union(A, B)` is an object of some member `B(a)`
of a family of types `B(x)`, indexed by `x` in `A`,
but the information about the index has been lost.

### 3.4.3 Information Loss: The Polymorphic Function Type

We know arrow type -- `(A) -> B` can be viewed as special case of
dependent function type -- `forall (P, Q[x])`,
where `Q` does not dependent on `x`.

This can also be viewed as information loss.

The examples above suggest that
we can play a syntactic game with the type constructors we have seen so far
whereby we choose to discard individual items of information.

Two forms of polymorphism arise naturally in this way.

The first, and more general form, we shall refer to as the intersection type constructor.

``` js
A type
{ x : A -> B(x) type }
--------------------------- // intersection-formation
Intersection(A, B) type
```

The polymorphic function type may be viewed as a special case of dependent function type,
whose objects are constant functions.

``` js
{ x : A -> b(x) : B(x) }
--------------------------- // forall-introduction
(x) => b(x) : forall (A, B)

{ x : A -> b : B(x) }
--------------------------- // intersection-introduction
b : Intersection(A, B)
```

The `intersection-introduction` rule imposes the restriction that
`x` may not appear free in the expression `b`.
(It may, on the other hand,
appear free in the type expression `B(x)`.)

Thus `b` is an element of `Intersection(A, B)`
if it is an element of each type in the family `B(x)`
where `x` ranges over elements of `A`.

If some element a of type `A` is exhibited
then `b` is an element of `B(a)`.
This is expressed by the `intersection-elimination` rule.

``` js
b : Intersection(A, B)
a : A
------------- // intersection-elimination
b : B(a)
```

The polymorphic identity function is an example.

``` js
(x) => x : Intersection(univ(1), (A) => (A) -> A)
```

TODO

# 4 Algorithm Design in Type Theory

## 4.1 Solution Strategy

## 4.2 Invariants Versus Inductive Hypotheses

## 4.3 Program Development

# 5 Binary Numerals

## 5.1 Binary Numerals as a Congruence Type

## 5.2 Binary Numerals Via Information Loss

# 6 Mutually Recursive Types

## 6.1 Trees and Forests

## 6.2 CFGs and Mutually Recursive Types

## 6.3 An Application: Games Playing

# 7 Conclusion
