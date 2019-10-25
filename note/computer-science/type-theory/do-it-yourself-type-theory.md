# Do-it-yourself Type Theory

------
- Authors: Roland Backhouse, Paul Chisholm, Erik Saaman and Grant Malcolm
- Date: 1988
------

# 2 Propositions As Types

## 2.1 The Membership Judgement Form

``` js
P type
p : P
p == q in P
P == Q as type
```

## 2.2 An Example Derivation

``` js
A type
------------------ // assumption
{ x : A --- x : A }

{ x : A --- f(x) : B }
------------------------ // lambda-introduction
[x] f(x) : A -> B

a : A
f : A -> B
-------------- // arrow-elimination
f(x) : B

a : A
---------------- // inl-introduction
inl(a) : A + B

b : B
---------------- // inr-introduction
inr(b) : A + B
```

example proof:

``` js
[f] f(inr([x] f(inl(x)))) : ((A + ((A) -> B)) -> B) -> B
```

example steps:

``` js
{ f : (A + ((A) -> B)) -> B
  ----------
  { x : A
    -----------
    inl(x) : A + ((A) -> B)
    f(inl(x)) : B
  }
  [x] f(inl(x)) : (A) -> B
  inr([x] f(inl(x))) : A + ((A) -> B)
  f(inr([x] f(inl(x)))) : B
}
[f] f(inr([x] f(inl(x)))) : ((A + ((A) -> B)) -> B) -> B
```

# 3 The Structure of the Rules

On first encounter, however, the universal reaction among computing scientists appears to
be that the theory is formidable. Indeed, several have specifically referred to the overwhelming
number of rules in the theory. On closer examination, however, the theory betrays a rich structure
-- a structure that is much deeper than is suggested by the superficial observation that types
are defined by formation, introduction, elimination and computation rules. Once recognised, this
structure considerably reduces the burden of understanding. The aim of this section is, therefore,
to convey that structure to you.

The rules defining individual type constructors can be divided into five sets.
1. A formation rule.
2. The introduction rules.
3. An elimination rule.
4. The computation rules.
5. The congruence rules.

The main contribution that we make here is to describe a scheme for inferring the elimination
rule and computation rules for a newly introduced type constructor. In other words, we show that
it suffices to provide the type formation rule and the introduction rules for a new type constructor;
together these provide sufficient information from which the remaining details can be deduced.

We have divided the discussion into three parts:
- free types
  "free" of additional equalities
- congruence types
  - quotient type
- types with information loss
  in which some information about proof objects is not recorded
  in the process of constructing the type or its elements.
  - equality type
  - subtype

- x -
  the classification of types is all about equational theory.

## 3.1 Free Type Structures

In a "free" type two canonical objects are equal
if they have the same constructor and they have equal components.

#### 3.1.1 Lists

*Formation and Introduction Rules*

``` js
A type
---------------- // list-formation
list_t(A) type

A type
---------------- // nil-introduction
nil : list_t(A)

A type
a : A
l : list_t(A)
---------------------- // cons-introduction
cons(a, l) : list_t(A)
```

It is normal to omit the premises of the formation rule from the premises of the introduction
rules. Thus the premise "A type" would normally be omitted from the nil- and cons-introduction
rules above. We shall follow this practice in the remainder of this discussion.

*Elimination Rule*

``` js
{ w : list_t(A)
  ---------
  C(w) type }
x : list_t(A)
y : C(nil)
{ a : A, l : list_t(A), h : C(l)
  ---------------------
  z(a, l, h) : C(cons(a, l)) }
--------------------------- // list-elimination
list_elim(x, y, z) : C(x)
```

``` js
list_append(l, m) = list_elim(l, m, [x, _, h] cons(x, h))

list_append : (list_t(A), list_t(A)) -> list_t(A)
list_append = [l, m] list_elim(l, m, [x, _, h] cons(x, h))

proof {
  { l : list_t(A)
    m : list_t(A)
    ---------------
    { x : A
      _ : list_t(A)
      h : list_t(A)
      ---------------
      cons(x, h) : list_t(A)
    }
    [x, _, h] cons(x, h) : list_t(A)
    list_elim(l, m, [x, _, h] cons(x, h)) : list_t(A)
  }
  ------------------------------------------
  list_append : (list_t(A), list_t(A)) -> list_t(A)
}

// jojo

list_append : { (- A list_t) (- A list_t) A list_t }
list_append = {
  [l, m]
  l m { [x, _, h] x h cons }
  list_elim
}
```

*Computation Rules*

## 3.2 More on Equality and Type Judgements

## 3.3 Congruence Types

## 3.4 Computational Redundancy and Types with Information Loss

# 4 Algorithm Design in Type Theory
