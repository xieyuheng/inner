# Do-it-yourself Type Theory

------
- Authors: Roland Backhouse, Paul Chisholm, Erik Saaman and Grant Malcolm
- Date: 1988
------

# 1 Introduction

Let us demystify Martin-Löf's theory.

# 2 Propositions As Types

- **[Xie]** Some preliminary remarks about the notation.

|-------------------------|------------------------|
| Abstraction             | `[x] f(x)`             |
| Application             | `f(x)`                 |
| Product type            | `A * B`                |
| Sum type                | `A + B`                |
| Arrow type              | `(A) -> B`             |
| Dependent function type | `forall (P, [x] Q(x))` |
| Dependent product type  | `exists (P, [x] Q(x))` |
|-------------------------|------------------------|

## 2.1 The Membership Judgement Form

``` js
P type
p : P
p == q : P
P == Q
```

## 2.2 An Example Derivation

``` js
A type
------------------ // assumption
{ x : A --- x : A }

{ x : A --- f(x) : B }
------------------------ // lambda-introduction
[x] f(x) : (A) -> B

a : A
f : (A) -> B
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

- **[Xie]**
  The classification of types is all about equational theory.

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
rules. Thus the premise `A type` would normally be omitted from the nil- and cons-introduction
rules above. We shall follow this practice in the remainder of this discussion.

*Elimination Rule*

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

To express the computation rules we need to make use of
the third judgement form in the theory -- that is, the form

``` js
p == q : P
```

- **[Xie]**
  When analysing or implementing the computation rules,
  sometimes we need to add direction to the equality judgement,
  and to view it as reduction.

The `nil-computation` rule is like the `list-elimination` rule.
Since `x` is replaced by `nil` in `list_elim(nil, y, z)`,
we replace `x : list_t(A)` in `list-elimination`
by the list of premises in `nil-introduction`.

The list of premises in `nil-introduction` is empty,
thus we simply delete `x : list_t(A)` from the `list-elimination` rule.

``` js
{ w : list_t(A) --- C(w) type }
y : C(nil)
{ a : A, l : list_t(A), h : C(l)
  ---------------------
  z(a, l, h) : C(cons(a, l)) }
---------------------------------- // nil-computation
list_elim(nil, y, z) == y : C(nil)
```

The `cons-computation` rule is like the `list-elimination` rule.
Since `x` is replaced by `cons(a, l)` in `list_elim(cons(a, l), y, z)`,
we replace `x : list_t(A)` in `list-elimination`
by the list of premises in `cons-introduction`.

The list of premises in `cons-introduction` is `{ a : A, l : list_t(A) }`.

We can just follow the type to get the right right hand side of the equality.

``` js
{ w : list_t(A) --- C(w) type }
a : A
l : list_t(A)
y : C(nil)
{ a : A, l : list_t(A), h : C(l)
  ---------------------
  z(a, l, h) : C(cons(a, l)) }
----------------------------------- // cons-computation
list_elim(cons(a, l), y, z)
== z(a, l, list_elim(l, y, z))
: C(cons(a, l))
```

### 3.1.2 Natural Numbers

### 3.1.3 Disjoint Sums

- **[Xie]** We can just say "Sum" instead of "Disjoint Sum", and say "Sum" is "Disjoint Union".

### 3.1.4 The Empty Type

## 3.2 More on Equality and Type Judgements

## 3.3 Congruence Types

## 3.4 Computational Redundancy and Types with Information Loss

# 4 Algorithm Design in Type Theory
