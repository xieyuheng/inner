---
title: C System
---

# C System

## Terminology

The C System was introduced by John Cartmell under the name "contextual categories",
but since the algebraic structure so introduced is not invariant under equivalences of categories.
The Terminology "pre-category" and "C System" was suggested by Vladimir Voevodsky.

## [John Cartmell] [1985] Generalised algebraic theories and contextual categories

- **[Xie]**
  This paper revises John Cartmell's 1978 thesis.

### 1. Introduction

- **[Xie]**
  The aim of this paper is to provide semantics to type theories by constructing algebraic structures.
  Such semantics if developed can guide the implementation of type theories.

  Given a syntactic theory (or say, logical deduction system) (such as type theory),
  we wish to find an algebraic structure as its model.

Example relationship between syntactic theory and algebraic structure,

- Propositional Theory of Classical Logic and Boolean Algebra,
- Propositional Theory of Intuitionistic Logic and Heyting Algebra,
- Single Sorted Algebraic Theory and Lawvere's notion of an Algebraic Theory,
- Equational Theory of the language of the typed lambda-calculus and Cartesian Closed Category,
- Theory of Higher Order Intuitionistic Logic and Topos,
- Coherent Theory and Grothendieck Site,

in each case there can be defined the notion of model of a given theory in a given structure,
and thus the structures provide a semantics for the theories.

We call such semantics **algebraic semantics**.

Intuitively, an algebraic semantics for a syntactic theory is "right",
if it is minimal and most general.
(or say, free from unnecessary codification.)

This intuitive concept can be captured by
categorical equivalence between
the categories of syntactic theory,
and the categories of the algebraic structure.

The claim of this paper is that the contextual categories (that will be developed)
provide us with the algebraic semantics of generalised algebraic theories.

## 2. Generalised algebraic theories

The notion of a generalised algebraic theory is a generalisation
of the notion of a many-sorted algebraic theory in just the following manner.

Whereas the sorts of a many-sorted algebraic theory are constant types
in the sense that they are to be interpreted as sets,
the sorts of a generalised algebraic theory need not all be constant types
some of them being nominated as variable types (dependent types),
in which case they are to be interpreted as families of sets.

The type or types over which the variation of a variable ranges must always be specified.

Thus, a generalised algebraic theory consists of
- (i) a set of sorts, each with a specified role either as a constant type
  or else as a variable type varying in some way,
- (ii) a set of operator symbols, each one with its argument types
  and its value type specified (the value type may vary as the argument varies),
- (iii) a set of axioms. Each axiom must be an identity between similar well-formed expressions,
  either between terms of the same possibly varying type or else between type expressions.

- **[Xie]**
  The term "generalised algebraic theories" can be viewed as
  using **class** of programming languages to formalize algebraic structures.

  An algebraic structure can be single sorted, such as `semigroup_t`, `group_t` and `ring_t` (the sort is elements), for example,

  ``` js
  class semigroup_t {
    // (i) a set of sorts
    elem_t : type
    // (ii) a set of operator symbols
    mul(x : elem_t, y : elem_t) : elem_t
    // (iii) a set of axioms
    mul_associative(x : elem_t, y : elem_t, z : elem_t) : eqv_t(mul(x, mul(y, z)), mul(mul(x, y), z))
  }
  ```

  An algebraic structure can can also be many sorted,
  such as `category_t` (the sorts are objects and arrows),

  ``` js
  class category_t {
    // (i) a set of sorts
    object_t : type
    morphism_t(object_t, object_t) : type
    // (ii) a set of operator symbols
    id(a : object_t) : morphism_t(a, a)
    compose[a : object_t, b : object_t, c : object_t](
      f : morphism_t(a, b),
      g : morphism_t(b, c),
    ) : morphism_t(a, c)
    // (iii) a set of axioms
    id_left[a : object_t, b : object_t](f : morphism_t(a, b)) : eqv_t(compose(id(a), f), f)
    id_right[a : object_t, b : object_t](f : morphism_t(a, b)) : eqv_t(compose(f, id(b)), f)
    compose_associative[a : object_t, b : object_t, c : object_t, d : object_t](
      f : morphism_t(a, b),
      g : morphism_t(b, c),
      h : morphism_t(c, d),
    ) : eqv_t(compose(f, compose(g, h)), compose(compose(f, g), h))
  }
  ```

  Note that,
  the formalization of category theory (the definition of class `category_t`),
  involves **dependent record type**,
  for the type `morphism_t` is indexed by elements of `(object_t, object_t)`.

  When thinking about providing semantics for type theory,
  the type theory we wish to talk about is a type theory
  in which mathematical practice can be naturally formalized.

  Such a type theory include Martin-Löf's type theory,
  thus this paper can also be viewed as providing algebraic semantics to Martin-Löf's type theory.

  In my view,
  to formalize mathematical practice in a natural way,
  we also need the following features,
  - Subtype (inheritance) -- to avoid re-implementation,
  - Quotient types -- which occurs very often in mathematical constructions,
  - Fulfilling type system -- to use type constructors in a flexible way.

Whenever we speak of a model of a theory `U`, without qualification,
then we shall mean a model in the usual sense, that is where
type symbols are interpreted as sets,
symbols for families of types are interpreted as families of sets,
operator symbols are interpreted as operators,
and so on.

## [Xie] Note about model theory

If the theory `U` is a class,
a model of `U` is an instance of the class.

| Programming    | Model theory | Proof theory    | Category theory |
|----------------|--------------|-----------------|-----------------|
| class          | theory       |                 |                 |
| instance       | model        |                 |                 |
| inductive type |              | inference rules | limit, adjoin   |

## 3. Examples

The first example is a theory which can be called
the theory of families of elements `b(x : A)`
of families of sets `B(x : A)`:

``` js
class U1 {
  A : type
  B(x : A) : type
  b(x : A) : B(x)
}
```

A model `M` of the theory, is an instance of the class,

``` js
M : U1

M.A : type
M.B : (x : M.A) -> type
M.b : (x : M.A) -> M.B(x)
```

Given two instances `M` and `M1` of `U1`,
then a homomorphism `f` between `M` and `M1` contains `fA`, `fB` and `fB_eqv`,

``` js
fA(M : U1, M1 : U1) : M.A -> M1.A
fB(M : U1, M1 : U1) : (a : M.A) -> M.B(a) -> M1.B(fA(M, M1)(a))
fB_eqv(M : U1, M1 : U1) : (a : M.A) ->
  the_eqv_t(
    M1.B (a),
    fB(M, M1)(a)(M.b(a)),
    M1.b(fA(a)))
```

This means that there is a generalised algebraic theory (generalised class)
whose models (instances) are just homomorphisms between the models of the given theory,

``` js
class U1F {
  M : U1
  M1 : U1
  fA(M : U1, M1 : U1) : M.A -> M1.A
  fB(M : U1, M1 : U1) : (a : M.A) -> M.B(a) -> M1.B(fA(M, M1)(a))
  fB_eqv(M : U1, M1 : U1) : (a : M.A) ->
    the_eqv_t(
      M1.B (a),
      fB(M, M1)(a)(M.b(a)),
      M1.b(fA(a)))
}
```

An example similar to the first example
we call the theory of families of families of elements `c(x : A, y : B(x))`
of families of families of sets `C(x : A, y : B(x))`:

``` js
class U2 {
  A : type
  B(x : A) : type
  C(x : A, y : B(x)) : type
  c(x : A, y : B(x)) : C(x, y)
}
```

Note that in the presentation of this theory
no harm is done if we replace the introductory rule for `C`
by the rule `C[x : A](y : B(x)) : type`,

``` js
class U2 {
  A : type
  B(x : A) : type
  C[x : A](y : B(x)) : type
  c[x : A](y : B(x)) : C(y)
}
```

this rule has the same meaning as the given rule.
The expression `C(x, y)` in the given rule depends explicitly on `x` and `y`.
We say that the expression `C(y)` in the alternative rule
depends implicitly on `x` by virtue of the explicit dependence of `y` on `x`.
In the alternative version of the theory we say that a variable has been omitted.
This is one method by which a theory may be informally presented.
This method and another can be used in an informal presentation of the theory of trees.

The theory of trees has countably many sort symbols,
no operator symbols and no axioms.
However, we chose to write the theory informally with just first few sort symbols,
one of these symbols doing the work that in a formal presentation
would be shared among countably many distinct symbols.
(`S1`, `S2`, ... would be replaced by `S(n)`)

``` js
class tree_t {
  S1 : type
  S2(x1 : S1) : type
  S3[x1 : S1](x2 : S2(x1)) : type
  S4[x1 : S1, x2 : S2(x1)](x3 : S3(x2)) : type
  ...
}
```

`S1`, then, is a symbol denoting the set of nodes at the base of the tree,
If `x` is any node of the tree,
then `S2(x)` is the set of successor nodes to `x`, and so on.

The same methods can be used in presenting the theory of functors informally.

``` js
class functor_t {
  dom : category_t
  cod : category_t
  map(a : dom.object_t) : cod.object_t
  fmap(f : dom.morphism_t(a, b)) : cod.morphism_t(map(a), map(b))
  fmap_respect_then(
    f : dom.morphism_t(a, b),
    g : dom.morphism_t(b, c),
  ) : the_eqv_t(
    cod.morphism_t(map(a), map(c)),
    fmap(dom.compose(f, g)),
    cod.compose(fmap(f), fmap(g)))
  fmap_respect_id(a : dom.object_t) : the_eqv_t(
    cod.morphism_t(map(a), map(a)),
    fmap(dom.id(a)),
    cod.id(map(a)))
}
```

The next example shows how to axiomatise the disjoint union of a family of types.

If `U` is a theory which includes a type symbol `A`
and a symbol `B` for an `A`-indexed family of types,
then `U` can be extended by three operator symbols, three axioms
and one type symbol `sigma_t(A, B)` in such a way that
- (i) every model `M` of `U` uniquely extends to a model of the extended theory
- (ii) every model `M` of the extended theory interprets the symbol `sigma_t(A, B)`
  by a set isomorphic to the set `{ (a, b) | a : M.A and b : M.B(a) }`,
  that is to say as a set isomorphic to the disjoint union of the family of sets interpreting `B`.
The extended theory is taken to be `U+`.

- **[Xie]** I will define `sigma_t` as a type as the following.

``` js
type sigma_t(A : type, B : A -> type) {
  pair(x : A, y : B(x)) : sigma_t(x, y)
}

fst[x : A, y : B(x)](z : sigma_t(x, y)) : A
fst(pair(x, y)) = x

snd[x : A, y : B(x)](z : sigma_t(x, y)) : B(x)
snd(pair(x, y)) = y

pair(fst(z), snd(z)) == z
fst(pair(x, y)) == x
snd(pair(x, y)) == y
```

Finally an example based upon program language semantics.
Morris [13] and others have noted how context free grammars give rise to many sorted signatures.
In Mosses [14] this leads to a way of expressing the semantics of a programming language
by means of equations between well-formed terms of the corresponding signature.
This kind of semantics is an algebraic semantics in the sense used in this paper.
It is initial (in the category theory sense) among notions of semantics.

The example is of a very simple typed programming language.
It has typed variables and expressions and assignment and sequencing.
It demonstrates how sort structures of the general kind
can be used to express the constraints of typing.

It should be noted that `ty_t` in the programming language
and `type` in the metalanguage mean different things.

``` js
class simple_typed_language_t {
  program_t : type
  ty_t : type
  exp_t(t : ty_t) : type
  var_t(t : ty_t) : type

  seq(p1 : program_t, p2 : program_t) : program_t
  write[t : ty_t](v : var_t(t), e : exp_t(t)) : program_t
  read[t : ty_t](v : var_t(t)) : program_t
  bool_ty : ty_t
  true : exp_t(bool_ty)
  false : exp_t(bool_ty)
  and(b1 : exp_t(bool_ty), b2 : exp_t(bool_ty)) : exp_t(bool_ty)
  if(b : exp_t(bool_ty), p1 : program_t, p2 : program_t) : program_t

  // and so on
}
```

## 4. Predicates as types

It is possible to introduce sort symbols into a generalised algebraic theory
and then to axiomatise them in such a way as they are effectively predicate symbols.

We do not work with relations directly
but rather with their characteristic families.

- **[Xie]** Which means that we define relations by predicates on tuple.

- **[Xie]**
  This chapter is about types that has either only one or none instance.

  Such types are called "prop" or "proposition" by some authors,
  because "a type has either only one or none instance"
  is like "a proposition is either true or false".

  The only information conveyed by a element (the only element) of a proposition-like type
  is its existence, other than its existence no extra information is conveyed.

  It is irrelevant how we proved a proposition-like type,
  all that matters is that we proved it.

  The `eqv_t` type is an important example of such types.

The following theory (class) has as models (instances)
just characteristic families of n-ary relations on a set.

``` js
class _ {
  A : type
  P(x1 : A, x2 : A, ..., xn : A) : type

  P_prop[x1 : A, x2 : A, ..., xn : A](
    y1 : P(x1 : A, x2 : A, ..., xn : A),
    y2 : P(x1 : A, x2 : A, ..., xn : A),
  ) : eqv_t(y1, y2)
}

// We may have this `prop` built-in the language.

class _ {
  A : type
  P(x1 : A, x2 : A, ..., xn : A) : prop
}
```

- **[Xie]** The term **universal conditionals** is introduced by the author,
  which might be understood as those axioms of predicates calculus
  such as `A1 & A2 & ... & An -> P`,
  that we plan to formalize by means of `prop`.

Three kinds of universal conditionals -- `A1 & A2 & ... & An -> P`,

- (1) Where `A1, A2, ..., An, P : prop`,
  for example the transitivity of a binary predicate `P`
  is expressed by the introduction of a new `transitive_t`,

  ``` js
  A : type
  P(x1 : A, x2 : A) : prop
  transitive_t[x1 : A, x2 : A, x3 : A](
    y1 : P(x1, x2),
    y2 : P(x2, x3),
  ) : P(x1, x3)
  ```

  The point is that once `P` is interpreted,
  then `transitive_t` is interpretable in at most one way
  and then only in case the interpretation of `P` is transitive.

  - **[Xie]** The above remark about model theory
    is always ensured by the relation between a class and its instances in our language.
    Is it so?

- (2) Where `A1, A2, ..., An : prop` and `P : eqv_t`,
  for example the anti-symmetry of a binary predicate `P`,

  ``` js
  A : type
  P(x1 : A, x2 : A) : prop
  anti_symmetric_t[x1 : A, x2 : A](
    y1 : P(x1, x2),
    y2 : P(x2, x1),
  ) : eqv_t(x1, x2)
  ```

- (3) Where one of `A` is of type `eqv_t`,
  the definition of `eqv_t` captures the notion of built-in equivalence,
  because of the same variable occurred twice.

  ``` js
  prop eqv_t[A : type](p : A, A) {
    refl : eqv_t(p, p)
  }
  ```

  for example the theory of a one-to-one function `f : A -> B`,

  ``` js
  A : type
  B : type
  f(x : A) : B
  one_to_one[x1 : A, x2 : A](
    y : eqv_t(f(x1), f(x2))
  ) : eqv_t(x1, x2)
  ```

## 5. Context diagrams

TODO

## 6. Essentially algebraic theories and categories with finite limits

## 7. The generality of the algebraic semantics

## 8. The formal definition

## 9. The substitution lemma

## 10. Informal syntax

## 11. Models and homomorphisms

## 12. The category GAT

## 13. Contexts and realisations

## 14. Contextual categories

## 15. Fundorial semantics

## 16. Universal algebra
