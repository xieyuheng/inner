---
title: Generalised algebraic theories and contextual categories
---

# Generalised algebraic theories and contextual categories

------
- Author: John Cartmell
- Date: 1985
------

## [Note] Terminology

Contextual pre-category can be used to provide algebraic semantics **dependent record type**,
which dependent record type is understood as "dependent type + structural subtyping (of record type)".

Contextual pre-category is also called C System.

The contextual pre-category was introduced by John Cartmell under the name "contextual category",
but since the algebraic structure so introduced is not invariant under equivalences of categories
(because contextual functor need to preserve more structures).

The Terminology "pre-category" and "C System" was suggested by Vladimir Voevodsky.

- **[Xie]** I use the term **contextual pre-category**,
  to suggest that there are other kinds of pre-categories,
  such as **substitutional pre-category**.

  Unlike [enriched category](https://en.wikipedia.org/wiki/Enriched_category), which add structure to homsets,
  pre-category add structure to objects.

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
    mul : { x, y : elem_t -> elem_t }
    // (iii) a set of axioms
    mul_associative : {
      x, y, z : elem_t
      ---------
      eqv_t(mul(x, mul(y, z)), mul(mul(x, y), z))
    }
  }
  ```

  An algebraic structure can can also be many sorted,
  such as `category_t` (the sorts are objects and arrows),

  ``` js
  class category_t {
    // (i) a set of sorts
    object_t : type
    morphism_t : { dom, cod : object_t -> type }
    // (ii) a set of operator symbols
    id : { a : object_t -> morphism_t(a, a) }
    compose : {
      [ a, b, c : object_t ]
      f : morphism_t(a, b)
      g : morphism_t(b, c)
      ---------
      morphism_t(a, c)
    }
    // (iii) a set of axioms
    id_left : {
      [ a, b : object_t ]
      f : morphism_t(a, b)
      ---------
      eqv_t(compose(id(a), f), f)
    }
    id_right : {
      [ a, b : object_t ]
      f : morphism_t(a, b)
      ---------
      eqv_t(compose(f, id(b)), f)
    }
    compose_associative : {
      [ a, b, c, d : object_t ]
      f : morphism_t(a, b)
      g : morphism_t(b, c)
      h : morphism_t(c, d)
      ---------
      eqv_t(compose(f, compose(g, h)), compose(compose(f, g), h))
    }
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
  B : { x : A -> type }
  b : { x : A -> B(x) }
}
```

A model `M` of the theory, is an instance of the class,

``` js
M : U1

M.A : type
M.B : { x : M.A -> type }
M.b : { x : M.A -> M.B(x) }
```

Given two instances `M` and `M1` of `U1`,
then a homomorphism `f` between `M` and `M1` contains `fA`, `fB` and `fB_eqv`,

``` js
M, M1 : U1
fA : {
  a : M.A
  ---------
  M1.A
}
fB : {
  a : M.A
  b : M.B(a)
  ---------
  M1.B(fA(a))
}
fB_eqv : {
  M, M1 : U1
  a : M.A
  ---------
  the_eqv_t(
    M1.B (a),
    fB(a)(M.b(a)),
    M1.b(fA(a)))
}
```

This means that there is a generalised algebraic theory (generalised class)
whose models (instances) are just homomorphisms between the models of the given theory,

``` js
class U1F {
  M, M1 : U1
  fA : {
    a : M.A
    ---------
    M1.A
  }
  fB : {
    a : M.A
    b : M.B(a)
    ---------
    M1.B(fA(a))
  }
  fB_eqv : {
    M, M1 : U1
    a : M.A
    ---------
    the_eqv_t(
      M1.B (a),
      fB(a)(M.b(a)),
      M1.b(fA(a)))
  }
}
```

An example similar to the first example
we call the theory of families of families of elements `c(x : A, y : B(x))`
of families of families of sets `C(x : A, y : B(x))`:

``` js
class U2 {
  A : type
  B : { x : A -> type }
  C : { x : A; y : B(x) -> type }
  c : { x : A; y : B(x) -> C(x, y) }
}
```

Note that in the presentation of this theory
no harm is done if we replace the introductory rule for `C`
by the rule `C[x : A](y : B(x)) : type`,

``` js
class U2 {
  A : type
  B(x : A) : type
  B : { x : A -> type }
  C : { [x : A] y : B(x) -> type }
  c : { [x : A] y : B(x) -> C(x) }
}
```

this rule has the same meaning as the given rule.
The expression `C(x, y)` in the given rule depends explicitly on `x` and `y`.
We say that the expression `C(y)` in the alternative rule
depends implicitly on `x` by virtue of the explicit dependence of `y` on `x`.
In the alternative version of the theory we say that a variable has been omitted.
This is one method by which a theory may be informally presented.
This method and another can be used in an informal presentation of the theory of trees.

- **[Xie]** The so called "theory of trees" will be used to define contextual category,
  to formalize the relation "definition of the type `X` depends on elements of type `Y`".
  It is actually the theory of dependent record type,
  or say, dependent type + structural subtyping.

The theory of trees has countably many sort symbols,
no operator symbols and no axioms.
However, we chose to write the theory informally with just first few sort symbols,
one of these symbols doing the work that in a formal presentation
would be shared among countably many distinct symbols.
(`S1`, `S2`, ... would be replaced by `S(n)`)

``` js
class tree_t {
  S1 : type
  S2 : { x1 : S1 -> type }
  S3 : { [x1 : S1] x2 : S2(x1) -> type }
  S4 : { [x1 : S1; x2 : S2(x1)] x3 : S3(x2) -> type }
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
  map : { a : dom.object_t -> cod.object_t }
  fmap : {
    [ a, b : dom.object_t ]
    f : dom.morphism_t(a, b)
    ---------
    cod.morphism_t(map(a), map(b))
  }
  fmap_respect_compose : {
    f : dom.morphism_t(a, b)
    g : dom.morphism_t(b, c)
    ---------
    the_eqv_t(
      cod.morphism_t(map(a), map(c)),
      fmap(dom.compose(f, g)),
      cod.compose(fmap(f), fmap(g)))
  }
  fmap_respect_id : {
    a : dom.object_t
    ---------
    the_eqv_t(
      cod.morphism_t(map(a), map(a)),
      fmap(dom.id(a)),
      cod.id(map(a)))
  }
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
sigma_t : {
  A : type
  D : { A -> type }
  ---------
  type
}
sigma_t = {
  case sigma : {
    x : A
    y : D(x)
    ---------
    sigma_t(x, y)
  }
}

sigma_fst : {
  [ A : type
    D : { A -> type } ]
  sigma_t(A, D)
  ---------
  A
}
sigma_fst(sigma(x, _)) = x

sigma_snd : {
  [ A : type
    D : { A -> type } ]
  si : sigma_t(A, D)
  ---------
  D(sigma_fst(si))
}
sigma_snd(sigma(_, y)) = y

sigma(sigma_fst(z), sigma_snd(z)) == z
sigma_fst(sigma(x, y)) == x
sigma_snd(sigma(x, y)) == y
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
  exp_t : { t : ty_t -> type }
  var_t : { t : ty_t -> type }

  seq : { p1, p2 : program_t -> program_t }
  write : {
    [ t : ty_t ]
    v : var_t(t)
    e : exp_t(t)
    ---------
    program_t
  }
  read : {
    [ t : ty_t ]
    v : var_t(t)
    ---------
    program_t
  }
  bool_ty : ty_t
  true : exp_t(bool_ty)
  false : exp_t(bool_ty)
  and : {
    b1, b2 : exp_t(bool_ty)
    ---------
    exp_t(bool_ty)
  }
  if : {
    b : exp_t(bool_ty)
    p1, p2 : program_t
    ---------
    program_t
  }

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
class {
  A : type
  P : { x1, x2, ..., xn : A -> type }

  P_prop : {
    [ x1, x2, ..., xn : A ]
    y1 : P(x1, x2, ..., xn)
    y2 : P(x1, x2, ..., xn)
    ---------
    eqv_t(y1, y2)
  }
}

// We may have this `prop` built-in the language.

class {
  A : type
  P : { x1, x2, ..., xn : A -> prop }
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
  P : { x1, x2 : A -> prop }
  transitive_t : {
    [ x1, x2, x3 : A ]
    y1 : P(x1, x2)
    y2 : P(x2, x3)
    ---------
    P(x1, x3)
  }
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
  P : { x1, x2 : A -> prop }
  anti_symmetric_t : {
    [ x1, x2 : A ]
    y1 : P(x1, x2)
    y2 : P(x2, x1)
    ---------
    eqv_t(x1, x2)
  }
  ```

- (3) Where one of `A` is of type `eqv_t`,
  the definition of `eqv_t` captures the notion of built-in equivalence,
  because of the same variable occurred twice.

  ``` js
  eqv_t : {
    [ A : type ]
    p : A
    q : A
    ---------
    prop
  } = {
    case refl : eqv_t(p, p)
  }
  ```

  for example the theory of a one-to-one function `f : A -> B`,

  ``` js
  A : type
  B : type
  f : { x : A -> B }
  one_to_one : {
    [ x1, x2 : A ]
    y : eqv_t(f(x1), f(x2))
    ---------
    eqv_t(x1, x2)
  }
  ```

## 5. Context diagrams

A method of diagrams is introduced informally in this section
as an alternative way of expressing the signature of a theory.
In this way we hope to motivate an understanding
of the algebraic semantics of our theories.

- **[Xie]** A new notation is introduced (like the arrow of category theory).
  I will use `-:>` for this new kind of arrow,
  `X -:> Y` can be read as
  "definition of the type `X` depends on elements of type `Y`".

`X -:> Y` in a diagram means variously,
- (1) Notion `X` is relative to notion `Y`.
  Mention of `X` only makes sense in the context of a mention of `Y`.
- (2) Data in `X` is organised (or indexed) by data in `Y`.
- (3) `X` is a type that varies over `Y`.
  What it means to be an `X` depends on which `Y`.

Arrows of both kinds represent functional relationships.
In the case of the `-:>` arrow the relationship is **analytical**
(a term introduced by Kant in his "Critique of Pure Reason").

An example theory of language, looks like this.

``` js
class {
  notion_of_language_t : type
  natural : notion_of_language_t
  language_t : {
    x : notion_of_language_t
    ---------
    type
  }
  sentence_t : {
    [ x : notion_of_language_t ]
    l : language_t
    ---------
    type
  }
  dialect_t : {
    x : language_t(natural)
    ---------
    type
  }
  country_t : type
  otficial_tongue : {
    c : country_t
    ---------
    language_t(natural)
  }
  english : language_t(natural)
  q : sentence_t(english)
}
```

We have the following `-:>` relations,

``` js
sentence_t -:> language_t -:> notion_of_language_t
dialect_t -:> language_t(natural)
```

We use `*` notation to represent particular cases of notions,
`a * B` is defined when `a` is a particular instance of some `A`
and `B` is a notion dependent on `A`.
`a * B` is the particular case of `B`
determined by particular instance `a` of `A`.

For example,

``` js
// English is a natural language
english : language_t(natural)
// the author writes
english : natural * language_t

// `q` is a sentence of the English natural language
q : sentence_t(english)
// the author writes
q : natural * language_t * sentence_t
// we might also write
q : english * sentence_t
```

The homomorphism example `U1F` from Section 3,
can be expressed by the following diagram,

``` js
fB : M.B -> M1.B
  M.B -:> M.A
  M1.B -:> M1.A
fA : M.A -> M1.A
```

## 6. Essentially algebraic theories and categories with finite limits

The essentially algebraic theories of Freyd can be seen to have
the same descriptive power as generalised algebraic theories,
at least as far as the usual set valued models are concerned.

- "Aspects of topoi", by Peter Freyd, 1972.

The essentially algebraic theories are many sorted **partial** algebraic theories.

- **[Xie]** This "essentially algebraic theory" and the use of partial function,
  is further developed in Vladimir Voevodsky's paper,
  "Subsystems and regular quotients of C-systems".

- **[Xie]** When try to formalize category theory
  in a programming language without dependent type,
  one often come up with such **partial** theories
  (class with partial functions).

  ``` js
  class category_t {
    object_t : type
    morphism_t : type
    id : { object_t -> morphism_t }
    dom, cod : { morphism_t -> object_t }
    compose : {
      f, g : morphism_t
      ensure { cod(f) == dom(g) }
      ---------
      morphism_t
    }
  }
  ```

  the above use of `ensure` can be viewed as
  contract system of some language.

- **[Xie]** In a mainstream programming language,
  one can use the following trick to handle the assertion.

  ``` js
  class category_t {
    object_t : type
    morphism_t : type
    id : object_t -> morphism_t
    dom, cod : morphism_t -> object_t
    compose_pure : (f, g : morphism_t) -> morphism_t
    compose : (f, g : morphism_t) -> morphism_t = {
      assert(cod(f) == dom(g))
      compose_pure(f, g)
    }
  }
  ```

  an instance of `category_t` would have to provide an implementation of `compose_pure`,
  the `compose` function with assertion will be generated for the instance.

In order to write an essentially algebraic theory as generalised algebraic,
all the equality predicates used in defining domains of partial operations must be introduced.

- **[Xie]** The above definition of `category_t` uses the equality predicate over `object_t`.

Every essentially algebraic theory can be rewritten as generalised algebraic,
and the converse is also the case.

Because indexed family of sets can be viewed as
inverse image (let's say "coimage") of function.

Given a function `f`,
its coimage is an indexed family of sets,

``` js
// given
A, C : type
f : { C -> A }
// we have
coimage(f) : { a : A -> type }
// `coimage(f)` this is an indexed family of sets
coimage(f, a) <: C
// where `<:` is the subtype relation
```

Given an indexed family of sets,
it is the coimage of some function `f`,

``` js
A : type
D : { a : A -> type }

sigma_fst : {
  [ A : type
    D : { A -> type } ]
  sigma_t(A, D)
  ---------
  A
}

// `sigma_fst` is the function,
// whose coimage on `a` is the set `D(a)`
```

- **[Xie]** Let's take `category_t` as a concrete example,
  in so called essentially algebraic theory,
  the dependent type `morphism_t : { dom, cod : object_t -> type }`
  is replaced by `morphism_t : type`
  and `domcod : { morphism_t -> (object_t, object_t) }`
  the coimage of `domcod` is the original dependent type,
  and `domcod` is factored into `dom, cod : morphism_t -> object_t`.

The notion of an essentially algebraic theory can be seen as a notion of type theory
in which the only type forming principles are for the formation of product types
and for the formation of types of the form `{x : A | t1 == t2}`,
where A is a type and `t1` and `t2` are terms of the same type.
Now, if we think of the objects of an arbitrary category as types,
then to have these two type forming principles is
just to have finite products and equalisers of pairs.
Since a category with finite products and equalisers of pairs
is precisely a category with finite limits,
the notions of essentially algebraic theory
and category with finite limits are closely connected.

In fact for every essentially algebraic theory `U`
there is a category with finite limits `C(U)`
such that the category of models of `U`
is equivalent to the category `LEX(C(U), set)`
of all finite limit preserving functors from `C(U)` to `set`.

## 7. The generality of the algebraic semantics

One of the advantages of generalised algebraic over essentially algebraic
is to be found in the syntax particularly with regard to the presentation of theories.
In presenting theories as essentially algebraic there is a coding process in that,
in general, families of sets indexed by a set
are represented by functions with codomain that set.
On the other hand, in presenting a theory as generalised algebraic
there need be no such coding.

The notion of type is adequately captured by the notion of object of category.

Suppose `cat_t` is the category of all small categories,
if `A` is a small category, a functor `B : A -> cat_t`
is an `A`-indexed family of categories.
(Note that `B` is not a morphism in `cat_t`.)

A homomorphism from `B : A -> cat_t` to `B' : A' -> cat_t`
is describable just as a pair `(F, eta)`,
where `F : A -> A'` is a functor
and `eta : B -> F o B'` is a natural transformation.

- **[Xie]** Above structure need examples.

## 8. The formal definition

- **[Xie]** The formal definition starts from low level,
  thus "well-formed" expression is hard to define.

  We can avoid this by define well-formed expression as abstract syntax (`exp_t`),
  and handle concrete syntax separately.

## 9. The substitution lemma

The substitution lemma states that the set of derived rules of a theory is closed
under the operation of substitution of correctly typed terms for variables.

## 10. Informal syntax

There is a discrepancy between the syntax adopted in the formal definition of Section 8
and the syntax used in informally presenting theories in other sections.

- **[Xie]** We do not have this problem.

## 11. Models and homomorphisms

The theory of trees was presented in Section 3
but from now on we want all our trees to be trees with a unique least element.

- **[Xie]** Preparing for the definition of contextual category.

Given a theory (class) `C`,
models (instances) of `C` and homomorphisms of `C`,
forms a category (can be used to define an instance of `category_t`).

## 12. The category GAT

The category GAT of generalised algebraic theories
is a category which has theories as objects
and equivalence classes of interpretations as morphisms.

- **[Xie]** We can define `class_category` as an instance of `category_t`,
  where `object_t = class`, and `morphism_t(A, B) = { A -> B }`.
  The morphisms (interpretations) is defined as functions in the meta-language,
  it is ensured that they preserve the structure of class,
  because axioms of structure is part of the definition of class.

## 13. Contexts and realisations

- **[Xie]** A class is defined as a dependent record,
  thus a dependent record can be viewed as a class.

  The realisation between two record types `(A, B)` described in this section,
  can be viewed as the `functor_category_t(A, B)`.

``` js
class A {
  x1 : X1
  ...
  xn : Xn
}

class A* {
  y1 : Y1
  ...
  ym : Ym
}

class B extends A* {
  y1 : Y1
  ...
  ym : Ym
  ym+1 : Ym+1
  ...
  ym+w : Ym+w
}

f : { (x1, ..., xn) : A -> A* } = new A* {
  y1 : Y1 = f1(x1, ..., xn)
  ...
  ym : Ym = fm(x1, ..., xn)
}

// then we have pullback
class f*B extends A {
  x1 : X1
  ...
  xn : Xn
  let (y1, ..., ym) = f(x1, ..., xn)
  ym+1 : Ym+1[y1, ..., ym]
  ...
  ym+w : Ym+w[y1, ..., ym]
}

q(f, B) : { (x1, ..., xn, ym+1, ..., ym+w) : f*B -> B } = new B {
  (y1, ..., ym) = f(x1, ..., xn)
  (ym+1, ..., ym+w) = (ym+1, ..., ym+w)
}
```

- **[Xie]** Maybe we can intuitive `q(f, B) : f*B -> B` as
  substitute `(x1, ..., xn)` in `f*B` with `(y1, ..., ym) = f(x1, ..., xn)`.

This motivates the next definition.

## 14. Contextual categories

The structural subtyping `A <: B` can be defined as `B -:> A`,

TODO

## 15. Fundorial semantics

- **[Xie]**
  Given a type theory `T` and an algebraic structure (a class) `C`,
  the algebraic semantics is using `T` to build an instance of `C`.

  I do not think the above statement is enough?

TODO

## 16. Universal algebra

TODO
