# Lambda Calculus with Types

------
- Authors: Henk Barendregt, Wil Dekkers, Richard Statman
- Date: 2010
------

# Preface

The emphasis of the book is on syntax. Models are introduced only in so far they give
useful information about terms and types or if the theory can be applied to them.

One of the recurring distinctions made in the book is the difference between the implicit
typing due to Curry versus the explicit typing due to Church. In the latter case the terms
are an enhanced version of the untyped terms, whereas in the Curry theory to some of
the untyped terms a collection of types is being assigned. The book is mainly about
Curry typing, although some chapters treat the equivalent Church variant.

# Introduction

## What this book is and is not about

This monograph focuses on mathematical properties of three classes of typing for lambda terms.

Simple types, constructed freely from type atoms, cause strong normalization, subject
reduction, decidability of typability and inhabitation, undecidability of lambda definabil-
ity. There turn out to be five canonical term models based on closed terms. Powerful
extensions with respectively a discriminator, surjective pairing, operators for primitive
recursion, bar recursion, and a fixed point operator are being studied. Some of these
extensions remain constructive, other ones are utterly non-constructive, and some will
be at the edge between these two realms.

Recursive types allow functions to fit as input for themselves, losing strong normaliza-
tion (restored by allowing only positive recursive types). Typability remains decidable.
Unexpectedly α-conversion, dealing with a hygienic treatment of free and bound vari-
ables among recursive types has interesting mathematical properties.

Intersection types allow functions to take arguments of different types simultaneously.
Under certain mild conditions this leads to subject conversion, turning the filters of
types of a given term into a lambda model. Classical lattice models can be described
as intersection type theories. Typability and inhabitation now become undecidable, the
latter being equivalent to undecidability of lambda definability for models of simple
types.

## What this book could have been about

This book could have been also about dependent types, higher order types and inductive
types, all used in some of the mathematical assistants. Originally we had planned a
second volume to do so. But given the effort needed to write this book, we will probably
not do so. Higher order types are treated in Girard, Lafont, and Taylor [1989], and
Sørensen and Urzyczyn [2006]. Research monographs on dependent and inductive types
are lacking. This is an invitation to the community of next generations of researchers.
Some notational conventions

- **[Xie]**
  "Research monographs on dependent and inductive types are lacking."
  what the authors meant by a good "Research monographs"

# Part 1. Simple types

## Space of expressions

First we consider curry style lambda calculus,
it assign types to untyped lambda expressions,
we can reuse our knowledge about untyped lambda calculus.

``` js
exp := var | (var) => exp | exp(exp)
```

## [note] Concrete syntax in meta language

It is convenient to use the concrete syntax
of the target language in meta language.

But it can be ambiguous,
to disambiguate some usages of concrete syntax,
I use `<c> </c>` to mark the concrete syntax,
where "c" denotes "concrete"

## [note] Equational theory

Equivalent relation is defined by convention relation.
Convention relation is studied by reduction relation.

## Conversion relation

Equational theory of lambda-beta-eta
- reflexivity
- symmetry
- transitivity
- congruence with respect to abstraction:
  ``` js
  M == N
  -------
  (x) => M == (x) => N
  ```
- beta
  ``` js
  {(x) => M}(N) == subst(M, x, N)
  ```
- eta
  ``` js
  (x) => M(x) == M
  // if x not in free_variables(M)
  ```

two expressions M and N are equivalent, is defined by conversion relations
(beta_conversion, eta_conversion or beta_eta_conversion)
i.e. we can convert M to N by the above rules.

note that, alpha_equivalence is not explicitly handled

## Reduction relation

Conversion relations are non-directed,
we can view them as adding non-directed edges to the space of expressions

The equational theory can be analyzed by
giving beta and eta direction
and viewing them as reduction relation

``` js
beta_step(<c> {(x) => M}(N) </c>) ==> subst(M, x, N)

eta_step(<c> (x) => M(x) </c>) ==> M
// if x not in free_variables(M)
```

we can also define reduction relations:
- beta_reduction
- eta_reduction
- beta_eta_reduction

church-rosser theorem seys, we can check M and N are equal
by normalize to normal-form

reduction relations (beta_reduction, eta_reduction or beta_eta_reduction)
are confluent:

``` js
(M: exp_t, N1: exp_t, N2: exp_t) ->
(reduction(M, N1), reduction(M, N2)) ->
(Z: exp_t, reduction(N1, Z), reduction(N2, Z))
```

thus it is easy to prove:

``` js
(M: exp_t, N: exp_t) ->
conversion(M, N) ->
(Z: exp_t, reduction(M, Z), reduction(N, Z))
```

- give a algorithm to check equivalence,
  means the equivalence problem is decidable,
  but we still need to analyze the complexity of the algorithm,
  and design better ones.

## Equational theory with axioms

we can extends the judgment of conversion by axioms,

``` js
axioms: set_t([exp_t, exp_t])

axioms |- M == N
```

which means we can prove M == N by conversion plus equations in the set of axioms

axioms are called inconsistent (otherwise consistent),
if we can use  they to prove any equation.

``` js
inconsistent(axioms) :=

forall M: exp_t, N: exp_t ->
axioms |- M == N

(M: exp_t, N: exp_t) -> conversion_with_axioms(axioms, M, N)
```

For example,

``` js
consistent(set![])
inconsistent(set![K == I])
inconsistent(set![I == S])
consistent(set![I == Omega])
```

- Omega is infinite loop

## Simple types

``` js
type := atom | (type) -> type
```

the set of atom
might only have one element,
might have finitely many elements,
might have infinitely many elements,
but it can not be empty.

we assume that types are freely generated,
i.e. there are no equations between types.
(such as conversion relations)

we can view atom as variable and define:
``` js
type_subst(A: type_t, a: atom_t, B: type_t)
```

- we also need to view atom as logic variable
  when implementing type inference

since we do not have abstraction over types
(in which bound variables are introduced)
we do not have to worry about free variable v.s. bound variable

## Assigning simple types

type assignment statement (or judgment)

``` js
M: A
```

A is called predicate of the statement
M is called subject of the statement

A declaration is a type assignment with variable as subject

A basis (or typing context) is a set of declarations
with distinct variables as subjects
(like axioms for equational theory)

we can define a judgment
for proving (deriving) type assignment in context:

``` js
ctx |- M: A
```

inference rules for this judgment are:

``` js
lookup_type(ctx, x) == A
------------------------ lookup type of variable
ctx |- x: A

ctx |- f: (A) -> B
ctx |- a: A
------------------------ arrow elimination
ctx |- f(a): B

ctx.ext(x, A) |- body : B
--------------------------- arrow introduction
ctx |- (x) => body : (A) -> B
```

examples:

``` js
ctx_empty |- I : (A) -> A
ctx_empty |- K : (A) -> (B) -> A
ctx_empty |- S : ((A) -> (B) -> C) -> ((A) -> B) -> (A) -> C

x: A |- I(x) : A
x: A, y: B |- K(x, y) : A
x: (A) -> (B) -> C, y: (A) -> B, x: A |- S(x, y, z) : C
```

The `Y` and `Omega` do not have type assignment
because only expressions have normal-form can have type assignment
but Y and Omega have no normal-form

`(x) => x(x)` is in nf but does not have a type either.

example proof:
to make it easy to write (top-down writing direction)
we trun inference rule upside down

(I will not provide syntax to distinguish them, only context)

``` js
ctx_empty |- (x) => (y) => x : (A) -> (B) -> A
------ (arrow introduction)
x: A |- (y) => x : (B) -> A
------ (arrow introduction)
x: A, y: B |- x: A
------ (lookup type of variable)
QED
```

a simpler one:

``` js
ctx_empty |- (x) => x : (A) -> A
----- (arrow introduction)
x: A |- x: A
------ (lookup type of variable)
QED
```

a example with non ctx_empty at beginning:

``` js
y: A |- {(x) => x}(y) : A
----- (arrow elimination)
{ y: A |- (x) => x : (A) -> A
  ------ (arrow elimination)
  y: A, x: A |- x : A
  ------ (lookup type of variable)
  QED }
{ y: A |- y: A
  ------ (lookup type of variable)
  QED }
QED
```

we use `{ }` to denote sub-proof

we can define the following:

``` js
synth(ctx: ctx_t, t: type_t): set_t(exp_t)
check(ctx: ctx_t, exp: exp_t, t: type_t): bool_t
```

For example,

``` js
check(ctx_empty, K, <c> (A) -> (B) -> A </c>) == true
check(ctx_empty,
  <c> K(x) </c>,
  <c> (A) -> (B) -> A </c>,
) == true
```

we can define the following predicates on type:

``` js
type_depth(type_t): nat_t
type_depth(atom) = 1
type_depth(<c> (A) -> B </c>) =
  max(type_depth(A), type_depth(B)) + 1

type_rank(type_t): nat_t
type_rank(atom) = 0
type_rank(<c> (A) -> B </c>) =
  max(type_rank(A) + 1, type_rank(B))

type_order(type_t): nat_t
type_order(atom) = 1
type_order(<c> (A) ->  B </c>) =
  max(type_order(A) + 1, type_order(B))
// type_order(x) = type_rank(x) + 1
```

the definitions can be extended to context,
by map over the context and compute the max of the resulting list.

the notion of "order" comes from logic,
where dealing with elements of type 0 is done in "first order" predicate logic.
The reason is that in first-order logic one deals with domains and their elements.
In second order logic one deals with functions between first-order objects.
In this terminology 0-th order logic can be identified with propositional logic.
The notion of "rank" comes from computer science (which count from 0).

type_depth is defined to do induction proofs on type.

The asymmetry in the definition of rank is intended
because the meaning of a type like `((0) -> 0) -> 0`
is more complex than that of `(0) -> (0) -> 0`

as can be seen by looking to the inhabitants of these types:
functionals with functions as arguments versus binary functions.

we can define the following function to generate type of the same tank:

``` js
type_iter(type_t, type_t, nat_t): type_t
type_iter(A, B, 0) = B
type_iter(A, B, 1) = (A) -> B
type_iter(A, B, 2) = (A) -> (A) -> B
type_iter(A, B, x + 1) = (type_iter(A, B, x)) -> B
```

## [note] Practical type checking

- We can not use curry style
  because we need to implement infer to implement check
  and we can not infer type of curry style lambda abstraction

## The size of the set of type atoms

infinite size and one size are main focus.

if the set of atoms only contains `c`, we can define

``` js
type_gen_rank(nat_t): type_t
type_gen_rank(0) = c
type_gen_rank(x + 1) = (type_gen_rank(x)) -> type_gen_rank(0)
```

for example:

``` js
type_gen_rank(0) = c
type_gen_rank(1) = (c) -> c
type_gen_rank(2) = ((c) -> c) -> c
type_gen_rank(3) = (((c) -> c) -> c) -> c

type_gen_rank_iter(nat_t, nat_t): type_t
type_gen_rank_iter(0, k) = type_gen_rank(0)
type_gen_rank_iter(n + 1, k) = type_iter(type_gen_rank(n), type_gen_rank(0), k)
```

we also define the following functions on type:

``` js
arity(type_t): nat_t
type_arg(type_t, nat_t): type_t
type_ret(type_t): type_t
```

## Church style

so far we are developing the theory in curry style,
namely assigning types to untyped expressions.

in church style,
the set of term variables is different:
each such variable is coupled with a unique type.
in such a way that every type has
infinitely many variables coupled to it.

``` js
type := atom | (type) -> type
exp := var: type | (var: type) => exp | exp(exp)
```

`exp` can contain type

- **[Xie]**
  not about lambda abstraction,
  but about variables!

  not

  ``` js
  exp := var | (var: type) => exp | exp(exp)
  ```

  but

  ``` js
  exp := var: type | (var: type) => exp | exp(exp)
  ```

we have

``` js
synth(t: type_t): exp_t
check(exp: exp_t, t: type_t): bool_t
```

synth return only one unique `exp_t` instead of `set_t(exp_t)`
and typing context is not needed

for example:

``` js
{y: (B) -> A}(x: B) : A
(x: A) => y: (B) => A : (A) => (B) => A
(x: A) => x: A : (A) => A
```

type substitution
`type_subst(A: type_t, a: atom_t, B: type_t)`
can be extended to expression,
because expression can contain type.
`type_subst(M: exp_t, a: atom_t, B: type_t)`

## de Bruijn style

it turn out to be this is the style where
only lambda abstraction is annotated

``` js
type := atom | (type) -> type
exp := var | (var: type) => exp | exp(exp)
```

as the occurrence of bound variable in body
is implicitly typed with the same type as the bound variable
the intended meaning is correct

for closed terms the church and the de bruijn notation are isomorphic

we have

``` js
synth(ctx: ctx_t, t: type_t): exp_t
check(ctx: ctx_t, exp: exp_t, t: type_t): bool_t
```

## Simple properties and comparisons

in curry style

``` js
ctx |- M: A
```

has unique proof (or say the system is "syntax directed")

because, for a proof of:

``` js
ctx |- f: (A) -> B
ctx |- x: A
---------------------
ctx |- f(x): B
```

although A is searched (infered),
but it must just be a simple lookup in ctx,
and variable is unique in ctx.

without ctx, up to substitution this typing is still unique

in church styles

we have unicity of types

``` js
(A: type_t, B: type_t) ->
(M: exp_t, check(M, A), check(M, B)) ->
A == B
```

in de bruijn

we have unicity of types

``` js
(ctx: ctx_t, A: type_t, B: type_t) ->
(M: exp_t, check(ctx, M, A), check(ctx, M, B)) ->
A == B
```

## Comparing church style and curry style

terms in the church style
"project" to legal terms in the curry style
conversely, legal terms in curry style
can be "lifted" to terms in church style

## Beta-normal-form

in the book
`NF` means normal-form
`vNF` means normal-form start with a variable

``` js
vNF := var | vNF(NF)
NF := vNF | (x) => NF
```

we use neutral instead of `vNF`

``` js
neu := var | neu(norm)
norm := neu | (x) => norm
```

## Normal inhabitants

an implementation of the synth function
which return long-normal-form

TODO

two generator is given
one for long-normal-form
one for normal-form
but I can not understand the difference between the two
because they look like the same

## Eepresenting data types

TODO

We need implementation to play with this

# Part 2. Recursive types

# Part 3. Intersection types
