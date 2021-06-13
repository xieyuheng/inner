# Lambda calculus notation with nameless dummies, a tool for automatic formula manipulation, with application to the church-rosser theorem

author: De Bruijn
date: 1972

## info

- this paper is about about de bruijn index

## What is the problem of lambda calculus?

Manipulations in lambda calculus need to rename bound variables.

If a free variable in an expression has to be replaced by a second expression,
some free variable of the second expression might be bound by the first one.

## What are the author's three criteria for a good notation?

- (i) easy to write and easy to read for the human reader;
- (ii) easy to handle in metalingual discussion;
- (iii) easy for the computer and for the computer programmer.

Nameless lambda expression will be good for (ii) and (iii) but not (i).

Thus, we should translate from the usual system to nameless system at the input stage,
and backwards at the output stage.

## What notation does the author used to handle meta language?

Use `{ <Exp> }` to eval a `Exp`, view evaluation as a function,
its type will be `Exp -> Exp` (There is no `Value` type here).

When `x` denotes a value, `{ x }` is the value.

Examples:

```
{ reverse(pqra) } == arqp
```

The following example, means author assume implicit string concatenation:

```
P = phi
S = sigma
{ P }{ S } == phisigma
{ P }-{ S } == phi-sigma
```

Example of nested `{ }`:

```
{ reverse({ S }) } == amgis
```

## In the author's planner tree representation of lambda expression, what are depth and level?

Each variable is assigned two integers,

- depth (reference depth):
  the number of lambdas we encounter when running down
  until we meet the lambda that bound this variable.

- level:
  total number of lambdas we encounter when running down
  until we meet the root.

By the author's definitions, the last lambda also counts.

In the author's notation, free variables are viewed as bound by lambda beyond the root,
thus when depth is greater than level, we know it is a free variable.

## How to get the author's De Bruijn index from the planner tree?

In the planner tree,
- erase the variable names
- erase the integers indicating the level
- erase the bound variable names of lambda
- only keep the reference depth

no information is lost.

The result is lambda expression represented by De Bruijn index.

## How to translate lambda expression to nameless lambda expression?

from: https://en.wikipedia.org/wiki/De_Bruijn_index

consider the application

```
(λ λ 4 2 (λ 1 3)) (λ 5 1)
```

which might correspond to the following term written in the usual notation

```
(λx. λy. z x (λu. u x)) (λx. w x)
```

- Step 1, we obtain the term `λ 4 _ (λ 1 _)`,
  where the variables that are destined for substitution are replaced with `_`.

- Step 2 decrements the free variables, giving `λ 3 _ (λ 1 _)`.

- Finally, in step 3, we replace the `_` with the argument, namely `λ 5 1`;
  the first `_` is under one binder, so we replace it with `λ 6 1` (free variables increased by 1);
  the second is under two binders, so we replace it with `λ 7 1` (free variables increased by 2).
  The final result is `λ 3 (λ 6 1) (λ 1 (λ 7 1))`.

## How to translate theorems of lambda calculus to nameless lambda calculus?

TODO

## references

1. BARENDREGT, H. P.,
   Some extensional  models for combinatory logics and lambda calculi.
   Doctoral Thesis, Utrecht 1971,

2. BRUIJN, N. G. DE,
   The mathematical language AUTOMATH, its usage, and some of its extensions,
   Symposium on Automatic Demonstration
   (Versailles December 1968), Lecture Notes in Mathematics, Vol. 125, Springer-Verlag, 29-61 (1970).

3. CHURCH, A.,
   The Calculi of Lambda Conversion,
   Annals of Math. Studies, vol. 6,
   Princeton University Press, 1941.

4. CURRY, H. B. and R. FEYS,
   Combinatory Logic.
   North-Holland Publishing Company, Amsterdam 1958.

## automath

automath is dependently typed forth
