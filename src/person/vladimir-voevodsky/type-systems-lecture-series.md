# Type Systems Lecture Series

## 2012-08-10 -- Type Systems and Proof Assistant (Type Systems 3)

Definition of Systems of expressions.

`T(M)` is the set of S-expressions, where `M` is the set of symbols.

``` lisp
<exp> := <symbol> | (<symbol> <exp> ...)
```

For examples,

``` lisp
(A (B C D E))
```

- **[Xie]**
  This means using generic untyped expression as syntax (like S-expression of lisp),
  instead of using typed algebraic datatype as syntax (normal AST in language implementations).
  Or we can view this as a syntax framework (just like S-expression) in middle of string and AST.

It is assumed that bound variables will not repeat,
And a bound variable must occurs in the sub-tree of the quantifier that introduces it.

## 2012-09-27 -- Overview of Univalent Foundations

Definition of C-systems.

A C-system -- C, is a collection of data of the following form,

- (1) A set-level category (`object_t(C)`, `morphism_t(X, Y)`)
  - "set-level" means we care about equality instead of isomorphism between elements of set.
- (2) A function `length : object_t(C) -> nat_t`
- (3) An object `point : object_t(C)`
- (4) A map `father : object_t(C) -> object_t(C)`
- (5) A map `previous : (X : object_t(C)) -> morphism_t(X, father(X))`
- ...

- **[Xie]**
  Note that contextual pre-category will only be used to model context in type theories.

## 2012-11-21 -- Type Systems 5

## 2012-11-28 -- Type Systems 6

## 2012-12-05 -- Type Systems 7
