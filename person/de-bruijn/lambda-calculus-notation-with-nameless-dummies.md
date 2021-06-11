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

## How to translate lambda expression to nameless lambda expression?

TODO

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
