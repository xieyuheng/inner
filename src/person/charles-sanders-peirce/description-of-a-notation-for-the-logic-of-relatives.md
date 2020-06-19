# Description of a notation for the logic of relatives

------
- Author: Charles Sanders Peirce
- Date: 1870
------

- Resulting from an Amplification
  of the Conceptions of Boole's Calculus of Logic

## General Definitions of the Algebraic Signs

- *Inclusion in* or *being as small as* is a *transitive* relation.
  The consequence holds that

  If    x -< y
  and   y -< z
  then  x -< z

- *Equality* is the conjunction of being as small as and its converse.
  To say x = y is to say that x -< y and y -< x.

- *Being less than* is being as small as with the exclusion of its converse.
  To say that x < y is to say that x -< y, and that it is not true that y -< x.

- *Being greater than* is the converse of being less than.
  To say that x > y is say that y < x.

- *Addition* is an associative operation.
  That is to say,

  (x +, y) +, z == x +, (y +, z)

  - I write a comma below the sign of addition,
    except when (as is the case in ordinary algebra)
    inverse operation (subtraction) is determinative.

- Addition is a commutative operation.
  That is,

  x +, y == y +, x

- *Invertible addition* is addition the corresponding inverse of which is determinative,
  The last two formulae hold good for it, and also the consequence that

  If    x + y == z
  and   x + y' == z
  then  y == y'

- *Multiplication* is an operation which is *doubly distributive with reference to addition*.
  That is,

  x * (y +, z) == x * y +, x * z
  (x +, y) * z == x * z +, x * z

- Multiplication is almost invariably an associative operation.

  (x * y) * z = x * (y * z)

  - **[Xie]**
    I use `x * y` instead of `x y`

- Multiplication is not generally commutative.
  If we write commutative multiplication with a comma, we have

  x *, y == y *, x

- *Invertible multiplication* is multiplication whose corresponding inverse operation
  (division) is determinative.
  We may indicate this by a dot;
  and then the consequence holds that

  If    x . y == z
  and   x . y' == z
  then  y == y'

- *Functional multiplication* is the application of an operation to a function.

  sin (a * b * c) * d * e * f
  log (a * b * c, d * e * f) * g * h * i

  - **[Xie]**
    I use `f (x, y)` for function application

- The operation of *Involution* obeys the formula

  (x ^ y) ^ z == x ^ (y * z)

  - **[Xie]**
    I use `x [y]` instead of `x ^ y`

- Involution, also, follows the *indexical principle*.

  x ^ (y +, z) == (x ^ y) *, (x ^ z)

- Involution, also, satisfies the *binomial theorem*.

  (x +, y) ^ z ==
  (x ^ z) +,
  sigma (p) ((p -< z), (x ^ (z - p)) *, (y ^ p)) +,
  (y ^ z)

  - **[Xie]**
    `sigma (<var>) (<pred>, <exp>)` introduces a bound variable

- *Subtraction* is the operation inverse to addition.
  We may write indeterminative subtraction
  with a comma below the usual sign.
  Then we shall have that

  (x -, y) +, y == x
  (x - y) + y == x
  (x + y) - y == x

- TODO

## [note] Motive for category theory

- The categorical understanding of the art of abstraction
  is not achieved at the author's time.

  This kinds of pre category theory striving for abstraction
  can be used as a motivation to introduce category theory.

- in the following sections,
  Peirce provides interpretation of the abstract algebraic system,
  starting from Letters.

## Use of the Letters

- **[Xie]**
  can all relations be reduced to
  composition of relations with arity less or equal than 3?

- **[Xie]**
  the author uses single letter variable name
  and uses different fonts
  to distinguish different classes of terms,
  I can not afford to do the same,
  I will give variable longer names.

- absolute terms :
  a. animal
  b. black
  f. Frenchman
  h. horse
  m. man
  p. President of the United States Senate
  r. rich person
  u. violinist
  v. Vice-President of the United States
  w. woman

- simple relative terms :
  a. enemy
  b. benefactor
  c. conqueror
  e. emperor
  h. husband
  I. lover
  m. mother
  n. not
  o. owner
  s. servant
  w. wife

- conjugative terms :
  g. giver to -- of --
  b. betrayer to -- of --
  w. winner over of -- to -- from --
  t. transferrer from -- to --

## Numbers corresponding to Letters

## The Signs of Inclusion, Equality, etc.

## The Signs for Addition

## The Signs for Multiplication
