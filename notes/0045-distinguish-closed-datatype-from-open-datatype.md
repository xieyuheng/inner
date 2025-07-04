---
title: Distinguish closed datatype from open datatype
date: 2022-10-25
---

When implementing languages, to represent essential datatype like `Exp`,
I used to switch the solution between algebraic datatype and OOP style class and subclass.

To make the right choice between these two solutions,
we need to distinguish closed datatype from open datatype:

- Closed datatypes: `Exp`, `Value`, ...
- Open datatypes: `Stmt`, ...

We should use object-oriented style for open datatypes,
and use functional style for closed datatypes.

# Nat as an extreme case

We can arrived at this conclusion by considering the `Nat` datatype,
when `Nat` is defined, it is closed, we will not add new sum types to it.

It will be absurd to implement `Nat` as an abstract class,
and to implement `Zero` & `Add1` as its subclasses.
Because we will write a lot of functions to process `Nat`,
subclassing will require us to edit the class definition very often.

# Exp vs. Stmt as typical examples

Similar for `Exp`, when it is defined, it is almost closed,
we might add new sum types to it, but not very often.

`Stmt` is considered open, it provides interfaces of our language to our users,
thus it will be changed more often,
and we will not write a lot of functions to process it.
