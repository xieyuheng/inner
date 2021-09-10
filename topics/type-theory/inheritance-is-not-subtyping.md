# Inheritance Is Not Subtyping

## 2 Background

## 2.1 A Typed Record Calculus

A typed polymorphic lambda-calculus with records is
used to describe the typing of inheritance.

## 2.2 Records

A record is a finite mapping of labels to values.

## 2.3 Record Combination

The language supports a simple record combination operator, `with`,
that joins two records.

An analogous operator, `+`, is defined on record types.

## 2.4 Recursive Types

## 2.5 Polymorphism

## 2.6 Objects

We represent objects as records whose fields contain methods.
The methods of an object may refer to each other,
so objects are naturally viewed as mutually recursive definitions.
