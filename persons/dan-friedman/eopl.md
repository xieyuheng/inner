---
title: Essentials of Programming Languages
authors: [Daniel P. Friedman, Mitchell Wand]
---

# preface

## goal

this book is an analytic study of programming languages.

our goal is to provide a deep, working understanding
of the essential concepts of programming languages.

most of these essentials relate to the semantics, or meaning, of program elements.

programs called interpreters provide the most direct,
executable expression of program semantics.

they process a program by directly analyzing
an abstract representation of the program text.

- x -
  we can view this as a handbook for
  a list of "how to implement ___?" questions.

# 1 Inductive Sets of Data

## 1.1 Recursively Specified Data

proof by structural induction -

To prove that a proposition IH(s)
is true for all structures s,
prove the following:

1. IH is true on simple structures
   (those without substructures).
2. If IH is true on the substructures of s,
   then it is true on s itself.

## 1.2 Deriving Recursive Programs

- the smaller-subproblem principle -

  If we can reduce a problem to a smaller subproblem,
  we can call the procedure that solves the problem
  to solve the subproblem.

- follow the grammar! -

  When defining a procedure
  that operates on inductively defined data,
  the structure of the program
  should be patterned after the structure of the data.

## 1.3 Auxiliary Procedures and Context Arguments

no mysterious auxiliaries!

When defining an auxiliary procedure,
always specify what it does on all arguments,
not just the initial values.

## 1.4 Exercises

# 2 Data Abstraction

## 2.1 Specifying Data via Interfaces

- All the knowledge about how the data is represented
  must therefore reside in the code of the implementation.

  The most important part of an implementation
  is the specification of how the data is represented.

- Most interfaces will contain

  1. some *constructors*
     that build elements of the data type,

  2. and some *observers*
     that extract information from values of the data type.

## 2.2 Representation Strategies for Data Types

- the interpreter recipe -

  1. look at a piece of data.
  2. decide what kind of data it represents.
  3. extract the components of the datum
     and do the right thing with them.

- x -
  interpreter of a programming language.
  interprets data which encode program.

  such data is called expression.

## 2.3 Interfaces for Recursive Data Types

designing an interface for a recursive data type -

1. Include one constructor
   for each kind of data in the data type.
2. Include one predicate
   for each kind of data in the data type.
3. Include one extractor
   for each piece of data
   passed to a constructor of the data type.

## 2.4 A Tool for Defining Recursive Data Types

## 2.5 Abstract Syntax and Its Representation

# 3 Expressions

## 3.1 Specification and Implementation Strategy

- specification will consist of assertions of the form :

  (value-of exp env) = val

  meaning that
  the value of expression exp
  in environment env
  should be val.

- We write down rules of inference and equations,
  like those in chapter 1,
  that will enable us to derive such assertions.

  We use the rules and equations by hand
  to find the intended value of some expressions.

## 3.2 LET: A Simple Language

## 3.3 PROC: A Language with Procedures

## 3.4 LETREC: A Language with Recursive Procedures

## 3.5 Scoping and Binding of Variables

## NAMELESS -- optimize searching to indexing

### 3.6 Eliminating Variable Names

- de Bruijn indices [or Lexical Address]

- This way of recording the information is useful
  because the lexical address predicts
  just where in the environment
  any particular variable will be found.

- x -
  thus, no runtime searching overhead.

### 3.7 Implementing Lexical Addressing

# 4 State

## 4.1 Computational Effects

- So far, we have only considered the *value*
  produced by a computation.
  But a computation may have *effects* as well.

  - Different from producing a value
    an effect is global, it is seen by the *entire* computation.

- We will be concerned primarily with a single effect :
  assignment to a location in memory.

  - Assignment is different from binding.
    binding is local, but variable assignment
    is potentially global.

- It is about the sharing of values between
  otherwise unrelated portions of the computation.

  1. Two procedures can share information
     if they both know about the same location in memory.

  2. A single procedure can share information
     with a future invocation of itself
     by leaving the information in a known location.

- We model memory as a finite map from *locations*
  to a set of values called the *storable values*.

  - The storable values in a language
    are typically, but not always,
    the same as the expressed values of the language.
    This choice is part of the design of a language.

- A data structure that represents a location
  is called a *reference*.

  - A location is a place in memory
    where a value can be stored,
    and a reference is a data structure
    that refers to that place.

  - References are sometimes called L-values.
    This name reflects the association
    of such data structures with variables
    appearing on the left-hand side of assignment statements.

  - Analogously, expressed values,
    such as the values of the right-hand side expressions
    of assignment statements,
    are known as R-values.

## 4.2 EXPLICIT-REFS: A Language with Explicit References

We leave the binding structures of the language unchanged,
but we add three new operations to create and use references.

1. newref
   allocates a new location and returns a reference to it.
2. deref
   dereferences a reference
   it returns the contents of the location
   that the reference represents.
3. setref
   changes the contents of the location
   that the reference represents.

## 4.3 IMPLICIT-REFS: A Language with Implicit References

The explicit reference design
gives a clear account of allocation, dereferencing,
and mutation because all these operations
are explicit in the programmer’s code.

Most programming languages take common patterns of
allocation, dereferencing, and mutation,
and package them up as part of the language.
Then the programmer need not worry about
when to perform these operations,
because they are built into the language.

## 4.4 MUTABLE-PAIRS: A Language with Mutable Pairs

## 4.5 Parameter-Passing Variations

### CALL-BY-REFERENCE

### Lazy Evaluation: CALL-BY-NAME and CALL-BY-NEED

# 5 Continuation-Passing Interpreters

## intro

In chapter 3,
we used the concept of environments
to explore the behavior of bindings,
which establish the data context
in which each portion of a program is executed.

Here we will do the same for the control context
in which each portion of a program is executed.

We will introduce the concept of a continuation
as an abstraction of the control context,
and we will write interpreters
that take a continuation as an argument,
thus making the control context explicit.

- a principle -
It is evaluation of operands,
not the calling of procedures,
that makes the control context grow.

## 5.1 A Continuation-Passing Interpreter

an environment is a representation of a function
from symbols to denoted values.

The continuation of an expression
represents a procedure
that takes the result of the expression
and completes the computation.

Tail Calls Don’t Grow the Continuation -
If the value of exp1 is returned as the value of exp2,
then exp1 and exp2 should run in the same continuation.

## 5.2 A Trampolined Interpreter

## 5.3 An Imperative Interpreter

## 5.4 Exceptions

## 5.5 Threads

THREADS

# 6 Continuation-Passing Style

## 6.1 Writing Programs in Continuation-Passing Style

## 6.2 Tail Form

## 6.3 Converting to Continuation-Passing Style

## 6.4 Modeling Computational Effects

# 7 Types

## intro

Our goal is to analyze a program
to predict whether evaluation of a program is *safe*,
that is, whether the evaluation will proceed
without certain kinds of errors.

Exactly what is meant by safety, however,
may vary from language to language.

we will consider languages that are similar to LETREC.
For these languages we say that an evaluation is safe
if and only if :

1. For every evaluation of a variable var,
   the variable is bound.

2. For every evaluation of a difference expression
   (diff-exp exp1 exp2),
   the values of exp1 and exp2 are both num-vals.

3. For every evaluation of an expression of the form
   (zero?-exp exp1),
   the value of exp1 is a num-val.

4. For every evaluation of a conditional expression
   (if-exp exp1 exp2 exp3),
   the value of exp1 is a bool-val.

5. For every evaluation of a procedure call
   (call-exp rator rand),
   the value of rator is a proc-val.

## 7.1 Values and Their Types

## 7.2 Assigning a Type to an Expression

## 7.3 CHECKED: A Type-Checked Language

## 7.4 INFERRED: A Language with Type Inference

# 8 Modules

## 8.1 The Simple Module System

## 8.2 Modules That Declare Types

## 8.3 Module Procedures

# 9 Objects and Classes

## 9.1 Object-Oriented Programming

## 9.2 Inheritance

## 9.3 The Language

## 9.4 The Interpreter

## 9.5 A Typed Language

## 9.6 The Type Checker
