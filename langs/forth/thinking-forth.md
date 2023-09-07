---
title: thinking forth
---

# intro

# the philosophy of forth -- component

## intro

to compare and contrast Forth
with these state-of-the-art programming principles.

## an armchair history of software elegance

to outline a history of the tools and techniques
for writing more elegant programs.

from assembly-language to "high-level-language",
linear correspondence between source code
and the resulting machine instructions,
is changed to nonlinear syntax-dependent correspondence.

this is widely considered to be an invaluable step
in the progress of programming methodology.

But as we’ll see, the approach ultimately
offers more restriction than freedom.

the idea of modules [function] having "one-entry, one-exit"
is that you can unplug them,
change their innards, and plug them back in,
without screwing up the connections with the rest of the program.

## the superficiality of structure

## looking back, and forth

## component programming

## hide from whom?

## hiding the construction of data structures

## but is it a high-level language?

## the language of design

## the language of performance

## summary

## references

# analysis -- specification -- prototype

- functional specification

- design specification

- x -
  we can do specification and go top down,
  but for other kinds of problems,
  we should design the bottom interfaces first,
  and go bottom up.

- k -
  so there are different types of problems.
  how many different types are there?
  sequent2 is of which type?

- x -
  in sequent2, we should go top down,
  with the help of a set of rules.

- three techniques
  for defining and documenting the conceptual model :
  1. defining the interfaces
  2. defining the rules
  3. defining the data structures

# preliminary design / decomposition

- to refactor, thinking about :
  1. reusing
  2. changing

- x -
  I had always thought about reusing,
  but not about changing.

# detailed design / problem solving

# implementation : elements of forth style

# factoring

Factoring means organizing code into useful fragments.
To make a fragment useful,
you often must separate reusable parts from non-reusable parts.
The reusable parts become new definitions.
The non-reusable parts become arguments or parameters to the definitions.

Factor at the point where you feel unsure about your code.

Factor at the point where a comment seems necessary.

separating the computation of something from
the operation to be performed on it.

Be sure you can name what you factor.

Factor definitions to hide details that may change.

Factor functions out of definitions that display results.

Don’t factor for the sake of factoring.

# handling data : stacks and states

# minimizing control structures

# forth's effect on thinking
