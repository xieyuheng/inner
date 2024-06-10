---
title: Revised Report on the Propagator Model
authors: [Alexey Andreyevich Radul, Gerald Jay Sussman]
year: 2011
source: "https://groups.csail.mit.edu/mac/users/gjs/propagators"
---

# Abstract

> In the past year we have made serious progress on elaborating the
> propagator programming model [^2][^3]. Things have gotten serious
> enough to build a system that can be used for real experiments.

> The most important problem facing a programmer is the revision of an
> existing program to extend it for some new situation. Unfortunately,
> the traditional models of programming provide little support for
> this activity. The programmer often finds that commitments made in
> the existing code impede the extension, but the costs of reversing
> those commitments are excessive.

这篇论文可以看作是 "Software Design for Flexibility" 的前兆了。

> Such commitments tend to take the form of choices of strategy. In
> the design of any significant system there are many implementation
> plans proposed for every component at every level of
> detail. However, in the system that is finally delivered this
> diversity of plans is lost and usually only one unified plan is
> adopted and implemented. As in an ecological system, the loss of
> diversity in the traditional engineering process has serious
> consequences.

> The Propagator Programming Model is an attempt to mitigate this
> problem. It is a model that supports the expression and integration
> of multiple viewpoints on a design. It incorporates explicit
> structure to support the integration of redundant pieces and
> subsystems that solve problems in several different ways. It will
> help us integrate the diversity that was inherent in the design
> process into the delivered operational product.

> The Propagator Programming Model is built on the idea that the basic
> computational elements are autonomous machines interconnected by
> shared cells through which they communicate. Each machine
> continuously examines the cells it is interested in, and adds
> information to some based on computations it can make from
> information from the others. Cells accumulate information from the
> propagators that produce that information. The key idea here is
> additivity. New ways to make contributions can be added just by
> adding new propagators; if an approach to a problem doesn't turn out
> to work well, it can be identified by its premises and ignored,
> dynamically and without disruption.

> This work was supported in part by the MIT Mind Machine Project.

# Propagator System

> Although most of this document introduces you to the
> Scheme-Propagator system that we have developed in MIT Scheme, the
> Propagator Model is really independent of the language. You should
> be able to write propagators in any language you choose, and others
> should be able to write subsystems in their favorite language that
> cooperate with your subsystems. What is necessary is that all users
> agree on the protocol by which propagators communicate with the
> cells that are shared among subsystems. These rules are very simple
> and we can enumerate them right here:

> Cells must support three operations:
>
> - add some content
> - collect the content currently accumulated
> - register a propagator to be notified
>   when the accumulated content changes

> When new content is added to a cell, the cell must merge the
> addition with the content already present. When a propagator asks
> for the content of a cell, the cell must deliver a complete summary
> of the information that has been added to it.

> The merging of content must be commutative, associative, and
> idempotent. The behavior of propagators must be monotonic with
> respect to the lattice induced by the merge operation.

# Getting Started

```scheme
(define-cell a)
(define-cell b)
(add-content a 3)
(add-content b 2)
(define-cell answer (e:+ a b))
(run)
(content answer) ==> 5
```

# The Details

> Now that you know how to play around with our propagators we have to
> tell you what we actually provide. In every coherent system for
> building stuff there are primitive parts, the means by which they
> can be combined, and means by which combinations can be abstracted
> so that they can be named and treated as if they are primitive.

# Making Propagator Networks

> The two basic operations when making a propagator network are making
> cells and attaching propagators to cells.

attaching propagators to cells 可以用函数作用来表示。

## Attaching Basic Propagators: d@

TODO

## Propagator Expressions: e@
## Late Binding of Application
## Provided Primitives: p:foo and e:foo
## Cells are Data Too
## Compound Data
## Propagator Constraints: c:foo and ce:foo
## Constants and Literal Values
## Constant Conversion
## Making Cells
## Conditional Network Construction

# Making New Compound Propagators

## Lexical Scope
## Recursion

# Using Partial Information

# Built-in Partial Information Structures

## Nothing
## Just a Value
## Numerical Intervals
## Propagator Cells as Partial Information
## Compound Data
## Closures
## Truth Maintenance Systems
## Contradiction
## Implicit Dependency-Directed Search

# Making New Kinds of Partial Information

## An Example: Adding Interval Arithmetic
## Generic Coercions
## The Partial Information Generics
## Individual Propagator Generics
## Uniform Applicative Extension of Propagators
## Interoperation with Existing Partial Information Types

# Making New Primitive Propagators

## Direct Construction from Functions
## Propagatify
## Compound Cell Carrier Construction
## Fully-manual Low-level Propagator Construction

# Debugging

# Miscellany

## Macrology
## Reboots
## Compiling
## Scmutils
## Editing
## Hacking
## Arbitrary Choices

# How this supports the goal

# Bibliography
