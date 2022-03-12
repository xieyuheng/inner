---
title: "1: Flexibility in Nature and in Design"
---

# Additive programming

Our goal in this book is to investigate how to construct
computational systems so that they can be easily adapted to
changing requirements. One should not have to modify a working
program. One should be able to add to it to implement new
functionality or to adjust old functions for new requirements.

We call this _additive programming_.

In order for additive programming to be possible, it is necessary
to minimize the assumptions about how a program works and how
it will be used. Assumptions made during the design and
construction of a program may reduce the possible future
extensions of the program. Instead of making such assumptions, we
build our programs to make just-in-time decisions based on the
environment that the program is running in. We will explore several
techniques that support this kind of design.

We can also build systems that combine multiple sources of
partial information to obtain more complete answers. This is most
powerful when the contributions come from independent sources of
information. In chapter 4 we will see how type inference is really a
matter of combining multiple sources of partial information. Locally
deducible clues about the type of a value, for example that a
numerical comparison requires numerical inputs and produces a
boolean output, can be combined with other local type constraints
to produce nonlocal type constraints.

> ... type inference is really a matter of combining multiple sources of partial information.

# 1.1 Architecture of computation

A parti (pronounced parTEE) is the central idea of an architectural work:
it is "the [architectural] composition being conceived as a whole,
with the detail being filled in later."

The parti is a model, but it is usually not a completely workable structure.
It must be elaborated with functional elements.

These elaborations may cause modifications of the parti,
but the parti continues to serve as a scaffold
around which these elaborations are developed.

In programming, the parti is the abstract plan for the computations to be performed.

# 1.2 Smart parts for flexibility

Not just composable, but also smart.

# 1.3 Redundancy and degeneracy

Prepare multiple ways to do the same thing.

# 1.4 Exploratory behavior

Use generate-and-test mechanism.

# 1.5 The cost of flexibility

Just do not care about cost, you are often wrong when thinking about it.

... And if a significant fraction of the time spent reprogramming a system for a new requirement
is replaced by having that system adapt itself to the new situation,
that can be an even bigger win.

**The problem with correctness**

We are arguing against the discipline of requiring proofs:

the requirement that everything must be proved to be applicable in a
situation before it is allowed to be used in that situation excessively
inhibits the use of techniques that could enhance the robustness of designs.

This is especially true of techniques that allow a method to
be used, on a tight leash, outside of its proven domain, and
techniques that provide for future expansion without putting limits
on the ways things can be extended.

Xie: We'd better write proofs for understanding than write proofs to be safe.
