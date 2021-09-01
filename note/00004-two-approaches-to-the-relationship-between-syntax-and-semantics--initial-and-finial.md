---
title: Two approaches to the relationship between syntax and semantics -- initial and finial
date: 2021-09-02
---

# Initial approach -- Operational

When we first learned about how to write interpreters
(for example, in [EOPL (Essentials of Programming Languages)][EOPL]),
we recursively define the set of *expressions* and interpret them --
evaluate them to value, or normalize them to normal form.

In this approach, the process of interpretation is the semantics.

[EOPL]: https://en.wikipedia.org/wiki/Essentials_of_Programming_Languages

# Finial approach -- Denotational

In the note about [logic theory and its models][00002],
a logic theory is the syntax, and its models are its semantics.

Which means a *class* is the syntax, and its *objects* are its semantics.

Or speaking more generally,
abstract pattern is the syntax,
concrete instances are the semantics.

[00002]: 00002-model-theory-can-be-implemented-by-class-and-object.md

# Terminology

The terminology "initial" and "finial"
come from category theory's [initial and terminal objects][].

[initial and terminal objects]: https://en.wikipedia.org/wiki/Initial_and_terminal_objects.

The terminology was learned from Oleg's serial paper about [Tagless-final style][tagless-final]
(for embedding DSLs in a typed functional host language).

[tagless-final]: http://okmij.org/ftp/tagless-final/index.html
