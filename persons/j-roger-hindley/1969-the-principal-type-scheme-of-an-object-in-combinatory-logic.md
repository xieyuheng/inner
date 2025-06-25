---
title: the principal type scheme of an object in combinatory logic
author: j roger hindley
year: 1969
---

# Introduction

> In their book Combinatory Logic [1], Curry and Feys introduced the
> notion of "functional character" (here called "type-scheme") of an
> object of combinatory logic. Roughly speaking, each object of
> combinatory logic ("ob" for short) represents a function or an
> operator on functions; for instance the ob `I` represents the
> identity operator, and we have for all obs X, `I X = X`.

> One of the aims of combinatory logic is to study the most basic
> properties of functions and other concepts, with as few restrictions
> as possible; hence in the simplest form of combinatory logic there
> is nothing to stop a function being applied to itself; thus `X X`
> is an ob for all obs `X`.

> However it is also natural to look at the consequences of
> introducing type-restrictions, formulated by assigning types to the
> obs according to certain rules, to be described later.

> Each type is a linguistic expression representing a set of
> functions, and for any type a the statement "X has type α" is
> intended to mean that the ob `X` represents a member of the set
> represented by `α`. Given types `α` and `β`, the set of all
> functions from the set `α` into the set `β` is represented by the
> type `F α β` (using Curry's notation).

TODO

# 1 Definitions
# 2 Lemmas on substitution
# 3 Existence of principal type-schemes
# 4 The principal type-scheme of [x].M
# 5 An alternative approach to typed combinators
# 6 Every type-scheme is a p.t.s
