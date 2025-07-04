---
title: Postfix Notations
date: 2022-05-10
---

# When to not use postfix notations?

## Too unfamiliar

Postfix notation is simply too unfamiliar to us.

## Bad for editing code

Postfix notation is bad for [editing code](0007-prefix-and-postfix-notation-are-not-symmetrical-because-of-the-direction-of-human-writing.md).

## No parentheses to denote the borders of applications

Not expression-based, thus no sub-expression,
i.e. no parentheses to help us know the borders of applications.

## Not essential for multi return values

It is easy to handle multi return values and zero return value in postfix notations.

But this is not essential,
because we can also use `let` to handle multi return values.

# Sequent calculus of linear logic

Postfix notation is the term for sequent calculus of linear logic,
specially when rules are written in right-side style
and the comma is understand as "through".

- The cut rule -- concatenation (or say composition).
- The exchange rule -- permutation operators like swap.

# Sequent calculus of intuitionistic logic

For sequent calculus of intuitionistic logic,
postfix notation is also the term,
where multiple return value should be understand as "and"
(while in sequent calculus for classical logic,
multiple value on the right side means "or").

For intuitionistic logic, we can use the stack to do computation directly.

The proof of cut-elimination theorem should give us
an algorithm to reduce terms to normal form.

# Function composition

In postfix notation, concatenation can be viewed as function composition.

Function composition which is associative,
function application is not associative.

# Context and interactive feedback

Postfix notation is good for keeping the context implicit,
where during interaction with user,
the context can be reported as feedback.

> Dance with expressions in context.

The context might be a stack.

- Forth uses a return stack and a value stack.
- Interaction nets, we can use a stack of ports to build graph.
- Coq's tactic process a stack of goals.
