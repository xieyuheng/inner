---
title: Linear Logic
---

# Questions

> In linear logic, we view proposition as resource.

- How to view its rules in terms of resources?

- How to design its terms, how to evaluate them?

  - Should we use stack machine for evaluation?

# Connectives

| Symbol | polarity |                |      | meaning                      |
| ------ | -------- | -------------- | ---- | ---------------------------- |
| `⊗`    | pos      | multiplicative | conj | I have both                  |
| `⊕`    | pos      | additive       | disj | Maybe; someone else's choice |
| `&`    | neg      | additive       | conj | I have a choice between      |
| `⅋`    | neg      | multiplicative | disj | either, if not A, then B     |
| `!`    | pos      | exponential    |      | of course; I can reuse       |
| `?`    | neg      | exponential    |      | why not                      |

| Symbol   | derive              | meaning                                  |
| -------- | ------------------- | ---------------------------------------- |
| `A -o B` | `Either(Not(A), B)` | can construct B, by using A exactly once |

Linear negation is involutive but constructive.

# Examples

Suppose for $5 a restaurant will provide a hamburger, a Coke,
as many french fries as you like, onion soup or salad (your choice),
and pie or ice cream (some else's choice, depending on availability).

```
dollar(5) -o Both(
  hamburger,
  coke,
  OfCourse(french_fries),
  Choose(onion_soup, salad),
  Maybe(pie, ice_cream),
)
```

# Sequent calculus rules

We study rules for propositional linear logic first.

The sequent `A, B |- C, D` means,

```
Lollipop(Both(A, B), Either(C, D))
```

i.e. multiplicative.

A sequent calculus inference rule consists of a set of hypothesis sequents,
displayed above a horizontal line,
and a single conclusion sequent,
displayed below the line, as below:

```
Hypothesis1
Hypothesis2
------
Conclusion
```

# Connections to other logics

Linear logic arose partly out of a study of intuitionistic implication.
Girard found that the intuitionistic implication `A -> B`
could be decomposed into two separate connectives:

```
!A -o B
```

Girard showed that one could thus translate intuitionistic (and also classical) logic
into linear logic directly, simply appending modals to certain subformulas
and making the right choice as to which sort of conjunction and disjunction should be used.

Here we see a first glimpse of the substance behind the slogan

> Linear logic is a logic behind logics.

# Connections to computer science

TODO

# Complexity

TODO

# References

- Girard, Lafont, and Taylor, "Proofs and types"

  is a book evolved from lecture notes of a graduate course of the same title.
  It is a good reference for an introduction to the proof-program correspondence,
  although it does not cover all topics of the matter.

- Girard, "Linear Logic"

  is the historical paper introducing linear logic.
  It is an unavoidable reference, although not the best way
  to discover the topic nowadays, since many aspects have been better understood since then.

- Girard, "Linear Logic: Its Syntax and Semantics"

  is an updated and more accessible presentation, written about ten years later.
