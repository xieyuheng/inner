---
title: Linear Logic
---

# Questions

> In linear logic, we view proposition as resource.

- How to view its rules in terms of resources?

- How to design its terms, how to evaluate them?

  - Should we use stack machine for evaluation?

# Connectives

| Symbol | polarity |             |      |                                          |
|--------|----------|-------------|------|------------------------------------------|
| `⊗`    | pos      | mul         | conj | I have both                              |
| `⊕`    | pos      | add         | disj | Maybe; someone else's choice             |
| `&`    | neg      | add         | conj | I have a choice between                  |
| `⅋`    | neg      | mul         | disj | either, if not A, then B                 |
| `!`    | pos      | exponential |      | of course; I can reuse                   |
| `?`    | neg      | exponential |      | why not                                  |
|--------|----------|-------------|------|------------------------------------------|
| `-o`   |          |             |      | can construct B, by using A exactly once |

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
