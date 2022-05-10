---
title: Linear Logic
---

# Questions

> In linear logic, we view proposition as resource.

- How to view its rules in terms of resources?

- How to design its terms, how to evaluate them?

  - Should we use stack machine for evaluation?

# Connectives

| symbol  | polarity | level          |      | name (Girard) | identifier | meaning                                    |
| ------- | -------- | -------------- | ---- | ------------- | ---------- | ------------------------------------------ |
| `A ⊗ B` | pos      | multiplicative | conj | times         | `Both`     | I have both `A` and `B`.                   |
| `A & B` | neg      | additive       | conj | with          | `Choose`   | I have a choice between `A` and `B`.       |
| `A ⊕ B` | pos      | additive       | disj | plus          | `Maybe`    | Someone else's choice between `A` and `B`. |
| `A ⅋ B` | neg      | multiplicative | disj | par           | `Through`  | If not `A`, then `B`.                      |
| `!A`    | pos      | exponential    |      | of course     | `OfCourse` | I can reuse `A`.                           |
| `?A`    | neg      | exponential    |      | why not       | `WhyNot`   |                                            |

Notes:

- Maybe we should use `Always` instead of `OfCourse`.

- If we view linear logic as type of channel,
  - additive conj `&` means we can choice to `send` different messages to the channel,
  - additive disj `⊕` means we might `receive` different messages from the channel.

- How about type of actor?

| symbol   | derive               | name (Girard) | identifier | meaning                                         |
| -------- | -------------------- | ------------- | ---------- | ----------------------------------------------- |
| `A -o B` | `Through(Not(A), B)` | entails       | `Entails`  | I can construct `B`, by using `A` exactly once. |

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
Entails(Both(A, B), Through(C, D))
```

i.e. multiplicative.

A sequent calculus infere rule consists of a set of hypothesis sequents,
displayed above a horizontal line,
and a single conclusion sequent,
displayed below the line, as below:

```plaintext
Hypothesis1
Hypothesis2
------------
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

Use linear logic To capture "resource" problems logically.

For example,

- An encoding of Petri net reachability

# Complexity results for linear logic

## Recall of complexity results for logic

TODO
