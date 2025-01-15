---
title: Linear Logic
---

# Questions

> In linear logic, we view proposition as resource.

- How to view its rules in terms of resources?
- How to design its terms, how to evaluate them?

# Connectives

| symbol | polarity | level |      | name (Girard) | identifier |
|--------|----------|-------|------|---------------|------------|
| A ⨂ B  | pos      | mul   | conj | times         | Both       |
| A & B  | neg      | add   | conj | with          | Choose     |
| A ⨁ B  | pos      | add   | disj | plus          | Maybe      |
| A ⅋ B  | neg      | mul   | disj | par           | Through    |
| !A     | pos      | exp   |      | of course     | OfCourse   |
| ?A     | neg      | exp   |      | why not       | WhyNot     |

| symbol | meaning                                |
|--------|----------------------------------------|
| A ⨂ B  | I have both A and B.                   |
| A & B  | I have a choice between A and B.       |
| A ⨁ B  | Someone else's choice between A and B. |
| A ⅋ B  | If not A, then B.                      |
| !A     | I can reuse A.                         |
| ?A     |                                        |

Notes:

- Note that, sending message is the dual of receiving message, and
  conditional is done by receiving messages and pattern matching, thus
  pattern matching is the dual of method call (messages passing) in
  object oriented programming.

  - Recall Sandi Metz' anti-conditional programming.

- TODO How about type of actor?

  - Maybe it is similarly linear logic,
    because actor and channel + process can simulate each other.

  - An actor can send messages to other actors,
    and can also receive messages other actors.

| symbol | derive             | name (Girard) | identifier |
|--------|--------------------|---------------|------------|
| A -o B | Through(Not(A), B) | entails       | Entails    |

| symbol | meaning                                     |
|--------|---------------------------------------------|
| A -o B | I can construct B, by using A exactly once. |

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

# Connections to other logics

Linear logic arose partly out of a study of intuitionistic implication.
Girard found that the intuitionistic implication `A -> B`
could be decomposed into two separate connectives:

```
!A -o B
```

Girard showed that one could thus translate intuitionistic (and also
classical) logic into linear logic directly, simply appending modals
to certain subformulas and making the right choice as to which sort of
conjunction and disjunction should be used.

因此 linear logic 可以看作是对
intuitionistic logic 和 classical logic 的进一步 factoring。

# Connections to computer science

Use linear logic to capture "resource" problems logically.

Examples:

- An encoding of Petri net reachability

