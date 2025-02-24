---
title: actor a model of concurrent computation in distributed system
author: gul agha
year: 1985
---

# 1 introduction

# 2 general dsign decisions

## intro

- the nature of the computing elements
- global synchrony versus asynchronous elements
- the mode of interaction between'computing elements
- degree of fairness
- reconfigurability and extensibility

# 3 computation in actor systems

## 3.1 defining an actor system

- computation in a system of actors
  is in response to communications sent to the system.

- communications are contained in tasks.

- as computation proceeds,
  an actor system evolves to include new actors
  that are created as a result
  of processing tasks already in the system.

- the configuration of an actor system is defined
  by the actors it contains
  as well as the set of unprocessed tasks.

## SAL -- algol-like syntax

## ACT -- lisp-like syntax

# 4 a more expressive language

# 5 a model for actor systems -- operational semantics

# 6 concurrency issues

# 7 abstraction and compositionality

# 8 conclusions
