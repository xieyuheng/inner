---
title: Linear Logic
subtitle: Its syntax and semantics
author: Jean-Yves Girard
year: 1995
---

# 1 THE SYNTAX OF LINEAR LOGIC

## 1.1 The connectives of linear logic

When designing new logic like linear logic,
we should introduces new connectives
instead of modify classical or intuitionistic connectives.

### 1.1.1 Exponentials: actions vs situations

To view causality as reaction (physical or chemical).

Use exponentials `!` and `?` to mark stable truth,
i.e. the absence of any reaction.

Intuitionistic implication

```
A => B
```

can be decomposed into exponential and linear implication:

```
!A −o B
```

### 1.1.2 The two conjunctions

```plaintext
A = to spend $1,
B = to get a pack of Camels,
C = to get a pack of Marlboro.
```

- **Xie**: Here, each proposition is viewed as an action, performed by an agent.

  Actions do side effects on the agent and its environment.

  `A ⅋ B` can be viewed as perform two actions one by one,
  thus called "par", which is French word for "through".

  `A −o B` is `¬A ⅋ B`, with the definitions above, it means,
  I spend $1 then you give me a pack of Camels.

  - Read in this way, `⅋` is symmetric, because it does not matter,
    whether I give money first or get Camels first.

## 1.2 Linear sequent calculus

### 1.2.1 Structural rules

TODO
