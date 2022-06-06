---
title: Logic of causality
date: 2022-06-06
---

Logic studies the relation between syntax and its models.
One logic system can has many models.

There are different logic systems.
They interpret logical connectives differently.

The common logical connective is "imply", written `A -> B`,
here is how it is interpreted in different logic systems:

|                | `A -> B`                                                       |
| -------------- | -------------------------------------------------------------- |
| classical      | If `A` is true, `B` is true.                                   |
| intuitionistic | Given elements of `A`, we can construct elements of `B`.       |
| linear         | Consuming an element of `A`, we can produce an element of `B`. |

Can we design a logic system which interpret "imply" as "cause"?

|        | `A -> B`        |
| ------ | --------------- |
| causal | `A` causes `B`. |

What is the interpretation of "cause"?

- **Computational**

  If we setup the init situation `A`,
  the "World Interpreter" will evaluate it
  into the result situation `B`.

- **Counterfactual**

  If we observe the correlation between `A` and `B`,
  and if we remove `A`, there will be no `B`.

  TODO This need to be revised.
