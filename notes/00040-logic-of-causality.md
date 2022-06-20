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

By works of Judea Pearl, we know
causal logic will have a rich probability theory as its model.

- `P(e | H)` is the degree of belief that
  "If (we see) `H` then (we see) `e`".

- Probability theory (conditional independence)
  also is a rich model of graph theory (axioms of graphoid).

For the meaning of "cause",
see [00041-causality-is-reactivity.md](./00041-causality-is-reactivity.md).
