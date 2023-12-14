---
title: SECD Machine
---

> The operating principle of this machine centers around the idea of
> delayed substitutions. Rather than performing β-reductions as atomic
> operations, the machine partitions them, as a measure to improve
> runtime eﬃciency, into two steps that are distributed over time. When
> encountering β-redices, it just collects mappings of bound variables
> to (operand) expressions in a runtime structure called the
> environment. All substitutions are subsequently done while traversing
> the body expressions only once in search of bound-variable
> occurrences.
>
> -- Abstract Computing Machines: A Lambda Calculus Perspective
> -- W. Kluge
