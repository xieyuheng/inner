---
title: The aim of a type system
date: 2021-09-04
---

When implementing a type system for a logic theory,
I sometimes be come unclear about how should we use it.

> How should we use an implementation of such a system?

I here make clear about the aim (the way we use it) of a type system:

> A type system interact with people by providing feedback about ones reasoning.

(1) Write proof in the formal language, and let the machine check the proof is valid.

This is the core usage, but the following (2) and (3) are also good usages.

(2) Let the machine search for proof of a given claim,
and it would be even better, if the machine is able to explain its strategy to us.

(3) The machine should be able to normalize terms (proofs) --
i.e. simplify proofs, and compare proofs for equivalence relation.

A related question is:

> How should we use an implementation of a evaluator?

The same as (3), we use evaluator to evaluate expression to value,
or normalize expression to normal form.
