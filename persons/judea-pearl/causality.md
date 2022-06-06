---
title: Causality
subtitle: Models, Reasoning, and Inference
author: Judea Pearl
year: [2000, 2009]
---

[ [WEBSITE](http://bayes.cs.ucla.edu/BOOK-2K) ]

# Preface

The two fundamental questions of causality are:

1. What empirical evidence is required
   for legitimate inference of cause–effect relationships?

2. Given that we are willing to accept causal information about a phenomenon,
   what inferences can we draw from such information, and how?

Ten years ago, when I began writing
_Probabilistic Reasoning in Intelligent Systems_ (1988),
I was working within the empiricist tradition.
In this tradition, probabilistic relationships
constitute the foundations of human knowledge,
whereas causality simply provides useful ways of
abbreviating and organizing intricate patterns of probabilistic relationships.
Today, my view is quite different.
I now take causal relationships to be the fundamental building blocks
both of physical reality and of human understanding of that reality,
and I regard probabilistic relationships as
but the surface phenomena of the causal machinery
that underlies and propels our understanding of the world.

# 1 Introduction to Probabilities, Graphs, and Causal Models

We will adhere to the **Bayesian interpretation of probability**,
according to which _probabilities encode degrees of belief about events in the world_
and data are used to strengthen, update, or weaken those degrees of belief.

In this formalism, degrees of belief are assigned to propositions
(sentences that take on true or false values) in some language,
and those degrees of belief are combined and manipulated
according to the rules of probability calculus.

It is worth reemphasizing that formulas like `P(A)`
are always understood to apply in some larger context `K`,
which defines the assumptions taken as common knowledge
(e.g., the fairness of dice rolling),
i.e. `P(A)` really means `P(A | K)`,
and when the context can change,
we need to write the context explicitly.

In the Bayesian formalism, belief measures
obey the three basic axioms of probability calculus:

```(1.1)
0 <= P(A) <= 1
```

```(1.2)
P(sure proposition) = 1
```

```(1.3)
P(A or B) = P(A) + P(B) // if A and B are mutually exclusive.
```

The basic expressions in the Bayesian formalism
are statements about _conditional probabilities_
-- for example, `P(A | B)` -- which specify the belief in `A`
under the assumption that `B` is known with absolute certainty.

If `P(A | B) = P(A)`, we say that `A` and `B` are _independent_,
since our belief in `A` remains unchanged upon learning the truth of `B`.

If `P(A | B, C) = P(A | C)`,
we say that `A` and `B` are _conditionally independent_ given `C`;
that is, once we know `C`, learning `B` would not change our belief in `A`.

Contrary to the traditional practice of defining
conditional probabilities in terms of joint events,

```(1.8)
P(A | B) = P(A, B) / P(B)
```

Bayesian philosophers see the conditional relationship
as more basic than that of joint events
-- that is, more compatible with the organization of human knowledge.
In this view, `B` serves as a pointer to a context or frame of knowledge,
and `A | B` stands for an event `A` in the context specified by `B`
(e.g., a symptom A in the context of a disease B).

Consequently, empirical knowledge invariably
will be encoded in conditional probability statements,
whereas belief in joint events (if it is ever needed)
will be computed from those statements via the product

```(1.9)
P(A, B) = P(A | B) * P(B)
```

The probability of any event `A` can be computed by conditioning it
on any set of exhaustive and mutually exclusive events `B(i)`,
and then summing:

```(1.10)
P(A) = sum (i: I) P(A | B(i)) * P(B(i))
```

This decomposition provides the basis
for hypothetical or "assumption-based" reasoning.
It states that the belief in any event `A`
is a weighted sum over the beliefs
in all the distinct ways that `A` might be realized.

The heart of Bayesian inference lies in the celebrated inversion formula,

```(1.13)
P(H | e) = P(e | H) * P(H) / P(e)
```

# 2 A Theory of Inferred Causation

# 3 Causal Diagrams and the Identification of Causal Effects

# 4 Actions, Plans, and Direct Effects

# 5 Causality and Structural Models in Social Science and Economics

# 6 Simpson’s Paradox, Confounding, and Collapsibility

# 7 The Logic of Structure-Based Counterfactuals

# 8 Imperfect Experiments: Bounding Effects and Counterfactuals

# 9 Probability of Causation: Interpretation and Identification

# 10 The Actual Cause

# 11 Reflections, Elaborations, and Discussions with Readers
