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
according to which probabilities encode degrees of belief about events in the world
and data are used to strengthen, update, or weaken those degrees of belief.

In this formalism, degrees of belief are assigned to propositions
(sentences that take on true or false values) in some language,
and those degrees of belief are combined and manipulated
according to the rules of probability calculus.

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
