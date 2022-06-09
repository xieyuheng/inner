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

## Contents

The sequence of discussion follows more or less the chronological order
by which our team at UCLA has tackled these topics,
thus re-creating for the reader some of our excitement
that accompanied these developments.

- (Chapter 1) The introductory chapter.

- (Chapter 2) We start with the hardest questions of
  how one can go about discovering cause-effect relationships in raw data
  and what guarantees one can give to ensure
  the validity of the relationships thus discovered.

- (Chapters 3 and 4) We then proceed to questions of identifiability
  -- namely, predicting the direct and indirect effects of actions and policies
  from a combination of data and fragmentary knowledge
  of where causal relationships might operate.

- (Chapters 5 and 6) The implications of these findings for the social
  and health sciences are then discussed in Chapters 5 and 6 (respectively),
  where we examine the concepts of structural equations and confounding.

- (Chapter 7) Offers a formal theory of counterfactuals and structural models,
  followed by a discussion and a unification of related approaches
  in philosophy, statistics, and economics.

- (Chapters 8-10) The applications of counterfactual analysis
  are then pursued in Chapters 8-10,
  where we develop methods of bounding causal relationships
  and illustrate applications to imperfect experiments,
  legal responsibility, and the probability of
  necessary, sufficient, and single-event causation.

- (Epilogue) We end this book with a transcript of a public lecture
  that I presented at UCLA, which provides a gentle introduction
  to the historical and conceptual aspects of causation.

Readers who wish to be first introduced to the nonmathematical aspects of causation
are advised to start with the Epilogue
and then to sweep through the other historical/conceptual parts of the book:

- 1.1.1,
- 3.3.3,
- 4.5.3,
- 5.1, 5.4.1,
- 6.1,
- 7.2, 7.4, 7.5,
- 8.3,
- 9.1, 9.3,
- 10.1.

More formally driven readers, who may be anxious to delve directly
into the mathematical aspects and computational tools,
are advised to start with Section 7.1
and then to proceed as follows for tool building:

- 1.2,
- 3,
- 4.2-4.4,
- 5.2-5.3,
- 6.2-6.3,
- 7.3,
- 8-10.

# 1 Introduction to Probabilities, Graphs, and Causal Models

## 1.1 Introduction to Probability Theory

### 1.1.2 Basic Concepts in Probability Theory

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

We say that `A` and `B` are \_independent, if

```
P(A | B) = P(A)
```

since our belief in `A` remains unchanged upon learning the truth of `B`.

We say that `A` and `B` are _conditionally independent_ given `C`, if

```
P(A | B, C) = P(A | C)
```

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

Another useful generalization
of the product rule (equation (1.9))
is the _chain rule_ formula.
It states that if we have a set of n events, `E1, E2, ..., En`,
then the probability of the joint event `(E1, E2, ..., En)`
can be written as a product of n conditional probabilities:

```(1.12)
P(E1, E2, ..., En) =
P(En | En-1, ..., E2, E1) * P(En-1, ..., E2, E1) =
P(En | En-1, ..., E2, E1) * ... * P(E2 | E1) * P(E1)
```

The heart of Bayesian inference lies in the celebrated inversion formula,

```(1.13)
P(H | e) = P(e | H) * P(H) / P(e)
```

which states that
the belief we accord a hypothesis `H`
upon obtaining evidence `e`
can be computed by multiplying our previous belief `P(H)`
by the likelihood `P(e | H)`
that `e` will materialize if `H` is true.
This `P(H | e)` is sometimes called the _posterior probability_ (or simply posterior),
and `P(H)` is called the _prior probability_ (or prior).
The denominator `P(e)` of (1.13) hardly enters into consideration
because it is merely a normalizing constant

```
P(e) = P(e | H) * P(H) + P(e | ~H) * P(~H)
```

which can be computed by requiring
that `P(H | e)` and `P(~H | e)` sum to unity.

The direction is important,
because the Bayesian subjectivist regards (1.13)
as a normative rule for updating beliefs in response to evidence.

In other words, although conditional probabilities `P(... | A)`
can be viewed as purely mathematical constructs,
the Bayes adherent views them as primitives of the language
and as faithful translations of the English expression

> ..., given that I know A.

Without the direction in the formula,
an equation is just an empirically verifiable relationship
between English expressions.
It asserts, among other things,
that the belief a person attributes to `B` after discovering `A`
is never lower than that attributed to before discovering `A`.
Also, the ratio between these two beliefs will increase proportionally
with the degree of surprise `1 / P(A)` one associates with the discovery of A.

The importance of (1.13) is that it expresses a quantity `P(H | e)`
-- which people often find hard to assess --
in terms of quantities that often can be drawn directly
from our experiential knowledge.

For example, if a person at the next gambling table declares the outcome "twelve",
and we wish to know whether he was rolling a pair of dice or spinning a roulette wheel,
our models of the gambling devices readily yield the quantities

```
P(twelve | dice) = 1 / 36
P(twelve | roulette) = 1 / 38
```

Similarly, we can judge the prior probabilities `P(dice)` and `P(roulette)`
by estimating the number of roulette wheels and dice tables at the casino.

Issuing a direct judgment of `P(dice | twelve)`
would have been much more difficult;
only a specialist in such judgments,
trained at the very same casino,
could do it reliably.

In order to complete this brief introduction,
we must discuss the notion of _probabilistic model_
(also called _probability space_).

A probabilistic model is an encoding of information
that permits us to compute the probability of every well-formed sentence `S`
in accordance with the axioms of (1.1)-(1.3).

- **Xie:** A sentence describe an event.

Starting with a set of atomic propositions `A, B, C, ...`,
the set of well-formed sentences consists of
all Boolean formulas involving these propositions, for example

```
S = (A and B) or (not C)
```

The traditional method of specifying probabilistic models
employs a _joint distribution function_, which is a function that
assigns nonnegative weights to every _elementary event_ in the language
(an elementary event being a conjunction
in which every atomic proposition or its negation appears once)
such that the sum of the weights adds up to 1.
For example, if we have three atomic propositions, `A`, `B`, and `C`,
then a joint distribution function should assign nonnegative weights
to all eight combinations

```
A and B and C
A and B and (not C)
...
(not A) and (not B) and (not C)
```

such that the eight weights sum to 1.

### 1.1.3 Combining Predictive and Diagnostic Supports

The essence of Bayes’s rule (equation 1.13)
is conveniently portrayed using the _odds_
and _likelihood ratio_ parameters.

```
P(H | e) = P(e | H) * P(H) / P(e)
P(~H | e) = P(e | ~H) * P(~H) / P(e)

P(H | e) / P(~H | e) = P(e | H) * P(H) / P(e | ~H) * P(~H)
```

TODO Define `O` and `L`, and explain the use of Bayes's rule by an example.

### 1.1.4 Random Variables and Expectations

TODO Define `E` and `V`.

### 1.1.5 Conditional Independence and Graphoids

Recall Conditional Independence

```
P(A | B, C) = P(A | C)
```

If we define a relation between three variables called irrelevant `I`:

```
I(X, Z, Y) := P(X | and(Y, Z)) == P(X | Z)
```

It will satisfy a set of axioms which defines a mathematical structure
called [Graphoid](https://en.wikipedia.org/wiki/Graphoid).

```cicada
class Graphoid {
  Element: Type

  I(X: Element, Y: Element, Z: Element): Type

  // The symmetry axiom states that,
  // in any state of knowledge Z,
  // if Y tells us nothing new about X,
  // then X tells us nothing new about Y.
  symmetry(
    implicit X: Element,
    implicit Y: Element,
    implicit Z: Element,
    i: I(X, Z, Y),
  ): I(Y, Z, X)

  // The decomposition axiom asserts that
  // if two combined items of information
  // are judged irrelevant to X,
  // then each separate item is irrelevant as well.
  decomposition(
    implicit X: Element,
    implicit Y: Element,
    implicit W: Element,
    implicit Z: Element,
    i: I(X, Z, or(Y, W)),
  ): I(X, Z, Y)

  // The weak union axiom states that
  // learning irrelevant information W -- Z becoming or(Z, W) --
  // cannot help the irrelevant information
  // Y become relevant to X.
  weak_union(
    implicit X: Element,
    implicit Y: Element,
    implicit W: Element,
    implicit Z: Element,
    i: I(X, Z, or(Y, W)),
  ): I(X, or(Z, W), Y)

  // The contraction axiom states that
  // if we judge W irrelevant to X after
  // learning some irrelevant information Y,
  // then W must have been irrelevant before we learned Y.
  contraction(
    implicit X: Element,
    implicit Y: Element,
    implicit W: Element,
    implicit Z: Element,
    i: I(X, Z, Y),
    j: I(X, or(Z, Y), W),
  ): I(X, Z, or(Y, W))

  // Together, the weak union and contraction properties mean
  // that irrelevant information should not alter the relevance status
  // of other propositions in the system;
  // what was relevant remains relevant,
  // and what was irrelevant remains irrelevant.

  // The intersection axiom states that
  // if Y is irrelevant to X when we know W
  // and if W is irrelevant to X when we know Y,
  // then W and Y (and their combination) is irrelevant to X.
  intersection(
    implicit X: Element,
    implicit Y: Element,
    implicit W: Element,
    implicit Z: Element,
    i: I(X, or(Z, W), Y),
    j: I(X, or(Z, Y), W),
  ): I(X, Z, or(Y, W))
}
```

Notes:

- Intersection is valid in strictly positive probability distributions.

Interpretations of `I(X, Z, Y)`:

- **Probability:**

  `X` is irrelevant to `Y` given that we know `Z`.

- **Graph theory (undirected):**

  All paths from a subset `X` of nodes
  to a subset `Y` of nodes
  are intercepted by a subset `Z` of nodes.

## 1.2 GRAPHS AND PROBABILITIES

### 1.2.1 Graphical Notation and Terminology

```cicada
class Graph {
  Vertex: Type
  Edge: Type

  start(e: Edge): Vertex
  end(e: Edge): Vertex

  Adjacent(x: Vertex, y: Vertex): Type {
    return exists (f: Edge, g: Edge)
      Equal(Vertex, end(f), start(g))
  }

  // TODO Path
  // TODO DirectedPath

  Root(x: Vertex): Type {
    return TODO
  }

  Sink(x: Vertex): Type {
    return TODO
  }
}

class AcyclicGraph extends Graph {
  // TODO
}

class Tree extends AcyclicGraph {
  // TODO
}
```

### 1.2.2 Bayesian Networks

The role of graphs in probabilistic and statistical modeling is threefold:

1. to provide convenient means of expressing substantive assumptions;
2. to facilitate economical representation of joint probability functions; and
3. to facilitate efficient inferences from observations.

TODO How Bayesian Networks can be viewed as representation of joint probability functions?

TODO How to construct Bayesian Networks?

- **Xie:** If the method by which we construct Bayesian Networks
  does not make the directed edge causal,
  we may need to build the graph model by our knowledge.

  - Maybe this is why people do not like model-based
    causal inferences, because it is subjective.

- **Xie:** If causal model makes it more efficient
  for compute to calculus probability,
  is it also true that our tendency for causal reasoning
  is evolved for the same reason?

- **Definition 1.2.2 (Markov Compatibility)**

  If a probability function `P`
  admits the factorization of (1.33)
  relative to DAG `G`,
  we say that `G` represents `P`,
  that `G` and `P` are compatible,
  or that `P` is Markov relative to `G`.

Ascertaining compatibility between DAGs and probabilities
is important in statistical modeling
primarily because compatibility is
a necessary and sufficient condition
for a DAG `G` to explain a body of empirical data
represented by `P`, that is,
to describe a stochastic process capable of generating `P`.

### 1.2.3 The d-Separation Criterion

TODO

### 1.2.4 Inference with Bayesian Networks

TODO Note about "abduction".

## 1.3 Causal Bayesian Networks

### 1.3.1 Causal Networks as Oracles for Interventions

The source of this flexibility rests on the assumption that
each paren-child relationship in the network represents
a stable and autonomous physical mechanism
-- in other words, that it is conceivable
to change one such relationship
without changing the others.
Organizing one's knowledge in such modular configurations
permits one to predict the effect of external interventions
with a minimum of extra information.
Indeed, causal models (assuming they are valid)
are much more informative than probability models.
A joint distribution tells us how probable events are
and how probabilities would change with subsequent observations,
but a causal model also tells us how these probabilities
would change as a result of external interventions
-- such as those encountered in policy analysis,
treatment management, or planning everyday activity.
Such changes cannot be deduced from a joint distribution,
even if fully specified.

## 1.4 Functional Causal Models

Finally, certain concepts that are ubiquitous in human discourse
can be defined only in the Laplacian framework.
We shall see, for example, that such simple concepts as
"the probability that event B occured because of event A" and
"the probability that event B would have been different if it were not for event A"
cannot be defined in terms of purely stochastic models.
These so-called counterfactual concepts will require a synthesis of
the deterministic and probabilistic components
embodied in the Laplacian model.

- **Xie:** Is it true that
  "the probability that event B occured because of event A"
  means a higher order probability `P(A -> B)`?

### 1.4.1 Structural Equations

TODO

## 1.5 Causal versus Statistical Terminology

**Causal versus Statistical Concepts**

The demarcation line between causal and statistical parameters
extends as well to general concepts and will be supported
by terminological distinction.

Examples of statistical concepts are:

- correlation,
- regression,
- conditional independence,
- association,
- likelihood,
- collapsibility,
- risk ratio,
- odds ratio,
- propensity score,
- Granger's causality,
- and so on.

Examples of causal concepts are:

- randomization,
- influence,
- effect,
- confounding,
- exogeneity,
- ignorability,
- disturbance (e.g., (1.40)),
- spurious correlation,
- path coefficients,
- instrumental variables,
- intervention,
- explanation,
- and so on.

The purpose of this demarcation line
is not to exclude causal concepts
from the province of statistical analysis
but, rather, to encourage investigators
to treat nonstatistical concepts
with the proper set of tools.

**Two Mental Barriers to Causal Analysis**

The sharp distinction between statistical and causal concepts
can be translated into a useful principle:

> behind every causal claim there must lie some causal assumption
> that is not discernable from the joint distribution
> and, hence, not testable in observational studies.

Such assumptions are usually provided by humans,
resting on expert _judgment_.
Thus, the way humans organize
and communicate experiential knowledge
becomes an integral part of the study,
for it determines the veracity of the judgments
experts are requested to articulate.

# 2 A Theory of Inferred Causation

# 3 Causal Diagrams and the Identification of Causal Effects

# 4 Actions, Plans, and Direct Effects

# 5 Causality and Structural Models in Social Science and Economics

# 6 Simpson's Paradox, Confounding, and Collapsibility

# 7 The Logic of Structure-Based Counterfactuals

# 8 Imperfect Experiments: Bounding Effects and Counterfactuals

# 9 Probability of Causation: Interpretation and Identification

# 10 The Actual Cause

# 11 Reflections, Elaborations, and Discussions with Readers
