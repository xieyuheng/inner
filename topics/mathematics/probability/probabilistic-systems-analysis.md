---
title: Probabilistic Systems Analysis
playlist: "https://www.youtube.com/playlist?list=PLUl4u3cNGP60A3XMwZ5sep719_nh95qOe"
---

# Intro

Probability as a mathematical framework
for reasoning about uncertainty.

# probability space

- **Xie:**
  Note that modern theory of probability
  is limited by the mathematical structure of `ProbabilitySpace`.

- A **sample space** is set of outcomes.

  We record which outcome is more likely to occur compare to others
  by assigning probability to outcome.

  But to deal with continuous sample space
  we must adjust our method by assigning probability to events
  where an event is a subset of sample space.

  This definition also handles discrete sample space well
  by viewing assigning probability to sets of one element.

- `outcome: SampleSpace`

  ```
  event =
  a set of outcomes =
  a subset of sample space
  ```

Probability axioms:

- **nonnegative**:

  ```
  (A : sample-space) -> P (A) >= 0
  ```

- **normalization**:

  ```
  P (sample-space) = 1
  ```

Notations about set :

```
| ^  | set intersection |
| +  | set union        |
| =< | sub set          |
| >= | super set        |
```

- **additivity**:

  ```
  (A ^ B = empty-set) -> P (A + B) = P (A) + P (B)
  ```

- **countable additivity**:

  the additivity axiom need to be generalized to sequence of subsets
  instead of just two subsets
  (where sequence implies countable)

the axioms looks like integration of some function over sample-space

- extra notes about subsets of sample-space
  ignore ugly and weird subsets
  because for some weird subsets we can not both
  assigning probability to them and maintain probability axioms

  - **Xie:**
    this also revealed set theory's inconvenience,
    but why we are not using type theory yet?
    will type theory be even more inconvenient?

- **Xie:**
  dealing with subsets remembers me of relational programming

- **discrete uniform distribution**

  all outcomes be equally likely

  ```
  compute probability = counting
  ```

- **continuous uniform distribution**

  ```
  probability = area
  ```

# TODO ProbabilitySpace

TODO We should use lattice (boolean algebra?)
to model the `SampleSpace` or the space of events.

TODO How to handle `Probability`?

```cicada
class ProbabilitySpace {
  SampleSpace: Type
  P(SampleSpace): Probability

  nonnegative(x: SampleSpace): GtEq(P(x), zero)
  normalization: Equal(Probability, P(SampleSpace), one)

  additivity(
    x: SampleSpace,
    y: SampleSpace,
    mutual_exclusive: Equal(
      Event,
      intersection(x, y),
      Empty,
    ),
  ): Equal(
    Probability,
    P(union(x, y)),
    add(P(x), P(y)),
  )
}
```

# zero probability

- if we define probability by volume in n dimension unit space
  zero probability does not mean impossible
  zero probability only means n dimensional volume is zero

# conditional probability

- the story goes like this,
  you know something about this uncertain world,
  and based on what you know, you build a probability model,
  by write down probabilities for different outcomes.
  then something happened, you get some new informations,
  you know more about the world,
  these new informations should change your beliefs
  about what may happen and what may not happen.

- partial informations about random experiments and revise beliefs

```
P (A | B) := the probability of A, given that B occurred
P (A | B) := P (A ^ B) / P (B)
- given P (B) != 0
- if P (B) = 0, P (A | B) is undefined
```

```
P (A ^ B) = P (B) * P (A | B)
P (A ^ B) = P (A) * P (B | A)
```

- with the above definition
  the proportion of probabilities in B is maintained
  suppose `A1 =< B` and `A2 =< B`

  ```
  P (A1 | B) / P (A2 | B) =
  P (A1 ^ B) / P (A2 ^ B) =
  P (A1) / P (A2)
  ```

- we specify the probability model by a conditional probability tree
  instead of calculate conditional probability of a given model

```
P (A ^ B) = P (A) * P (B | A)

P (A ^ B ^ C) =
P ((A ^ B) ^ C) =
P (A ^ B) * P ((A ^ B) | C) =
P (A) * P (B | A) * P ((A ^ B) | C)
```

```
P (B) =
P (B ^ (A + ~A)) =
P (B ^ A) + P (B ^ ~A) =
P (A) * P (B | A) + P (~A) * P (B | ~A)

P (B) =
P (B ^ (A1 + A2 + A3)) =
P (B ^ A1) + P (B ^ A2) + P (B ^ A3) =
P (A1) * P (B | A1) +
P (A2) * P (B | A2) +
P (A3) * P (B | A3)
```

- `P (A | B) = P (A ^ B) / P (B)`
  can be interpreted as an inference problem
  suppose `B`, what is the probability of `A`

  - where `P (A ^ B)` and `P (B)` can be calculated
    by the above two sections
    which goes from `P (B | Ai)` to `P (Ai | B)`

  - bayes-rule :
    we know
    `Ai => B -- P (B | Ai)`
    we observe B, and we infer
    `B => Ai -- P (Ai | B)`

  - the => above can be viewed as
    causal relation in the sense of hume

# independence

```
P (B | A) = P (B)

P (A ^ B) =
P (A) * P (B | A) =
P (A) * P (B)
```

- do not confuse independence with disjointness
  disjoint means

  ```
  P (A + B) = P (A) + P (B)
  ```

- A and B are independent means
  the fact that A happens conveys no information about B

- since conditional probability is probability
  independence can be generalized to conditional independence

  - **Xie:**

    ```
    conditional under B:
    (ProbabilitySpace) -> ProbabilitySpace

    conditional:
    (Event, ProbabilitySpace) -> ProbabilitySpace
    ```

    - where a probabilistic-model is like one of our belief

- for many events, independence is a very strong conditional
  we can also define pairwise independence
  - a weaker version of independence

# RandomVariable

- random variable can be viewed statistically first

  - without introducing probabilistic-model

- after we developed the theory of random variable
  we want to push probability space to the background
  only go back to it when really necessary

- a random variable is a function from sample to number

- random variable is used to specify subset of sample space
  on which we can calculate probability

  - **Xie:**
    the concept of function give us a handle
    to discuss more about probabilistic-model
    to express properties of what we are interested in

    - categorical pushout?

- probability mass function -- for discrete random variables

  for continuous random variables
  we need probability density function

  where `sum-up` is changed to `integral`

- **Xie:**
  the above formalization is rough
  we need to formalize set theory first

- expected value -- center of mass

- expected value of function composition

- variance

# RandomField

- note the increasing number of sets here

  | Sample | ProbabilitySpace |
  | State  | RandomVariable   |
  | Index  | RandomField      |

# continuous random variables

- instead of using probability mass function (PMF)
  we need probability density function (PDF)
  for continuous random variables

- density is no probability,
  density is rate at which probabilities accumulate

- to keep the over all probability equal to one,
  density at one point does not have to be less than one.

- **Xie:**
  how to formalize continuous random variables?
  we may need to formalize integral first

- cumulative distribution function (CDF)
  is well defined for both continuous and discrete random variables

- PMF of discrete random variables
  can be viewed as impulse function (Dirac delta function)
  https://en.wikipedia.org/wiki/Dirac_delta_function

- if a CDF is useful (such as normal distribution)
  we do not calculate it in close form
  we just tabulate it and use it

- conditional PDF is normalized sections of PDF
  - normalized with infinitesimal as the dominator
    re-distribute over infinitesimal

# Iterated Expectations

# Bernoulli Process

# Poisson Process

# Markov Chains

# Weak Law of Large Numbers

# Central Limit Theorem

# Bayesian Statistical Inference

# Classical Statistical Inference

# Classical Inference
