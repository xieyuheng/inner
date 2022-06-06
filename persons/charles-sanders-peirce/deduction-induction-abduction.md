---
title: Deduction Induction Abduction
---

[ [PLATO.STANFORD](https://plato.stanford.edu/entries/peirce/#dia) ]

- Peirce's trichotomy of inferences [arguments]
  can be obtained by asking what would happen
  if one were to interchange propositions in the syllogism AAA-1

  and providing syllogism as drawing conclusions on the basis of taking samples

  - i.e. statistical interpretation

- deduction -- syllogism AAA-1 (Barbara)

  - necessary inference
    if major premise and minor premise are both "true"
    the result conclusion is also "true"

    where "true" can be defined as constructable

  ```
  | M | membership of some population |
  | P | property of members |
  | S | sample taken from population |
  ```

  ```
  | M -> P | rule   | X% population have property         |
  | S -> M | case   | sample is taken from the population |
  |--------|--------|-------------------------------------|
  | S -> P | result | X% sample have property             |
  ```

  - while the syllogism can be viewed as function composition
    it also can be viewed as subtype relation

  ```
  M <: P
  S <: M
  ------
  S <: P
  ```

- induction -- syllogism AAA-3

  - exchange result with rule

  ```
  | S -> P | result | X% sample have property             |
  | S -> M | case   | sample is taken from the population |
  |--------|--------|-------------------------------------|
  | M -> P | rule   | X% population have property         |
  ```

  - the core meaning of induction is to argue from sample to population

    the proportion of a trait found in the sample
    is attributed also to the population

  - **Xie:**
    like constant conjunction of hume

- abduction -- syllogism AAA-2

  - a.k.a. hypotheses, retroduction and "educated guess"

  - exchange result with case

  ```
  | M -> P | rule   | X% population have property         |
  | S -> P | result | X% sample have property             |
  |--------|--------|-------------------------------------|
  | S -> M | case   | sample is taken from the population |
  ```

  - arguing a plausible explanation

  - **Xie:**
    we see similarity between A and B,
    then we conjecture relations behind the similarity.

- practice the trichotomy by classifying argument that occurs in everyday life

  - analogy should be construed as inductions
    -- arguments from a sample of the properties of things
    to a population of the properties of things

    or abductions -- conjectures made on the basis of sufficient similarity
    which notion might not easily be analyzed in terms of sets of properties

    - mathematical structure is one example of "sufficient similarity"

- **Xie:**
  we can also write down the function application version of the trichotomy

  - deduction

  ```
  A -> B
  A
  -------
  B
  ```

  - induction

  ```
  B
  A
  -------
  A -> B
  ```

  - induction in application form mirrors
    abduction in composition form

  ```
  M -> P
  S -> P
  ------
  S -> M
  ```

  - abduction

  ```
  A -> B
  B
  -------
  A
  ```

- **Xie:**
  the function composition view of syllogism
  also give us a non-commutative algebra

  - https://en.wikipedia.org/wiki/Noncommutative_ring

  | argument type | equation to solve |
  | ------------- | ----------------- |
  | deduction     | a b == x          |
  | induction     | x b == c          |
  | abduction     | a x == c          |

- Peirce extend the trichotomy of arguments
  to three phases of the methodology of science
  -- a systematic procedure for seeking truth
  that he called the "scientific method"

  Scientific method begins with abduction or hypothesis:
  because of some perhaps surprising or puzzling phenomenon,
  a conjecture or hypothesis is made about what actually is going on.

  This hypothesis should be such as to explain the surprising phenomenon,
  such as to render the phenomenon more or less a matter of course
  if the hypothesis should be true.

  Scientific method then proceeds to the stage of deduction:
  by means of necessary inferences, conclusions are drawn
  from the provisionally adopted hypothesis
  about the obtaining of phenomena other than the surprising one
  that originally gave rise to the hypothesis.

  Conclusions are reached, that is to say,

  - beliefs are fixed
    about other phenomena that must obtain if the hypothesis should actually be true.
    These other phenomena must be such that experimental tests
    can be performed whose results tell us
    whether the further phenomena do obtain or do not obtain.

  the "feedback loop" of scientific method goes on and on

  If the deduced consequences do obtain,
  then we loop back to the deduction stage,
  deducing still further consequences of our hypothesis
  and experimentally testing for them again.

  But, if the deduced consequences do not obtain,
  then we loop back to the abduction stage
  and come up with some new hypothesis
  that explains both our original surprising phenomenon
  and any new phenomena we have uncovered
  in the course of testing our first, and now failed, hypothesis.

- hypothesis-testing, but not merely that part of it
  that consists of arguing from sample to population

- "the economy of research"
  The idea is that, because research is difficult,
  research labor-time is valuable and should not be wasted.
  Both in the creation of hypotheses to be tested
  and in the experiments chosen to test these hypotheses.

  The object is to proceed at every stage
  so as to maximize the reduction in indeterminacy of our beliefs.

  - mathematical theory of the economy of research can be developed

- **Xie:**
  it is as if intelligence are this kind of creature
  who are always seeking for beliefs
  if its beliefs are broken, it trys its best to form new beliefs.
