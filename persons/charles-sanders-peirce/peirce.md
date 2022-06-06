---
title: Charles Sanders Peirce
---

# links

- https://en.wikisource.org/wiki/Author:Charles_Sanders_Peirce

- https://en.wikipedia.org/wiki/Charles_Sanders_Peirce
- https://en.wikipedia.org/wiki/Charles_Sanders_Peirce_bibliography

- https://plato.stanford.edu/entries/peirce/
- https://plato.stanford.edu/entries/peirce-logic/

- http://peirce.iupui.edu/index.html

  A philosophical website,
  providing access to resources for
  the life, work, and continuing interest in
  the American philosopher, scientist, and humanist
  Charles Sanders Peirce

- http://www.pragmaticism.net/

- https://academic.oup.com/monist

# [note]

- **Xie**:
  When practicing physical science,
  the whole meaning of a physical concept
  is determined by an exact method of measuring it.

# [note] deduction induction abduction

- https://plato.stanford.edu/entries/peirce/#dia

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

# [note] pragmaticism and scientific method

- For Peirce, the entire universe and everything in it
  is an evolutionary product. Indeed, he conceived that
  even the most firmly entrenched of nature's habits

  - for example, even those habits
    that are typically called "natural laws"
    have themselves evolved, and accordingly can and should be
    subjects of philosophical and scientific inquiry.

  One can sensibly seek evolutionary explanations
  of the existence of particular natural laws.

- both Hegel and Peirce make the whole evolutionary interpretation
  of the evolving phaneron to be a process that is said to be logical,
  the "action" of logic itself.

- Hegel's logic is vague

  but Peirce's logic is clear and is the whole logical apparatus
  of the physical and social sciences.

- scientific method is

  - essentially public
  - reproducible in its activities
  - self-correcting in the following sense:
    No matter where different researchers may begin,
    as long as they follow the scientific method,
    their results will eventually converge toward the same outcome.

    - the equivalence between theories
      are defined by pragmatic principle

    - **Xie:**
      how about diverging theories in mathematics?

- optimistic
  Peirce tends to hold that every genuine question

  - that is, every question whose possible answers
    have empirical content (or pragmatic content)
    can be answered in principle, or at least
    should not be assumed to be unanswerable.

  For this reason, one his most important dicta,
  which he called his first principle of reason,
  is "Do not block the way of inquiry!"

- the scientific method involves three phases or stages:

  - abduction -- making conjectures or creating hypotheses
  - deduction -- inferring what should be the case
    if the hypotheses are the case
  - induction -- the testing of hypotheses

  The process of going through the stages
  should also be carried out with concern for the economy of research.

- economy of research
  science is essentially a human and social enterprise
  and that it always operates in some given historical,
  social, and economic context.

# [note] continuum

- if a line is cut into two portions,
  the point at which the cut takes place actually becomes two points.

- **Xie:**
  to develop a good theory of continuum
  we need a theory of boundary

# [note] probability

- statistical information is often the best information
  that we can have about phenomena

- there are three distinct and
  mutually incommensurable measures of uncertainty
  (of imperfection of certitude)

  only one was probability

  | probability | deduction |
  | verisimilitude (likelihood) | induction |
  | plausibility | abduction |

  - for plausibility :
    seeking hypothesis for some actual fact that is surprising,
    the hypothesis must be plausible in order to taken seriously.

    - for example, using god or UFO
      to explain some surprising phenomena
      is not plausibility (is not to taken seriously)

    - **Xie:**
      plausibility is said to be hard to be formalized,
      how about using markov logic networks?

- two interpretations of probability

  - subjectivists -- conceptualists
    these believed that probability was
    a measure of the strength of belief
    actually accorded to a proposition
    or a measure of the degree of rational belief
    that ought to be accorded to a proposition

  - objectivists -- materialists
    these believed that probability was
    a measure of the relative frequency
    with which an event of some specific sort repeatedly happened

- peirce adopted an objectivist notion of probability

  - probability is a notion with clear empirical content
    and that there are clear empirical procedures
    for ascertaining that content

  - probability is not just a measure of events
    rather, is an argument,
    an argument having premisses and a conclusion

    - just like the view of Kolmogoroff that
      all probabilities are conditional probabilities

- peirce rejects Bayesianism and the method of inverse probabilities

  in fact no probability at all can be assigned to inductive arguments.
  Instead of probability,
  a different measure of imperfection of certitude
  must be assigned to inductive arguments:
  verisimilitude or likelihood.

  - about hypothesis-testing

# (1868) some consequences of four incapacities

# (1868) questions concerning certain faculties claimed for man

- **Xie:**
  about semiotics

# (1868) on a new list of categories

- http://www.iupui.edu/~arisbe/menu/library/bycsp/newlist/nl-frame.htm

- https://en.wikipedia.org/wiki/Categories_(Peirce)
  In Aristotle's logic, categories are adjuncts to reasoning
  that are designed to resolve equivocations, ambiguities
  that make expressions or signs recalcitrant to being ruled by logic.
  Categories help the reasoner to render signs
  ready for the application of logical laws.
  An equivocation is a variation in meaning
  -- a manifold of sign senses -- such that,
  as Aristotle put it about names in the opening of Categories
  "Things are said to be named 'equivocally' when,
  though they have a common name,
  the definition corresponding with the name differs for each".
  So Peirce's claim that three categories are sufficient amounts
  to an assertion that all manifolds of meaning
  can be unified in just three steps.

- the act of lifting as argument of relation and increasing arity

- classification of relations

- analysis of dependence between relations
  prescind <-> depend

- analysis relations of arity 0 1 2 3

# (1869) grounds of validity of the laws of logic

- Further Consequences of Four Incapacities

# (2000) (geraldine brady) from peirce to skolem

## 1 The Early Work of Charles S. Peirce

## 2 Peirce's Calculus of Relatives: 1870

- (1870) Description of a Notation for the Logic of Relatives

- Peirce's belief that
  this was the most important advance since Boole
  was certainly based on the fact that
  the algebra ofrelations is far more expressive
  than the algebra of propositions,
  and reflects a great deal more of everyday logical inference
  than does Boole'stheory of sets,
  since relations, not just sets,
  are the bread and butter ofreasoning.

- In this early work on the calculus of relatives,
  Peirce shows more concern with
  maintaining the analogies between
  the notation he is setting forth
  and ordinary algebraic notation
  than with giving a direct account of the problem at hand.

## 3 Peirce on the Algebra of Logic: 1880

- (1880) On the algebra of logic

## 4 Mitchell on a New Algebra of Logic: 1883

## 5 Peirce on the Algebra or Relatives: 1883

## 6 Peirce's Logic of Quantifiers: 1885

## 7 Schroder's Calculus of Relatives

## 8 Lowenheim's Contribution

## 9 Skolem's Recasting

# [note] semiotics (semeiotics)

## etymology

- semiotics
  Coined by John Locke
  from σημειωτικός (sēmeiōtikós, "fitted for marking, portending")
  from σημειοῦν (sēmeioûn, "to mark, interpret as a portend")
  from σημεῖον (sēmeîon, "a mark, sign, token")
  from σῆμα (sêma, "mark, sign")

- portend
  Borrowed from Latin portendere ("to foretell")
  from por- ("forward") + tendere ("to stretch"),
  present active infinitive of tendo

## triadic definition

- [Semiotic_theory_of_Charles_Sanders_Peirce](https://en.wikipedia.org/wiki/Semiotic_theory_of_Charles_Sanders_Peirce)

- Peirce adopted the term semiosis (or semeiosis)
  and defined it to mean an "action, or influence,
  which is, or involves, a cooperation of three subjects,
  such as a sign, its object, and its interpretant,
  this trirelative influence
  _not being in any way resolvable_
  into actions between pairs".

  - **Xie:**
    in my view, the three means,

    1. formal syntax and abstract syntax
    2. objects
    3. interpretation -- operations on objects
       note that,
       interpretation might happens in context (or in environment)

  - one might say my view is wrong,
    because many formal syntax can denote the same system of objects,
    so objects are independent from syntax,
    but it is also true that there must at least one syntax,
    and different syntax might make us think differently.
    thus the three are intertwined.

- his semiotics is not contained
  in a mathematical or deductive system
  and does not proceed chiefly by drawing necessary conclusions
  about purely hypothetical objects or cases.

- [Interpretant](https://en.wikipedia.org/wiki/Interpretant)
  For example, one way to approach the concept of an interpretant
  is to think of a psycholinguistic process.
  In this context, an interpretant can be understood as
  a sign's effect on the mind, or on anything that acts like a mind,
  what Peirce calls a quasi-mind.
  An interpretant is what results from a process of interpretation,
  one of the types of activity
  that falls under the heading of semiosis.

- Peirce argued that logic is the formal study of signs
  in the broadest sense, not only signs that are
  artificial, linguistic, or symbolic,
  but also signs that are semblances or are indexical
  such as reactions.

- [Indexicality](https://en.wikipedia.org/wiki/Indexicality)

# [todo] (1902) the simplest mathematics

- [Hypostatic_abstraction](https://en.wikipedia.org/wiki/Hypostatic_abstraction)
  to convert an adjective or predicate into an extra subject,
  thus increasing by one the number of "subject" slots,
  called the arity or adicity, of the main predicate.
  - for example "Honey is sweet"
    is transformed into "Honey has sweetness"
