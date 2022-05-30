---
title: The Book of Why
authors: [Judea Pearl, Dana Mackenzie]
year: 2018
---

# INTRODUCTION Mind over Data

The aim is to build a mathematical model of cause and effect -- a calculus of causation.

- **Xie**: How does the model of Judea Pearl relate to
  - logic (deduction system)
  - Polya's "Patterns of plausible inference"

The calculus of causation consists of two languages:

- causal diagrams, to express what we know,
- and a symbolic language, resembling algebra,
  to express what we want to know.

The causal diagrams are simply dot-and-arrow pictures
that summarize our existing scientific knowledge.

The dots represent quantities of interest, called "variables",
and the arrows represent known or suspected causal relationships between those variables
-- namely, which variable "listens" to which others.

- **Xie**: How does causal diagrams relate to
  reactive programming
  and Sussman's propagator model?

These diagrams are extremely easy to draw, comprehend, and use,
and the reader will find dozens of them in the pages of this book.
If you can navigate using a map of one-way streets,
then you can understand causal diagrams,
and you can solve the type of questions
posed at the beginning of this introduction.

Side by side with this diagrammatic "language of knowledge",
we also have a symbolic "language of queries"
to express the questions we want answers to.

For example, if we are interested in
the effect of a drug `D` on lifespan `L`,
then our query might be written symbolically as: `P(L | do(D))`.

The vertical line means "given that",
so we are asking: what is the probability `P`
that a typical patient would survive `L` years,
given that he or she is made to take the drug `do(D)`?
In many cases we may also wish to compare `P(L | do(D))` with `P(L | do(not-D))`.
The do-operator signifies that we are dealing with
an intervention rather than a passive observation;
classical statistics has nothing remotely similar to this operator.

We must invoke an intervention operator `do(D)` to ensure that the
observed change in Lifespan `L` is due to the drug itself
and is not confounded with other factors that tend to shorten or lengthen life.
If, instead of intervening, we let the patient himself
decide whether to take the drug, those other factors might
influence his decision, and lifespan differences between
taking and not taking the drug would no longer be solely due to the drug.
For example, suppose only those who were terminally ill took the drug.

Mathematically, we write the observed frequency of Lifespan `L` among
patients who voluntarily take the drug as `P(L | D)`, which is the standard
conditional probability used in statistical textbooks. This expression stands
for the probability `P` of Lifespan `L` conditional on seeing the patient take
Drug `D`. Note that `P(L | D)` may be totally different from `P(L | do(D))`.

This difference between seeing and doing is fundamental and explains why we do
not regard the falling barometer to be a cause of the coming storm. Seeing
the barometer fall increases the probability of the storm, while forcing it to
fall does not affect this probability.

One of the crowning achievements of the Causal Revolution has been to
explain how to predict the effects of an intervention without actually
enacting it.

When the scientific question of interest involves retrospective thinking,
we call on another type of expression unique to causal reasoning called a
**counterfactual**. For example, suppose that Joe took Drug `D` and died a month
later; our question of interest is whether the drug might have caused his
death. To answer this question, we need to imagine a scenario in which Joe
was about to take the drug but changed his mind. Would he have lived?

As with predicting the effect of interventions (mentioned above),
in many cases we can emulate human retrospective thinking
with an algorithm that takes what we know about the observed world
and produces an answer about the counterfactual world.
This "algorithmization of counterfactuals"
is another gem uncovered by the Causal Revolution.

Counterfactual reasoning, which deals with what-ifs, might strike some
readers as unscientific. Indeed, empirical observation can never confirm or
refute the answers to such questions. Yet our minds make very reliable and
reproducible judgments all the time about what might be or might have been.
We all understand, for instance, that had the rooster been silent this morning,
the sun would have risen just as well. This consensus stems from the fact that
counterfactuals are not products of whimsy but reflect the very structure of
our world model. Two people who share the same causal model will also
share all counterfactual judgments.

Counterfactuals are the building blocks of moral behavior as well as
scientific thought. The ability to reflect on one's past actions and envision
alternative scenarios is the basis of free will and social responsibility. The
algorithmization of counterfactuals invites thinking machines to benefit from
this ability and participate in this (until now) uniquely human way of
thinking about the world.

In the world of AI, you do not really understand a topic
until you can teach it to a mechanical robot. That is
why you will find me emphasizing and reemphasizing notation, language,
vocabulary, and grammar.

For example, I obsess over whether we can express a certain claim
in a given language and whether one claim follows from others.
It is amazing how much one can learn from just following the
grammar of scientific utterances. My emphasis on language also comes from
a deep conviction that language shapes our thoughts. You cannot answer a
question that you cannot ask, and you cannot ask a question that you have no
words for. As a student of philosophy and computer science, my attraction to
causal inference has largely been triggered by the excitement of seeing an
orphaned scientific language making it from birth to maturity.

- **Xie**: Can we develop a logic for counterfactual judgments?

- **Xie**: About the relation between logic and causation.

  If we think about inference rules in a logic system.

  A judgment takes some data as arguments,
  and a group of inference rules for a judgment
  is a definition of the judgment as a relation
  (thinking about Prolog or inductive datatype in dependent type system).

  The relations are about about cause and effect at all.

  There is a Prolog book called "Clause and Effect", but not "Cause and Effect".

  Relation has no direction, but when we use the idea of "bidirectional type checking"
  to turn a type checking judgment to a function that does type checking,
  we introduced a direction to the relation.

## A Blueprint of Reality

A blueprint for a "causal inference engine" that
might handle causal reasoning for a future artificial intelligence.

The inference engine is a machine that
accepts three different kinds of inputs
-- `Assumptions`, `Queries`, and `Data`
-- and produces three kinds of outputs.

The first of the outputs is a Yes/No decision as to whether the given
query can in theory be answered under the existing causal model, assuming
perfect and unlimited data.

If the answer is Yes, the inference engine next produces an `Estimand`.

This is a mathematical formula that can be thought of
as a recipe for generating the answer from any hypothetical data, whenever
they are available.

Finally, after the inference engine has received the `Data` input,
it will use the recipe to produce an actual `Estimate` for the answer,
along with statistical estimates of the amount of uncertainty in that estimate.
This uncertainty reflects the limited size of the data set as well as possible
measurement errors or missing data.

```typescript
class CausalInferenceEngine {
  assumptions: Array<Assumption>

  compile(query: Query): Estimand | undefined
  estimate(estimand: Estimand, data: Data): Estimate
}

class Estimate {
  uncertainty: Uncertainty
}
```

Notes:

- `Assumption` is formalized knowledge,
  for example in the form of causal diagrams.

  In a causal diagram, a variable `X` is a cause of `Y`
  if `Y` "listens" to `X` and determines its value in response to what it hears.

- `Estimand` comes from Latin, meaning "that which is to be estimated".
  This is a statistical quantity to be estimated from the data
  that, once estimated, can legitimately represent the answer to our query.

  While written as a probability formula
  -- for example, `P(L | D, Z) × P(Z)`
  -- it is in fact a recipe for answering the causal query
  from the type of data we have, once it has been certified by the engine.

- `Query`, for example "What is the effect of Drug D on Lifespan L?"

- `Estimate`, for example "Drug `D` increases the Lifespan `L`
  of diabetic Patients Z by 30 percent, plus or minus 20 percent".

- `Data` we collect data only after we posit the causal model,
  after we state the scientific query we wish to answer,
  and after we derive the estimand.

Note that, information about the effects of actions or interventions
is simply not available in raw data, unless it is collected
by controlled experimental manipulation.
By contrast, if we are in possession of a causal model,
we can often predict the result of an intervention
from hands-off, intervention-free data.

The case for causal models becomes even more compelling
when we seek to answer counterfactual queries such as
"What would have happened had we acted differently?"
We will discuss counterfactuals in great detail
because they are the most challenging queries for any artificial intelligence.
They are also at the core of the cognitive advances that made us human
and the imaginative abilities that have made science possible.
We will also explain why any query about the mechanism
by which causes transmit their effects
-- the most prototypical "Why?" question --
is actually a counterfactual question in disguise.
Thus, if we ever want robots to answer "Why?" questions
or even understand what they mean, we must equip them with a causal model
and teach them how to answer counterfactual queries.

If I could sum up the message of this book in one pithy phrase, it would
be that you are smarter than your data. Data do not understand causes and
effects; humans do. I hope that the new science of causal inference will
enable us to better understand how we do it, because there is no better way to
understand ourselves than by emulating ourselves. In the age of computers,
this new understanding also brings with it the prospect of amplifying our
innate abilities so that we can make better sense of data, be it big or small.

# CHAPTER 1 The Ladder of Causation

# CHAPTER 2 From Buccaneers to Guinea Pigs: The Genesis of Causal Inference

# CHAPTER 3 From Evidence to Causes: Reverend Bayes Meets Mr. Holmes

# CHAPTER 4 Confounding and Deconfounding: Or, Slaying the Lurking Variable

# CHAPTER 5 The Smoke-Filled Debate: Clearing the Air

# CHAPTER 6 Paradoxes Galore!

# CHAPTER 7 Beyond Adjustment: The Conquest of Mount Intervention

# CHAPTER 8 Counterfactuals: Mining Worlds That Could Have Been

# CHAPTER 9 Mediation: The Search for a Mechanism

# CHAPTER 10 Big Data, Artificial Intelligence, and the Big Questions
