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

The connection between imagining and causal relations is almost self-evident.
It is useless to ask for the causes of things unless you can imagine
their consequences. Conversely, you cannot claim that Eve caused you to eat
from the tree unless you can imagine a world in which, counter to facts, she
did not hand you the apple.

## The three levels of causation

A causal learner must master at least three distinct levels of cognitive ability:
seeing, doing, and imagining.

**The Ladder of Causation**

1. **Association**

activity: Seeing, Observing

questions:

- What if I see ...?
- How are the variables related?
- How would seeing X change my belief of Y?

examples:

- What does a symptom tell me about a disease?
- What does a survey tell us about the election results?

2. **Intervention**

activity: Doing, Intervening

questions:

- What if I do ...? How?
- What would Y be if I do X?
- How can I make Y happen?

examples:

- If I take aspirin, will my headache be cured?
- What if we can ban cigarettes?

3. **Counterfactuals**

activity: Imagining, Retrospection, Understanding

questions:

- What if I had done ...? Why?
- Was it X that caused Y?
- What if X had not occured?
- What if I had acted differently?

examples:

- Was it the aspirin that stopped my headache?
- Would Kennedy be alive if Oswald had not killde him?
- What if I had not smoked for the last 2 years?

- **Xie**: We answer a why-question by creating a function
  which can answer all the questions of the form: "What if I had done ...?"

  The development of a theory that will enable us to
  predict what will happen in situations
  we have not even envisioned yet.

The laws of physics, for example,
can be interpreted as counterfactual assertions,
such as "Had the weight on this spring doubled,
its length would have doubled as well" (Hooke's law).

- **Xie:** How to formaliz the above in a logic system?

  Maybe a functional relationship? A function from weight to length?

  Note that, if to be model by a programmer, a spring might be a class:

  ```typescript
  class Spring {
    weight: number

    length(): number {
      // Maybe depends on weight and a coefficient.
    }
  }
  ```

- **Xie:** Maybe a _counterfactual assertion_ can be viewed as a _logical judgment_.

  In the view, logic and mathematics only study

  - **Logic**

    - How to use judgments?
    - How to proof more propositions from given propositions?

  - **Mathematics**

    - What judgments should we study? (mathematical structures)
    - What are the relations between judgments?
    - To collect pattens and to reuse proofs.

If we want our computer to understand causation,
we have to teach it how to break the rules.

- **Xie:** If we model by a class, to understand causation,
  is to be able to change the dependencies between class' fields,
  i.e. change the definition of methods.

  Thus a computer that understands causation,
  is a computer that can program itself.

  I think this is possible, but it requires a different programming model,
  maybe it should uses probability theory.

- **Xie:** One problem of causal diagram is that it only records dependencies,
  but not "how the dependencies"
  i.e. one variable depends on other variables by what function?

  And maybe some dependencies also require us to take uncertainties into acount,
  we will need probability theory and probabilistic programming.

## On probabilities and causation

> Causation is not reducible to probabilities.

It is wrong to define causation as "probability raising":

```
X causes Y = X raises the probability of Y
```

It is wrong to use conditional probabilities `P(Y | X) > P(Y)`.

Because it speaks only about observations:
"If we see `X`, then the probability of `Y` increases".
But this increase may come about for other reasons,
including `Y` being a cause of `X` or some other variable `Z`
being the cause of both of them.

We should use `P(Y | do(X)) > P(Y)`.

I have not attempted to define causation anywhere in this book:
definitions demand reduction, and reduction demands going to a lower rung.
Instead, I have pursued the ultimately more constructive program of explaining how to
answer causal queries and what information is needed to answer them.

The main point is this: while probabilities encode our beliefs about a static world,
causality tells us whether and how probabilities change when the world
changes, be it by intervention or by act of imagination.

# CHAPTER 2 From Buccaneers to Guinea Pigs: The Genesis of Causal Inference

[Sir Francis Galton](https://en.wikipedia.org/wiki/Francis_Galton)
demonstrates his "Galton board" or "quincunx" at the Royal Institution.
He saw this pinball-like apparatus as an analogy
for the inheritance of genetic traits like stature.
The pinballs accumulate in a bell-shaped curve that is similar
to the distribution of human heights.
The puzzle of why human heights don't spread out
from one generation to the next, as the balls would,
led him to the discovery of ["regression to the mean"](https://en.wikipedia.org/wiki/Regression_toward_the_mean).

For the first time, Galton's idea of correlation gave an objective measure,
independent of human judgment or interpretation, of how two variables are
related to one another. The two variables can stand for height, intelligence,
or income; they can stand in causal, neutral, or reverse-causal relation. The
correlation will always reflect the degree of cross predictability between the
two variables.

## Galton and the abandoned quest

It is an irony of history that
Galton started out in search of causation
and ended up discovering correlation,
a relationship that is oblivious of causation.

In supreme irony, what had started out as an attempt to
mathematize the framework of the _Origin of Species_
ended with the essence of that great work being discarded as unnecessary!

The right [causal model of the stability of the genetic endowment](https://en.wikipedia.org/wiki/Hardy%E2%80%93Weinberg_principle)
was explained by [G. H. Hardy](https://en.wikipedia.org/wiki/G._H._Hardy)
and [Wilhelm Weinberg](https://en.wikipedia.org/wiki/Wilhelm_Weinberg) in 1908,
-- [the Mendelian theory of inheritance](https://en.wikipedia.org/wiki/Mendelian_inheritance).

It is interesting to note how close Galton came to finding the right framework
and also how the causal diagram makes it easy to zero in on his mistaken assumption:
the transmission of luck from one generation to the next.

Unfortunately, he was led astray by his beautiful but flawed causal model,
and later, having discovered the beauty of correlation,
he came to believe that causality was no longer needed.

## Pearson: The wrath of the zealot

Galton had proved only that one phenomenon
-- regression to the mean -- did not require a causal explanation.
Now [Karl Pearson](https://en.wikipedia.org/wiki/Karl_Pearson)
was completely removing causation from science.
What made him take this leap?

Pearson had been wrestling with the philosophical foundation of physics and
wrote (for example), "Force as a cause of motion is exactly on the same
footing as a tree-god as a cause of growth."
More generally, Pearson belonged to a philosophical school called positivism,
which holds that the universe is a product of human thought
and that science is only a description of those thoughts.
Thus causation, construed as an objective process
that happens in the world outside the human brain,
could not have any scientific meaning.
Meaningful thoughts can only reflect patterns of observations,
and these can be completely described by correlations.
Having decided that correlation was a more
universal descriptor of human thought than causation,
Pearson was prepared to discard causation completely.

## Sewall Wright, guinea pigs, and path diagrams

> [Sewall Wright](https://en.wikipedia.org/wiki/Sewall_Wright).

## E pur si muove (And yet it moves)

> [And yet it moves](https://en.wikipedia.org/wiki/And_yet_it_moves).

... Maybe his Midwestern
upbringing and the tiny college he went to encouraged his self-reliance and
taught him that the surest kind of knowledge is what you construct yourself.

If only we could go back and ask Wright's contemporaries,
"Why didn't you pay attention?"
Crow suggests one reason:
path analysis "doesn't lend itself to 'canned' programs.
The user has to have a hypothesis
and must devise an appropriate diagram
of multiple causal sequences.”
Indeed, Crow put his finger on an essential point:
path analysis requires scientific thinking,
as does every exercise in causal inference.
Statistics, as frequently practiced,
discourages it and encourages "canned" procedures instead.
Scientists will always prefer routine calculations on data
to methods that challenge their

R. A. Fisher, the undisputed high priest of statistics in the generation
after Galton and Pearson, described this difference succinctly.
In 1925, he wrote, "Statistics may be regarded as ...
the study of methods of the reduction of data."

Wright abhorred the idea of statistics as merely a collection of methods;
Fisher embraced it. Causal analysis is emphatically not just about
data; in causal analysis we must incorporate some understanding of the
process that produces the data, and then we get something that was not in the
data to begin with. But Fisher was right about one point: once you remove
causation from statistics, reduction of data is the only thing left.

## From objectivity to subjectivity -- The bayesian connection

One other theme in Wright's rebuttal may hint at another reason for the
resistance of statisticians to causality. He repeatedly states that he did not
want path analysis to become "stereotyped." According to Wright,
"The unstereotyped approach of path analysis differs profoundly from the
stereotyped modes of description designed to avoid any departures from
complete objectivity."

# CHAPTER 3 From Evidence to Causes: Reverend Bayes Meets Mr. Holmes

# CHAPTER 4 Confounding and Deconfounding: Or, Slaying the Lurking Variable

# CHAPTER 5 The Smoke-Filled Debate: Clearing the Air

# CHAPTER 6 Paradoxes Galore!

# CHAPTER 7 Beyond Adjustment: The Conquest of Mount Intervention

# CHAPTER 8 Counterfactuals: Mining Worlds That Could Have Been

# CHAPTER 9 Mediation: The Search for a Mechanism

# CHAPTER 10 Big Data, Artificial Intelligence, and the Big Questions
