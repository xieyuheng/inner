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

  The relations are not about cause and effect at all.

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
  assumptions: Array<Assumption>;

  compile(query: Query): Estimand | undefined;
  estimate(estimand: Estimand, data: Data): Estimate;
}

class Estimate {
  uncertainty: Uncertainty;
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
    weight: number;

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
of multiple causal sequences."
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

What does he mean? First, he means that path analysis should be based
on the user's personal understanding of causal processes, reflected in the
causal diagram. It cannot be reduced to mechanical routines, such as those
laid out in statistics manuals. For Wright, drawing a path diagram is not a
statistical exercise; it is an exercise in genetics, economics, psychology, or
whatever the scientist's own field of expertise is.

- **Xie:** Maybe our way of understanding the world by causation
  is fundamentally subjective.

Most of the tools of statistics strive for complete objectivity.
There is one important exception to this rule
-- [Bayesian statistics](https://en.wikipedia.org/wiki/Bayesian_statistics).

The prototype of Bayesian analysis goes like this:

> Prior Belief + New Evidence -> Revised Belief.

Unfortunately, the acceptance of Bayesian subjectivity in mainstream
statistics did nothing to help the acceptance of causal subjectivity,
the kind needed to specify a path diagram.
Why? The answer rests on a grand linguistic barrier.
To articulate subjective assumptions,
Bayesian statisticians still use the language of probability,
the native language of Galton and Pearson.
The assumptions entering causal inference, on the other hand,
require a richer language (e.g., diagrams)
that is foreign to Bayesians and frequentists alike.
The reconciliation between Bayesians and frequentists shows that
philosophical barriers can be bridged with goodwill and a common language.
Linguistic barriers are not surmounted so easily.

On the positive side, causal inference is objective
in one critically important sense:
once two people agree on their assumptions,
it provides a 100 percent objective way
of interpreting any new evidence (or data).

I arrived at the theory of causality through a circuitous route
that started with Bayesian probability and then
took a huge detour through Bayesian networks.
I will tell that story in the next chapter.

# CHAPTER 3 From Evidence to Causes: Reverend Bayes Meets Mr. Holmes

"It's elementary, my dear Watson."

So spoke Sherlock Holmes just before dazzling his faithful assistant
with one of his famously nonelementary deductions.
But in fact, Holmes performed not just deduction,
which works from a hypothesis to a conclusion.
His great skill was induction, which works in the
opposite direction, from evidence to hypothesis.

- **Xie:** We know Peirce has
  [deduction](https://en.wikipedia.org/wiki/Deductive_reasoning),
  [induction](https://en.wikipedia.org/wiki/Inductive_reasoning) and
  [abduction](https://en.wikipedia.org/wiki/Abductive_reasoning).

  How do they relate to causal inference?

Bayesian networks is a tool for inductive reasoning.

## Bonaparte, the computer detective

Bonaparte is a state-of-the-art disaster victim identification program.
This software, developed in the mid-2000s by a team from Radboud University in Nijmegen,
uses Bayesian networks to combine DNA information
taken from several different family members of the victims.

## Reverend Bayes and the problem of inverse probability

> [Thomas Bayes](https://en.wikipedia.org/wiki/Thomas_Bayes).

```
P(S | T) * P(T) = P(T | S) * P(S)
```

This innocent-looking equation came to be known as "Bayes's rule".
If we look carefully at what it says, we find that
it offers a general solution to the inverse-probability problem.
It tells us that if we know the probability of `S given T`, `P(S | T)`,
we ought to be able to figure out the probability of `T given S`, `P(T | S)`,
assuming of course that we know `P(T)` and `P(S)`.

This is perhaps the most important role of Bayes's rule in statistics:
we can estimate the conditional probability directly in one direction,
for which our judgment is more reliable, and use mathematics
to derive the conditional probability in the other direction,
for which our judgment is rather hazy.
The equation also plays this role in Bayesian networks;
we tell the computer the forward probabilities,
and the computer tells us the inverse probabilities when needed.

We can also look at Bayes's rule as a way to update our belief in a
particular hypothesis. This is extremely important to understand, because a
large part of human belief about future events rests on the frequency with
which they or similar events have occurred in the past.
Indeed, when a customer walks in the door of the restaurant,
we believe, based on our past encounters with similar customers,
that she probably wants tea.
But if she first orders scones, we become even more certain.
In fact, we might even suggest it: "I presume you want tea with that?"
Bayes's rule simply lets us attach numbers to this reasoning process.
From Table 3.1, we see that the prior probability that the customer wants tea
(meaning when she walks in the door, before she orders anything) is two-thirds.
But if the customer orders scones, now we have additional information about her
that we didn't have before.
The updated probability that she wants tea, given that she has ordered
scones, is `P(T | S) = 4/5`.

Mathematically, that's all there is to Bayes's rule. It seems almost trivial.
Why it makes Bayes famous and why people have argued over his rule for 250 years?

Here I must confess that in the teahouse example, by deriving Bayes's
rule from data, I have glossed over two profound objections, one
philosophical and the other practical. The philosophical one stems from the
interpretation of probabilities as _a degree of belief_, which we used implicitly
in the teahouse example. Who ever said that beliefs act, or should act, like
proportions in the data?

The crux of the philosophical debate is whether we can legitimately
translate the expression "given that I know" into the language of
probabilities. Even if we agree that the unconditional probabilities `P(S)`, `P(T)`,
and `P(S AND T)` reflect my degree of belief in those propositions, who says
that my revised degree of belief in `T` should equal the ratio `P(S AND T)/P(S)`,
as dictated by Bayes's rule? Is "given that I know T" the same as "among
cases where `T` occurred""? The language of probability, expressed in symbols
like `P(S)`, was intended to capture the concept of frequencies in games of
chance. But the expression "given that I know" is epistemological and
should be governed by the logic of knowledge, not that of frequencies and
proportions.

From the philosophical perspective, Thomas Bayes's accomplishment lies in
his proposing the first formal definition of conditional probability as the ratio

```
P(S | T) = P(S AND T)/P(T)
```

As we saw, Bayes's rule is formally an elementary consequence
of his definition of conditional probability.
But epistemologically, it is far from elementary.
It acts, in fact, as a normative rule for
_updating beliefs in response to evidence_.

In other words, we should view Bayes's rule not just as a convenient definition
of the new concept of "conditional probability" but as an empirical claim
to faithfully represent the English expression "given that I know".

It asserts, among other things, that the belief a person attributes
to `S` after discovering `T` is never lower than the degree of belief that person
attributes to `S AND T` before discovering `T`.
Also, it implies that the more surprising the evidence `T`
-- that is, the smaller `P(T)` is
-- the more convinced one should become of its cause `S`.
No wonder Bayes and his friend Price, as Episcopal ministers,
saw this as an effective rejoinder to Hume.
If `T` is a miracle ("Christ rose from the dead"),
and `S` is a closely related hypothesis ("Christ is the son of God"),
our degree of belief in `S` is very dramatically increased
if we know for a fact that `T` is true.
The more miraculous the miracle,
the more credible the hypothesis that explains its occurrence.
This explains why the writers of the New Testament
were so impressed by their eyewitness evidence.

In many ways, Bayes's rule is a distillation of the scientific method.
The textbook description of the scientific method goes something like this:

- (1) formulate a hypothesis,
- (2) deduce a testable consequence of the hypothesis,
- (3) perform an experiment and collect evidence,
- (4) update your belief in the hypothesis.

Usually the textbooks deal with simple yes-or-no tests and updates;
the evidence either confirms or refutes the hypothesis.
But life and science are never so simple!
All evidence comes with a certain amount of uncertainty.
Bayes's rule tells us how to perform step (4) in the real world.

## From Bayes's rule to Bayesian networks

In the early 1980s, the field of artificial intelligence had worked itself into a
cul-de-sac. Ever since Alan Turing first laid out the challenge in his 1950
paper "Computing Machinery and Intelligence," the leading approach to AI
had been so-called rule-based systems or expert systems, which organize
human knowledge as a collection of specific and general facts, along with
inference rules to connect them. For example: Socrates is a man (specific
fact). All men are mortals (general fact). From this knowledge base we (or an
intelligent machine) can derive the fact that Socrates is a mortal, using the
universal rule of inference:

```
if all A's are B's,
and x is an A,
------
then x is a B.
```

- **Xie:**: If we formalize man as a type `Man`.

```
f: (x: Man) -> Mortal(x)
x: Man
------
f(x): Mortal(x)
```

The approach was fine in theory, but hard-and-fast rules can rarely capture
real-life knowledge. Perhaps without realizing it, we deal with exceptions to
rules and uncertainties in evidence all the time. By 1980, it was clear that
expert systems struggled with making correct inferences from uncertain
knowledge. The computer could not replicate the inferential process of a
human expert because the experts themselves were not able to articulate their
thinking process within the language provided by the system.

- **Xie**: The representation of knowledge
  as judgments and inferences rules in a logic system,
  is not enough.

TODO Note about belief propagation.

## Bayesian networks: What causes say about data

Although Bayes didn't know it, his rule for inverse probability represents the
simplest Bayesian network. We have seen this network in several guises now:

```
Tea -> Scones
Disease -> Test
```

or, more generally,

```
Hypothesis -> Evidence
```

Unlike the causal diagrams we will deal with throughout the book,
a Bayesian network carries no assumption that the arrow has any causal meaning.
The arrow merely signifies that we know the "forward" probability,
`P(scones | tea)` or `P(test | disease)`.
Bayes's rule tells us how to reverse the procedure,
specifically by multiplying the prior probability by a likelihood ratio.

The next step after a two-node network with one link is, of course,
a three-node network with two links, which I will call a "junction".
These are the building blocks of all Bayesian networks
(and causal networks as well).
There are three basic types of junctions,
with the help of which we can characterize
any pattern of arrows in the network.

1. `A -> B -> C`.

   This junction is the simplest example of a "chain", or of mediation.
   In science, one often thinks of `B` as the mechanism, or "mediator",
   that transmits the effect of `A` to `C`.

   A familiar example is `Fire -> Smoke -> Alarm`.
   Although we call them "fire alarms", they are really smoke alarms.

   For example, imagine a faulty alarm system that
   fails to respond correctly 5 percent of the time.
   If we look only at the rows where `Smoke = 1`,
   we will find that the probability of `Alarm = 1` is the same (95 percent),
   regardless of whether `Fire = 0` or `Fire = 1`.

   The process of looking only at rows in the table where `Smoke = 1`
   is called conditioning on a variable.
   Likewise, we say that `Fire` and `Alarm` are conditionally independent,
   given the value of `Smoke`.

2. `A <- B -> C`.

   This kind of junction is called a "fork",
   and `B` is often called a common cause or confounder of `A` and `C`.
   A confounder will make `A` and `C` statistically correlated
   even though there is no direct causal link between them.

   A good example (due to David Freedman) is
   `Shoe Size <- Age of Child -> Reading Ability`.

   We can eliminate this spurious correlation, as Karl Pearson
   and George Udny Yule called it, by conditioning on the child's
   age. For instance, if we look only at seven-year-olds, we expect
   to see no relationship between shoe size and reading ability. As
   in the case of chain junctions, `A` and `C` are conditionally
   independent, given `B`.

   Before we go on to our third junction, we need to add a
   word of clarification. The conditional independences I have just
   mentioned are exhibited whenever we look at these junctions in
   isolation. If additional causal paths surround them, these paths
   need also be taken into account.

   The miracle of Bayesian networks lies in the fact that the three
   kinds of junctions we are now describing in isolation are sufficient
   for reading off all the independencies implied by a Bayesian network,
   regardless of how complicated.

3. `A -> B <- C`.

   This is the most fascinating junction, called a "collider".

   Felix Elwert and Chris Winship have illustrated this
   junction using three features of Hollywood actors:
   `Talent -> Celebrity <- Beauty`.

   Here we are asserting that both talent and
   beauty contribute to an actor's success,
   but beauty and talent are completely unrelated to one another
   in the general population.

   We will now see that this collider pattern works in exactly
   the opposite way from chains or forks when we condition on
   the variable in the middle.
   If `A` and `C` are independent to begin with,
   conditioning on `B` will make them dependent.
   For example, if we look only at famous actors
   (in other words, we observe the variable `Celebrity = 1`),
   we will see a negative correlation between talent and beauty:
   finding out that a celebrity is unattractive
   increases our belief that he or she is talented.

   This negative correlation
   is sometimes called collider bias
   or the "explain-away" effect.

   For simplicity, suppose that you don't need both
   talent and beauty to be a celebrity;
   one is sufficient.
   Then if a celebrity `A` is a particularly good actor,
   that "explains away" his success,
   and he doesn't need to be any more beautiful
   than the average person.
   On the other hand, if a celebrity `B` is a really bad actor,
   then the only way to explain his success is his good looks.
   So, given the outcome `Celebrity = 1`,
   talent and beauty are inversely related
   -- even though they are not related in the population as a whole.
   Even in a more realistic situation,
   where success is a complicated function of beauty and talent,
   the explain-away effect will still be present.
   This example is admittedly somewhat apocryphal,
   because beauty and talent are hard to measure objectively;
   nevertheless, collider bias is quite real,
   and we will see lots of examples in this book.

These three junctions
-- chains, forks, and colliders
-- are like keyholes through the door
that separates the first and second levels of the Ladder of Causation.
If we peek through them, we can see the secrets of the causal process
that generated the data we observe;
each stands for a distinct pattern of causal flow
and leaves its mark in the form of
conditional dependences and independences in the data.

In my public lectures I often call them "gifts from the gods"
because they enable us to test a causal model,
discover new models, evaluate effects of interventions,
and much more.

Still, standing in isolation, they give us only a glimpse.
We need a key that will completely open the door
and let us step out onto the second rung.
That key, which we will learn about in Chapter 7,
involves all three junctions, and is called d-separation.
This concept tells us, for any given pattern of paths in the model,
what patterns of dependencies we should expect in the data.
This fundamental connection between causes and probabilities
constitutes the main contribution of Bayesian networks
to the science of causal inference.

## Where is my bag? from Aachen to Zanzibar

So far I have emphasized only one aspect of Bayesian networks
-- namely, the diagram and its arrows that
preferably point from cause to effect.
Indeed, the diagram is like the engine of the Bayesian network.
But like any engine, a Bayesian network runs on fuel.
The fuel is called a _conditional probability table_.

Another way to put this is that the diagram describes
the relation of the variables in a qualitative way,
but if you want quantitative answers,
you also need quantitative inputs.
In a Bayesian network, we have to specify
the conditional probability of each node given its "parents".
(Remember that the parents of a node are
all the nodes that feed into it.)
These are the forward probabilities, `P(evidence | hypotheses)`.

TODO Try the calculation in the "Where is my bag?" example.

## Bayesian networks in the real world

Before the turbo revolution,

2G cell phones used "soft decoding" (i.e., probabilities) but not belief propagation.
3G cell phones used Berrou's turbo codes,
and 4G phones used Gallager's turbo-like codes.

From the consumer's viewpoint,
this means that your cell phone uses less energy and the battery lasts longer,
because coding and decoding are your cell phone's most energy-intensive
processes. Also, better codes mean that you do not have to be as close to a
cell tower to get high-quality transmission. In other words, Bayesian networks
enabled phone manufacturers to deliver on their promise: more bars in more places.

## From Bayesian networks to causal diagrams

Bayesian network can not tell the difference between seeing and doing,
or indeed to distinguish a fork from a chain.
In other words, both a chain and a fork would predict that
observed changes in `A` are associated with changes in `C`,
making no prediction about the effect of "wiggling" `A`.

Now we come to the second, and perhaps more important, impact of
Bayesian networks on causal inference. The relationships that were
discovered between the graphical structure of the diagram and the data that it
represents now permit us to emulate wiggling without physically doing so.

# CHAPTER 4 Confounding and Deconfounding: Or, Slaying the Lurking Variable

The randomized controlled trial (RCT) is indeed a wonderful invention.

One of my goals in this chapter is to explain,
from the point of view of causal diagrams,
precisely why RCTs allow us to estimate the causal effect `X -> Y`
without falling prey to confounder bias.
Once we have understood why RCTs work,
there is no need to put them on a pedestal
and treat them as the gold standard of causal analysis,
which all other methods should emulate.
Quite the opposite:
we will see that the so-called gold standard
in fact derives its legitimacy from more basic principles.

- **Xie:** Just like how Girard show that
  the constructiveness of intuitionistic logic
  come from the structure of sequent,
  and we can use the basic principles to get linear logic.

## The chilling fear of confounding

- **Xie:** Even after removed almost all the confounders,
  a research team still do not dare to declare causation.

## The skillful interrogation of nature: Why RCTs work

- **Xie:** Analysis RCT by causal diagrams.

  RCT is one way of performing do-operator.

## The new paradigm of confounding

Lacking a principled understanding of confounding, scientists could not say
anything meaningful in observational studies where physical control over
treatments is infeasible.

Confounding should simply be defined as
anything that leads to a discrepancy between seeing and doing:

```
P(Y | X) != P(Y | do(X))
```

We can answer such questions by referring to the causal diagram
and checking which variables produce a discrepancy between seeing and doing.

Historically, the concept of "confounding"
has evolved around two related conceptions:
incomparability and a lurking third variable.
Both of these concepts have resisted formalization.

When we talked about comparability,
in the context of Daniel's experiment,
we said that the treatment and control groups
should be identical in all relevant ways.
But this begs us to distinguish relevant from irrelevant attributes.

How do we know that age is relevant in the Honolulu walking study?
How do we know that the alphabetical order
of a participant's name is not relevant?
You might say it's obvious or common sense,
but generations of scientists have struggled
to articulate that common sense formally,
and a robot cannot rely on our common sense
when asked to act properly.

## The do-operator and the back-door criterion

TODO Make clear sense of the graph model and the notion of "control".

- I can not make sense of the graph based arguments in this chapter.
  - Maybe I need to read some "Causality -- Models, Reasoning and Inference" first.

# CHAPTER 5 The Smoke-Filled Debate: Clearing the Air

Because scientists had no straightforward definition of the word "cause"
and no way to ascertain a causal effect without a randomized controlled trial,
they were ill prepared for a debate over whether smoking caused cancer. They
were forced to fumble their way toward a definition in a process that lasted
throughout the 1950s and reached a dramatic conclusion in 1964.

## Smoking for Newborns

A example of collider and "explains away" effect.

The observation in the data:

- `smoking mother` increases `low birth weight baby`

- given `low birth weight baby`,
  babies who has `smoking mother`,
  has lower `mortality of baby`.

It looks like `smoking mother` helped
`low birth weight baby`'s survival.

```
smoking mother -> low birth weight baby
serious genetic abnormalities -> low birth weight baby

smoking mother -> mortality of baby
serious genetic abnormalities -> mortality of baby

low birth weight baby -> mortality of baby
```

The collider is `Birth Weight`.
By looking only at babies with low birth weight,
we are conditioning on that collider.
This opens up a back-door path
between `Smoking` and `Mortality`
that goes `Smoking -> Birth Weight <- Birth Defect -> Mortality`.
This path is noncausal because
one of the arrows goes the wrong way.

## Passionate Debates: Science vs. Culture

In fact, the same birth-weight paradox is observed in children of black
mothers as in children of smokers. Black women give birth to underweight
babies more often than white women do, and their babies have a higher
mortality rate. Yet their low-birth-weight babies have a better survival rate
than the low-birth-weight babies of white women. Now what conclusions
should we draw? We can tell a pregnant smoker that she would help her baby
by stopping smoking. But we can't tell a pregnant black woman to stop being
black.

Instead, we should address the societal issues that cause the children of
black mothers to have a higher mortality rate. This is surely not a
controversial statement. But what causes should we address, and how should
we measure our progress?

# CHAPTER 6 Paradoxes Galore!

The birth-weight paradox, with which we ended Chapter 5,
is representative of a surprisingly large class of paradoxes
that reflect the tensions between causation and association.
The tension starts because they stand on two different rungs
of the Ladder of Causation and is aggravated by the fact that
human intuition operates under the logic of causation,
while data conform to the logic of probabilities and proportions.
Paradoxes arise when we misapply the rules we have learned
in one realm to the other.

## The perplexing monty hall problem

"Suppose you're on a game show, and you're given the choice of three
doors. Behind one door is a car, behind the others, goats. You pick a
door, say #1, and the host (Monty), who knows what's behind the doors,
opens another door, say #3, which has a goat. He says to you, 'Do you
want to pick door #2?' Is it to your advantage to switch your choice
of doors?"

| Door 1 | Door 2 | Door 3 | Outcome if you switch | Outcome if you stay |
| ------ | ------ | ------ | --------------------- | ------------------- |
| Auto   | Goat   | Goat   | Lose                  | Win                 |
| Goat   | Auto   | Goat   | Win                   | Lose                |
| Goat   | Goat   | Auto   | Win                   | Lose                |

Or

| Door your choice | Door with auto | Door opened by host                                | Outcome if you switch | Outcome if you stay |
| ---------------- | -------------- | -------------------------------------------------- | --------------------- | ------------------- |
| 1                | 1              | 2 or 3 (host's choice dose not effect the outcome) | Lose                  | Win                 |
| 1                | 2              | 3 (host has no choice)                             | Win                   | Lose                |
| 1                | 3              | 2 (host has no choice)                             | Win                   | Lose                |

What if the host opens the remaining two doors randomly?

| Door your choice | Door with auto | Door opened by host | Outcome if you switch | Outcome if you stay |
| ---------------- | -------------- | ------------------- | --------------------- | ------------------- |
| 1                | 1              | 2 (Goat)            | Lose                  | Win                 |
| 1                | 1              | 3 (Goat)            | Lose                  | Win                 |
| 1                | 2              | 2 (Auto)            | Lose                  | Lose                |
| 1                | 2              | 3 (Goat)            | Win                   | Lose                |
| 1                | 3              | 2 (Goat)            | Win                   | Lose                |
| 1                | 3              | 3 (Auto)            | Lose                  | Lose                |

The key element in resolving this paradox is that we need to take into
account not only the data (i.e., the fact that the host opened a
particular door) but also the data-generating process-in other words,
the rules of the game. They tell us something about the data that
could have been but has not been observed. No wonder statisticians in
particular found this puzzle hard to comprehend. They are accustomed
to, as R. A. Fisher (1922) put it, "the reduction of data" and
ignoring the data-generating process.

In the first case, `Location of Car` is a collider:

```
Your Door -> Door Opned
Location of Car -> Door Opned
```

Once we obtain information on this variable, all our probabilities
become conditional on this information. But when we condition on a
collider, we create a spurious dependence between its parents.

In the second case, `Location of Car` independent from `Door Opned`:

```
Your Door -> Door Opned
```

Our minds rebel at this possibility because from earliest infancy, we
have learned to associate correlation with causation. If a car behind
us takes all the same turns that we do, we first think it is following
us (causation!). We next think that we are going to the same place
(i.e., there is a common cause behind each of our turns). But
causeless correlation [created by the condition on a collider]
violates our common sense. Thus, the Monty Hall paradox is just like
an optical illusion or a magic trick: it uses our own cognitive
machinery to deceive us.

Remember we use the **Bayesian interpretation of probability**,
according to which _probabilities encode degrees of belief about events in the world_
and data are used to strengthen, update, or weaken those degrees of belief.

After Monty opened Door 3,
what evidence (data) increased the degree of your belief,
so that your belief in Door 2 has gone up from one-third to two-thirds?

The answer is that Monty could not open Door 1 after you chose it-but
he could have opened Door 2. The fact that he did not makes it more likely
that he opened Door 3 because he was forced to. Thus there is more evidence
than before that the car is behind Door 2. This is a general theme of Bayesian
analysis:

> Any hypothesis that has survived some test
> that threatens its validity becomes more likely.

The greater the threat, the more likely it becomes after
surviving. Door 2 was vulnerable to refutation (i.e., Monty could have
opened it), but Door 1 was not. Therefore, Door 2 becomes a more
likely location, while Door 1 does not. The probability that the car
is behind Door 1 remains one in three.

- **Xie:** Comparing with Polya's "Patterns of plausible inference",
  where you have a guess (hypothesis) about a problem,
  and after you fail to refute it, you believe it even more.

Notice that I have really given two explanations of the Monty Hall
paradox. The first one uses causal reasoning to explain why we observe a
spurious dependence between Your Door and Location of Car; the second
uses Bayesian reasoning to explain why the probability of Door 2 goes up in
Let's Make a Deal.

Both explanations are valuable. The Bayesian one
accounts for the phenomenon but does not really explain why we perceive it
as so paradoxical. In my opinion, a true resolution of a paradox should
explain why we see it as a paradox in the first place. Why did the people who
read her column believe so strongly that vos Savant was wrong? It wasn't just
the know-it-alls. Paul Erdos, one of the most brilliant mathematicians of
modern times, likewise could not believe the solution until a computer
simulation showed him that switching is advantageous. What deep flaw in our
intuitive view of the world does this reveal?

- **Xie:** "A true resolution of a paradox should explain why
  we see it as a paradox in the first place."

  What will happen if we apply this principle to other logic paradoxes?

"Our brains are just not wired to do probability problems very well, so I'm
not surprised there were mistakes," said Persi Diaconis, a statistician at
Stanford University, in a 1991 interview with the New York Times. True, but
there's more to it. Our brains are not wired to do probability problems, but
they are wired to do causal problems. And this causal wiring produces
systematic probabilistic mistakes, like optical illusions. Because there is no
causal connection between My Door and Location of Car, either directly or
through a common cause, we find it utterly incomprehensible that there is a
probabilistic association. Our brains are not prepared to accept causeless
correlations, and we need special training—through examples like the Monty
Hall paradox or the ones discussed in Chapter 3—to identify situations where
they can arise. Once we have "rewired our brains" to recognize colliders, the
paradox ceases to be confusing.

## More collider bias: Berkson's paradox

In 1946, Joseph Berkson, a biostatistician at the Mayo Clinic, pointed out a
peculiarity of observational studies conducted in a hospital setting: even if
two diseases have no relation to each other in the general population, they can
appear to be associated among patients in a hospital.

To understand Berkson's observation, let's start with a causal diagram
(Figure 6.3). It's also helpful to think of a very extreme possibility: neither
Disease 1 nor Disease 2 is ordinarily severe enough to cause hospitalization,
but the combination is. In this case, we would expect Disease 1 to be highly
correlated with Disease 2 in the hospitalized population.

```
Disease 1 -> Hospitalization <- Disease 2
```

Since we know about collider, we know why now.

By performing a study on patients who are hospitalized, we are
controlling for Hospitalization. As we know, conditioning on a
collider creates a spurious association between Disease 1 and
Disease 2. In many of our previous examples the association was
negative because of the explain-away effect, but here it is positive
because both diseases have to be present for hospitalization
(not just one).

Try this experiment: Flip two coins simultaneously one hundred times
and write down the results only when at least one of them comes up
heads. Looking at your table, which will probably contain roughly
seventy-five entries, you will see that the outcomes of the two
simultaneous coin flips are not independent. Every time Coin 1 landed
tails, Coin 2 landed heads. How is this possible? Did the coins
somehow communicate with each other at light speed? Of course not. In
reality you conditioned on a collider by censoring all the tails-tails
outcomes.

In _The Direction of Time_, published posthumously in 1956, philosopher
Hans Reichenbach made a daring conjecture called the "common cause
principle." Rebutting the adage "Correlation does not imply causation,"
Reichenbach posited a much stronger idea: "No correlation without
causation." He meant that a correlation between two variables, X and Y,
cannot come about by accident. Either one of the variables causes the other,
or a third variable, say Z, precedes and causes them both.

Our simple coin-flip experiment proves that Reichenbach's dictum was too
strong, because it neglects to account for the process by which observations
are selected. There was no common cause of the outcome of the two coins,
and neither coin communicated its result to the other. Nevertheless, the
outcomes on our list were correlated. Reichenbach's error was his failure to
consider collider structures—the structure behind the data selection.

- **Xie:** Does this "data selection" relates to quantum mechanics?

The distorting prism of colliders is just as prevalent in everyday life. As
Jordan Ellenberg asks in How Not to Be Wrong, have you ever noticed that,
among the people you date, the attractive ones tend to be jerks? Instead of
constructing elaborate psychosocial theories, consider a simpler explanation.
Your choice of people to date depends on two factors: attractiveness and
personality. You'll take a chance on dating a mean attractive person or a nice
unattractive person, and certainly a nice attractive person, but not a mean
unattractive person. It's the same as the two-coin example, when you
censored tails-tails outcomes. This creates a spurious negative correlation
between attractiveness and personality. The sad truth is that unattractive
people are just as mean as attractive people—but you'll never realize it,
because you'll never date somebody who is both mean and unattractive.

## Simpson's paradox

The study was observational, not randomized,
with sixty men and sixty women.
This means that the patients themselves decided
whether to take or not to take the drug.
Table 6.4 shows how many of each gender received Drug `D`
and how many were subsequently diagnosed with heart attack.

|        | No drug, Heart attact | No drug, No heart attact | Took drug, Heart attact | Took drug, No heart attact |
| ------ | --------------------- | ------------------------ | ----------------------- | -------------------------- |
| Female | 1                     | 19                       | 3                       | 37                         |
| Male   | 12                    | 28                       | 8                       | 12                         |
| Total  | 13                    | 47                       | 11                      | 49                         |

In mathematics, we do not believe the following inference at all.

```
A/B > a/b
C/D > c/d
-------------------------
(A+C)/(B+D) > (a+c)/(b+d)
```

For almost twenty years, I have been trying to convince the scientific
community that the confusion over Simpson's paradox is a result of incorrect
application of causal principles to statistical proportions. If we use causal
notation and diagrams, we can clearly and unambiguously decide whether
Drug D prevents or causes heart attacks. Fundamentally, Simpson's paradox
is a puzzle about confounding and can thus be resolved by the same methods
we used to resolve that mystery. Curiously, three of the four 2016 papers that
I mentioned continue to resist this solution.

- **Xie:** We have a logic intuition:

  ```
  A -> R
  B -> R
  -----------
  A or B -> R
  ```

  The statistics works against our logic intuition.

How to explain Simpson's paradox?
In vain will you seek guidance from Table 6.4.
To answer the question, we must look beyond the data to the data-generating process.
As always, we draw a causal diagram.

```
Taking Drug <- Gender -> Heart Attack
Taking Drug -> Heart Attack
```

(men being at greater risk) and whether the patient chooses to take Drug D.
In the study, women clearly
had a preference for taking Drug `D`
and men preferred not to.
Thus `Gender` is a confounder of `Taking Drug` and `Heart Attack`. For
an unbiased estimate of the effect of `Taking Drug` on `Heart Attack`,
we must adjust for the confounder.
We can do that by looking at the data for men and women
separately, then taking the average:

```
A/B > a/b
C/D > c/d
-------------------------
(A/C)+(C/D) > (a/b)+(c/d)
```

The Drug `D` is bad for man, bad for women and bad for all.

I don't want you to get the impression from this example that aggregating
the data is always wrong or that partitioning the data is always right. It
depends on the process that generated the data. In the Monty Hall paradox, we
saw that changing the rules of the game also changed the conclusion. The
same principle works here. I'll use a different story to demonstrate when
pooling the data would be appropriate. Even though the data will be precisely
the same, the role of the "lurking third variable" will differ and so will the
conclusion.

Let's begin with the assumption that blood pressure is known to be a
possible cause of heart attack, and Drug `B` is supposed to reduce blood
pressure. Naturally, the Drug `B` researchers wanted to see if it might also
reduce heart attack risk, so they measured their patients' blood pressure after
treatment, as well as whether they had a heart attack.

|                     | No drug, Heart attact | No drug, No heart attact | Took drug, Heart attact | Took drug, No heart attact |
| ------------------- | --------------------- | ------------------------ | ----------------------- | -------------------------- |
| Low blood pressure  | 1                     | 19                       | 3                       | 37                         |
| High blood pressure | 12                    | 28                       | 8                       | 12                         |
| Total               | 13                    | 47                       | 11                      | 49                         |

```
Taking Drug -> blood pressure -> Heart Attack
Taking Drug -> Heart Attack
```

Table 6.6 shows the data from the study of Drug `B`. It should look
amazingly familiar: the numbers are the same as in Table 6.4! Nevertheless,
the conclusion is exactly the opposite. As you can see, taking Drug `B`
succeeded in lowering the patients' blood pressure: among the people who
took it, twice as many had low blood pressure afterward (forty out of sixty,
compared to twenty out of sixty in the control group). In other words, it did
exactly what an anti–heart attack drug should do. It moved people from the
higher-risk category into the lower-risk category. This factor outweighs
everything else, and we can justifiably conclude that the aggregated part of
Table 6.6 gives us the correct result.

As usual, a causal diagram will make everything clear and allow us to
derive the result mechanically, without even thinking about the data or
whether the drug lowers or increases blood pressure. In this case our "lurking
third variable" is `Blood Pressure`, and the diagram looks like Figure 6.5. Here,
`Blood Pressure` is a mediator rather than a confounder. A single glance at the
diagram reveals that there is no confounder of the `Taking Drug -> Heart Attack`
relationship (i.e., no back-door path), so stratifying the data is unnecessary. In
fact, conditioning on `Blood Pressure` would disable one of the causal paths
(maybe the main causal path) by which the drug works. For both these
reasons, our conclusion is the exact opposite of what it was for Drug `D`:
Drug `B` works, and the aggregate data reveal this fact.

- **Xie:** Given different cases,
  in what order should we carry out `+` and `/`?

  - doing `+` first, means "aggregate the data".
  - doing `/` first, means "separate (stratifying, partitioning) the data".

  When we try to explain the phenomenon by the semantics (the meaning) of the data,
  we might success, but in different semantics we must give different arguments.

  If we only use the syntax (the causal graph), we can say,

  - when see mediator, we should aggregate the data,
    because the mediator is part of the cause.

  - when see confounder, we should separate the data,
    because the confounder is another which should be conditioned.

# CHAPTER 7 Beyond Adjustment: The Conquest of Mount Intervention

## The simplest route: The back-door adjustment formula

TODO Understand the argument and calculation of back-door adjustment.

TODO Is it true that back-door prediction is the same as randomized controlled experiment?

## The front-door criterion

TODO Understand the argument and calculation of front-door criterion.

Glynn and Kashin’s results show why the front-door adjustment is such a
powerful tool: it allows us to control for confounders that we cannot observe
(like Motivation), including those that we can’t even name. RCTs are
considered the “gold standard” of causal effect estimation for exactly the
same reason. Because front-door estimates do the same thing, with the
additional virtue of observing people’s behavior in their own natural habitat
instead of a laboratory, I would not be surprised if this method eventually
becomes a serious competitor to randomized controlled trials.

## The do-calculus, or mind over matter

In both the front- and back-door adjustment formulas,
the ultimate goal is to calculate the effect of an intervention,

```
P(Y | do(X))
```

in terms of data such as

```
P(Y | X, A, B, Z, ...)
```

that do NOT involve a do-operator.

If we are completely successful at eliminating the do’s,
then we can use observational data to estimate the causal effect,
allowing us to leap from rung one to rung two of the Ladder of Causation.

- **Xie:** There is a method that can transform
  a do-query to probabilistic expression if possible,
  and this method depends on the causal graph,
  where else have I seen this kind of mathematical phenomenon?

TODO Rules for do-transforms .

## The tapestry of science, or the hidden players in the do-orchestra

TODO

## The curious case(s) of Dr. Snow

TODO

## Good and bad cholesterol

TODO

# CHAPTER 8 Counterfactuals: Mining Worlds That Could Have Been

For decades or even centuries,
lawyers have used a relatively straightforward test
of a defendant’s culpability called “but-for causation”:
the injury would not have occurred but for the defendant’s action.
We will see how the language of counterfactuals
can capture this elusive notion and how to estimate
the probability that a defendant is culpable.

Counterfactual analysis allows climate scientists to make much more
precise and definite statements than before. It requires, however, a slight
addition to our everyday vocabulary. It will be helpful to distinguish three
different kinds of causation:

- necessary causation (the same as but-for causation.),
- sufficient causation,
- and necessary-and-sufficient causation.

Using these words, a climate scientist can say,

- “There is a 90 percent probability that
  man-made climate change was a necessary cause of this heat wave,”

- or “There is an 80 percent probability that
  climate change will be sufficient
  to produce a heat wave this strong
  at least once every 50 years.”

The first sentence has to do with _attribution_:
Who was responsible for the unusual heat?
The second has to do with _policy_.
It says that we had better prepare for such heat waves
because they are likely to occur sooner or later.

## From Thucydides and Abraham to Hume and Lewis

> Thus we remember to have seen that species
> of object we call flame, and to have felt that species of sensation we call
> heat,” he writes. “We likewise call to mind their constant conjunction in all
> past instances. Without any further ceremony, we call the one cause and the
> other effect, and infer the existence of the one from the other.
>
> -- Hume, in "Treatise of Human Nature"

This is now known as the “regularity” definition of causation.

> We may define a cause to be an object followed by
> another, and where all the objects, similar to the first,
> are followed by objects similar to the second.
> Or, in other words, where, if the first object had not been,
> the second never had existed.
>
> -- Hume, Nine years later, in "An Enquiry Concerning Human Understanding"

Although Hume tries to pass these two definitions off as one,
by means of his innocent interjection “in other words”,
the second version is completely different from the first.
It explicitly invokes a counterfactual,
so it lies on the third rung of the Ladder of Causation.
Whereas regularities can be observed,
counterfactuals can only be imagined.

It is worth thinking for a moment about why Hume chooses to define
causes in terms of counterfactuals, rather than the other way around.
Definitions are intended to reduce a more complicated concept to a simpler one.
Hume surmises that his readers will understand the statement
“if the first object had not been, the second had never existed”
with less ambiguity than they will understand
“the first object caused the second”.

One philosopher who defied convention, David Lewis,
called in his 1973 book _Counterfactuals_
for abandoning the regularity account altogether
and for interpreting “A has caused B”
as “B would not have occurred if not for A”.
Lewis asked, “Why not take counterfactuals at face value:
as statements about possible alternatives to the actual situation?”

- [David Lewis](https://plato.stanford.edu/entries/david-lewis)

Like Hume, Lewis was evidently impressed by the fact that humans make
counterfactual judgments without much ado, swiftly, comfortably, and
consistently. We can assign them truth values and probabilities with no less
confidence than we do for factual statements. In his view, we do this by
envisioning “possible worlds” in which the counterfactual statements are true.

# CHAPTER 9 Mediation: The Search for a Mechanism

# CHAPTER 10 Big Data, Artificial Intelligence, and the Big Questions
