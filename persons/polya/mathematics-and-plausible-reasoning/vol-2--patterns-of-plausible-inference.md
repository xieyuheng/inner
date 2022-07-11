---
title: Patterns of Plausible Inference
subtitle: Mathematics and Plausible Reasoning, Vol 2
author: Polya
year: 1954
---

# Preface

The study of patterns of plausible reasoning is done in the manner of the naturalist:

- I collect observations, state conclusions,
- and emphasize the points in which my observations seem to support my conclusions.

# Chapter 12. Some Conspicuous Patterns

## 1. Verification of a consequence

Take one of Euler's conjecture, and Euler's verification of it, as an example.

```cicada
function euler_s_conjecture(n: Nat): [
  x: Nat, p: Nat, _: Prime(p) |
  Equal(
    Nat,
    add(mul(8, n), 3),
    add(mul(x, x), add(p, p))
  )
] {
  return @TODO "euler_s_conjecture"
}
```

We have a classical elementary pattern of reasoning,
the "modus tollens" of the so-called hypothetical syllogism:

```
A implies B
B false
------------
A false
```

What happens if B turns out to be true?

A conjecture, verified in one more case, becomes somewhat more credible.

We have here a pattern of plausible inference:

```
A implies B
B true
------------
A more credible
```

We shall call this pattern **the fundamental inductive pattern**
or, somewhat shorter, the "inductive pattern."

This inductive pattern says nothing surprising.
On the contrary, it expresses a belief which no reasonable person seems to doubt:
The verification of a consequence renders a conjecture more credible.
With a little attention, we can observe countless reasonings in everyday life,
in the law courts, in science, etc.,
which appear to conform to our pattern.

## 2. Successive verification of several consequences

In the present section, I use the phrase "discussion of a theorem" in the specific meaning:
"discussion, or survey, of some particular cases and some more immediate consequences of the theorem."

I think that the discussion of the theorems presented is useful
both in advanced and in elementary classes.

"discussion of a theorem" give us a pattern of plausible reasoning
which is closely related to, but more sophisticated than, the fundamental inductive pattern:

```
A implies B(n+1)
B(n+1) is very different from the formerly verified consequences B(1), ... B(n) of A
B(n+1) true
------------
A much more credible
```

And a complementary form of it:

```
A implies B(n+1)
B(n+1) is very similar from the formerly verified consequences B(1), ... B(n) of A
B(n+1) true
------------
A just a little more credible
```

The verification of a new consequence counts more or less according
as the new consequence differs more or less from the formerly verified consequences.

## 3. Verification of an improbable consequence

Euler guessed the whole from a few scattered details.

Quite similarly,

- An archaeologist may reconstitute with reasonable certainty a whole inscription
  from a few scattered letters on a worn-out stone.

- A paleontologist may describe reliably the whole animal
  after having examined a few of its petrified bones.

- When a person whom you know very well starts talking in a certain way,
  you may predict after a few words the whole story he is going to tell you.

- Quite similarly, Euler guessed the whole story, the whole mathematical situation,
  from a few clearly recognized points.

The following pattern appears as a modification
or a sophistication of the fundamental inductive pattern:

```
A implies B
B very improbable in itself
B true
------------
A very much more credible
```

Let us add, without specific illustration for the moment,
the complementary pattern which explains the same idea from the reverse side:

```
A implies B
B quite probable in itself
B true
------------
A just a little more credible
```

The verification of a consequence counts more or less
according as the consequence is more or less improbable in itself.
The verification of the most surprising consequences is the most convincing.

## 4. Inference from analogy

... Both tables start with the circle which has the
shortest perimeter among the ten figures listed and also the lowest principal
frequency, and this suggests two theorems:

- Of all plane figures with a given area the circle has the shortest perimeter.
- Of all membranes with a given area the circle has the lowest principal frequency.

The geometric minimum property of the circle,
inductively supported by Table II, has been proved.
It is natural to expect that the analogous physical minimum property of the circle,
inductively supported by Table III, will also turn out to be true.
In expecting this we follow an important pattern of plausible inference:

```
A analogous to B
B true
------------
A more credible
```

A conjecture becomes more credible when an analogous conjecture turns out to be true.

The application of this pattern to the situation discussed seems sensible.
Yet there are further promising indications in this situation.

## 5. Deepening the analogy

The arrangement in Table II appears not very different
from that in Table III, but this is not the main point.
The tables contain various kinds of figures: rectangles, triangles, sectors.

- How are the figures of the same kind arranged ?
- How would a shorter table look listing only figures of one kind?

The tables contain a few regular figures:
the equilateral triangle, the square, and, let us not forget it, the circle.

- How are the regular figures arranged?

A conclusion from analogy become stronger if the analogy itself,
on which the conclusion is based, becomes stronger.

```
A analogous to B, in a very deep way
B true
------------
A very much more credible
```

## 6. Shaded analogical inference

```
A analogous to B
B more credible
------------
A somewhat more credible
```

A conjecture becomes somewhat more credible
when an analogous conjecture becomes more credible.

This is a weakened or shaded form of the pattern formulated in sect. 4.

# Chapter 13. Further Patterns and First Links

> When we have intuitively understood some simple propositions . . . it is
> useful to go through them with a continuous, uninterrupted motion of thought,
> to meditate upon their mutual relations, and to conceive distinctly several of
> them, as many as possible, simultaneously. In this manner our knowledge
> will grow more certain, and the capacity of the mind will notably increase.
>
> -- DESCARTES, The eleventh of his Rules for the Direction of the Mind.

## 1. Examining a consequence

We consider a situation which frequently occurs in mathematical
research. We wish to decide whether a clearly formulated mathematical
proposition A is true or not. We have, perhaps, some intuitive
confidence in the truth of A, but that is not enough: we wish to prove
A or disprove it. We work at this problem, but without decisive
success. After a while we notice a consequence B of A. This B is a
clearly formulated mathematical proposition of which we know that it
follows from A:

```
A implies B
```

Yet we do not know whether B is true or not. Now it seems that B is
more accessible than A; for some reason or other we have the
impression that we shall have better success with B than we had with
A. Therefore, we switch to examining B. We work to answer the
question: is B true or false? Finally we succeed in answering
it. _How does this answer influence our confidence in A?_

That depends on the answer.

_Demonstrative_

```
A implies B
B false
------------
A false
```

_Heuristic_

```
A implies B
B true
------------
A more credible
```

We met these patterns already in sect. 12.1 where we called the heuristic
pattern the fundamental inductive pattern. We shall meet with similar
but different patterns in the following sections.

## 2. Examining a possible ground

TODO

# Chapter 14. Chance, the Ever-present Rival Conjecture

# Chapter 15. The Calculus of Probability and the Logic of Plausible Reasoning

## 1. Rules of plausible reasoning?

## 2. An aspect of demonstrative reasoning

features in this pattern of reasoning: it is

- (1) impersonal
- (2) universal
- (3) self-sufficient
- (4) definitive

(4) **definitive**
If the premises are unquestionably certain,
we can "detach" the conclusion from the syllogism.
That is, if you know for certain both that "A implies B"
and that "B is false", you may forget about these premises
and just keep the conclusion "A is false"
as your definitive mental possession.

- **Xie:** "definitive" means classical or non-constructive,
  where we do not care about which prove (which evidence) is given.

## 3. A corresponding aspect of plausible reasoning

(4) **definitive**
We cannot "detach" the conclusion of our pattern of plausible reasoning.
"A is rendered more credible" is meaningless without reference to the
premises that explain by which circumstances it was rendered so. Referred
to the premises, the plausible conclusion makes perfectly good sense and is
perfectly reasonable, but it may diminish in value as time goes by, although
the premises remain intact. The plausible conclusion may be very valuable
in the moment when it emerges, but the advance of knowledge is likely to
depreciate it: its importance is only momentary, transitory, ephemeral,
_provisional_.

- **Xie:** This case is the same as constructive logic,
  in the sense that which evidence is used in a proof,
  matters to the conclusion.

  But constructive logic is not _provisional_,
  while in plausible reasoning time matters,
  like in database where data can be changed or deleted in time.

From the outset it was clear that the two kinds of reasoning have
different tasks. From the outset they appeared very different:
demonstrative reasoning as definite, final, "machinelike"; and
plausible reasoning as vague, provisional, specifically "human." Now
we may see the difference a little more distinctly. In opposition to
demonstrative inference, plausible inference leaves indeterminate a
highly relevant point: the "strength" or the "weight" of the
conclusion. This weight may depend not only on clarified grounds such
as those expressed in the premises, but also on unclarified
unexpressed grounds somewhere in the background of the person who
draws the conclusion. A person has a background, a machine has not.
Indeed, you can build a machine to draw demonstrative conclusions for
you, but I think you can never build a machine that will draw
plausible inferences.

- **Xie:** A person has a background,
  a machine might also has a background -- a causal graph.

  And "building a machine that can draw plausible inferences"
  is exactly our goal.

## 4. An aspect of the calculus of probability -- Difficulties

... And so it is not excluded a priori that
the same mathematical theory may serve two purposes.
Perhaps, we may use the calculus of probability
both in describing random mass phenomena
and in systematizing our rules of plausible inference.

- **Xie:** It seems the "random mass phenomena" is about frequency and data,
  while plausible inference is Bayesian inference.

First, there is an ambiguity to avoid.
The symbol `P(A)` should represent the credibility of `A`,
or the strength of the evidence for the conjecture `A`.
Such evidence is strong if it is convincing.
It is convincing if it convinces somebody.
Yet we did not say whom it should convince:
you, or me, or Mr. Smith, or Mrs. Jones, or whom?
The strength of the evidence could also be conceived _impersonally_.
If we conceive it so, the degree of belief that you or me
or any other person may happen to have in a proposed conjecture is irrelevant,
but what matters is the _degree of reasonable belief_
that anyone of us _should_ have.
We did not say yet, and we have still to decide,
in what exact sense we should use the term "credibility of A"
and the corresponding symbol `P(A)`.

There is another difficulty. The magnitudes considered by the
physicists such as "mass," "electric charge," or "reaction velocity"
have an operational definition; the physicist knows exactly which
operations he has to perform if he wishes to ascertain the magnitude
of an electric charge, for example. The definition of "long range
relative frequency," although in some way less distinct than that of
an electric charge, is still operational; it suggests definite
operations that we can undertake to obtain an approximate numerical
value of such a frequency. The trouble with the concept of the
"credibility of a conjecture" is that we do not know any operational
definition for it.

We have still to give a suitable interpretation of the term
"credibility of the conjecture A," and to the corresponding symbol
`P(A)`. This interpretation must be such that the difficulty of an
operational definition does not interfere with it. Moreover, and this
is the main thing, this interpretation should enable us to view the
rules of plausible reasoning systematically and realistically.

## 5. An aspect of the calculus of probability -- An attempt

## 6. Examining a consequence

- **Xie:** `P(A | B)` is interpreted as
  the degree of credence (belief)
  that he [Mr. Anybody] could place in `A`
  if he knew that `B` is true.

```
P(A) * P(B | A) = P(B) * P(A | B)
```

if `A -> B` is true, we know `P(B | A) = 1`, and

```
P(A) = P(B) * P(A | B)
```

- **Xie:** The interpretion of `P(B | A)` as `P(A -> B)` is profound,
  because classically we have `A -> B = or(and(~A, B))`,

  We know

  ```
  P(B | A) = P(A, B) / P(A)
  ```

  and

  ```
  P(or(~A, B)) = P(A, B) + 1 - P(A)
  ```

  they are equal only when

  ```
  P(B | A) = P(A, B) / P(A) = 1
  ```

  thus we know this interpretion is not classical.

If Mr. Anybody proves `B`, we will have `P(A) = P(A | B)`.

- **Xie:** `P(A)` will be updated to `P(A | B)` by proving `B` is true.

If `B` is still uncertain,

```
0 < P(B) < 1
```

we know

```
P(A) < P(A | B)
```

Suppose `P(A | B)` is fixed,
and `P(A)` varies according to `P(B)`.

```
P(A) = P(B) * P(A | B)
```

We know that if we prove `B`, `P(A)` will increase to `P(A | B)`.

- **Xie:** Here is one way of understanding

  > The verification of `B` updates our belief of `A` from `P(A)` to `P(A | B)`".

  At the moment we verified `B`,
  the following assignments happened one by one:

  ```
  P(A) := P(A | B) = P(A) / P(B)
  P(B) := 1
  ```

  then, since `B` is verified,

  ```
  P(A | B) = P(A) / P(B) = P(A)
  ```

  it has no use of updating `P(A)` again.

- **Xie:** Note that, it is not right to say

  ```
  P(A | B) = P(A) / P(B)
  ```

  is an assignment, the right assignment is

  ```
  P(A) := P(A | B)
  ```

Now, `P(A)` and `P(A | B)` represent the credibility of `A`
before and after the proof of `B`, respectively.
Therefore the above inequality is the formal expression
of a principle with which we met so often:

> the verification of a consequence renders a conjecture more credible.
>
> -- cf. sect. 12.1

Suppose `P(A)` is fixed,

```
P(A | B) = P(A) / P(B)
```

How does the variation of `P(B)`
influences the weight of the evidence resulting
resulting from the verification of the consequence `B`?

Let us pay due attention to the extreme cases.
Since `B` is a consequence of `A`,
`B` is certainly true when `A` is true,
and so `P(B)`, the credibility of `B`,
cannot be less than `P(A)`, the credibility of `A`.
On the other hand no credibility can exceed certainty:
`P(B)` cannot be greater than `1`. We have determined:

```
P(A) <= P(B) < 1
```

The lower bound is attained, when not only `A` implies `B`,
but also `B` implies `A`,
so that the two assertions `A` and `B` are equivalent,
stand and fall together,
in which case they are, of course, equally credible.
The upper bound `1` cannot be really attained:
if it were attained, `B` would be certain before investigation,
and we have not included this case in our consideration.
Yet the upper bound can be approached:
`B` can be almost certain before examination.
How does the evidence resulting from the verification of `B` change
when `P(B)` varies between its extreme bounds?

The evidence is stronger, when `P(A | B)`,
the new confidence in `A`
resulting from the verification of the consequence `B`, is greater.
It is visible from the above assignment that

```
as P(B) decreases from 1 to P(A),
P(A | B) increases from P(A) to 1.
```

This statement expresses in a new language a point
that we have recognized before (sect. 12.3):

> the increase of our confidence in a conjecture due to the verification
> of one of its consequences varies inversely
> as the credibility of the consequence before such verification.

The more unexpected a consequence is,
the more weight its verification carries.
The verification of the most surprising consequence
is the most convincing,
whereas the verification of a consequence
that we did not doubt much anyway (`P(B)` almost 1)
has little value as evidence.

- **Xie:** `P(A)` will be updated to `P(A | B)` by proving `B` is true,
  and the increase of belief in this update,
  depends on `P(B)` -- our belief of `B` before it is proved.

- TODO We can use the same analysis to get:

  ```
  as P(B | ~A) decreases from 1 to 0,
  P(A | B) increases from P(A) to 1.
  ```

  Where `P(B | ~A)` is our belief of
  "given `A` is false, `B` is true" (cf. sect. 13.10),
  and `P(A | B)` is still the update of `P(A)` after the verification of `B`.

## 7. Examining a possible ground

After the broad and cautious discussion in the foregoing section we
can proceed a little faster in surveying similar situations.

Here is such a situation: the aim of our research is a certain
conjecture `A`. We notice a possible ground for `A`, that is, a
proposition `B` from which `A` would follow:

```
B -> A
```

We start investigating `B`. If we succeeded in proving `B`, `A` would
also be proved. Yet `B` turns out to be false. How does the disproof
of `B` affect our confidence in `A`?

From `B -> A`, we know

```
P(A | B) = 1
```

TODO

and hence we see that

```
P(A | ~B) < P(A)
```

TODO

- **Xie:** Is it possible to use the geometric interpretion
  to view the reasoning in this chapter?

  Maybe for geometric interpretion to be applicable,
  the random variables must be viewed as
  attributes or properties some objects.

  What are these objects here?

  Remember [Po-Shen Loh's Way of solving quadratic equations](https://www.poshenloh.com/quadratic)?

  Where geometric interpretion of intermedium expressions
  can help us understand the problem in a very insightful way.

## 8. Examining a conflicting conjecture

We consider now another situation:
we examine two conflicting conjectures, `A` and `B`.
When we say that `A` conflicts with `B` or

```
A is incompatible with B
```

we mean that the truth of one of them implies the falsity of the other. We
are, in fact, primarily concerned with `A` and we have started investigating `B`
because we thought that the investigation of `B` could shed some light on `A`.
In fact, a proof of `B` would disprove `A`. Yet we succeeded in disproving `B`.
How does this result affect our confidence in `A`?

Let the calculus of probability give the answer. Let us begin by expressing
in the language of this calculus that `A` and `B` are incompatible. This means
in other words that `A` and `B` cannot both be true, and so

```
P(A, B) = 0
```

- **Xie:** Note that
  "the truth of one of them implies the falsity of the other"
  is not interpreted as

  ```
  (A -> ~B) and (B -> ~A)
  ```

TODO

## 9. Examining several consequences in succession

TODO

## 10. On circumstantial evidence

TODO

## Examples and comments on Chapter 15

### 4. Probability and credibility

Let `A(n)` denote the conjecture that
the fair die that I am about to roll
will show `n` spots `(n = 1, 2, ... 6)`.

- **Xie:** The term "credibility" (or as I would use "belief")
  is used in a subjective sense,
  this is distinguished from the term "probability"
  which is defined by the proposition of
  a given kind of event in all events.

The credibility of the conjecture `A(1)`
turned out to have the same numerical value
as the probability of the event
that a fair die shows one spot.
Yet this is not surprising at all:
we admitted the same rules and assumed
the same interchangeability (or symmetry)
in computing credibilities and probabilities.
(The reader should not forget, of course,
that credibility and probability are quite differently defined.)

- **Xie:** Does the geometric intuition
  that comes from viewing probability as proportion
  also apply to belief (credibility)?

  I think not.

  So what is the right intuition should we use
  to explain Bayes' theorem for belief?

  What is the intuition of the following equation for updating belief?

  ```
  P(A | B) = P(A, B) / P(B)
  ```

  Can we generalize this equation to mathematical objects other than number?
  because our belief might be represented by more structured object than number.

  Can we say view

  ```
  P(A, B) = P(A | B) * P(B)
  ```

  and say that, to show evidence of `A` and `B`,
  it is not necessary to show evidence of `A` and evidence of `B` independently,
  it is enough to show evidence of `B` and the construction of evidence of `A`
  can use (depend on) the evidence of `B`?

# Chapter 16. Plausible Reasoning in Invention and Instruction

## 1. Object of the present chapter

The examples in the first part of this work and the discussions in the
foregoing chapters of the second part elucidated somewhat, I hope, the
role of plausible reasoning in the discovery of mathematical
facts. Yet the mathematician does not only guess; he also has problems
to solve, and he has to prove the facts that he guessed.

What is the role of plausible reasoning in the discovery of the
solution or in the invention of the proof?

And, by the way, this is the question that attracted the author who,
primarily concerned with the methods of problem-solving,
was eventually led to the subject of the present book.

## 2. The story of a little discovery

## 3. The process of solution

Solving a problem is an extremely complex process. No description or
theory of this process can exhaust its manifold aspects, any
description or theory of it is bound to be incomplete, schematic,
highly simplified. I wish to point out the place of plausible
reasoning in this complex process, and I shall choose the simplest
description I am able to find in which this place can be recognizably
located. And even the beginning of such a simple description will
suffice here.

- **Xie:** What is plausible reasoning?

  If the foregoing chapter is plausible reasoning,
  then it is a calculus of evidence
  based on some simple applications of Bayes' rule.

  - Is it the same as Bayesian network?

  - How about causality and model?

- **Xie:** Since plausible reasoning is
  at the first level of the causal ladder,
  it does not have a model.

  A model of problem solving would be
  a model about deduction, induction and hypothesis (guessing),
  we can ask a causal reasoning robot, why he beliefs `A`,
  he will explain by showing the inductive and hypothetical evidences,
  we can ask a deeper why about why such evidences made him belief `A`,
  he will explain his model.

  A causal reasoning robot
  can know about how he is programmed,
  how is this possible?

  A model about deduction, induction and hypothesis (guessing),
  does not cause the robot to belief something,
  but cause him to explore in certain directions.

  Suppose the robot can also do proof search,
  we ask him why he searched the proof of `B`,
  him might say, he is interested in proving `A`,
  and he know `A -> B`, thus based on his model,
  a proof of `B` can be used as a guess
  to increase his belief of `A`.

**(1) Setting a problem to yourself.**

A problem becomes a problem for you when you propose it to yourself.
A problem is not yet your problem just because you are supposed to solve
it in an examination. If you wish that somebody would come and tell
you the answer, I suspect that you did not yet set that problem to
yourself in earnest. But if you are anxious to find the answer
yourself, by your own means, then you have made the problem really
yours, you are serious about it.

Setting a problem to yourself is the beginning of the solution, the
essential first move in the game. It is a move in the nature of a
decision.

**(2) Selective attention.**

You need not tell me that you have set that problem to yourself, you
need not tell it to yourself; your whole behavior will show that you
did. Your mind becomes selective; it becomes more accessible to
anything that appears to be connected with the problem, and less
accessible to anything that seems unconnected. You eagerly seize upon
any recollection, remark, suggestion, or fact that could help you to
solve your problem, and you shut the door upon other things. When the
door is so tightly shut that even the most urgent appeals of the
external world fail to reach you, people say that you are absorbed.

**(3) Registering the pace of progress.**

There is another thing that shows that you are seriously engaged in
your problem; you become sensitive. You keenly feel the pace of your
progress; you are elated when it is rapid, you are depressed when it
is slow. Whatever comes to your mind is quickly sized up: "It looks
good," "It could help," or "No good," "No help." Such judgments are,
of course, not infallible. (Although they seem to be more often
correct than not, especially with talented or experienced people.) At
any rate, such judgments and feelings are important for you
personally; they guide your effort.

**(4) Where plausible reasoning comes in.**

- **Xie:** Plausible reasoning can be used to evaluate (judge)
  the value of a plan and its progress.

Let us see somewhat more concretely a typical situation.

You try to attain the solution in a certain direction, along a certain line.
(For example, in trying to solve the geometrical problem of sect. 2 you reject
fig. 16.2 and attempt to work with the more hopeful fig. 16.3.) You may
feel quite keenly that you work in the right direction, that you follow a
promising line of approach, that you are on the scent. You may feel so,
by the way, without formulating your feeling in words. Or even if you say
something such as, "It looks good," you do not take the trouble to analyze
your confidence, you do not ask, "Why does it look good?" You are just
too busy following up the scent.

Yet you may have bad luck. You run into difficulties, you do not make
much progress, nothing new occurs to you and then you start doubting:
"Was it a good start? Is this the right direction?" And then you may
begin to analyze your feeling: "The direction looked quite plausible
-- but why is it plausible?" Then you may start debating with
yourself, and some more distinct reasons may occur to you:

- "The situation is not so bad. I could bring in a triangle. People
  always bring in triangles in such problems."

- "It was probably the right start, after all. It looks like the right
  solution. What do I need for a solution with this kind of problem?
  Such a point -- and I have it. And that kind of point -- I have it
  too. And ..."

It would be interesting to see more distinctly how people are
reasoning in such a situation -- in fact, it is our main purpose to
see just that. Yet we need at least one more example to broaden our
observational basis.

## 4. Deus ex machina [God out of the machine]

## 7. Some typical indications

We consider a situation in which plausible reasoning comes naturally
to the problem-solver. You are engaged in an exciting problem. You
have conceived a plan of the solution, but somehow you do not like it
quite. You have your doubts, you are not quite convinced that your
plan is workable. In debating this matter with yourself, you are, in
fact, examining a conjecture:

**A. This plan of the solution will work.**

Several pros and cons may occur to you as you examine your plan from
various angles. Here are some conspicuous typical indications that may
speak for the conjecture A.

**B1. This plan takes all the data into account.**

- **Xie:** The plausible reasoning is the following:

  ```
  f: A -> B1 // Solution takes all the data into account.
  b: B1 // This plan takes all the data into account.
  ----------------------------------
  guess(f, b): Hypothesis(A) // This plan might be the solution.
  ```

- **Xie:** `B2`, `B3`, `B4` are all hypothesis in the sense of Peirce.

**B2. This plan provides for a connection between the data and the unknown.**

**B3. This plan has features that are often useful in solving problems of this kind.**

- **Xie:** From this indication, we recognize that,
  this is typical Bayesian reasoning.

  Thus the "hypothesis" mode of reasoning of Peirce,
  is the same as Bayesian reasoning.

**B4. This plan is similar to one that succeeded in solving an analogous problem.**

**B5. This plan succeeded in solving a particular case of the problem.**

**B6. This plan succeeded in solving a part of the problem (in finding some of the unknowns, or in proving a weaker conclusion).**

- **Xie:** `B5` and `B6` can be viewed as induction,
  because to do induction is to give partial solution.

  "a particular case of the problem" and "a part of the problem"
  can both be viewed as a condition `c: C`
  under which we can prove `A(c)` (the plan works for `c`).

This list is by no means exhaustive. There are still other typical
indications and signs, but we need not list them here. At any rate, it
would be useless to list them without proper illustration.

- **Xie:** We see that, it is not enough to know about
  the "hypothesis" mode of reasoning of Peirce,
  we must also know a lot of concrete example of `B` in `A -> B`.

## Xie: Induction v.s. Hypothesis

We see in the following chapter,
Polya confused induction with hypothesis,
sometimes when using the word "induction" in this section,
he really means hypothesis.

**DEDUCTION.**

    Rule. - All the beans from this bag are white.
    Case. - These beans are from this bag.
    ∴ Result. - These beans are white.

    f: (bean: Bean) -> Bag(bean) -> White(bean)
    bean: Bean, c: Bag(bean)
    -------------------------
    f(bean, c): White(bean)

**INDUCTION.**

    Case. - These beans are from this bag.
    Result. - These beans are white.
    ∴ Rule. - All the beans from this bag are white.

    bean: Bean, c: Bag(bean)
    i: White(bean)
    -------------------------
    memo(bean, c, i): Parital((bean: Bean) -> Bag(bean) -> White(bean))

**HYPOTHESIS.**

    Rule. - All the beans from this bag are white.
    Result. - These beans are white.
    ∴ Case. - These beans are from this bag.

    f: (bean: Bean) -> Bag(bean) -> White(bean)
    bean: Bean, i: White(bean)
    -------------------------
    guess(f, bean, i): Hypothesis(Bag(bean))

## 8. Induction in invention

(1) When the problem-solver debates his plan of the solution with
himself, this plan is usually more "fluid" than "rigid," it is more
felt than formulated. In fact it would be foolish of the
problem-solver to fix his plan prematurely. A wise problem-solver
does not commit himself to a rigid plan. Even at a later stage, when
the plan is riper, he keeps it ready for modification, he leaves it a
certain flexibility, he reckons with unforeseen difficulties to which
he might be obliged to adapt his plan. Therefore, when the problem-
solver investigates the workability of his plan, he examines a
changeable, sometimes a fleeting, object.

On the other hand, the conjectures that the mathematician or the
naturalist investigates are usually pretty determinate: they are
clearly formulated, or at least reasonably close to a clear
formulation. Also the jury has a pretty determinate conjecture to
examine: an indictment, the terms of which have been carefully laid
down by the prosecution.

Let us note this striking difference that separates the
problem-solver's investigation of the workability of his plan from the
inductive investigation of a mathematical or physical conjecture, or
from the judicial investigation of a charge: it is the difference
between a changeable, or fleeting, and a determinate, relatively well
defined object.

(2) The proceedings and acts of a court of justice are laid down in
the record. The conjecture examined by the naturalist, and the
evidence gathered for or against it, are also destined for a permanent
record. Not so the problem-solver's conjecture concerning the
workability of his scheme, or the signs speaking for or against it:
their importance is ephemeral. They are extremely important as long as
they guide the problem-solver's decisions.  Yet, when the
problem-solver's work enters a new phase, the plan itself may change,
and then the indications speaking for or against it lose almost all
interest. At the end, when the solution is attained and the problem is
done, all such accessories are cast away. The final form of the
solution may be recorded, yet the changing plans and the arguments for
or against them are mostly or entirely forgotten. The building erected
remains in view, but the scaffoldings, which were necessary to erect
it, are removed.

Let us note this aspect of the difference between an
inductive, or judicial, investigation on the one hand, and the
problem-solver's appraisal of the prospects of his plan on the other:
one is, the other is not, for permanent record.

- **Xie:** We have seen from the foregoing sections,
  to avoid Deus ex machina in demonstration,
  we should better provide the middle thoughts
  leading up to the final solution.

  Maybe it is useful to develop a tool
  called "personal problem-solving adviser",
  which can help us to record the workability evidences of a plan
  (just like the court record).

  - To help us be clear about our conjectures,
    and make it easy to review the history of our through process.
