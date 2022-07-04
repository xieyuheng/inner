---
title: Patterns of Plausible Inference
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

The geometrical minimum property of the circle,
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
and `P(A)` is assigned the expression `P(B) * P(A | B)`,

```
P(A) := P(B) * P(A | B)
```

We know that if we prove `B`, `P(A)` will increase to `P(A | B)`.

- **Xie:** Here we see that Judea's idea about
  "viewing equation as assignment" is very useful.

Now, `P(A)` and `P(A | B)` represent the credibility of `A`
before and after the proof of `B`, respectively.
Therefore the above inequality is the formal expression
of a principle with which we met so often:

> the verification of a consequence renders a conjecture more credible.
>
> -- cf. sect. 12.1

Suppose `P(A)` is fixed,
and we have the following assignment

```
P(A | B) := P(A) / P(B)
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

- **Exercise:** We can use the same analysis to get:

  ```
  as P(B | ~A) decreases from 1 to 0,
  P(A | B) increases from P(A) to 1.
  ```

  Where `P(B | ~A)` is our belief of
  "given `A` is false, `B` is true" (cf. sect. 13.10),
  and `P(A | B)` is still the update of `P(A)` after the verification of `B`.

## 7. Examining a possible ground

TODO

# Chapter 16. Plausible Reasoning in Invention and Instruction
