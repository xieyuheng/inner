---
title: Patterns of Plausible Inference
author: Polya
date: 1954
---

# Preface

The study of patterns of plausible reasoning is done in the manner of the naturalist:

- I collect observations, state conclusions,
- and emphasize the points in which my observations seem to support my conclusions.

# Chapter 12. Some Conspicuous Patterns

## 1. Verification of a consequence

Take one of Euler's conjecture, and Euler's verification of it, as an example.

``` cicada
euler_s_conjecture(n: Nat): (
  x: Nat, p: Nat, Prime(p)
) * Equal(
  Nat,
  add(mul(8, n), 3),
  add(mul(x, x), add(p, p))
) {
  TODO("euler_s_conjecture")
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

TODO

## Examples and comments on chapter 12

TODO

# Chapter 13. Further Patterns and First Links

# Chapter 14. Chance, the Ever-present Rival Conjecture

# Chapter 15. The Calculus of Probability and the Logic of Plausible Reasoning

# Chapter 16. Plausible Reasoning in Invention and Instruction
