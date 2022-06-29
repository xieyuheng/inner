---
title: Judea Pearl
---

# Info

[ [HOMEPAGE](http://bayes.cs.ucla.edu/jp_home.html) ]

# Talks

## Judea Pearl, 2011 ACM Turing Award Recipient

[ [VIDEO](https://youtu.be/JQr0LFvR1OE?t=1630) ]

**Interviewer:** What is your advice to young scientist?
One of the things, it sounds like "Rebel".

**Judea:** But not rebel in the contrarian sense.
Rebel if you don't understand,
and insist on understanding it your way, ok?
and until you understand it your way,
don't take a standard answer as an answer
if it doesn't satisfy the question you have in mind.

It is exactly what Galileo told us
-- "I respect Aristotle,
but I wouldn't take his authority
as a replacement for my curiosity."

## (2010) Judea Pearl Tribute Symposium: Causality

[ [VIDEO](https://www.youtube.com/watch?v=78EmmdfOcI8) ]

- Science should be taught with its history.

  After been told the stories of the persons who did great science,
  you are under the illusion that you can do too.

- Just solving constraint satisfaction problems is not enough,
  there's something there about persistence things in the world
  that is not encoded but should be encoded,
  we call it environment.
  By the way, my definition of causality is
  encoding of the invariant elements in the environment.

- An equation contains more information than its solution,
  for an equation we can ask,
  what will happen to the solution
  if we change the connection,
  but for the solution,
  all information about connection and direction are lost.

  - Algebra of assignments and structural causal model.

- The disciplines of computer scinece
  and the paradigm of artifical intelligance
  that force us to explicate things in algorithmic details
  and force us to teach those details to brainless robot,
  and in this way not only to achieve robot emulating us,
  but us understanding ourselves,
  and to paraphrase Descartes, I would say,

  > **I compute, therefore I think**.

## (2022) A conversation between Judea Pearl and Stephen Wolfram

[ [VIDEO](https://www.youtube.com/watch?v=230PsGBxkCo) ]

### Heuristics is optimal solution to a simplified problem

- **Stephen:** When you do theorem proving or logic programming and so on,
  you are always trying to find the best path.
  The story of pruning seems to be a consistent theme,
  a still somewhat unsolved problem.

- **Judea:** Yeah but you do it with heuristics,
  and then the question is can the program generate heuristics
  on its own automatically?

  The idea was, yes you can do it by the following scheme

  > heuristics is optimal solution to a simplified problem.

  Instead of dealing with complex problem,
  simplify it, to a point where you can find the optimal solution.

  The optimal solution to a simplified problem
  is the heuristics to the original problem.

  And that could be automated,
  if you know what you mean by simplification and so on.

  A simplification can be a relaxation.

  - **Xie:** Progress in general AI is always about yet a higher level of abstraction,
    and if we are careful about our definitions and our research questions,
    maybe we can get closer and closer.

    We have a interpretation of nodes and edges of graph in a causal diagram,
    What could be a interpretation of faces and bodies in causal model?

    Note that, this question is not driven by research question,
    but driven by using cell-complex as syntax for a model.

- **Stephen:** Take chess for example.

- **Judea:** Example in chess can be crude,
  let's look a simple thing like finding the optimal path in a maze,
  the heuristics there will be finding the shortest path without any obstacles,
  that is known to be a straight line,
  so the optimal path in the unobstructed game
  will be heuristics in the maze.

  - **Xie:** Is this also the principle behind
    the heuristics given by Polya for problem solving?

- **Stephen:** But what if the heuristics can not lead you to the right path?

- **Judea:** We can always do reasoning in addition to evaluation,
  you do look ahead and you do backtrack, so eventually you find it.
  The question is how much effort you're going to spend before you find it.

  There is a notion of admissible heuristics,
  which is built on relaxation of original problem,
  and can be proven admissible in the sense that
  it will eventually find the global optimal.

### Why expert systems failed?

- **Stephen:** Why expert systems did not take over the world?

- **Judea:** I think it is because
  they try to model the experts rather than the environment,
  they try to model doctor instead of the disease.
  The doctor wasn't able to articulate the various procedures
  he or she go through when doing diagnosis,
  specially when it comes to combining different source of knowledge.

## TODO (2019) The Foundations of Causal Inference [The Book of WHY]

[ [VIDEO](https://www.youtube.com/watch?v=nWaM6XmQEmU) ]

### Outline

- Why we need **a new logic** to answer causal questions.
- Why AI need **a new engine** to operationalize the new logic.
  - Predicate logic and probability theory are insufficient.
- The two fundamental laws ("double-helix") of causal inference.
  - You need to not only learn but also internalize them!
- _The Seven Pillars of Causal Wisdom_
  - Victories!
    There are things we want to do,
    but we couldn't do,
    and now we can do.

## (2021) CS 201 JUDEA PEARL MARCH 9 2021

[ [VIDEO](https://www.youtube.com/watch?v=pZkCecwE-xE) ]

### Aristotle's two questions

In Athenian democracy, people gathered to argue
about who can rule the city better.

People developed Euclidean geometry,
because they craving the simplicity and objectivity of geometry,
and they got sick and tired of arguing.

Because the arguments get to the point
where they are endless, pointless
and leading to demagoguery.

Aristotle's two questions:

1. What distinguishes philosophy (love of wisdom)
   from demagoguery (leading people)?

2. What distinguishes truth from rhetoric?

**Logic** is quest for objective truth.

**Truth** comes out of **from**.

```
A -> B
B -> C
------
A -> C
```

Only after 2000 years,
[Boole](https://en.wikipedia.org/wiki/George_Boole) and
[De Morgan](https://en.wikipedia.org/wiki/Augustus_De_Morgan)
developed symbolic logic.

### What is a valid inference?

Aristotle syllogisms:

```
premise1, premise2 -> conclusion
```

An inference is valid,
tf the conclusion is true
in all **worlds** in which the premises are true.

**World** is defined as
truth value assignment to all propositions of interest.

[Propositional logic](https://en.wikipedia.org/wiki/Propositional_calculus)
is just a truth table,
while in probability we have probability table,
how can we go from propositional logic to causal logic?

### Interpret propositional logic by set theory

We can interpret propositions as sets,
and interpret the following inference as relations between sets.

```
premise1, premise2 -> conclusion
```

```
inclusion(intersection(premise1, premise2), conclusion)
```

An element of a set is a _World_,
which assigns truth value to propositions.

Note that, a proposition have two interpretations,

- A variable which can be assigned truth value.
- The set of worlds in which it is assigned true.

How does this relates to _causal reasoning_?

### What is causal reasoning?

We define causal reasoning as:

> Logic and Tools for answering causal questions (instead of propositions).

Typical causal questions:

- How effective is a given treatment in **preventing** a disease?
- Did the new tax law **cause** our sales to go up, or was it our dvertising campaign?
- What is the health-care cost **attributable** to obesity?
- Can hiring records prove an employer is guilty of a policy of sex **discrimination**?
- I'm about to quit my job. Should I? Will I **regret** it?

You can **NOT** express the above terms in classical logic.

They are inarticulable in the standard grammar of science.

|    equation | assignment   |
| ----------: | :----------- |
| `Y = a * X` | `Y <- a * X` |

Because _equality_ is the dominating connective (judgment)
of our scientific language, but we are talking about asymmetry.

During causal reasoning,
we can filling the asymmetry in our mind,
but robot can not.

My talk today can be thought of as a calculus of _assignment_.

I can express all the terms above,
in terms of assignment,
but not in terms of equality.

### Traditional statistical inference paradigm v.s. structural model paradigm

In traditional statistical inference paradigm,
_joint distribution_ is the Oracle.

In structural model paradigm,
the new Oracle is a model.
A model `M` is the invariant strategy
by which the nature assigns values to variables.

- **Xie:** The term "Oracle" is used,
  because we want to build a machine
  that can answer our queries.

A model is like a collection of springs,
where variables listen to each other and react to each other.

The phrases "listen to" and "react to" are about assignment.

The edges in the model are like ropes (or springs) behind the data.

## TODO (2019) Interpretability and explainability from a causal lens

[ [VIDEO](https://www.youtube.com/watch?v=mfh4fp_8oPg) ]

## TODO (2020) Data versus Science: Contesting the Soul of Data-Science [CIFAR]

[ [VIDEO](https://www.youtube.com/watch?v=X_1MG4ViVGM) ]

## (2012) Turing Award Lecture "The Mechanization of Causal Inference"

[ [VIDEO](https://www.youtube.com/watch?v=iNm4nFBFmvo) ]

- **Xie:** The structure of this talk is better than latter takls.
