---
title: Judea Pearl
---

[ [HOMEPAGE](http://bayes.cs.ucla.edu/jp_home.html) ]

# Judea Pearl Tribute Symposium: Causality

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

# A conversation between Judea Pearl and Stephen Wolfram

[ [VIDEO](https://www.youtube.com/watch?v=230PsGBxkCo) ]

## Heuristics is optimal solution to a simplified problem

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

## Why expert systems failed?

- **Stephen:** Why expert systems did not take over the world?

- **Judea:** I think it is because
  they try to model the experts rather than the environment,
  they try to model doctor instead of the disease.
  The doctor wasn't able to articulate the various procedures
  he or she go through when doing diagnosis,
  specially when it comes to combining different source of knowledge.
