---
title: interaction nets might be wrong
date: 2025-01-14
---

trying to understand the simple (no DT) linear type system,
i am reading this again: https://protovision.github.io/#!/documents/blog/linear-logic.html

i found that interaction nets might be wrong.

evidences:

- evidence 1:
  proof nets is the graph-based syntax for recording proof for linear logic.
  the starting point of interaction nets
  is to solve the "proof reducion" problem for proof nets.
  i.e. making clear the computational meaning of proof nets by graph rewriting.
  but inet fail to capture the whole linear logic,
  only the multiplicative part.

- evidence 2:
  in the 1990-interaction-nets paper,
  there is a distinction between input ports and output ports,
  which is important for capturing the semantics linear logic.
  but in the 1997-interaction-combinators paper,
  due to the choose of the combinators,
  the distinction must be removed.
  this means the link between inet and linear logic is not yet clearly understood,
  and only an interesting new model of computation is extracted out of the project.

- evidence 3:
  interaction nets limit the number of principle ports of a node to one,
  so the rewrite system will have good properties.
  but relax this limit actually does not hurt the good rewriting properties.

但是，只是从 inet rewrite rule 的角度来看，
inet 本身确实是不区分 inputs 和 outputs 的。

其实 inet 对 principle 的要求，
是 linear sequent 对 inputs 的要求。

即便是 nat-max disj conj 这些二元函数，
也不需要两个参数都到齐才能作用。

如何用 inet 实现能提前结束的 disj conj？
目前看来好像是必须要把两个参数都 reduce 之后才能结束。
