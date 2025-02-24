---
title: concurrency note
---

# difference between process and automata

- 'process' as in process calculi.

- process as a special way of encoding states of automata.

# about implementation

- a process is a function of type (-> Event -- Process)

- a process is a function
  that can be stepped
  each step of the process is a state of the process
  process is a automata whose states are encoded by steps
  let's call them 'step-states'

- a process is a function
  that can be composed
  according to the natural of its step-states

# primitivity prejudice

- suppose :
  1. model-1 is more primitive then model-2
  2. model-2 can be reduced to model-1
  3. one can use model-1 to implement model-2
  4. it is meaningless to use model-2 to model-1
     because model-2 is already more powerful than model-1

- when ever such reduction as (2) can be done,
  a theoretical author might choose to
  use the more primitive model in his study,

  regardless the fact that although (3) is possible,
  but in practice, such implementation might be :
  1. more complicate than direct implementation
  2. has bad performance

- this kind of bad design decisions
  is called *primitivity prejudice*,
  which very often occurs in developments
  of mathematical theories.
