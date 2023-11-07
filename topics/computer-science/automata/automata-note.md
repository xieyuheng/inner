---
title: automata note
---

# typing

transition is a partial function

```cicada
// deterministic finite state machine
transition : (state_t, input_t) -> state_t
// non-deterministic finite state machine
transition : (state_t, input_t) -> set_t(state_t)

// finite-state transducer
// for mealy model
emit : (state_t, input_t) -> output_t
// for moore model
emit : (state_t, input_t) -> output_t
```
