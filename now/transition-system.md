---
title: Transition System
---

```cicada
class TransitionSystem {
  State: Type
  transition: [State, State]
}

class DeterministicTransitionSystem {
  State: Type
  transition: (State) -> State
}

class LabelledTransitionSystem {
  State: Type
  Label: Type // To model action, event and condition.
  transition: [State, Label, State]
}

class DeterministicLabelledTransitionSystem {
  State: Type
  Label: Type
  transition: (State, Label) -> State
}
```
