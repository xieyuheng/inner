---
title: Transition System
---

https://en.wikipedia.org/wiki/Transition_system

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

CamelCase is hard to read indeed.

```scheme
(class Transition-System
  (claim State Type)
  (claim transition (tuple State State)))

(class Deterministic-Transition-System
  (claim State: Type)
  (claim transition (-> State State)))

(class Labelled-Transition-System
  (claim State Type)
  (claim Label Type) ;; To model action, event and condition.
  (claim transition (tuple State Label State)))

(class Deterministic-Labelled-Transition-System
  (claim State Type)
  (claim Label Type)
  (claim transition (-> State Label State)))
```

# Examples

- Finite State Machine is finite Transition System.

- Rewrite system is Transition System

  - Interaction net is graph system.

- TODO How about Petri net?

  which is both a generalization of Finite State Machine
  and a generalization of flowchart.

- TODO How about Operational semantics?

# TODO Related

- Kripke structure https://en.wikipedia.org/wiki/Kripke_structure_(model_checking)

- Action Language https://en.wikipedia.org/wiki/Action_language

  - Fluent https://en.wikipedia.org/wiki/Fluent_(artificial_intelligence)
  - Linear temporal logic as type system https://en.wikipedia.org/wiki/Linear_temporal_logic
