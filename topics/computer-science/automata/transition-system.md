---
title: Transition System
references:
  wikipedia: "https://en.wikipedia.org/wiki/Transition_system"
---

# 不同类型的转换系统 -- Transition System

最一般的 `TransitionSystem` 没有单值性，
因此 `transition` 不是函数是关系。

例子有 logic deduction system，逻辑式语言。

```cicada
class TransitionSystem {
  State: Type
  transition: [State, State]
}
```

确定性转换系统中 `transition` 是函数。

例子有各种 rewrite system：

- term rewrite system，例如 lambda calculus。
- graph rewrite system，例如 interaction net。

```cicada
class DeterministicTransitionSystem {
  State: Type
  transition: (State) -> State
}
```

带有 `Label` 转换系统。
其中 `Label` 可以代表 action、event 或 condition 等等。

```cicada
class LabelledTransitionSystem {
  State: Type
  Label: Type
  transition: [State, Label, State]
}
```

带有 `Label` 的确定性转换系统。

典型的例子是 finite automata。

```cicada
class DeterministicLabelledTransitionSystem {
  State: Type
  Label: Type
  transition: (State, Label) -> State
}
```

# 待分类的计算模型

- Petri net。

  - 即是有限状态机的推广，流程图（flowchart）的推广。

- Operational semantics。

- SECD 和 Forth。

# 相关的主题

- [Kripke structure](https://en.wikipedia.org/wiki/Kripke_structure_(model_checking))

- [Action Language](https://en.wikipedia.org/wiki/Action_language)

  - [Fluent](https://en.wikipedia.org/wiki/Fluent_(artificial_intelligence))
  - [Linear temporal logic as type system](https://en.wikipedia.org/wiki/Linear_temporal_logic)
