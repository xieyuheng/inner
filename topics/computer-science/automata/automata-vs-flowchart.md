---
title: automata v.s. flowchart
author: Xie Yuheng
date: 2023-11-07
---

# In UML

In UML, we use _State Machine Diagram_ to draw automata,
and we use _Activity Diagram_ to draw flowchart.

About names:

- In activity diagram, actions are named.
- In state machine diagram, states AND actions are BOTH named.

About roles:

- In one activity diagram, there might be many actors.
- In one state machine diagram, there can ONLY be one actor.

To reduce an activity diagram involving many actors
to many state machine diagrams,
we must first partition the activity diagram by actors,
and to use message passing to handle across-actor actions.

# Concurrency

Activity Diagram supports concurrency.

In automata, if we want to support concurrency,
we need to be able to spawn new state node
and "join" multiple state nodes.

```
             | (B1) -> (B2) |
(A) -spawn-> |              | -join-> (D)
             | (C1)         |
```

Maybe we should implement Actor Model,
and use automata as private state of actor.

# Complexity

- flowchart = imperative programming language
- automata = regular grammar

# Duality and Topological relation

Suppose our flowchart is simple,
without any variables, the only state of
the flowchart is the PC (program counter).

|           | node   | edge       |
|-----------|--------|------------|
| flowchart | action | state (PC) |
| automata  | state  | action     |

State machine diagram is the cover sapce of flowchart.

```
flowchart =unfold=> state machine diagram
```

Unfolding (tracing) a loop in flowchart,
will generate a line in state machine diagram.

Automata is simple, because a state
is represented by a single value,
instead of a production of values.

In UML State Diagram, the state machine is said to have "extended states",
when it has local variables, and guard can reference these local variables.
In this sense, UML State Diagram is Turing complete and not limited to finite states,
which is very much like the local state of an Actor in Actor Model.

# Representation

An automata can be represented by a state-transition table.
Which is the same as the adjacency list representation
of directed graph (with labeled edges).

A flowchart is the same as imperative programming language,
thus should better be represented by programming language syntax.

# Translate from flowchart to state machine with "extended states"

To translate a flowchart to state machine with "extended states":

- View the edge before an action node as a single-action state of the state machine.
- View a decision node (or say, a branching node) as a multi-action state of the state machine.
- When the flowchart has multiple actors, partition the flowchart by actor,
  and view the two edges that go out of the boundary and come back from the boundary
  as a single state that waiting for event the other actor.
