---
title: automata v.s. flowchart
author: Xie Yuheng
date: 2023-11-07
---

# In UML

In UML, we use _State Machine Diagram_ to draw automata,
and we use _Activity Diagram_ to draw flowchart.

- Activity Diagram supports concurrency.

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

# Representation

An automata can be represented by a state-transition table.
Which is the same as the adjacency list representation
of directed graph (with labeled edges).

A flowchart is the same as imperative programming language,
thus should better be represented by programming language syntax.
