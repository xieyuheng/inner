---
title: connection graphs
author: alan bawden
year: 1986
---

# motive

[2025-01-23]
根据 [wikipedia](https://en.wikipedia.org/wiki/Linear_graph_grammar)：

> a linear graph grammar (also a connection graph reduction system or
> a port graph grammar) is a class of graph grammar on which nodes
> have a number of ports connected together by edges and edges connect
> exactly two ports together.  Interaction nets are a special subclass
> of linear graph grammars in which rewriting is confluent.

因此下面的三个名字时同义词：

- linear graph grammar
- connection graph reduction system
- port graph grammar

并且 interaction nets 是其特殊情况。

# Introduction

> When thinking about programming languages, it is important to choose
> an appropriate abstract machine model. Such an abstract model serves
> to modularize the programming language problem into two pieces:
> translation of some high level language into the language of the
> abstract machine, and implemention of the abstract machine on real
> hardware. This paper presents connection graph grammars as an
> abstract model for parallel computation.

这和 inet-lisp 的实现方式一样。

# Connection Grauhs

> Intuitively, a connection graph is similar to the topological
> structure of an electronic circuit. An electronic circuit consists
> of a collection of gadgets joined together by wires. Gadgets come in
> various types -- transistors, capacitors, resistors, etc. Each type
> of gadget always has the same number and kinds of terminals. A
> transistor, for example, always has three terminals called the
> collector, the base, and the emitter. Each terminal of each gadget
> can be joined, using wires, to some number of other terminals of
> other gadgets.

> A connection graph differs from a circuit chiefly in that we
> restrict the way terminals can be connected. In a connection graph
> each terminal must be connected to exactly one other terminal; in
> particular, there can be no unconnected terminals.

> Some convenient terminology: The gadgets in a connection graph are
> called _vertices_. As in a circuit, the type of a vertex is called
> simply a _type_, and the terminals of a vertex are called
> _terminals_. The wires that join pairs of terminals are called
> _connections_. The number of terminals a vertex has is its
> _valence_.

也许我也应该用 valence 一词来代替 arity。

> The type of a terminal is called a _label_. Thus, associated with
> each vertex type is a set of terminal labels that determine how many
> terminals a vertex of that type will possess, and what they are
> called.

虽然称为类型，
但是如果 vertex 的类型中只包含 label 的名字的话，
其实类似动态类型的 record。

> Given a set of types, each with an associated set of labels, we can
> consider the set of connection graphs over that set of types, just
> as given a set of letters called an alphabet, we can consider the
> set of strings over that alphabet.

这个类比其实可以看作是推广，因为 string 是特殊的 graph。

# Connection Graph Grammars

> A _connection graph grammar_ is a collection of production rules
> called _methods_. Each method describes how to replace a certain
> kind of subgraph with a different subgraph. If the connection graphs
> over some set of types are analogous to the strings over some
> alphabet, then a connection graph grammar is analogous to the
> familiar string grammar.

TODO
