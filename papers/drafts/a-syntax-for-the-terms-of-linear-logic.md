---
title: A Syntax for The Terms of Linear Logic
subtitle: A Syntax for Building Interaction Nets
author: Xie Yuheng
date: 2022-05-06
keywords: [Sequent Calculus, Linear Logic, Interaction Nets, Syntax Design]
---

# Abstract

By syntax, I mean expression implemented by syntax tree, not graph
(if we use graph, we can draw interaction nets directly).

And I use "syntax" as a countable noun syntaxes

# NOTE

- V.S. The Linear Abstract Machine

# Net -- typed undirected graph

A net is a typed undirected graph.

A graph is typed, if each node has fixed numebr of typed ports.

One connection consumes two ports.

When we want to connect two nodes by an edge,
the connection must go through the ports of the nodes,
and the types of the ports must match with each other.

> [Interaction nets](https://en.wikipedia.org/wiki/Interaction_nets)
> studies how to use interaction
> between nodes of net
> to model computations.

# Node and its ports

A node has a unique name.

A node's ports are split into two lists,
_input ports_ and _output ports_.

A node has one **principal port**,
which might be input port or output port.

We distinguish two kinds of nodes

- **Constructor**, whose principal port is its last output ports.
- **Eliminator**, whose principal port is its first input ports.

Using this convention,
we do not need to label
which port is principal port.

```
constructor <node> {
  <input-port> ...
  ------
  <output-port> ...
}


eliminator <node> {
  <input-port> ...
  ------
  <output-port> ...
}
```

# Type

A type has a unique name and an arity.

# Building nets

We use _postfix notation_ to build nets
with the help of _a stack of ports_.

When we call a node,
it connects its input ports to ports on the stack,
and consumes them,
then it puts its output ports back to the stack.

# Rule

A rule specifies
how to disconnect two nodes (active pair)
and reconnect them.

After disconnecting, we put input ports back to the stack.

# Examples

## Nat

```clojure
(define-type Nat Type)

(define-cons zero Nat)
(define-cons add1 (- Nat) Nat)
(define-elim add (- Nat) (- Nat) Nat)

(define-rule (zero add))
(define-rule (add1 add)
  add add1)

(claim-net two Nat)
(define-net two
  zero add1
  zero add1
  add)
```

## Trivial

```clojure
(define-type Trivial Type)
(define-cons sole Trivial)
```

## List

```
constructor null {
  vague (A: Type)
  ------
  A List
}

constructor cons {
  vague (A: Type)
  A A List
  ------
  A List
}

eliminator append {
  implicit (A: Type)
  A List A List
  ------
  A List
}

rule null append {}

rule cons append {
  rot rot append swap cons
}

rule cons append {
  let [that tail head]
  that tail append head cons
}

rule cons append {
  let head let tail let that
  that tail append head cons
}

rule cons append {
  let head append head cons
]

claim six-soles { Trivial List }

define six-soles {
  null sole cons sole cons sole cons
  null sole cons sole cons sole cons
  append
}
```

## Vector

```clojure
(define-type Vector (- Type) (- Nat) Type)

(define-cons null-vector
  (vague ((A Type)))
  zero A Vector)

(define-cons cons-vector
  (vague ((A Type) (prev Nat)))
  (- A) (- prev A Vector)
  prev add1 A Vector)

(define-elim vector-append
  (implicit ((A Type) (y Nat)))
  (- y A Vector)
  (implicit ((x Nat)))
  (- x A Vector)
  x y add A Vector)

(define-rule (null-vector vector-append))

(define-rule (cons-vector vector-append)
  (let head) vector-append head cons-vector)

(check-net (six Trivial Vector)
  null-vector sole cons-vector sole cons-vector sole cons-vector
  null-vector sole cons-vector sole cons-vector sole cons-vector
  vector-append)
```

## DiffList

```
// |- ~List(A), ~List(A), DiffList(A)

claim diff {
  vague (A: Type)
  A List A List
  ------
  A DiffList
}
```

```clojure
(define-type DiffList (- Type) Type)

(define-cons diff
  (vague ((A Type)))
  (- A List) (- A List)
  A DiffList)

(define-elim diff-append
  (implicit ((A Type)))
  (- A DiffList)
  (- A DiffList)
  A DiffList)

(define-elim diff-open
  (implicit ((A Type)))
  (- A DiffList)
  (- A List)
  A List)

(define-rule (diff diff-open)
  (let that left right)
  that left connect right)

(define-rule (diff diff-append)
  (let that left right)
  left that diff-open right diff)
```

`wire` places the two ports of a special edge on the stack.

If a wire's two ports are connected with port `A` and `B`,
after building a net, we remove the wire, and connect `A` with `B`.

```clojure
(check-net (Trivial DiffList)
  wire diff)

(check-net (Trivial DiffList)
  wire sole cons diff
  wire sole cons sole cons diff
  diff-append)
```
