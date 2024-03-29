---
title: Interaction Combinators
author: Yves Lafont
year: 1997
---

# Xie: Drawing of nets

In the first paper "Interaction Nets",
drawing of a net is based on layered-tree,
input ports below a node, output ports above a node.

In this paper, there is no distinction
between input ports and output ports
(so that a node can connect to itself).

And drawing of node is changed, to highlight the principle port.

Without the input-output distinction,
the layered-tree meaning is losted in the drawing.

This makes the drawing much more hard to understand,
because based on layered-tree, a drawing of net
is similarly to a syntex tree,
and to highlight the principle,
we can just highlight the edge
without changing the layered-tree drawing.

# Xie: Reduced the right side of rule

The author require the right side of a rule to be reduced.

This means when defining a rule,
we can (and we should) eager evaluate the right side,
and the evaluation must terminate.
