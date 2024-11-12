---
title: wire instead of port
date: 2024-11-12
---

Actually, the model of `(node)-port-<>-port-(node)` is:

| (node)-port | -< | >- | port-(node) |

A port is like a slot on a node,
we should not just say "port connecting to port",
but must emphasize say "port connecting to port via -<".
what should we call this -< ?

We should call it "wire".
-- a wire-like connector with different terminals,
just like in EE.

Possible situation of node wire connections:

```
(1)            -<
(2) (node)-port-<
(3) (node)-port-<>-
(4)            -<>-
(5) (node)-port-<>-port-(node)
```
