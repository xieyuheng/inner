---
title: par note
repo: "https://github.com/faiface/par-lang"
---

The types are quite literally linear logic propositions:

```
(A) B == A times B
[A] B == not A par B

either { .left A, .right B }
==
A plus B

{ .left => A, .right => B }
==
A with B

! == one
? == bottom

either {} == zero
{} == top
```
