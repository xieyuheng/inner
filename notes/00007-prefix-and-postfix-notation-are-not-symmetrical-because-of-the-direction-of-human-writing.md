---
title: Prefix and postfix notation are not symmetrical because of the direction of human writing
date: 2021-09-03
---

We should use prefix notation (polish notation)
instead of postfix notation (reverse polish notation) (like in [Forth][]).

[Forth]: https://en.wikipedia.org/wiki/Forth_(programming_language)

Because in prefix notation we can use the following indentation to format nested expressions:

```
+ * a b
  * + c d
    + e f
```

While in postfix notation the indentation will be:

```
f e +
d c + *
  b a * +
```

In prefix notation, the indentation is easier to edit,
because we (English speaker) are custom to to write from left to right.

A human culture often chooses one direction to write text,
and all tools about writing text are designed for this direction,
this will make one of the prefix or postfix notation easier to edit.

Thus, prefix and postfix notation are not symmetrical.

Note that, symmetry (like equivalence) should always be considered in context.

[2024-11-22] There's another argument to be made here: under
left-to-right reading order, prefix notation is action first and
postfix notation is data first.

And what if you structured the code to break and indent based on stack depth?

```
f e +
  d c +
    *
  b a *
    +
```

In another note,
f < g < h is much harder to flow together than h > g > f.
(or step 3 < step 2 < step 1 v.s. step 1 > step 2 > step 3).

This is why applicative language have statements:

```
step 1;
step 3 < step 2;
```

Example of how statements are required in prefix programming language:

- code from: https://github.com/evincarofautumn/hap-wip/blob/main/hap/hello.hap

```
put-in :var x 8;
put-in :var y 4;

put-in :var s 20;

color 0 0 0; clear;

color 238 102 119;

rect    mul s add  0 x    mul s       y    mul s 1    mul s 6;
rect    mul s add  1 x    mul s add 2 y    mul s 2    mul s 1;
rect    mul s add  3 x    mul s       y    mul s 1    mul s 6;

color 204 187 68;

rect    mul s add  5 x    mul s       y    mul s 1    mul s 6;
rect    mul s add  6 x    mul s       y    mul s 3    mul s 1;
rect    mul s add  6 x    mul s add 2 y    mul s 2    mul s 1;
rect    mul s add  6 x    mul s add 5 y    mul s 3    mul s 1;

color 34 136 51;

rect    mul s add 10 x    mul s       y    mul s 1    mul s 6;
rect    mul s add 11 x    mul s add 5 y    mul s 3    mul s 1;

color 68 119 170;

rect    mul s add 15 x    mul s       y    mul s 1    mul s 6;
rect    mul s add 16 x    mul s add 5 y    mul s 3    mul s 1;

color 170 51 119;

rect    mul s add 20 x    mul s       y    mul s 1    mul s 6;
rect    mul s add 21 x    mul s       y    mul s 2    mul s 1;
rect    mul s add 21 x    mul s add 5 y    mul s 2    mul s 1;
rect    mul s add 23 x    mul s       y    mul s 1    mul s 6;

show;
```
