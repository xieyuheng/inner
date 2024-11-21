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
