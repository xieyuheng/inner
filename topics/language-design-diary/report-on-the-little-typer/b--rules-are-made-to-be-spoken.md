---
title: B. Rules Are Made to Be Spoken
date: 2021-10-12
---

A Type system is a system for guiding human judgment,
In an implementation, each **form of judgment**
corresponds to a function that determines
whether a particular judgment is believable
by the Laws and Commandments.

| Form of judgment             | Reading                                 |
|------------------------------|-----------------------------------------|
| is_ctx(ctx)                  | ctx is a context.                       |
| fresh(ctx) ~> x              | ctx does not bind x.                    |
| lookup(ctx, x) ~> ct         | Looking up x in ctx yields the type ct. |
| is_type(ctx, et) ~> ct       | et represents the type ct.              |
| same_type(ctx, c1, c2)       | c1 and c2 are the same type.            |
| chech(ctx, e, ct) ~> ce      | check e can have type ct results in ce. |
| synth(ctx, e) ~> the(ct, ce) | From e, inter the ct ce.                |
| the_same(ctx, ct, c1, c2)    | c1 is the same ct as c2.                |
