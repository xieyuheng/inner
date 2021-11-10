---
title: We should build things to understand the things we build
date: 2021-09-27
keywords: [type checking, elaboration]
---

# How to build tools for understanding?

- How to make it easier to debug?
- How to make it easier to understand?
  - Not just the structure of the code,
    but also the behavior of the system.
- How to make it easier to test?
- How about "when in doubt, evaluate."?
- How to improve internal error message to help debug?

# Narration of elaboration during type checking

We can inject a `Narrator` into `check` and `infer`,
and mark an expression by `@elaborate`,
for narration of elaboration during type checking.
