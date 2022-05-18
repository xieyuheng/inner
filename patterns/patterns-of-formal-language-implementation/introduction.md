---
title: Introduction
author: Xie Yuheng
---

To summarize patterns in our implementation of cicada language.

TODO Why this book?

TODO Why a book like this might be a bad idea?

Categories:

- Syntax
- Type System

# Syntax

- **Lexical Scope and Closure**

- **Telescope**

  Telescope is used in data construction, function application and class telescope.

  Node that, in our implementation, telescope is `env` (not `ctx`).

  Telescope is about scope and binding.
  formal semantics of natural language, also study scope and binding.

  - <https://en.wikipedia.org/wiki/Scope_(formal_semantics)>
  - <https://en.wikipedia.org/wiki/Binding_(linguistics)>

- **Statements and Expressions**

  Two levels of interfaces to a language statements and expressions.

  Use statements to support features like module system (with side effects).

  Use statements to provide Interactive feedback.

# Type System

- **Bidirectional Type Checking**

  How to derive type checking algorithm from inference rules by bidirectional type checking

- **Normalization by Evaluation (NbE)**

  To compare equivalence between lambda terms.

- **Inductive Types**

- **Implicit and Vague Arguments**
