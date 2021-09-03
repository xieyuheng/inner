---
title: Shallow embedding of DSL can reuse the means of abstraction of hosting language
date: 2021-09-03
---

# Shallow embedding v.s. Deep embedding

Deep embedding means we can design new syntax freely, but we need to write our own parser.

Shallow embedding is like designing API of a library or framework.

# Advantages of shallow embedding

We can reuse the means of abstraction of the hosting language -- such as closure and module system.

We can use the hosting language as a meta-language to extend the DSL -- like writing macros.

# About hosting language

JavaScript and TypeScript are good choice, because their are popular and practical.

# Examples

- Logic programming can use `(v) => [ ... ]` to scope logic variables.
- Testing frameworks often use `(t) => { ... }` to control the scope of a test case.
- Parser generator -- can reuse hosting language's module system, and use closure to write high order grammar.
