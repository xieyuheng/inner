---
title: An ignorable behavior of REPL
date: 2021-09-29
keywords: [repl, design]
---

When implementing the REPL of cicada language.
I found an ignorable behavior of REPL,
which is implemented differently among languages.

Inputing the following multiple lines into a REPL at once:

```
1



2



3



```

Different REPLs' behavior are different in the aspects of:

- handling empty line
  - output empty prompt on empty line
  - do not output prompt on empty line
  - do not allow empty line to be inputed

- handling the end of input
  - waiting for an extra enter
  - do not wait for an extra enter

- and so on...

Because this behavior is ignorable,
the choice is diversified
without any arguing.
