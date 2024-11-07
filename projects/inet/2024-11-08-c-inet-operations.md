---
title: c inet operations
date: 2024-11-08
---

| word          | operation | comment                                                              |
|---------------|-----------|----------------------------------------------------------------------|
| `name`        | `op_exe`  | execute a program, use the `frame_stack` of `worker_t`               |
| `name`        | `op_app`  | apply a node, input: claim free ports, output: create new free ports |
| `@connect`    | `op_con`  | pop two free ports to connect their opposites                        |
| `(node)-port` | `op_gfp`  | get a free port in current frame and push it to the stack            |
| `port-(node)` |           | the same as `(node)-port @connect`                                   |
