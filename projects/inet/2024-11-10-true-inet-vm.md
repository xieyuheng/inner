---
title: true inet VM
date: 2024-11-10
---

VM 在于 bytecode 和 assembler。
定义以 label: memory layout 的方式给出。

为了实现 inet 的 VM，label 可能要能够支持 set，
比如 {add, zero} {add, add1}。

另外 program 的定义和 forth 一样，
难点在于 node 的定义如何进一步分解成 bytecode，
但是这样做合理吗？
