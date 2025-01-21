---
title: opcode difference between scheme and forth
date: 2025-01-21
---

在实现 forth 时，我称带有 dictionary 的数据类型为 mod，
称 dictionary entry 为 definition 简称 def。
在引用一个全局名字时，用 CALL 这个 opcode，
CALL 先从 dictionary 中找到 definition，
然后 apply 这个 definition。

在实现 scheme 时，mod 中包含的是简单的 name 到 value 的 kv map。
没有 definition 的概念，所有东西都是 value。
value 可以被 apply。
一个函数调用可以被编译为，从 mod 中找到 value（称为 GET）然后再 APPLY。

- 在实现 inet-lisp 时，一个问题是每次 GET 函数时得到的是同一个 function，
  但是每次 GET node 时得到的是一个新的 node。

这两种实现方式在 EOPL 中有讨论过，
在 EOPL 中 def 被称为 den 代表 denotation。

也许需要重读 EOPL 才能处理好这个问题。

但是与 EOPL 不同的是，我现在正在编译到 opcode。
