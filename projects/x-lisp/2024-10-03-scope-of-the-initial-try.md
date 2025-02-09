---
title: scope of the initial try
date: 2024-10-03
---

为了做到循序渐进地开发，
开始的时候可以限制项目的 scope：

- 实现 dependent type
- 实现 subtype relation
- 实现 implicit argument
- 实现 无限制的 inductive datatype

这些都是旧的 cicada 已经实现过的功能。

可以先不做的功能：

- 不实现 generic function
- 不实现 完备性检查

为了避免在写类型检查器的时候需要用 monad，
可能还需要实现 try catch 和 exception 的功能，
但是也许 propagator 可以让我们同时避免实用 exception 和 monad。

为了实用与自举，还需要读写文件等 IO 操作，
也许可以用外部解释器的方式来实现 IO，
就是用纯函数的方式生成 IO 相关的表达式，
然后由另一个专门做 IO 的解释器去解释这些表达式。

- 甚至 IO 解释器可以是更 primitive 的，
  带 byte code 的 VM，把表达式编译成 byte code
  的过程也可以用纯函数的风格完成。
