---
title: drop explicit substitution
date: 2024-09-28
---

放弃在这个项目中使用 explicit substitution。

因为：

- 实现一个模块中函数的相互递归调用时，
  还是要对 Mod 形成类似 closure 的东西。

- 如果没有 Exp 与 Value 的区分，
  就很难界定 partial evaluation 的边界。

这么说来，现在已经准备好了：

- 还是保留 Exp 与 Value 的区分。

- 不在用 Core 来实现 elaboration，
  尝试用 propagator 所带来的
  reactive programming 功能来实现 elaboration。

- 这个实现将会是：
  - 上一个 cicada 实现
  - 更换语法到 lisp
  - 实现上一个实现漏掉的 datatype
