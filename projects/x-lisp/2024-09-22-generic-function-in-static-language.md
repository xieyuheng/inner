---
title: Generic function in static language
date: 2024-09-22
---

也许很难在静态语言中实现 generic function，
因为在写 handler 时，写下的类型检查中用到的类型，
而在运行是 dispatch 的依据是 value 中带有的类型。

- 方案 A：在 elaboration 时让所有 value 都带有类型。

- 方案 B：在编译时解决 dispatch 的问题。

  这可能是很难做到的。
