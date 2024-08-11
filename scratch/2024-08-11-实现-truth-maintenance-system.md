---
title: 实现 Truth Maintenance System
date: 2024-08-11
---

读了 Truth Maintenance System 的论文之后，
发现这里用的是与 datalog 类似的正向推理系统，
我还没实现过类似的系统（之前只实现过 prolog 的反向推理系统）。

现在很有必要实现一个自己的正向推理系统。

记得之前看一个 linear logic 的演讲，
用的也是类似 datalog 的正向推理。

[Extremely interesting talk about logic programming](https://www.youtube.com/watch?v=rICThUCtJ0k)

- backend chaining + persistent facts = prolog
- forward chaining + persistent facts = datalog
- forward chaining + resource = evolving state (linear logic!)
- forward chaining + resource (can be user input) = interactive state

所以一旦实现了自己的系统，
就可以实验很多新的东西。
