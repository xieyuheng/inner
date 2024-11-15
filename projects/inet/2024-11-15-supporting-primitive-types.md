---
title: supporting primitive types
date: 2024-11-15
---

必须支持 primitive types，因为：

- 为了高效处理 int 和 float 之类的数字类型。
- 为了实现 map 和 filter 一类的高阶 node。
  即 node 的 port 所连的不是 wire，而是保存另外的 node。

有两种方式支持 primitive types：

- 经过 wire，用特殊的 box 保存 value：

  ```inet
  (node)-port-<>-[value]
  ```

- node 可以直接保存 value：

  ```inet
  (node)-port-[value]
  ```

  TODO 这样是最高效的，
  但是是否会影响 shared-memory parallelism？
