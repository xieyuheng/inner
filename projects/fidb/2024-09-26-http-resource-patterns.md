---
title: HTTP Resource Patterns
author: 谢宇恒
date: 2024-09-26
---

模仿 OOP 的前辈们，
用 pattern 的形式总结使用 http resource 的知识。

能想到的 patterns：

- curd
- metadata
- tokens and login
- users 是很特殊的

  > Note that, we can only express relations between something with user.
  >
  > Because the target of access control is a user.
  >
  > This is not a bad limitation,
  > because we are using a client/server architechure,
  > in which every request is sent by a user or a guest.

  可能需要再次回顾旧笔记：2023-10-14-implementing-relations.md
  并且转写一写到新笔记。
