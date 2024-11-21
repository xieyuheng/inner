---
title: feature matrix
date: 2024-11-22
---

| plan      | tagged v.s. raw | GC  | type system  | generic  |
|           | about value     |     | maybe linear | dispatch |
|-----------|-----------------|-----|--------------|----------|
| initial   | raw             | no  | no           | no       |
| revised   | raw             | no  | yes          | no       |
| revised^2 | tagged          | yes | no           | no       |
|           | tagged          | no  | no           | yes      |

想要加入 generic dispatch，
但是我不知道应该如何设计语法。

如何在定义一个 generic function 的时候描述 predicates?

- stack: 1 2 "a"
- predicate: int? int? string?

总不能用一段 quoted program，
在里面置换参数来完成整体的 predicate 判断。

也许可以以 `( predicate predicate ... )` 为运行时的判断。

```
= add ( int? int? ) ...
= add ( int? vector? ) ...
```

so that i can;

```
= length ( null? ) ...
= length ( cons? ) ...
```
