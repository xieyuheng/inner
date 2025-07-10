---
title: type as predicate
date: 2025-07-10
---

考虑 structural type system，
其中的结构化的 type 好像是在用结构化的方式来表示 predicate，
使得 predicate 之间经过 union 和 inter，还是得到结构化的 type，
而结构化的 type 的特点就是，可以判断其等价与子类型。

这种以 type 为表示 predicate 的「正规方式」的想法，
只有在考虑函数类型时会遇到问题。

其实函数也可以被纳入到这种想法中，
这需要我们不要考虑作为 value 的函数：

```scheme
(claim apply (-> value-t value-t value-t))
```

而是考虑作为 exp 的函数：

```scheme
(claim evaluate (-> env-t exp-t value-t))
```

因为把 type 转化为 predicate 的是 `check`，
而 `check` 的参数是 exp 而不是 value：

```scheme
(claim check (-> ctx-t exp-t type-t bool-t))
```
