---
title: Normal Form in jojo
---

> **Normal Forms**
>
> Given a type,
> every expression described by that type has a normal form,
> which is the most direct way of writing it.
>
> If two expressions are the same,
> then they have identical normal forms,
> and if they have identical normal forms,
> then they are the same.
>
> -- The Little Typer, Chapter 1

也许在不同类型的语义中考虑 Normal Form，对于理解 Normal Form 有帮助。

也许在 jojo 里考虑 Normal Form，对于理解 Normal Form 有帮助。

我之前认为，其中的难点在于多返回值。

也许我应该模仿 lambda 演算的定义序列，
先处理 untyped 版本的 partial evaluation。
- 定义有 reduction step 定义。
- 用 reduction step 定义有方向的 (multi-step) reduction。
- 用有方向的 reduction 定义无方向的 equivalent。

``` cicada
datatype Jo {
  v(name: String): Jo
  let(name: String): Jo
  jojo(list: Array(Jo)): Jo
  exe(): Jo
}

datatype Value {
  jojo(list: Array(Jo), env: Env): Value
}

datatype Neutral {
  jojo(list: Array(Jo), env: Env): Neutral
}
```

也许 equivalent relation 应该被定义于 jojo 而不是 jo。

reduction step:

``` jojo
... [ f g h ] ! ... => ... f g h ...
```

partial evaluation:

- no recursive.
- inline all definitions.
- do all the reductions (including in-closure reductions).

example of in in-closure reductions:

``` jojo
[ (f) [f! swap f! swap] ]
```

the only pattern of reduction is `[  ]!`.

- variable bound by define, is different from variable bound by (x) and (y).

``` jojo
define swap [ (x) (y) x y ]

[1 2] [3 4] swap
[1 2] [3 4] [ (x) (y) x y ]!
[1 2] [3 4] (x) (y) x y
[1 2] (y) x y
x y
[3 4] y
[3 4] [1 2]
```
