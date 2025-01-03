---
title: data types as lattices
author: dana scott
year: 1976
---

# 学习动机

[2025-01-03]
看了 1970-outline-of-a-mathematical-theory-of-computation，
但是感觉太抽象了。
这篇论文应该有更多具体的例子。

# Abstruct

> The meaning of many kinds of expressions in programming languages
> can be taken as elements of certain spaces of "partial" objects. In
> this report these spaces are modeled in one universal domain Pw. the
> set of all subsets of the integers. This domain renders the
> connection of this semantic theory with the ordinary theory of
> number theoretic (especially general recursive) functions clear and
> straightforward.

这个 Abstruct 看起来确实有趣。
真的能把 lambda expression 解释为
"the set of all subsets of the [non-negative] integers"
中的元素呢？

# 0. introduction

> The main innovation in this report is to model everything within one
> "universal" domain, the domain of all subsets of the set of
> non-negative integers.

# 1. continuous functions

TODO

# 2. computability and definability

给 lambda 演算加上 0，add1，sub1 这三个 primitives。
然后把 lambda expression 到 PN := Power(Nat) 的 denotation 定义如下：

```
0 : PN
0 = {0}

add1 : PN -> PN
add1(x) = set-map(x, n => n + 1)

sub1 : PN -> PN
sub1(x) := set-map(x, n => max(n - 1, 0))

if : PN -> PN -> PN
if(z, x, y) = ...

ap : (N, N) -> (N) -> (N)
ap(u, x) = { m | exists (n: N) encode(n) within x, (n, m) in u }
// within 是集合的 included-in

given t : PN // with variable x
fn(x, t) : (N, N)
fn(x, t) = ...
```

那么 `add2 = fn(x, add1(add1(x)))` 作为 PN 的元素是什么？
也许应该先看看更简单的 `my-add1 = fn(x, add1(x))`，
其中 `add1 : PN -> PN`， `my-add1 : (N, N)`，
根据定义 `my-add1 = { (n, m) | m in add1(encode(n)) }`，
其中 `encode : N -> PN` 是 n 的二进制表示所对应的自然数子集。
为什么要用这个 `encode` 函数？
看看更简单的 `id = fn(x, x) = { (n, m) | m in encode(n) }`。
这个 `id : (N, N)` 有什么意义呢？
`id` 作为一个二元关系编码了集合的属于关系 `m in encode(n)`。
可以检查以下 `ap(id, x)` 的属性。

TODO 上面错了，`(n, m)` 并不是 `(N, N)` 的子集，
而是被定义为一个 `N`。

# 3. enumeration and degrees
# 4. retracts and data types
# 5. closure operations and algebraic lattices
# 6. subsets and their classification
# 7. total functions and functionality
