---
title: extensible records with scoped labels
author: daan leijen
year: 2005
---

# My Motive

[2026-03-14] 要给 x-lisp/meta-lisp 增加 row-polymorphism 功能。

# Abstract

> Records provide a safe and flexible way to construct data
> structures.  We describe a natural approach to typing polymorphic
> and extensible records that is simple, easy to use in practice, and
> straightforward to implement.

> A novel aspect of this work is that records can contain duplicate
> labels, effectively introducing a form of scoping over the labels.

> Furthermore, it is a fully orthogonal extension to existing type
> systems and programming languages. In particular, we show how it can
> be used conveniently with standard Hindley-Milner, qualified types,
> and MLF.

# 1 Introduction

# 2 Record operations

> ... Effectively, we use parametric polymorphism to
> model a limited form of subtyping [2].

这是说 row polymorphism 自动给我们了一种 subtyping？
subtyping 的判断可以被转化为 subst instance 的判断。

- 这和我目前在 meta-lisp 中所独立实现的 subtyping 有什么不同？

- 是不是说有了 row polymorphism 就不需要额外实现 subtyping 了？

- 目前实现的 subtyping 可以处理递归定义的 datatype，
  如果不允许 record type 递归定义，
  是不是就不用在 subtyping 中处理递归了？

## 2.1 Safe operations

extension 的类型有两种：

- strict extension -- 要求所 extension 的 key 不存在。
- strict extension -- 允许所 extension 的 key 已经存在。

  此时 extension 有两种语义：

  - 不允许 key 重复，覆盖之前的 value。
  - 允许 key 重复，这是这篇论文所选择的语义。

我需要的是不允许 key 重复的语义，因为这样 record 的实现方式更简单。
允许 key 重复的话，在 runtime 就要实现为 record of stack。

允许 key 重复的一个用处是，
在局部给 object 增加 key，
在返回时再消除 key。

> However, the type system can always issue a warning if a record with
> a fixed type contains duplicate labels, which could be attributed to
> a programmer mistake.

学习了这里的实现方法后，在类型检查的过程中，
遇到 duplicated key 时，我可以直接报错。

另外，允许 duplicated key，
也许对于 dependent type 中的
dependent interface（class） 来说很有用。

# 3 The types of records

# 6 Type rules

# 7 Type inference

重点在于，如何 unify `{ x :: Int | A } = { y :: Int | B }`。

显然不能 unify `A = { y :: Int | B }`，
因为这样会得到 unify `{ x :: Int, y :: Int | B } = { y :: Int | B }`，
进而得到 unify `{ x :: Int | B } = B`，
这不能通过 occur check。

解决方案是，先扩展 `A` 和 `B`，按照等号对面的 record 补全 fields：

```
{ x :: Int | A } = { y :: Int | B }

A = { y :: T1 | R1 }
B = { x :: T2 | R2 }

{ x :: Int, y :: T1 | R1 } = { y :: Int, x :: T2 | R2 }
T1 = Int
T2 = Int
R1 = R2

A = { y :: Int | R1 }
B = { x :: Int | R1 }
```

类似地，如果遇到一个 type variable 和 record type 之间的 unify，
也需要先把 type variable 按照等号对面的 record 补全 fields 展开成 record type，
然后再 unify。

在我看来，这可能就是这篇论文的核心 idea 了。

论文中强调了一种需要注意的情况：
"rows with a common tail but a distinct prefix"

```
{ x :: Int | R } = { y :: Int | R }

R = { y :: T1 | A }
R = { x :: T2 | B }

{ y :: T1 | A } = { x :: T2 | B }

A = { x :: T3 | R1 }
B = { y :: T4 | R2 }

{ y :: T1, x :: T3 | R1 } = { x :: T2, y :: T4 | R2 }
T1 = T4
T2 = T3
R1 = R2

{ x :: T3 | R } = { y :: T4 | R }
```

就循环了。

需要用一些条件排除这种情况。

# 8 Implementing records

> Providing an efficient implementation for extensible and polymorphic
> records is not entirely straightforward. In this section we discuss
> several implementation techniques and show in particular how
> standard compilation techniques can be used to provide constant-time
> access for label selection.

也就是要避免使用 associative list 或 record of stack 来实现 duplicated key。

# 9 Related work

> An impressive amount of work has been done on type systems for
> records and we restrict ourselves to short overview of the most
> relevant work.

> The label selective calculus [5, 3] is a system that labels function
> parameters. Even though this calculus does not describe records,
> there are many similarities with our system and the unification
> algorithm contains a similar side condition to ensure termination.

可能是把 lambda calculus 扩展成支持 named parameter，
为什么会遇到与这里类似的 unification 的特殊情况？
