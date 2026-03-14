---
title: a polymorphic type system for extensible records and variants
author: [benedict r. gaster, mark p. jones]
year: 1996
---

# My Motive

[2026-03-14] 要给 x-lisp/meta-lisp 增加 row-polymorphism 功能。

# Abstract

> Records and variants provide flexible ways to construct datatypes,
> but the restrictions imposed by practical type systems can prevent
> them from being used in flexible ways.  These limitations are often
> the result of concerns about efficiency or type inference, or of the
> difficulty in providing accurate types for key operations.

> This paper describes a new type system that remedies these problems:
> it supports extensible records and variants, with a full complement
> of polymorphic operations on each; and it offers an effective type
> inference algorithm and a simple compilation method. It is a
> practical system that can be understood and implemented as a natural
> extension of languages like Standard ML and Haskell.  In an area
> that has already received a great deal of attention from the
> research community, the type system described here is the first to
> combine all of these features in a practical framework.

> One important aspect of this work is the emphasis that it places on
> the use of rows in the construction of record and variant types. As
> a result, with only small extensions of the core type system, we are
> able to introduce new, powerful operations on records using features
> such as row polymorphism and first-class labels.

这可能是 row polymorphism 一词第一次出现。

# 1 Introduction

介绍为什么要用 record 来代替 product。
因为明显写出 field 的名字，可以让代码更清晰，
并且不依赖于顺序。

这里用 row 这个词，而没有用 record 这个词，
是因为想要让 record type 和 variant type 复用 row 这个结构。
也就是复用从有限的 key 到 value 的无序 mapping。

## 1.1 Polymorphism and extensibility

用最简单的方式实现 record type，
对 record 的操作将会受到很大限制，
将会失去 polymorphism 和 extensibility。

介绍前人在这方面的工作。

- 关于 subtype，luca 引入了 bounded quantification。
- 关于 row extension，wand 引入了 row variable。
  - [26] M. Wand. Complete type inference for simple objects

TODO
