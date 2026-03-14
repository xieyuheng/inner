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

提到了，当前论文的工作了，
是以 1994-qualified-types-theory-and-practice--mark-p-jones.pdf 为基础的。
但是 jones 的论文看起来是处理 type class 的。
难道 row polymorphism 和 type class 的实现机制是类似的？

普通的 type system 对应于普通的 logic programming，
qualified type system 对应于普通的 constraint logic programming。

> The type system is an application of qualified types, extended to
> deal with a general concept of rows. Positive information about the
> fields in a given row is captured in the type language using row
> extension, while negative information is reflected by the use of
> predicates.

为什么需要表达 negative information？

# 2 Overview

用来表达 row 的具体数据类型是 associative list：

- associative list 作为 row 的等价判断要忽略顺序，而看对应的 key。
- associative list 的末尾可能不是 null，而是一个类型变量。

## 2.1 Basic operations

> There is, however, one complication; we do not allow
> repeated uses of any label within a particular row,
> so the expression `{|l : α | r|}` is only valid
> if `l` does not appear in `r`.

这就是为什么要表达 negative，
也是为什么要 qualified type，
也是为什么 daan leijen 之后要写：
2005-extensible-records-with-scoped-labels。
