---
title: koka -- programming with row-polymorphic effect types
author: daan leijen
year: 2014
---

# My Motive

[2026-03-14] 要给 x-lisp/meta-lisp 增加 row-polymorphism 功能。

想要实现的是 record type 的 row-polymorphism，
但是这篇论文是 effect 的 row-polymorphism。

daan leijen 之前的论文 2005-extensible-records-with-scoped-labels，
应该是只关于 row-polymorphism 的。

先看这篇新的论文，因为从其引用中能看到新的发展。

# Abstract

> We propose a programming model where effects are treated in a
> disciplined way, and where the potential side-effects of a function
> are apparent in its type signature. The type and effect of
> expressions can also be inferred automatically, and we describe a
> polymorphic type inference system based on Hindley-Milner style
> inference.

> A novel feature is that we support polymorphic effects through
> row-polymorphism using duplicate labels.

duplicate labels 就是 daan leijen 2005 年的论文。

## 2.3. Polymorphic effects

> The approach we advocate in this paper and which is adopted by Koka
> is the use of row-polymorphism on effects. Row polymorphism is well
> understood and used for many inference systems for record calculi
> [7, 17, 23, 31, 34, 35].

这里的引用是：

- [7] Ben R. Gaster & Mark P. Jones (1996):
  A Polymorphic Type System for Extensible Records and Variants.
  Technical Report NOTTCS-TR-96-3, University of Nottingham.

- [17] Daan Leijen (2005):
  Extensible records with scoped labels.
  In: Proceedings of the 2005 Symposium
  on Trends in Functional Programming, pp. 297–312.

- [23] Sam Lindley & James Cheney (2012):
  Row-based effect types for database integration.
  In: TLDI’12, pp. 91–102, doi:10.1145/2103786.2103798.

- [31] Didier Remy (1994):
  Programming Objects with ML-ART,
  an Extension to ML with Abstract and Record Types.
  In: TACS ’94: Proc. Int. Conf.
  on Theoretical Aspects of Computer Software,
  pp. 321–346, doi:10.1007/3-540-57887-0 102.

- [34] Martin Sulzmann (1997):
  Designing record systems.
  Technical Report YALEU/DCS/RR-1128, Yale University.

- [35] Martin Sulzmann (1998):
  Type systems for records revisited. Unpublished.
