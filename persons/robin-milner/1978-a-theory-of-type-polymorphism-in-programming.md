---
title: a theory of type polymorphism in programming
author: robin milner
year: 1978
---

# My Motive

[2025-06-10] 通过 the little typer，
我学会了实现 dependent type system，
但是其实对 type system 的学习应该从更简单的入手，
应该从 Hindley-Milner type system 入手。

# Abstract

> The aim of this work is largely a practical one. A widely employed
> style of programming, particularly in structure-processing languages
> which impose no discipline of types, entails defining procedures
> which work well on objects of a wide variety.  We present a formal
> type discipline for such polymorphic procedures in the context of a
> simple programming language, and a compile time type-checking
> algorithm W which enforces the discipline.  A Semantic Soundness
> Theorem (based on a formal semantics for the language) states that
> well-type programs cannot “go wrong” and a Syntactic Soundness
> Theorem states that if W accepts a program then it is well typed.
> We also discuss extending these results to richer languages; a
> type-checking algorithm based on W is in fact already implemented
> and working, for the metalanguage ML in the Edinburgh LCF system,

# 1 Introduction

> The aim of this work is largely a practical one. A widely employed
> style of programming, particularly in structure-processing languages
> which impose no discipline of types (LISP is a perfect example),
> entails defining procedures which work well on objects of a wide
> variety (e.g., on lists of atoms, integers, or lists). Such
> flexibility is almost essential in this style of programming;
> unfortunately one often pays a price for it in the time taken to
> find rather inscrutable bugs -- anyone who mistakenly applies CDR to
> an atom in LISP, and finds himself absurdly adding a property list
> to an integer, will know the symptoms.

刚好我就是在给 lisp 加 structural type，
其中 structural 和 subtyping 的问题可以通过
luca cardelli 的 1993 论文 "subtyping recursive types" 来解决，
而带有类型参数的 parametric polymorphism 就需要这篇论文来解决。

> On the other hand a type discipline such as that of ALGOL 68 [22]
> which precludes the flexibility mentioned above, also precludes the
> programming style which we are talking about. ALGOL 60 was more
> flexible -- in that it required procedure parameters to be specified
> only as “procedure” (rather than say “integer to realprocedure”) --
> but the flexibility was not uniform, and not sufficient.

就像有人在 typescript 中只用 `Function` 类型，
而不具体给出参数类型和返回值类型。

> An early discussion of such flexibility can be found in Strachey
> [19], who was probably the first to call it polymorphism. In fact he
> qualified it as “parametric” polymorphism, in contrast to what he
> called “adhoc” polymorphism.  An example of the latter is the use of
> “+” to denote both integer and real addition (in fact it may be
> further extended to denote complex addition, vector addition, etc.);
> this use of an identifier at several distinct types is often now
> called “overloading,” and we are not doncerned with it in this
> paper.

从 Strachey 1967 年的论文 "fundamental concepts in programming languages"，
到这篇论文已经过了十年了，人们对 parametric polymorphism 的认识，
才从观察和总结现象，变成严谨完备的认识。

> First, everything concerning types is done at compile time; once the
> type checker (part of the compiler) has accepted a program or
> program phrase, code may be generated which assumes that no objects
> carry their types at run-time. This is widely accepted as yielding
> efficient object code, though it does impose constraints on the use
> of types compared with, for example, the approach in EL1 [21].

这也是 Strachey 1967 论文中所说的 manifest (comptime) vs. latent (runtime)。

所以我可能不会用这种纯编译时的类型实现方式，因为：

- 我想让语言能有一个 untyped (dynamicly typed) 版本，
  在实现时可以先实现这个版本。
- 由于我想在运行时能以 type 为 value，来形成 predicate。

> Second, many nontrivial programs can avoid mentioning types
> entirely, since they be inferred from context. (In ML however, as in
> other languages, the user may -- indeed often should -- define his
> own types together with operations over these types. Recent
> languages which allow the user to define his own types in this
> manner are CLU [8], ALPHARD [23] and Euclid [6]). Although it can be
> argued convincingly that to demand type specification for declared
> variables, including the formal parameters of procedures, leads to
> more intelligible problems, it is also convenient -- particularly in
> on-line programming -- to be able to leave out these
> specifications. In any case, the type checker which we present is
> quite simple and could not be made much simpler even if the types of
> variables were always specified in declarations.

"simple and could not be made much simpler"！

> Third, polymorphism plays a leading role. For example, a procedure
> is assigned a polymorphic type (which we abbreviate to _polytype_)
> in general; only when the types of its arguments and result can be
> uniquely determined from the context is it monomorphic (i.e.,
> assigned a _monotype_).

> We do not discuss in this paper -- except briefly at the end --
> either coercions or the “overloading” of identifiers. Our view is
> that these concepts, and also run-time type manipulation, are
> somewhat orthogonal to a compile-time polymorphic type discipline,
> and may (to some extent) be incorporated without invalidating it.

> In Section 2 we illustrate our type discipline by examples in a
> fragment of ML. This fragment should be self-explanatory, but an
> outline of ML is given in [3] and a full description appears in [2].

为了了解 ML，与 standard ML 的文档相比，
也许上面的两个引用更合适：

- [3] 1978-a-metalanguage-for-interactive-proof-in-lcf
- [2] 1979-edinburgh-lcf--a-mechanised-logic-of-computation

TODO

# 2 Illustrations of The Type Discipline

# 3 A Simple Applicative Language and Its Types

## 3.1 The Language Exp
## 3.2 Semantic Equations for Exp
## 3.3 Discussion of Types
## 3.4 Types and their Semantics
## 3.5 Type Assignments
## 3.6 Substitutions
## 3.7 Well-Typed Expressions Do Not Go Wrong

# 4 A Well-typing Algorithm and Its Correctness

## 4.1 The Algorithm W
## 4.2 The Soundness of W
## 4.3 Implementation of W; a Simplified Algorithm J

# 5 Types in Extended Languages

# 6 Conclusion
