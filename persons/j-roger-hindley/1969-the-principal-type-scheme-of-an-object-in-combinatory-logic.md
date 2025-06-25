---
title: the principal type scheme of an object in combinatory logic
author: j roger hindley
year: 1969
---

# My Motive

[2025-06-26] 想要了解 Hindley-Milner 类型系统中 Hindley 的贡献。
看起来，Hindley 的论文更强调 combinatory logic 的传统（毕竟他是 Curry 的合作者），
而 Milner 更强调 Data Scott 的指称语义。

# Introduction

> In their book Combinatory Logic [1], Curry and Feys introduced the
> notion of "functional character" (here called "type-scheme") of an
> object of combinatory logic. Roughly speaking, each object of
> combinatory logic ("ob" for short) represents a function or an
> operator on functions; for instance the ob `I` represents the
> identity operator, and we have for all obs X, `I X = X`.

> One of the aims of combinatory logic is to study the most basic
> properties of functions and other concepts, with as few restrictions
> as possible; hence in the simplest form of combinatory logic there
> is nothing to stop a function being applied to itself; thus `X X`
> is an ob for all obs `X`.

> However it is also natural to look at the consequences of
> introducing type-restrictions, formulated by assigning types to the
> obs according to certain rules, to be described later.

> Each type is a linguistic expression representing a set of
> functions, and for any type a the statement "X has type α" is
> intended to mean that the ob `X` represents a member of the set
> represented by `α`. Given types `α` and `β`, the set of all
> functions from the set `α` into the set `β` is represented by the
> type `F α β` (using Curry's notation).

但是恒等函数 `I` 的类型，用 `F` 表示时，
可能有多个，`F α α` 和 `F β β` 等等。

因此需要引入类型变元 `a, b, c`，
称 `I` 的类型为 `F a a`，
并称带有类型变元的类型为 type-scheme
（模仿 formal system 中的 axiom-scheme）。

> We shall see later that all the types which the rules assign to `I`
> have the form `F α α`, so the types of `I` are just those obtained
> by substituting a type `α` for the variables in the type-scheme
> `F a a`.

> It will not be obvious from the rules that every ob `X` has a
> type-scheme with this property. If `X` does have a type-scheme from
> which all the types of `X` (and no extra types) can be obtained by
> substituting types for variables, it will be called a _principal
> type-scheme_ of `X`.

> In §3 of this paper it will be proved that every ob `X` that has a
> type at all, has in fact a principal type-scheme which can be
> effectively determined from the structure of the ob.

> Finally, in §6 it will be shown that if `α` is a principal
> type-scheme of an ob `X`, then any substitution-instance `β` of `α`
> is a principal type-scheme of some ob `Xβ` (which is reducible to
> `X` by the reduction-rules for combinators).

这个结果还挺令人惊讶的。

> Besides its intrinsic interest, this result goes part of the way
> towards justifying a conjecture of Curry's that the alternative
> system of combinatory logic with type-restrictions (in which an
> infinity of basic combinators is postulated, each with a unique
> type) can be defined in the system described here. This point will
> be explained in §5.

是要避免引入 infinity of basic combinators 吗？

# 1 Definitions

TODO

# 2 Lemmas on substitution
# 3 Existence of principal type-schemes
# 4 The principal type-scheme of [x].M
# 5 An alternative approach to typed combinators
# 6 Every type-scheme is a p.t.s
