---
title: subtyping recursive types
authors: [roberto amadio, luca cardelli]
year: 1993
---

# My Motive

[2025-06-08] 如何处理递归定义的 structural type 之间的等价判断，
或者更确切地说，判断 compatible 以及 subtyping，
是我一直想解决的问题。

如果想实现带有 structural type 的 lisp，就又要解决这个问题。

看来这篇论文解决了这个问题。

# Abstract

> We investigate the interactions of subtyping and recursive types, in
> a simply typed λ-calculus. The two fundamental questions here are:
>
> 1. whether two (recursive) types are in the subtype relation,
> 2. and whether a term has a type.

> To address the first question, we relate various definitions of type
> equivalence and subtyping that are induced by a model, an ordering
> on infinite trees, an algorithm, and a set of type rules. We show
> soundness and completeness between the rules, the algorithm, and the
> tree semantics. We also prove soundness and a restricted form of
> completeness for the model.

> To address the second question, we show that to every pair of types
> in the subtype relation we can associate a term whose denotation is
> the uniquely determined coercion map between the two
> types. Moreover, we derive an algorithm that, when given a term with
> implicit coercions, can infer its least type whenever possible.

# 1 Introduction

作者为了研究 structural type + recursive type，
专门设计了两个语言 Amber 和 Quest，
在这一节的开头有引用到相关的论文。

## 1.1 Types

> A type, as normally intended in programming languages, is a
> collection of values sharing a common structure or shape.

从集合论意义上理解 type，
正是 structural type 的特点。

recursive structural type 的例子：

```cicada
Tree = Int + (Tree × Tree)
List = Unit + (Int × List)

Cell = (Unit → Int) × (Int → Cell) × (Cell → Cell)
Cell = {
  read: (Unit → Int)
  write: (Int → Cell),
  add: (Cell → Cell)
}
```

> Recursive types can hence be described by equations, and we shall
> see that in fact they can be unambiguously _defined_ by
> equations. To see this, we need some formal way of reasoning about
> the solutions of type equations.

> These formal tools become particularly useful if we start examining
> problematic equations such as t = t, s = s×s, r = r→r, etc., for
> which it is not clear whether there are solutions or whether the
> solutions are unique.

> It is appealing to set up sufficient conditions so that type
> equations have canonical solutions.  Then, if we have an equation
> such as t = Unit+(Int×t), we can talk about the solution of the
> equation. Such a canonical solution can then be indicated by a term
> such as µt.Unit+(Int×t); the type t that is equal to
> Unit+(Int×t). Here µt.α is a new type construction just introduced
> for denoting canonical solutions.

为什么人们总是用希腊字母 mu 来表示这里的解？
mu 来自 recursion theory 中的 mu operator。
有时间了需要仔细学一下 recursion theory。

用 lisp 的语法表示 mu 的话就是：

```scheme
(define (list-t A) (union unit-t (pair-t A (list-t A))))
(define (list-t A) (mu (T) (union unit-t (pair-t A T))))
```

> To say that L @ µt.Unit+(Int×t) (where @ means equal by definition)
> is the solution of the List equation, implies that L must satisfy
> the equation; that is, L = Unit+(Int×L) must be provable. This
> requirement suggests the most important rule for the µt.α
> construction, which amounts to a one-step unfolding of the
> recursion:
>
>    µt.α = α[µt.α/t]
>
> meaning that µt.α is equal to α where we replace t by µt.α
> itself. In our example we have:
>
>    L = µt.Unit+(Int×t) = (Unit+(Int×t))[L/t] = Unit+(Int×L)
>
> which is the equation we expected to hold.

> Having discussed recursive types, we now need to determine when a
> value belongs to a recursive type. The rule above for µt.α allows us
> to expand recursive types arbitrarily far, for a finite number of
> expansions. Hence, we can postulate that a finite value belongs to a
> recursive type if it belongs to one of its finite expansions
> according to the ordinary typing rules. That is, we push the
> troublesome µ's far enough until we no longer need to consider them.

> However, if the values are not finite, for example if they are
> defined recursively, we may not be able to push the µ's out of the
> way. In that case, we need to provide adequate notions of finite
> _approximations_ of values and types, and postulate that a value
> belongs to a type when every approximation of the value belongs to
> some approximation of the type. An approximation αn of a type
> expression α^n is an appropriate truncation of α at depth n, hence
> it is different from an unfolding. This will be made precise in
> later sections.

当考虑到递归定义的函数的时候，就会出现上面的问题。

## 1.2 Subtypes

> If types are collections of values, _subtypes_ should be subcollections.

对 subtype 的定义依然是集合论意义上的。

> For example, we can introduce two new basic types ⊥ (bottom), the
> collection containing only the divergent computation, and ⊤ (top),
> the collection of all values. Then ⊥ should be a subtype of every
> type, and every type should be a subtype of ⊤. We write these
> relations as ⊥≤α and α≤⊤.

在 lisp 中也许可以用如下名字：

- bottom type -- `error-t`
- top type -- `any-t`

注意，bottom 和 top 这两个词隐喻着其背后的 lattice theory。

> Function spaces α→β have a subtyping rule that is _antimonotonic_ in
> the first argument. That is,
>
>    α→β ≤ α'→β' if α' ≤ α and β ≤ β'
>
> For example, if Nat ≤ Int, and f: Int→Cell stores an integer into a
> cell, then f is also willing to store a natural number into a cell,
> that is f: Nat→Cell. Hence, it is sound to have Int→Cell ≤ Nat→Cell,
> but not the opposite.

注意这里对子类型关系的集合论解释。
`f: Int→Cell` 蕴含了 `f: Nat→Cell`，
因此前者是更小的集合。

即 `A ⊆ B` 定义为，对于任意 `x ∈ A` 也有 `x ∈ B`。

> Adequate subtyping rules can be found for all the other type
> constructions we may have. For example, for products we have
>
>    α×β ≤ α'×β' if α≤α' and β≤β'.
>
> Similarly, for disjoint unions we have
>
>    α+β ≤ α'+β' if α≤α' and β≤β'.

> What is, then, subtyping for recursive types?

下面介绍了几种错误的定义。

TODO

## 1.3 Equality of Recursive Types

等价的推演规则要独立于 subtyping 的推演规则给出。

## 1.4 Subtyping of Recursive Types
## 1.5 Algorithm outline
## 1.6 Formal development

# 2 A Simply Typed λ-calculus with Recursive Types

## 2.1 Types
## 2.2 Terms
## 2.3 Equations

# 3 Tree Ordering

## 3.1 Subtyping Non-recursive Types
## 3.2 Folding and Unfolding
## 3.3 Tree Expansion
## 3.4 Finite Approximations

# 4 An Algorithm

## 4.1 Canonical Forms
## 4.2 Computational Rules
## 4.3 Soundness and Completeness of the Algorithm
## 4.4 An Implementation

# 5 Typing Rules

# 5.1 Type Equivalence Rules
# 5.2 Completeness of Equivalence Rules
# 5.3 Subtyping Rules
# 5.4 Completeness of Subtyping Rules

# 6 A Per Model

## 6.1 Realizability Structure
## 6.2 Complete Uniform Pers
## 6.3 Completeness of an F-interpretation

# 7 Coercions

## 7.1 Definability
## 7.2 Inference

# 8 Conclusion

# 9 Acknowledgments

# References
