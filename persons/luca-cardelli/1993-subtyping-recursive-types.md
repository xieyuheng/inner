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

# My Notes

## directed graph 生成 infinite tree

[2025-06-08] 只看算法而不看对算法正确性的证明的话，其实 idea 非常简单。
本质上是要比较 infinite tree，
而这些 infinite tree 都是由 rooted directed graph 生成的，
想要比较两个 rooted directed graphs，
只需要在遍历的过程中记录所走过的两个 pathes 上的 nodes 的 address pair，
称这个 address pair 的 list 为 trail，
递归的时候带上这个 trail 即可。

# Abstract

> We investigate the interactions of subtyping and recursive types, in
> a simply typed λ-calculus. The two fundamental questions here are:
>
> 1. whether two (recursive) types are in the subtype relation,
> 2. and whether a term has a type.

注意，这里只讨论 simply typed λ-calculus，而不带 polymorphism。
因此举例的时候用了 list of int 而没有用带有类型参数的 list。

> To address the first question, we relate various definitions of type
> equivalence and subtyping that are induced by a model, an ordering
> on infinite trees, an algorithm, and a set of type rules. We show
> soundness and completeness between the rules, the algorithm, and the
> tree semantics. We also prove soundness and a restricted form of
> completeness for the model.

> To address the second question, we show that to every pair of types
> in the subtype relation we can associate a term whose denotation is
> the uniquely determined coercion map between the two types.
> Moreover, we derive an algorithm that, when given a term with
> implicit coercions, can infer its least type whenever possible.

需要用 coercion 是否代表这里所处理的 subtype 关系，
并非纯粹的集合之间的 inclusion 关系？

# 1 Introduction

> Subtyping is an inclusion relation between types that is present to
> some degree in many programming languages. Subtyping is especially
> important in object-oriented languages, where it is crucial for
> understanding the much more complex notions of inheritance and
> subclassing.

在我想要设计的 x-lisp 中，好像更简单，
没有 subtyping 和 subclassing 之分。

> Recursive types are also present in most languages. These types are
> supposed to _unfold_ recursively to match other types. Moreover,
> unfolding must preserve typing soundness and not cause the compiler
> to diverge.

在需要的时候 "to unfold recursively"，
看起来就是用有限的表示来实现无限的 recursion 的唯一方法。

TODO

作者为了研究 structural type + recursive type，
专门设计了两个语言 Amber 和 Quest，
在这一节的开头有引用到相关的论文。

## 1.1 Types

> A type, as normally intended in programming languages, is a
> collection of values sharing a common structure or shape.

从集合论意义上理解 type，这正是 structural type 的特点。

recursive structural type 的例子：

```cicada
Tree = Int + (Tree × Tree)
List = Unit + (Int × List)
```

```cicada
Cell = (Unit → Int) × (Int → Cell) × (Cell → Cell)
Cell = {
  read: (Unit → Int),
  write: (Int → Cell),
  add: (Cell → Cell),
}
```

> Recursive types can hence be described by equations, and we shall
> see that in fact they can be unambiguously _defined_ by equations.
> To see this, we need some formal way of reasoning about the
> solutions of type equations.

> These formal tools become particularly useful if we start examining
> problematic equations such as `t = t`, `s = s×s`, `r = r→r`, etc.,
> for which it is not clear whether there are solutions or whether the
> solutions are unique.

> It is appealing to set up sufficient conditions so that type
> equations have canonical solutions.  Then, if we have an equation
> such as `t = Unit+(Int×t)`, we can talk about the solution of the
> equation. Such a canonical solution can then be indicated by a term
> such as `µt.Unit+(Int×t)`; the type `t` that is equal to
> `Unit+(Int×t)`. Here `µt.α` is a new type construction just
> introduced for denoting canonical solutions.

为什么人们总是用希腊字母 mu 来表示这里的解？
mu 来自 recursion theory 中的 mu operator。
有时间了需要仔细学一下 recursion theory。

论文中不允许类型参数：

```scheme
(define int-list-t (union unit-t (tau int-t int-list-t)))
(define int-list-t (mu (T) (union unit-t (tau int-t T))))
```

我用 lisp 的语法表示 `mu`，
并且允许类型参数：

```scheme
(define (list-t A) (union unit-t (tau A (list-t A))))
(define (list-t A) (mu (T) (union unit-t (tau A T))))
```

当允许类型参数时，
把递归定义的 type 函数 `list-t`，改写为使用 `mu` 的定义，
假设了递归调用 `list-t` 时的参数不变。

因为用 `mu` 引入的 `T` 并不能带参数。

可以理解为是把「带类型参数」和「递归」这两件事分开表示了。

> To say that `L ≜ µt.Unit+(Int×t)` (where `≜` means equal by
> definition) is the solution of the `List` equation, implies that `L`
> must satisfy the equation; that is, `L = Unit+(Int×L)` must be
> provable. This requirement suggests the most important rule for the
> `µt.α` construction, which amounts to a one-step unfolding of the
> recursion:
>
>    µt.α = α[µt.α/t]
>
> meaning that `µt.α` is equal to `α` where we replace `t` by
> `µt.α` itself.  In our example we have:
>
>    L = µt.Unit+(Int×t) = (Unit+(Int×t))[L/t] = Unit+(Int×L)
>
> which is the equation we expected to hold.

用 lisp 语法表示：

```scheme
(define L (mu (T) (union unit-t (tau int-t T))))

(same-as-chart
  L
  (mu (T) (union unit-t (tau int-t T)))
  (let ((T L)) (union unit-t (tau int-t T)))
  (union unit-t (tau int-t L)))
```

TODO

> Having discussed recursive types, we now need to determine when a
> value belongs to a recursive type. The rule above for `µt.α` allows
> us to expand recursive types arbitrarily far, for a finite number of
> expansions. Hence, we can postulate that a finite value belongs to a
> recursive type if it belongs to one of its finite expansions
> according to the ordinary typing rules. That is, we push the
> troublesome `µ`'s far enough until we no longer need to consider
> them.

> However, if the values are not finite, for example if they are
> defined recursively, we may not be able to push the `µ`'s out of the
> way. In that case, we need to provide adequate notions of finite
> _approximations_ of values and types, and postulate that a value
> belongs to a type when every approximation of the value belongs to
> some approximation of the type. An approximation `α^n` of a type
> expression `α` is an appropriate truncation of `α` at depth `n`,
> hence it is different from an unfolding. This will be made precise
> in later sections.

当考虑到递归定义的函数的时候，就会出现上面的问题。

## 1.2 Subtypes

> If types are collections of values, _subtypes_ should be subcollections.

对 subtype 的定义依然是集合论意义上的。

> For example, we can introduce two new basic types `⊥` (bottom), the
> collection containing only the divergent computation, and `⊤` (top),
> the collection of all values. Then `⊥` should be a subtype of every
> type, and every type should be a subtype of `⊤`. We write these
> relations as `⊥≤α` and `α≤⊤`.

在 lisp 中也许可以用如下名字：

- bottom type -- `error-t`
- top type -- `any-t`

注意，bottom 和 top 这两个词隐喻着其背后的 lattice theory。

> Function spaces `α→β` have a subtyping rule that is
> _antimonotonic_ in the first argument. That is,
>
>    α→β ≤ α'→β' if α' ≤ α and β ≤ β'
>
> For example, if `Nat ≤ Int`, and `f: Int→Cell` stores an integer
> into a cell, then `f` is also willing to store a natural number into
> a cell, that is `f: Nat→Cell`. Hence, it is sound to have
> `Int→Cell ≤ Nat→Cell`, but not the opposite.

注意，这里对子类型关系的集合论解释。
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

强调等价的推演规则要独立于 subtyping 的推演规则给出。

TODO

## 1.4 Subtyping of Recursive Types

介绍判断 recursive structural type 的 subtype relation 时的难点。

TODO

## 1.5 Algorithm outline

介绍利用 trail 来比较 rooted directed graph 的算法。

整个 idea 可以被总结为：

- 递归定义所生成的无限树，某种意义上来说都是无限循环树，
  正如分数所生成的无穷小数都是无限循环小数的。
  无限循环数显然能被表示为有根有向图。

- 想要递归地比较两个有根有向图，
  只需要在比较时记录所走过的 path 为 trail。

TODO

> For other interesting examples,
> check how `µt.(t→t) ≤ µs.(s→s)` succeeds,
> and how `µt.(t→⊥) ≤ µs.(s→⊤)` fails.

```
{} µt.(t→t) ≤ µs.(s→s)
---------------------- {
  {t ≤ s} t→t ≤ s→s
  ----------------- {
    {t ≤ s} t ≤ s
    ------------- {
      success
    }
    {t ≤ s} s ≤ t
    ------------- [loopback s and t] {
      {t ≤ s} µs.(s→s) ≤ µt.(t→t)
      --------------------------- {
        {t ≤ s, s ≤ t} s→s ≤ t→t
        ------------------------ {
          {t ≤ s, s ≤ t} s ≤ t
          -------------------- {
            success
          }
          {t ≤ s, s ≤ t} t ≤ s
          -------------------- {
            success
          }
        }
      }
    }
  }
}
```

```
{} µt.(t→⊥) ≤ µs.(s→⊤)
---------------------- {
  {t ≤ s} t→⊥ ≤ s→⊤
  ----------------- {
    {t ≤ s} s ≤ t
    ------------- [loopback s and t] {
      {t ≤ s} µs.(s→⊤) ≤ µt.(t→⊥)
      --------------------------- {
        {t ≤ s, s ≤ t} s→⊤ ≤ t→⊥
        ------------------------ {
          {t ≤ s, s ≤ t} ⊤ ≤ ⊥
          -------------------- {
            fail
          }
          {t ≤ s, s ≤ t} t ≤ s
          -------------------- {
            success
          }
        }
      }
    }
    {t ≤ s} ⊥ ≤ ⊤
    ------------- {
      success
    }
  }
}
```

> One of the main aims of this paper is to show that the algorithm
> above is consistent with, and in fact equivalent to, the rules (2)
> and (3) of sections 1.2 and 1.3. For this we need to place both the
> rules and the algorithm in a more formal framework.

## 1.6 Formal development

> So far we have discussed rules for the subtyping of recursive types
> which are motivated by some operational intuition. In the following
> we will broaden our perspective and consider various notions of type
> equivalence, `α = β`, and subtyping, `α ≤ β`. These are induced by:

| (a) An ordering on infinite trees | `α =T β, α ≤T β` | (Section 3) |
| (b) An algorithm                  | `α =A β, α ≤A β` | (Section 4) |
| (c) A collection of typing rules  | `α =R β, α ≤R β` | (Section 5) |
| (d) A collection of per models    | `α =M β, α ≤M β` | (Section 6) |

> The mathematical content of the paper consists mainly in analyzing
> the relationships between these notions. For a simply typed lambda
> calculus with recursive types (described in Section 2) we show,
> among other properties:
>
>     α =T β <=> α =A β <=> α =R β <=> α =M β
>     α ≤T β <=> α ≤A β <=> α ≤R β <=> α ≤M β

> Moreover, we prove a restricted form of completeness with respect to
> the model (6.3), we show the definability in the calculus of certain
> maps that interpret coercions (7.1), and we give an algorithm for
> computing the minimal type of a term with respect to `≤T` (7.2).
> All these results support the relevance of the theory for the
> subtyping of recursive types sketched in this introduction.

# 2 A Simply Typed λ-calculus with Recursive Types

TODO

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
