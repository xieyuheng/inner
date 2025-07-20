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
只需要在遍历的过程中记录所走过的两个 paths 上的 nodes 的 address pair，
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

> In this paper we investigate the interaction of _unrestricted
> recursive_ types with subtyping.  This interaction is present in
> some modern languages based on _structural type matching_ (where
> type equality or subtyping is determined by some abstract type
> structure, and not by how types are syntactically presented).

这里说要处理 abstract type structure，
而不是 syntactical type structure，
但是后者才是我想要实现的 structural type。

> Name-matching determines type equality by relying, at least
> partially, on the names assigned to types in a given program,
> instead of on their structure. With name matching, recursive
> analysis can stop at occurrences of type names.

> The inconsistency of name-matching rules becomes a problem in
> distributed environments, where type definitions and data may
> migrate outside the compiler or program run in which they are
> created.  Types and data should have a meaning independent of
> particular runs, hence languages such as Modula-3 [22] and other
> experimental languages such as Amber [10] and Quest [9, 12]
> concerned with data persistence and data migration, have again
> adopted structural matching. Since these languages also rely on
> subtyping, structural subtyping becomes an issue.  Because of
> various language design issues, Modula-3 restricts itself to
> structural equivalence plus a limited form of structural subtyping;
> in this paper we deal with the unrestricted combination of recursion
> and subtyping, which forms the basis of Amber and Quest.

这里说所要实现的是 structural matching 而不是 name matching，
看起来又是我想要实现的 structural type。

难道说前面说的 "not by how types are syntactically presented"
是指 name matching？

这就是不给例子的坏处，
因为消除这种误解的最佳方式是具体例子所带来的经验。

作者为了研究 structural type + recursive type，
专门设计了两个语言 Amber 和 Quest。

Summary:

> - Section 1 provides the basic intuitions about recursive subtypes,
>   and we illustrate the main problems along with several
>   non-solutions.
>
> - Section 2 formalizes the syntax of a basic calculus with recursive
>   types.
>
> - Section 3 introduces a subtyping relation based on a tree
>   ordering.
>
> - Section 4 describes a subtyping algorithm.
>
> - Section 5 describes the corresponding type rules.
>
> - Section 6 gives a partial equivalence relation model.
>
> - Section 7 relates subtyping to type coercions.

## 1.1 Types

> A _type_, as normally intended in programming languages, is a
> collection of values sharing a common structure or shape.

从集合论意义上理解 type，这正是 structural type 的特点。

Examples:

> - Basic types:
>   - `Unit` -- the trivial type containing a single element.
>   - `Int` -- the collection of integer numbers.
> - Structured types:
>   - `Int→Int` -- the functions from integers to integers.
>   - `Int×Int` -- the pairs of two integers.
>   - `Unit+Int` -- the disjoint union of `Unit` and `Int` consisting
>     of either a unit value marked “left” or an integer marked
>     “right” (given two arbitrary but distinct marks).

这里用的是 disjoint union 而不是 union。

> A recursive type is a type that satisfies a recursive type equation.

```cicada
Tree = Int + (Tree × Tree)

List = Unit + (Int × List)
```

> Note that these are not definitions of `Tree` and `List`; they are
> equational properties that any definition of `Tree` and `List` must
> satisfy.

为什么说这些等式不是对类型的定义？
因为没有同时给出 data constructors 和 data eliminators？

在我看来这已经是对类型的具体定理了。
因为这里的定义利用了 either 和 pair 的 data constructors，
而 pattren matching 可以作为 data eliminators。
所以其实并不需要额外的定义了。

看了后面的 mu 之后，
这里说「等式不是类型的定义」，
可能是指一个等式可能被多种不同个的 mu 满足。

> There are also useful examples of recursion involving function
> spaces, typical of the object-oriented style of programming:

```cicada
Cell = (Unit → Int) × (Int → Cell) × (Cell → Cell)
```

> In each of these functions the current cell is implicit, so for
> example add needs only to receive another cell in order to perform a
> binary addition.

用 record type 来写：

```cicada
Cell = {
  read: (Unit → Int),
  write: (Int → Cell),
  add: (Cell → Cell),
}
```

> Recursive types can hence be described by equations, and we shall
> see that in fact they can be unambiguously _defined_ by equations.
> To see this, we need some formal way of reasoning about the
> solutions of type equations.  To see this, we need some formal way
> of reasoning about the solutions of type equations.  These formal
> tools become particularly useful if we start examining problematic
> equations such as `t = t`, `s = s×s`, `r = r→r`, etc., for which
> it is not clear whether there are solutions or whether the solutions
> are unique.

这里的 formal tools 指 denotational semantics 吗？

> It is appealing to set up sufficient conditions so that type
> equations have _canonical_ solutions.  Then, if we have an equation
> such as `t = Unit+(Int×t)`, we can talk about _the_ solution of the
> equation. Such a canonical solution can then be indicated by a term
> such as `µt.Unit+(Int×t)`; the type `t` that is equal to
> `Unit+(Int×t)`. Here `µt.α` is a new type construction just
> introduced for denoting canonical solutions.

用 mu 来做递归等式的解，就像是把递归的机制加入到语法中来，
正如用 lambda 定义函数，是把 substitution 的机制加入到语法中来。

combinatory logic 旨在保持语法简单，
通过引入新的元素来处理这些机制。

为什么人们总是用希腊字母 mu 来表示这里的解？
mu 来自 recursion theory 中的 mu operator。
有时间了需要仔细学一下 recursion theory。

论文中不允许类型参数：

```scheme
(define-type int-list-t (either-t unit-t (tau int-t int-list-t)))
(define-type int-list-t (mu (T) (either-t unit-t (tau int-t T))))
```

也许可以模仿 `tau` 给 `either-t` 一个类似的扩展，
比如 `(choice :left unit-t :right (tau int-t int-list-t))`，
这样就可以用 structural 的名字来 choose，而不是用 `left` 和 `right`。

如果说不带类型参数的递归类型定义是方程：

```math
x = 1 + c * x
x - c * x = 1
x * (1 - c) = 1
x = 1 / (1 - c)
```

那么带有参数的递归类型构造子定义就是函数方程：

```math
f(x) = 1 + x * f(x)
f(x) - x * f(x) = 1
f(x) * (1 - x) = 1
f(x) = 1 / (1 - x)
```

当然，类型所构成的代数结构并不允许我们做上面的等式变换，
比如集合之间的 union 不能把一项从右边，改变符号以转移到左边。
因此，确定类型所构成的代数结构具体是什么，就很重要。

我用 lisp 的语法表示 `mu`，
并且允许类型参数：

```scheme
(define-type (list-t A) (either-t unit-t (tau A (list-t A))))
(define-type (list-t A) (mu (T) (either-t unit-t (tau A T))))
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

正如 lambda calculus 中，
关于 lambda term 的 beta-reduction 是用 substitution 定义的，
这里关于 mu 的 one-step unfolding 也是用 substitution 定义的。

用 lisp 语法表示：

```scheme
(define-type L (mu (T) (either-t unit-t (tau int-t T))))

(same-as-chart
  L
  (mu (T) (either-t unit-t (tau int-t T)))
  (let ((T L)) (either-t unit-t (tau int-t T)))
  (either-t unit-t (tau int-t L)))
```

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
> way.

当考虑到递归定义的函数的时候，就会有这种问题。

> In that case, we need to provide adequate notions of finite
> _approximations_ of values and types, and postulate that a value
> belongs to a type when every approximation of the value belongs to
> some approximation of the type. An approximation `α^n` of a type
> expression `α` is an appropriate truncation of `α` at depth `n`,
> hence it is different from an unfolding. This will be made precise
> in later sections.

这里的 approximation 就类似「数列的极限」。
但是这里不是数，而是一个类型系统中的类型，和一个程序语言中的值。

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

即 `A ⊆ B` 定义为，对于任意 `x ∈ A` 蕴含 `x ∈ B`。

> Adequate subtyping rules can be found for all the other type
> constructions we may have. For example, for products we have
>
>    α×β ≤ α'×β' if α≤α' and β≤β'.
>
> Similarly, for disjoint unions we have
>
>    α+β ≤ α'+β' if α≤α' and β≤β'.

这里介绍了就子类型关系的判断而言，
不同 type constructor 的推演规则分别是什么。
那么就 lattice 中的 join（union）和 meet（inter）而言，
子类型关系的推演规则是什么呢？

注意，推演规则是为了手写证明，
或者作为一个寻找判断关系之算法的中间步骤，
对于 union 和 inter 的子类型关系的判断，
可否直接找到算法？

如果有 lattice 中的 normalization 算法，就可以判断子类型关系，
因为 lattice 中的 less 判断，可以转化为等式的判断。

但是先 normalization 再判断相等，可能会指数爆炸。
实现子类型检查函数，主要的难点是 union，
可能需要让函数带有当前 union 的所有参数。

比如：

```scheme
(less (union (tau :x A :y C)
             (tau :x B :y C))
      (tau :x (union A B) :y C))
```

可以转化为：

```scheme
(and (less (tau :x A :y C)
           (tau :x (union A B) :y C))
     (less (tau :x B :y C)
           (tau :x (union A B) :y C)))
```

但是：

```scheme
(less (tau :x (union A B) :y C)
      (union (tau :x A :y C)
             (tau :x B :y C)))
```

不能转化为：

```scheme
(or (less (tau :x (union A B) :y C)
          (tau :x A :y C))
    (less (tau :x (union A B) :y C)
          (tau :x B :y C)))
```

> What is, then, subtyping for recursive types?

下面介绍了几种错误的定义。

> The intuition we adopt is that two recursive types `α` and `β` are
> in the subtype relation if their _infinite_ unfoldings also are in
> this relation, in some appropriate sense. We might at first just
> consider finite unfoldings `α+` of a type `α`, and require that
> “`α ≤ β` if for every `α+` of `α` there is a `β+` of `β`
> with `α+ ≤ β+`”.

这里的「错误方案 A」，是模仿用 approximation
来定义「recursive value 属于 recursive type」的方式，
来定义「recursive type 小于 recursive type」。

> However, we shall see shortly that this condition is not strong
> enough. Hence, we insist on inclusion of infinite unfoldings. This
> is made precise by the notion, mentioned above, of finite
> approximations `α^n` of a type `α`, and by defining “`α ≤ β`
> if, for every `n`, `α^n ≤ β^n`”.

这里严格要求 `n` 相互对映看起来是不对的，
因为每次迭代两个递归类型展开的 wrapper 层数可能不一样。
而「错误方案 A」中的 `forall ... exists ...` 就是为了解决这个问题的。
为什么不能用「错误方案 A」？

看下面的例子，可能是因为 finite approximation
和 finite unfolding 的定义完全不同，
前者不带被递归定义的 name，
后者依然带有被递归定义的 name。

> Unfortunately, the formal subtyping rules for recursive types and
> the related algorithms cannot rely on approximations, since
> “`α^n ≤ β^n` for every `n`” involves testing an infinite number
> of conditions. The subtyping rules should rely instead on
> “finitary” rules, and it is therefore not so obvious how to invent
> a collection of rules that achieve the desired effect.

上面的「错误方案 A」是语义方面的错误方案，
下面的「错误方案 B」是推演规则方面的，即语法方面的错误方案。

或者根据 "topology via logic" 中，
按照 finite observations 来给断言分类的方式来说，
涉及到 "testing an infinite number of conditions" 的断言，
是不可证实的（non-affirmative），可证伪的（refutative）。

> For example, a first idea might be simply to say that:
>
>     if α ≤ β then µt.α ≤ µt.β    (1)

写成推演规则：

```
|- α ≤ β
--------------
|- µt.α ≤ µt.β
```

由这个推演规则，可以推出 `µt.⊤→t ≤ µt.⊥→t`，这是正确的；
但是也可以推出 `µt.t→⊥ ≤ µt.t→⊤`，这是错误的，
因为展开两次可以得到 `(α→⊥)→⊥ ≤ (β→⊤)→⊤`。

错误来自于在 antimonotonic 位置的递归展开，
如果递归变元只是出现在 monotonic 位置，
那么这个推演规则是没错的。

> A correct (and finitary) rule for inclusion of recursive types is
> instead the following:
>
>     (s ≤ t ⇒ α ≤ β) ⇒ µs.α ≤ µt.β    (2)
>
> That is, if by assuming the inclusion of the recursive variables we
> can verify the inclusion of the bodies, then we can deduce the
> inclusion of the recursive types.

写成推演规则：

```
s ≤ t |- α ≤ β
--------------
|- µs.α ≤ µt.β
```

搜索 `|- µt.(t→⊥) ≤ µs.(s→⊤)` 可以从下面找到，
这个推演规则如何推出这个 inclusion 应判断为假。

判断为真的例子，假设 `Nat≤Int` 可以推出 `NatList≤IntList`：

```cicada
NatList ≜ µs. Unit+(Nat×s)
IntList ≜ µt. Unit+(Int×t)
```

判断为假的例子，不能推出 `NatCell≤IntCell`：

```cicada
NatCell ≜ µs. (Unit→Nat) × (Nat→s) × (s→s)
IntCell ≜ µt. (Unit→Int) × (Int→t) × (t→t)
```

这应该为假，因为从程序员的直觉上说：

> For example, a `NatCell` might have a _write_ function of type
> `Nat→NatCell` that fails on negative numbers. If such a cell were
> considered as an `IntCell`, it would be possible to pass a negative
> integer to this write and cause it to fail.

## 1.3 Equality of Recursive Types

> We need now to consider strong notions of equality of recursive types.
> This is necessary because the rule (2) above is weak in some areas;
> for example, we cannot deduce directly from it that:
>
>     µt.t→t ≤ µs.s→s
>
> because this would require assuming both `s ≤ t` and `t ≤ s`.
> The combination of rule (2) and equality rules will finally give us
> all the power we need.

强调等价的推演规则要独立于 subtyping 的推演规则给出。

也就是要为 `=` 增加推演规则：

```
s = t |- α = β
--------------
|- µs.α = µt.β
```

如果真是这样，那么由推演规则所定义的子类型关系，
就不能用来定义类型所构成的 poset 了，
因为不满足 antisymmetry。

但是其实，搜索 `|- µt.(t→t) ≤ µs.(s→s)` 可以从下面找到，
用 loopback 强化后的推演规则如何推出这个 inclusion 应判断为真。

> This would work for `µt.t→t` and `µs.s→s`. But now consider the
> types:
>
>     α ≜ µs.Int→s
>     β ≜ µt.Int→Int→t
>
> They both expand infinitely into `Int→Int→Int→Int→...`, and they
> also have the same set of values (for example, recursive terms like
> `µf.λx:Int.f`).

> However, the assumption `s=t` does not show `Int→s = Int→Int→t`;
> we get stuck on the question whether `s = Int→t`.

> Another attempt might involve expanding the `µ`'s, but unfortunately
> we cannot expand them out of existence. By unfolding alone we can
> get only:
>
>    α = µs.Int→s = Int→(µs.Int→s) = Int→Int→(µs.Int→s) = Int→Int→α
>    β = µt.Int→Int→t = Int→Int→(µt.Int→Int→t) = Int→Int→β
>
> which after a few unfoldings leaves us with the original problem of
> determining whether `α = β`.  This is what we meant earlier by the
> insufficiency of “`α ≤ β` if for every expansion `α+` of `α`
> there is a `β+` of `β` with `α+` ≤ `β+`”.

> In fact, we seem to have made some progress here; we have come back
> to the original question `α = β` only after analyzing the entire
> structure of `α` and `β`. It seems that we should then be able to
> conclude that `α = β`, because a complete analysis of `α` and
> `β` has found no contradiction.

> This kind of reasoning is possible but it has to be carefully
> justified, and in general we need to determine the conditions under
> which this stronger notion of equality does not lead to a circular
> argument.

注意，上面发现 subtyping 和 equality 的正确推演规则的方式，
也是按照 "this kind of reasoning" 来的。
虽然所 reason 的对象是 mu 表达式而不是递归，
但是其实本质是一样的。

这里说的用这种方法的时候要小心，
就像是用「海伦方法」的迭代来计算平方根一样：
[Heron's_method](https://en.wikipedia.org/wiki/Square_root_algorithms#Heron's_method)

```math
x * x = S
x = S / x
x = x + S / x
x = (x + S / x) / 2
```

海伦等式总是成立的，
但是显然只有当这个迭代收敛时，
其计算才有效的。

> Note that in the process above we have found a single context
> `C[X] ≜ @ Int→Int→X` such that `α = C[α]` and `β = C[β]`; that is,
> both `α` and `β` are fixpoints of `C`. We shall be able to show
> that all the non-trivial (formally, _contractive_) type contexts
> `C[X]` have unique fixpoints over infinite trees, and therefore if
> they have two fixpoints these must be equal. Hence, the necessary
> rule for determining type equality can be formulated as follows:

>    α = C[α] ∧ β = C[β] ∧ C contractive ⇒ α=β    (3)

> It remains to be shown how to generate contractive contexts that
> allow us to equate any two types that have equal infinite
> expansions. This can be done via an algorithm, and in fact a natural
> one. We will show that this algorithm is sound (it will not equate
> types with different infinite expansions) and complete (it will
> equate all types that have equal infinite expansions). Such proofs
> of correctness of algorithms are among our major goals here, but
> first we need to carefully develop a formal framework.

## 1.4 Subtyping of Recursive Types

> The problem of equating recursive types such as `α` and `β` above
> can be related to well-known solvable problems, such as the
> equivalence of finite-state automata. However, the similar problem
> for subtyping has no well-known parallel.

最应该给出引用的地方，没有给出引用。

就是说，上面提到的讨论 equality 时所遇到的例子，
也许可以称为「结构错位」的例子，
在处理 subtyping 时也会遇到。

> Take, for example:
>
>     γ ≜ µs.Int→s
>     δ ≜ µt.Nat→Nat→t
>
> Again, looking at the infinite expansions we obtain `γ =
> Int→Int→...`, and `δ = Nat→Nat→...`, from which we would like
> to deduce `γ ≤ δ` by antimonotonicity. But what are the exact
> rules?  Attempts to unfold `γ` and `δ` fall into the same
> difficulties as before.

> The strategy here is to reduce the subtyping problem to an equality
> problem, which we solve by rule (3), plus rule (2). That is, we
> first show that `δ' ≜ µt.Nat→t = µt.Nat→Nat→t ≡ δ`. After
> that, we can use rule (2) to show `γ ≤ δ'`, and hence `γ ≤ δ`.

> Initially, this strategy suggests a two-step algorithm that first
> synchronizes the recursions in some appropriate way, and then uses
> rule (2) without additional folding/unfolding. Instead, we present
> an algorithm that tests subtyping of recursive types directly; the
> correspondence between the algorithm and the rules is then less
> obvious.

具体的算法比对推演规则的讨论还要容易理解。

> As a slightly more plausible example, suppose we define the type of
> lists of alternating integers and naturals:
>
>     IntNatList ≜ µt.Unit+Int×(Unit+Nat×t)

> This definition could arise more naturally from a mutual recursion
> construct in some programming language, for example:
>
>     IntNatList = Unit+Int×NatIntList
>     NatIntList = Unit+Nat×IntNatList
>
> One would certainly expect `NatList ≤ IntNatList` to hold. But,
>
>     NatList ≜ µs.Unit+Nat×s
>
> hence we have first to show that
>
>     NatList = µs.Unit+Nat×(Unit+Nat×s)
>
> and only then can we apply rule (2) successfully.

## 1.5 Algorithm outline

> We describe the algorithm informally and we show some sample
> runs. This is only an approximation of the algorithm analyzed in the
> formal part, but it should explain the main ideas.  A more detailed
> description is given in section 4.4.

后面将会看到，这里描述的算法没法处理
`µt.µs. ...t...s...` 之类的外层有多个 mu type 的情况。

> A recursive type of the form `µt. ...t...` can be represented in
> memory as a cyclic linked structure such that every occurrence of
> `t` in the recursive body is represented by the address of the
> corresponding `µt` structure, i.e., by a _back-pointer_.

递归定义在具体实现中的存在形式总是如此，
即 rooted directed graph，其中 directed edge 就是 C 意义上的 pointer。

考虑 directed graph 时，
`µt.µs. ...t...s...` 的两个 back pointer 会指向同一个 root，
把类型表达式转化为有向图，正是 section 4.4 处理这种情况的方式。

> Otherwise, all subexpressions of a type expression, including `µ`
> subexpressions, are uniquely determined by their address in memory.
> Every time the algorithm reaches a `µ` structure, possibly through a
> back-pointer, it has the option of analyzing the interior of the
> structure ("unfolding" the recursive type) or to compare its address
> with other addresses as a termination condition.

> The algorithm for `α ≤ β` operates on a pair of linked structures
> and a _trail_. A trail is a set of address pairs that records the
> pairs of addresses that have been jointly encountered when following
> a pair of paths in the two linked structures. To avoid diverging on
> cyclic structures, the algorithms registers a local successful
> termination when it reaches a pair of addresses that have already
> been _seen_, that is, a pair of addresses that are contained in the
> trail.

核心 idea 可以总结为：
通过记录 trail -- a pair of paths，
来比较两个 rooted directed graph 是否相等。

递归定义所生成的无限树，某种意义上来说都是无限循环树，
正如分数所生成的无穷小数都是无限循环小数的。
无限循环数显然能被表示为有根有向图。

想要递归地比较两个有根有向图，
只需要在比较时记录所走过的 path 为 trail。

> The algorithm to determine whether `α ≤ β` starts with an empty trail
> and proceeds through the following steps in sequence. We only
> consider basic types, function types, and recursive types.
>
> - [1] Succeed if the pair of addresses of `α` and `β` (in this
>   order) is contained in the trail.  (In this situation, we have
>   completely explored a pair of cyclic paths and found no subtyping
>   failures; hence we declare success for these paths.)
>
> - [2] Succeed if `α` and `β` are type constants that are equal or
>   in the subtype relation.  (This is the base case for the given
>   collection of basic types and basic inclusions.)
>
> - [3] When `α` is `α' → α"` and `β` is `β' → β"`, recur on
>   `β' ≤ α'` and on `α" ≤ β"`. Succeed if both recursions
>   succeed.  (This is the case for function types; note the swapping
>   of inclusion on the domains because of antimonotonicity of `→`;
>   no such swapping would occur for data type constructors such as
>   `×` and `+`).
>
> - [4.1] When `α` is `µt.α'` and `β` is `µs.β'`, add the pair of
>   addresses of `α` and `β` (in this order) to the trail, and recur
>   on `α' ≤ β'`. Succeed if the recursion succeeds.  (The presence
>   of `µ`'s signals potential cyclic paths, hence we store the current
>   pair of addresses in the trail so that case [1] can prevent
>   looping. We use an ordered pair of addresses because inclusion is,
>   obviously, not symmetric; this detail differs from the standard
>   trail algorithms for type equivalence. The next two cases are
>   similar.)
>
> - [4.2] When `α` is `µt.α'`, add the pair of addresses of `α` and
>   `β` to the trail, and recur on `α' ≤ β`.  Succeed if the
>   recursion succeeds.
>
> - [4.3] When `β` is `µs.β'`, add the pair of addresses of `α` and
>   `β` to the trail, and recur on `α ≤ β'`.  Succeed if the
>   recursion succeeds.
>
> - [5] Otherwise, fail. (This means we have found a pair of
>   incomparable type expressions, such as a function type and a base
>   type.)

注意，步骤 [4.2] 和 [4.3] 保存到 trail 的地址，
包含一般表达式的地址，这些地址在递归展开的过程中不稳定，
因此步骤 [1] 在判断 trail 中的一对元素是否相等时，
如果遇到两个非 `µ` 的类型，还是需要用 deep equal，
但是这种 deep equal 遇到 `µ` 不会展开。

通过假设地址稳定，可以看出这种实现方式是正确的。

> A faithful description of a run of this algorithm would involve
> assigning arbitrary addresses to subexpressions of type expressions;
> this would only obscure the exposition. Instead, we display the type
> expressions and we leave their addresses implicit: the reader is
> urged to keep this in mind.

给每个 subexpressions 以地址的过程，
可以被上面所说的 deep equal 代替。

给 `µ` 以地址还是需要的，实现方式可以模仿 lambda 的 closure
（因为 closure 作为 value 的 pointer 就是唯一地址），
每次求值 mu 表达式的时候，带上当前的 module 地址，
外加一个 fresh recursive variable name（或者直接用 pointer）。

```scheme
(define-type int-list-t (mu (T) (either-t unit-t (tau int-t T))))

['mu-value
 :mod <mod>
 :name 'T
 :exp '(either-t unit-t (tau int-t T))]
```

如果不用 `µ`，而是想直接处理递归定义，只要做 lazy unfolding 就可以，
被 lazy unfolding 的对象，可以是求值递归定义时获得一类新值，
带上当前的 module 地址，不用生成 fresh name，
直接以所定义的名字为 recursive variable name 就可以。

```scheme
(define-type int-list-t (either-t unit-t (tau int-t int-list-t)))

['mod-mu-value
 :mod <mod>
 :name 'int-list-t
 :exp '(either-t unit-t (tau int-t int-list-t))]
```

考虑带有类型参数的情况。

（1）使用 `mu` 就和上面一样，
只要把 `list-t` 看成是返回
`mu-value` 的 closure 就行了。

虽然 `list-t` 每次作用都会返回地址不同的 `mu-value`，
但是 `list-t` 本身已经不是递归变量了，所以没有关系。

```scheme
(define-type (list-t A) (mu (T) (either-t unit-t (tau A T))))
```

（2）使用递归定义时，`list-t` 开始可以被处理成 closure，
但是此时 loopback 的依据，不能是 type 的地址了，
而是在形成 type 之前，type constructor 的地址，
因为 type constructor 是递归变量。

也就是说，谁是递归变量，谁的地址就是依据。

type constructor 的地址，只是 loopback 依据的一部分，
另外一部分是 type constructor 的 arguments。
因此，当 type constructor 作用到 arguments 上，
而得到 type 时，可以直接把这个 type 的来源记下来：

```scheme
(define-type (list-t A) (either-t unit-t (tau A (list-t A))))

(list-t int-t)
=>
['delayed-type-constructor-application
 :type-constructor list-t
 :args [int-t]]
=>
['type
 :type-constructor either-t
 :args [unit-t
        (tau int-t
             ['delayed-type-constructor-application
              :type-constructor list-t
              :args [int-t]])]
 :origin ['delayed-type-constructor-application
          :type-constructor list-t
          :args [int-t]]]
```

看起来是可行的。

我用 `|-` 来分割左边的 context 和右边的 judgement。

带有 `{}` 的推演步骤是反向，
用大括号 `{}` 将前提包裹起来，并放在结论的下方，
类似 prolog 中个的 Horn clause。
不带 `{}` 的推演规则是传统的正向。

> The first sample run involves two types with matching `µ`
> structures; their inclusion is non-trivial because of
> antimonotonicity.

```logic
|- µt.((t→t)→⊥) ≤ µs.((s→⊥)→⊤)
------------------------------ [4.1] {
  t≤s |- (t→t)→⊥ ≤ (s→⊥)→⊤
  ------------------------ [3] {
    t≤s |- s→⊥ ≤ t→t
    ---------------- [3] {
      t≤s |- t ≤ s
      ------------ [1] {
        success
      }
      t≤s |- ⊥ ≤ t
      ------------ [2] {
        success
      }
    }
    t≤s |- ⊥ ≤ ⊤
    ------------ [2] {
      success
    }
  }
}
```

其实可以完全省略 `---`：

```logic
|- µt.((t→t)→⊥) ≤ µs.((s→⊥)→⊤) [4.1] {
  t≤s |- (t→t)→⊥ ≤ (s→⊥)→⊤ [3] {
    t≤s |- s→⊥ ≤ t→t [3] {
      t≤s |- t ≤ s [1] { success }
      t≤s |- ⊥ ≤ t [2] { success }
    }
    t≤s |- ⊥ ≤ ⊤ [2] { success }
  }
}
```

> The second sample run involves two types with mismatching `µ`
> structures. This mismatch introduces the need to examine a cyclic
> path more that once. For this, we use a `loopback` step, which
> corresponds to following a cyclic structure back to its original
> entry point (an artificial loopback step is needed only because, as
> we said, we keep the address information implicit). In the algorithm
> above, a loopback situation corresponds to a failure of step [1]
> followed by some dereferencing of back-pointers that leads to step
> [4].

```logic
|- µt.(⊤→t) ≤ µs.(⊥→(⊥→s)) [4.1] {
  t≤s |- ⊤→t ≤ ⊥→(⊥→s) [3] {
    t≤s |- ⊥ ≤ ⊤ [2] { success }
    t≤s |- t ≤ ⊥→s [loopback] {
      t≤s |- µt.(⊤→t) ≤ ⊥→s [4.2] {
        t≤s, t≤⊥→s |- ⊤→t ≤ ⊥→s [3] {
          t≤s, t≤⊥→s |- ⊥ ≤ ⊤ [2] { success }
          t≤s, t≤⊥→s |- t ≤ s [1] { success }
        }
      }
    }
  }
}
```

> Hence, in this run we go around the `µt` loop twice in order to go
> around the `µs` loop once.

就像 unify 所得到的 subst 是等式的解一样，
上面类型检查后所得到的 context 中的 constraints
-- `t≤s, t≤⊥→s` 也可以被看成是不等式的解。

这甚至和 subst 有类似的形式，
因为如上所得的 constraint，
总有一项是 recursive variable。

> For other interesting examples,
> check how `µt.(t→t) ≤ µs.(s→s)` succeeds,
> and how `µt.(t→⊥) ≤ µs.(s→⊤)` fails.

```logic
|- µt.(t→t) ≤ µs.(s→s) {
  t ≤ s |- t→t ≤ s→s {
    t ≤ s |- t ≤ s { success }
    t ≤ s |- s ≤ t [loopback s and t] {
      t ≤ s |- µs.(s→s) ≤ µt.(t→t) {
        t ≤ s, s ≤ t |- s→s ≤ t→t {
          t ≤ s, s ≤ t |- s ≤ t { success }
          t ≤ s, s ≤ t |- t ≤ s { success }
        }
      }
    }
  }
}
```

```logic
|- µt.(t→⊥) ≤ µs.(s→⊤) {
  t ≤ s |- t→⊥ ≤ s→⊤ {
    t ≤ s |- s ≤ t [loopback s and t] {
      t ≤ s |- µs.(s→⊤) ≤ µt.(t→⊥) {
        t ≤ s, s ≤ t |- s→⊤ ≤ t→⊥ {
          t ≤ s, s ≤ t |- ⊤ ≤ ⊥ { fail }
          t ≤ s, s ≤ t |- t ≤ s { success }
        }
      }
    }
    t ≤ s |- ⊥ ≤ ⊤ { success }
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

从「工程师」的角度来说，"operational intuition" 是最为重要的，
但是其他的视角也很重要，可以帮助我们看到当前理论与其他理论的联系。

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

> We consider a simply typed λ-calculus with recursive types and two
> ground types bottom and top; the latter play the roles of least and
> greatest elements in the subtype relation.  Although this calculus
> is very simple, it already embodies the most interesting problems
> for which we can provide solutions sufficiently general to extend to
> other domains. In the conclusions we comment on which techniques can
> be applied to more complex calculi.

Conclusion 中提到了只有 regular tree 才能用这里的方法每，
什么是 regular tree？能够被有限的 directed graph 生成的吗？
递归的类型可以，但是递归函数不可以？

可能可以通过读 "tree automata techniques and applications"
来补全 regular tree 相关的知识。
但是这应该在 x-lisp 的实现之后，
用 x-lisp 来做实验。

## 2.1 Types

> In an informal BNF notation, types are defined as follows:

```bnf
t, s, ... type variables and type constants, indifferently
α ::= t | ⊥ | ⊤ | α→β | µt.α
```

> Types are identified up to renaming of bound variables.

## 2.2 Terms

`fold` 和 `unfold` 被作为 explicit syntax 加入到了 term 的定义中。
可能是因为使用了 church 风格的 term，
在建立起 type 之间的等价理论之前，
不这样做，就不能保证每个 term 有唯一个类型。

## 2.3 Equations

# 3 Tree Ordering

## 3.1 Subtyping Non-recursive Types

non-recursive types 的 subtyping 非常简单。

可以发现，structural type system 中的 union 和 inter，
其实是在把 poset 补全成 lattice。

## 3.2 Folding and Unfolding

## 3.3 Tree Expansion

> Let us first explain how to associate a finitely branching, labeled,
> regular tree with any recursive type.

finitely branching 是说 rank 有上界，但是 tree 依然可以是无穷的。

> Paths in a tree are represented by finite sequences of natural
> numbers `π,σ ∈ ω*`, with `πσ` for concatenation and `nil` as
> the empty sequence.

> Nodes in a tree are labeled by a ranked alphabet
>
>     L = {⊥/0, ⊤/0, →/2} ∪ {t/0 | t is a type variable},
>
> where the superscripts indicate arity.

> A tree `A ∈ ω* -> L` is a partial function from (paths) `ω*` into
> (node labels) `L`, whose domain is non-empty and prefix-closed, and
> such that each node has a number of children equal to the rank of
> the associated label.

我不用 superscript，而用类似 prolog 的 arity 标记。

这确实就是 "tree automata techniques and applications" 中定义 tree 的方式。
这里竟然没有引用 tree automata 相关的文献。

> Formally, let `A(π)↓` indicate that `π` is in the domain of `A`
> (and `A(π)↑` indicate the opposite).  Then the collection
> `Tree(L)` of finitely-branching labeled trees over `L`, is given by
> the partial maps:
>
>     A: ω* -> L such that:
>       A(nil)↓
>       A(πσ)↓ ⇒ A(π)↓
>       A(π) = p/i ⇒ ∀j:0≤j<i. A(πj)↓

这其实是在定义 partial map 的 domain 需要满足的条件。

下面定义 type 到 `Tree(L)` 的映射。

只需要注意对 mu type 的处理：

- `μt.t` 和 `μt.μt1.t` 之类的 type 被映射为了 bottom。
- `T(µt.α)(π) ≜ T([µt.α/t]α)(π)` -- 就是说遇到 mu type 就展开。

这种映射确实体现了 denotational semantics 的意义，
首先 tree 作为数学对象的可构造性很强，
其次考虑 tree 这种数学对象的时候，
语法上的构造 mu 被消除了。

语法构造与永恒的数学对象相比，
前者给人的感受是相当任意的设计。

感觉在计算机科学的，
强调可构造和可计算的领域待地太久，
我已经忘记数学的感受了。
也许应该看 "proofs-from-the-book" 来复习一下。

> **Remark 3.3.3** For every `α`, `Tα` is a regular tree, that is, a
> tree with a finite number of different subtrees.

> Every tree is completely specified by the language of its
> occurrences, where if `p ∈ L` and `A ∈ Tree(L)` then the
> occurrences are `Occ(p, A) ≜ {π ∈ ω* | A(π) = p}`. In particular,
> every regular tree `A` has an associated set `{πp | π ∈ Occ(p, A),
> p ∈ L}` which is a regular language [16].

[16] Courcelle, B. Fundamental properties of infinite trees,
Theoretical Computer Science, 25, pp 95-169, 1983.

> From this it follows that given types `α`, `β`, the problem of
> deciding if `Tα = Tβ` is reducible to the problem of the equivalence
> of deterministic finite-state automata.

这里说的甚至不是 tree automata，就是普通的 automata，
这些关于 automata 和 tree automata 的基础知识是一定要学的了。

## 3.4 Finite Approximations

用 tree 的 finite approximation，
把 finite tree 上所定义的序关系，
扩展到一般 tree 上。

注意，这里用深度 k 截断来定义 approximation，
截断之后会依据 antimonotonic 或 monotonic 位置，
分别填 top 和 bottom 进去。

这可以用来证明：`α ≜ µt.⊤→t ≤T µt.⊥→(⊥→t) ≜ β`。
比如某个截断之后，分别得到：
`⊤ → ⊤ → ⊤ → ⊥` 与
`⊥ → ⊥ → ⊥ → ⊤`。

这样就能用来定义一般 type 之间的序关系。

type 到 tree 的转化相当于是做了一次 normalization，
很多语法表示上不同的类型都被转化为相同的 tree 了。

这样 finite approximation 就很好定义了。
否则去考虑形态各异的 mu type 的有限展开就会很复杂。

这种使用 denotational semantics 的方式很不错！

如果我接受了被定义为函数的，
可能 infinite 的 tree 为具体的数学结构，
那么是否也能接受被定义为函数的实数了？
这样也就不用批评 domain theory 里用到的拓扑和极限技巧了。

Remarks 3.4.4 还提到了另外一种把 finite tree 上的序关系，
扩展到一般 tree 的方式，就是简单地用递归定义。
这确实也能把一些 infinite tree 包含到关系中来，
但是这不能处理上面所提到的 approximation 所能处理的例子。

# 4 An Algorithm

## 4.1 Canonical Forms

在 type 的语法结构相等，
和作为 tree 相等之间，
在引入一层 canonical form 所定义的等价关系。

> For example, the recursive type `(µt.µs.t→s)→ ((µt.t)→(µt.⊤))`
> can be simplified to the canonical form `(µv.v→v)→(⊥→⊤)` without
> changing the denoted tree.

考虑如何用 directed graph 表示 mu type 就理解这种等价关系了，
说 infinite tree 是由 rooted directed graph 生成的，
说的就是这种 directed graph。

这和 lambda expression 可以用 rooted directed graph 来表示类似，
差异在于表示 mu type 的 directed graph 没有专门的 mu 节点，
而表示 lambda expression 的 directed graph 有有专门的 lambda 节点。

确实不做这种 canonical form 就没法使用第一章所提描述的算法。
但是，如果我们直接写递归定义，而不是 mu type 这种东西，
可能是不需要这里的 canonical form 的。

这一节所用的技巧也非常有趣，说是处理 equations，
其实是有方向的（等式两边不对称），
这应该等价于处理 directed graph。

TODO

## 4.2 Computational Rules
## 4.3 Soundness and Completeness of the Algorithm
## 4.4 An Implementation

# 5 Typing Rules

> In this section we introduce a certain number of axioms and rules
> for type equality and subtyping. These are intended as natural rules
> for a language based on subtyping, and as a specification of a
> subtyping algorithm for such a language. In section 4 we have
> studied such a subtyping algorithm; here we see that the algorithm
> and the rules match each other perfectly, by relating them both to
> trees.

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
