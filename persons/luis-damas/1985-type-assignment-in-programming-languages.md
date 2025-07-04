---
title: type assignment in programming languages
author: luis damas
year: 1985
---

# My Motive

[2025-06-27] 一边练习实现 Hindley-Milner 类型系统，一边读这篇论文。

是否真的要实现一个练习用的语言，
还是直接实现实用的 x-lisp，
取决于读这篇论文的感受。

# My Notes

## domain theory 是否实用？

Milner 之所以推崇 domain theory，是想用永恒的方式写程序。
比如先定义好 value 的 domain，然后在 domain theory 中写解释器，
把表达式映射到 value。

之所以 Milner 认为这种方式是永恒的，
是因为所构造的 value domain 是数学对象。

但是在我看来这无疑是强迫自己以不能运行也不能测试的方式写程序。

如果对于理论的实用性也做一个排序，
相比用 lisp 写解释器，这种处理方式无疑是实用性很低的。

除非我们直接把 domain 理解为，
用来实现解释器的程序语言中，
具体的数据类型。

Domain theory 也确实可以为我所用，
因为首先可以把 domain 扩展为 complete lattice，
然后用来作为 propagator model 的理论基础。

给一般的集合添加 bottom 而形成的 flat domain，
其中序关系是非常具体的，
`x ⊑ y` 只有两种可能 `x = y` 或者 `x = ⊥ ⊑ y`。

注意，function domain 的序关系是逐点（pointwise）定义的，
看起来更复杂，但是逐点定义的有限情况就像是 record，所以也不复杂。

# Abstract

> The purpose of this work is to present and study a family of
> polymorphic type disciplines for programming languages similar to
> the type discipline of ML, the metalanguage of the LCF system, which
> are based on the use of type inference systems to define the notion
> of well typed expressions and programs and on the use of type
> assignment algorithms to compute the type or types that can be
> inferred for those same expressions or programs.

> Previous work on the theoretical foundations of the ML type
> discipline is reexamined and completed here. It is also extended in
> two directions, namely to handle overloading of identifiers and also
> to cope with a semantics involving references to a store as first
> class objects.

> For each of the theories studied here we present proofs of the
> semantic soundness of type inference, i.e. that well typed
> expressions evaluate to objects of the correct type and that in
> particular they do not lead to run-time errors like trying to add an
> integer to a list.

> Algorithms for computing the type or types which can be inferred for
> expressions are also presented together with proofs of the soundness
> and completeness of the algorithms, i.e. that the algorithms compute
> exactly the types which can be actually inferred for the
> expressions.

# Introduction

批评 algol68 没有 polymorphism，例如：

```algol68
struct IntList = (Int hd, Ref IntList tl);
struct RealList = (Real hd, Ref RealList tl);
```

> This restrictions may explain, in part, why those languages had a
> very small impact on areas such as Artificial Intelligence where
> programs usually involve a relatively large number of different, but
> in many cases similar, data types which tend to change often during
> program development.

介绍把类型作为函数 explicit 参数的解决方案。
这很矛盾，因为在大多数情况下，在运行时类型参数都是用不到的。

因此需要 polymorphic type。

```scheme
(the (nu (A) (-> A A)) (lambda (x) x))
```

```scheme
(claim compose
  (nu (A B C)
    (-> (-> A B) (-> C A)
        (-> C B))))

(define (compose f g)
  (lambda (x) (f (g x))))
```

ML 可以完全省略类型声明，因此：

> Thus types play only a passive role in the semantics of the language
> in the sense that they are used, as a filter, to restrict the set of
> acceptable programs to those for which a type can be derived.

这个符合 Curry 风格的类型系统。
也符合集合论的直觉。

下面介绍了几个 ML 代码的例子，
也介绍了在 ML 中定义新类型的方式，
即定义 type constructor 的同时，
定义相关的 data constructor 和 data eliminator。

并且强调，定义新类型的功能，
并不影响把类型当作 filter 的直觉：

> Now, even the introduction of abstract data types does not change
> much the point of view mentioned above of ML being seen as an
> untyped language in which type assignment is used as a filter.

如果用 structural type，
而不是定义新的 data constructor，
filter 的直觉就会更明显，不需要额外解释。

> Having acquainted those readers unfamiliar with ML, with the main
> features of its type discipline we should also point out that ML,
> even if it provides assignable variables, is essentially a purely
> applicative language, and its lack of updatable structures like,
> e.g. arrays, does limit its acceptability as a general purpose
> programming language.

作为纯函数语言，
不带有 updatable structures 才是正确的，
而不是不允许 assignable variables。

此时 assignment 应该处理为 shadowing，
这样可以避免人们用 closure 和真的 assignment
实现 updatable structures。

> The theoretical basis for the ML type discipline was introduced in
> [Milner 78] where a type asslgnnient algorithm was defined and type
> assignment was shown to be semantically sound.

> The main aim of this work is to complete Miler's work and to extend
> it in two directions.
>
> - On one hand we will present here the proofs of the results
>   anounced in [Damas & Milner 82] stating the existence of principal
>   type schemes and the completeness of the ML type assignment
>   algorithm.
>
> - On the other hand we will extend that theory to handle overloading
>   of identifiers and to a semantics including references to an
>   updatable store as first class objects.

> We will now present the criteria that, in our opinion, a theory of
> type assignment for a programming language should satisfy if it is
> to be of any practical use.

> First of all it is desirable from a pragmatic point of view that the
> type discipline should be stated in some simple form. In accordance
> with previous works on type assignment we will use inference systems
> to specify what expressions are well typed and what types can be
> inferred for them. For an algebraic approach to type assignment see
> [Shultis 82].

[Shultis 82] "Type Checking in Exp: An Algebraic Approach".

> Secondly the type system should be semantically sound in the sense
> that if a type can be inferred for an expression than the result of
> evaluating the expression should be of that type. An example of an
> application where such strict view about soundness of the type
> discipline is essential, is provided by the LCF system itself where
> one of the primitive types is type "theorem" and one wants to ensure
> that objects of that type can only be produced using primitive
> functions modelling axioms and inference rules.

注意，想要论述 semantic soundness，
就要区分 expression 与 type 的关系，
和 value 与 type 的关系。

- infer: expression -> type
- in: value, type -> bool

当提到 semantic 的时候，作者指的是指称语义，
或者也可以理解为运行时（运行时的对象是 value 而不是 expression）。

这种理解方式对于单纯做 infer 的类型系统可能合适，
但是对于有时需要 type annotation 和 check 的类型系统可能不合适：

- check: expression, type -> bool

> Thirdly, for any practical purpose, it is essential to have some
> form of algorithm to infer types for expressions. Moreover the type
> inferred by the algorithm should be as general as any other type
> which could be inferred for the expression because otherwise it
> could fail to come up with a type which the user was entitled to
> expect from the inference rules. This also means that the algorithm
> should only fail if no type could be inferred for the expression.

infer 的结果必须是 principal type-scheme。

> Finally the type discipline achieved should be powerful enough to
> overcome the limitations of strict type disciplines exemplified at
> the very beginning of this introduction.

需要 polymorphic type。

下面介绍需要给 lambda calculus 的语法增加 let 表达式，
才能实现这里的 infer。

这应该就是 bidirectional type checking 思想的开端。
但是还没有完全形成理论。

也可以说不是引入新的 let 表达式，而是说，
在类型推导时，必须把 `((lambda (x) e*) e)` 看成是一个整体。

可能作者对于「infer 成功与否，受到 bate-reduction 影响」，
这个事实感觉不是很好，所以说要介绍 let-reduction。
以保持「infer 成功与否」在 reduction 下的不变性。

> We can now outline the remainder of this work in which we present
> three theories which meet the criteria mentioned above.

> In chapter I we study an inference system for inferring types for
> expressions. This system overcomes the limitation of at most one
> assumption for each variable of the system presented in [Damas &
> Milner 82] while preserving the existence of principal types and of
> a type assignment algorithm. From a practical point of view the
> importance of the results of that chapter is that they provide a
> basis for handling overloading of identifiers such as arithmetic
> operators or the equality operator `=`.

取消 "at most one assumption for each variable" 这个限制，
可能等同于引入 union type。

> In chapter II we study the type scheme inference system of [Damas &
> Milner 82] and, besides presenting the proofs of the results stated
> in that work, we also study its relation with the inference system
> of chapter I.

> In chapter III we extend the theory of chapter II to the case where
> the language semantics is no longer purely applicative but includes
> references to a store as first class objects. We will also present
> some programming examples showing how familiar data structures like
> arrays and records with updatable fields can be adequately handled
> with this extension to the ML type discipline.

# 1 A type inference system for an applicative language

## 1.1 Introduction

> The type inference system is essentially that of basic functionality
> theory in combinatory logic and λ-calculus [Curry & Feys 58].

Curry 已经开始做 type inference 了吗？

## 1.2 Expressions

> The syntax of expressions is described by the following ambiguous
> BNF grammar:

```bnf
e ::= x | e e' | λx.e | let x = e in e'
```

用 lisp 语法：

```bnf
<exp> ::= <var>
        | (<exp> <exp>)
        | (lambda (<var>) <exp>)
        | (let ((<var> <exp>)) <exp>)
```

与经典的 lambda calculus 相比，只是增加了 `let` 表达式。

注意，在用 explicit substitution 实现 lambda calculus 时，
substitution 必须被明显地（explicitly）表示在语法中，
而表达 substitution 的语法正好与 scheme 的 `let` 一致（而不是 `let*`）。

也许上面不应该用 scheme 的一般的多元素 `let`，而应该用：

```bnf
<exp> ::= (let-one (<var> <exp>) <exp>)
```

因为 `let-one` 的嵌套所形成的是 `let*`。

如果是为了减少括号，甚至可以写成：

```bnf
<exp> ::= (let-one <var> <exp> <exp>)
```

带有多个 `<exp>` 的 body 也是支持的：

```bnf
<exp> ::= (let-one <var> <exp> <exp> ...)
```

## 1.3 Types

> Assuming we are given a set `Tv` of type variables `α` and α set
> `Pt` of primitive types `ι`, the syntax of types is given by

```bnf
τ ::= α | ι | τ → τ'
```

用 lisp 语法：

```bnf
<type> ::= <type-var> | <primitive-type> | (-> <type> <type>)
```

> We will denote the set of all the types by `Ty`.

也就是说这里的语言与第二章的差别在于此，
暂时没有引入 `<type-scheme>`
-- 其中有 bound type variables。

> A _substitution of types for type variables_ `S` is a map
> from type variables to types.

也许可以用下面的 lisp 语法来表示 substitution：

```scheme
(make-subst
  (<type-var> <type>)
  ...)
```

```scheme
(define-type subst-t (-> type-var-t type-t))
```

> A substitution `S` extends naturally to a map from types to types by
> replacing (simultaneously) each occurrence of a variable `α` in a
> type with `S α` (the same can be done when instead of types we have
> any larger syntactic class involving types).

这里 larger syntactic class 最重要的例子就是 type context。

这里 substitution 可以用函数作用语法，
作用于类型以表达 "substitute" 的现象，
又很像是在遵循 combinatory logic 的宗旨，
即，尽量 overload 函数作用语法，
把所有的 object 都看成是函数。

但是，上面这种说法又有问题，因为在 combinatory logic 中，
应该只有语义层次的函数，而不能有修改语法的函数，
比如上面的 substitution 就是修改语法的函数，
又比如不能实现 expression 的 check 函数，
因为要模式匹配所输入的语法结构。

TODO 上面这种对 combinatory logic 的理解正确吗？
确实不能对 expression 做模式匹配吗？

> The _composition_ of two substitutions R and S is defined by
>
>     (S R) α = S (R α)

但是提到 substitution 的复合时，
又不遵循 combinatory logic 的宗旨了，
用 substitution 之间的 concatenation
表示了 composition 而不是 application。
可能作者只是为了尽量简化语法，
而没有在考虑 combinatory logic 的传统。

定义了一些关于 substitution 的函数：

```scheme
(claim subst-domain (-> subst-t (set-t type-var-t)))
(claim subst-range  (-> subst-t (set-t type-var-t)))
```

其中：

- domain 出现在 subst 的 key 中，但是不被映射到自身的类型变量。
- range 出现在 subst 的 value 中（这里没有显示不是来自到自身的映射）。

感觉如果实现了 x-lisp，
上面些对于 formal system 的描述，
便可以完全形式化（可测试，可运行）。

```scheme
(claim subst-involved (-> subst-t (set-t type-var-t)))
(define (subst-involved subst)
  (set-union (subst-domain subst) (subst-range subst)))
```

```scheme
(claim subst-restriction (-> subst-t (set-t type-var-t) subst-t))
(define (subst-restriction subst vars)
  (lambda (v)
     (if (set-has? vars v)
       (subst v)
       v)))
```

定义 substitution 的 simultaneous composition，
条件是 domain 相交的地方，映射的值相等。

我称为 `subst-merge`：

```scheme
(claim subst-merge (-> subst-t subst-t subst-t))
```

对于两个 substitution `S` 和 `R`，和类型 `T`，
如果 `(S T) = (R T)`，那么：

```scheme
(let ((A (free-type-vars T)))
  (assert-subst-equal
   (subst-restriction S A)
   (subst-restriction R A)))
```

特别的，如果 `(S T) = T*`，
可以求出 minimal substitution `S*`，
满足 `(S* T) = T*`：

```scheme
(define S* (subst-restriction S (free-type-vars T)))
```

> The above discussion about minimal substitutions still applies when
> instead of just one equation we have any finite number of equations
> where a substitution `S` occurs applied to types or other larger
> syntactic structures involving types.

这里的 minimal substitution，
应该就是求 principal type-scheme 的技巧。

> Finally a type `t'` is said to be an _instance_ of a type `t` iff
> there is a substitution `S` such that `t' = S t`.

`<type>` 在 substitution 的诱导下，形成序关系。

如何理解这里序关系的方向？
由于是在讨论类型，而不是 value，
因此 substitution instance 是更具体的类型，因此应该「更小」。

> If `t` and `t'` are instances of each other then we will say that
> `t'` is a _trivial variant_ of `t`.

## 1.4 Semantics

> Since our semantic domains will be complete partial orders we will
> recall briefly the definitions of complete partial order and of
> other related notions.

沿用 Scott 和 Milner 的指称语义。

> A _complete partial order (cpo)_ `D` is a partial order with a least
> or _bottom_ element and such that every ascending ω-chain in `D` has a
> least upper bound (lub) [in `D`].

> A map between cpos is _continuous_ iff it is monotonic and also
> preserves lubs of ω-chain.

cpo 的定义很乱。
这种也称为 ω-complete partial order (ω-cpo)，
其中 ω-chain 就是形如 {x1 ≤ x2 ≤ x3 ≤ ...} 的序列。

为的是保证 `f` 的迭代所构成的序列有极限存在。
要求 bottom 存在就是让 bottom 作为 `f` 第一次迭代的参数。

定义如何给集合的 discrete order 添加 bottom 以形成 cpo。

定义 cpo 之间的 _coalesced sum_，
即 disjoint union 外加把所有的 bottom 等同。

重复 Milner 论文中所定义的一些函数。

> Starting with a given domain `B` of _basic values_ we define the
> domains of _values_ `V`, of _functions_ `F` and of the _error value_
> `W`, by the following domain equations

```
V = B0 + B1 + ... + F + W   (disjoint sum)
F = V → V                   (function space)
W = {·}                     (error element)
```

下面就是要写解释器。

## 1.5 Type inference
## 1.6 A type assignment algorithm
## 1.7 Principal types and completeness of T
## 1.8 Type schemes, assumption schemes and type inference
## 1.9 Type assignment and overloading

# 2 A type scheme inference system

## 2.1 Introduction
## 2.2 Preliminaries
## 2.3 Type inference
## 2.4 The type assignment algorithm W
## 2.5 The completeness of W and principal type schemes
## 2.6 Comparison with the inference system of chapter I

# 3 References to a store and type inference

# 3.1 Introduction
# 3.2 The language and its semantics
# 3.3 Types, type schemes and their semantics
# 3.4 Type inference
# 3.5 A type assignment algorithm
# 3.6 Weak polymorphism and programming examples

# 4 Conclusions and directions for further research
