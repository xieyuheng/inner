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

批评 algol68 没有 polymorphism，例如。

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
(claim compute
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

TODO

## 1.2 Expressions
## 1.3 Types
## 1.4 Semantics
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
