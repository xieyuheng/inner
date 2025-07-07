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

[2025-07-04]

Milner 之所以推崇 domain theory，是想用永恒的方式写程序。
比如先定义好 value 的 domain，然后在 domain theory 中写解释器，
把表达式映射到 value。

之所以 Milner 认为这种方式是永恒的，
是因为所构造的 value domain 是数学对象。

但是在我看来这无疑是强迫自己以不能运行也不能测试的方式写程序。

如果对于理论的实用性也做一个排序，
相比用 lisp 写解释器，这种处理方式无疑是实用性很低的。

Domain theory 也确实可以为我所用，
因为首先可以把 domain 扩展为 complete lattice，
然后用来作为 propagator model 的理论基础。

给一般的集合添加 bottom 而形成的 flat domain，
其中序关系是非常具体的，
`x ⊑ y` 只有两种可能 `x = y` 或者 `x = ⊥ ⊑ y`。

注意，function domain 的序关系是逐点（pointwise）定义的，
看起来更复杂，但是逐点定义的有限情况就像是 record，所以也不复杂。

在今后，我会直接把 domain 理解为，具体程序语言中的具体数据类型。
并且把 denotational semantics 理解为，用具体的语言去写编译器和解释器。

尽管如此，我还是认为 Milner 和 Scott
想要把程序表示为数学对象的行为是本质上错误的，
因为正如 Sussman 所说，
计算机科学给我们提供了一种语言，
使得我们可以记录过程性的知识。
这与数学的研究对象有着本质上的不同。

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

```scheme
(claim in? (-> value-t type-t bool-t))
(define (in? v D)
  (cond ((element-of? v D) true)
        ((equal? v (undefined value-t))
         (undefined bool-t))
        (else false)))
```

```scheme
(claim the (pi ((D type-t) (v value-t)) D))
(define (the D v) (if (in? v D) v (undefined D)))
```

> Starting with a given domain `B` of _basic values_ we define the
> domains of _values_ `V`, of _functions_ `F` and of the _error value_
> `W`, by the following domain equations

```
V = B + F + W   (disjoint sum)
F = V → V       (function space)
W = {·}         (error element)
```

```scheme
(claim wrong wrong-t)

(claim true bool-t)
(claim false bool-t)

(define-type value-t
  (union bool-t
         (-> value-t value-t)
         wrong-t))
```

下面就是要写解释器。
与 Milner 的解释器的差异在于，
这里对于参数为 wrong 的情况，
和 let 的 rhs 为 wrong 的情况，
没有提前返回 wrong。

```scheme
(define-type env-t (-> var-t value-t))
(define-type function-t (-> value-t value-t))

(claim evaluate (-> exp-t env-t value-t))
(define (evaluate exp env)
  (match exp
    ((the var-t x)
     (env x))
    ([(the exp-t e1) e2]
     (let ((target (evaluate e1 env))
           (arg (evaluate e2 env)))
       (if (in? target function-t) (target arg) wrong)))
    (`(lambda (,x) ,e)
     ;; use the lambda of meta-language instead of closure.
     (lambda (v) (evaluate e (env-cons env x v))))
    (`(let ((,x ,e1)) ,e2)
     (evaluate e2 (env-cons env x (evaluate e1 env))))))
```

> The semantics above is an extension of the formal semantics of the
> λ-calculus defined in [Stoy 77]. Since one obviously has

```scheme
(evaluate `(let ((,x ,e1)) ,e2)) =
(evaluate `((lambda (,x) e2) ,e1))
```

> it follows that the results of [Stoy 77] stating that the value
> denoted by a A-term is not altered by any of the conversion rules,
> still holds for expressions and for let-conversion.

这里提到了 [Stoy 77]，
damas 可能是看 stoy 的 1977-denotational-semantics
学习的 domain theory。

> Comparing our semantics with the one defined by [Milner 78] for a
> similar language we see that the latter is more strict in its
> treatment of non-termination and of error values and more near
> actual implementations of applicative languages. We will refer to it
> as the _strict_ semantics for expressions.

Milner 对 application 和 let 的 evaluate 定义不同：

```scheme
(define (evaluate exp env)
  (match exp
    ...
    ([(the exp-t e1) e2]
     (let ((target (evaluate e1 env))
           (arg (evaluate e2 env)))
       (if (in? target function-t)
         ;; be strict about arg be wrong.
         (if (in? arg wrong-t) wrong (target arg))
         wrong)))
    ...
    (`(let ((,x ,e1)) ,e2)
     (let ((v1 (evaluate e1 env)))
       ;; be strict about rhs be wrong.
       (if (in? v1 wrong-t)
         wrong-t
         (evaluate e2 (env-cons env x v1)))))))
```

下面将要用 domain `V` 的 ideal 来把类型映射为数学对象。
domain 的 ideal 的集合，构成 "topology via logic" 中的 frame。
注意，这里是利用所有 value 的类型 `value-t`，
把整个类型系统理解为了一个 frame，
而不是把一个类型理解为一个 frame。

注意，这里对函数类型的 ideal 的定义方式。

给类型语义：

```scheme
(define-type type-valuation-t (-> type-var-t (ideal-t value-t)))
(claim evaluate-type (-> type-t type-valuation-t (ideal-t value-t)))

(claim evaluate-type
  (-> type-t
      (-> type-var-t (ideal-t value-t))
      (ideal-t value-t)))
```

`type-valuation-t` 是语法元素 `ctx-t` 的语义。

想要在具体的 meta-language 中实现 `evaluate-type`，
就必须能实现 `ideal-t`，只要考虑 `(ideal-t V)`
作为 frame 的 presentation 就可以，
因为 presentation 可以被实现为 inductive datatype。

TODO 补充这里的语义定义。

下面主要是想用数学语言（一阶逻辑和集合论），
定义语法元素上的关系 `A |= e: τ`：

```scheme
(claim check (-> ctx-t exp-t type-t judgement-t))
```

这个关系要定义两次 `|=` 代表利用 model 的定义，
`|-` 代表利用 inference rule 的定义。

## 1.5 Type inference

我就不重复用 lisp 写推演规则了。
注意这里的 `LET` 规则处理了 overloading。

也可能不是 overloading，
而是 `let` 的 variable
在 body 中的多次出现可能有不同的类型。

但是感觉这种情况不应该在这个 rule 以这种方式处理。
在下一章使用 type scheme 应该可以避免这种方式。

> **Theorem 1** (Semantic soundness of type inference).
> For any expression `e` type `τ` and assumptions `A` if
>
>     A |- e: τ
>
> holds then
>
>     A |= e: τ
>
> also holds.

## 1.6 A type assignment algorithm

令人惊讶的是，`infer` 不是把 ctx 当作参数，
而是把 ctx 当作返回值的一部分。

```scheme
(define-type ctx-t (list-t (tau var-t type-t)))
(define-type subst-t (list-t (tau type-var-t type-t)))
(claim subst-on-type (-> subst-t type-t type-t))
(claim subst-on-ctx (-> subst-t ctx-t ctx-t))

(claim infer (-> exp-t (tau ctx-t type-t)))
(define (infer exp)
  (match exp
    ((the var-t v)
     (= var-type (type-var-gen))
     [[v var-type] var-type])
    ([(the exp-t e1) e2]
     (= [ctx1 target-type] (infer e1))
     (= [ctx2 arg-type] (infer e2))
     (= ret-type (type-var-gen))
     (= subst (unify target-type ['-> arg-type ret-type]))
     [(subst-on-ctx subst (ctx-merge ctx1 ctx2))
      (subst-on-type subst ret-type)])
    (`(lambda (,v) ,e)
     (= [ctx body-type] (infer e))
     (cond ((not (ctx-has? ctx v))
            [ctx [-> (type-var-gen) body-type]])
           ((ctx-has-one? ctx v)
            [(ctx-delete ctx v)
             [-> (ctx-get ctx v) body-type]])
           ;; the following case actually covers the above case.
           ((ctx-has-many? ctx v)
            (= (subst (unify-many (ctx-get-many ctx v))))
            [(subst-on-ctx (ctx-delete-many ctx v))
             (subst-on-type [-> (ctx-get ctx v) body-type])])))
    (`(let ((,v ,e1)) ,e2)
     (= [ctx2 body-type] (infer e2))
     (if (not (ctx-has? ctx2 v))
       [ctx2 body-type]
       (begin
         (= [ctx1 rhs-type] (infer e1))
         (= found-types (ctx-get-many ctx2 v))
         (claim refresh (-> (tau ctx-t type-t) (tau ctx-t type-t)))
         (= fresh-results
            (repeat (length found-types)
                    (lambda () (refresh [ctx1 rhs-type]))))
         (= fresh-rhs-types (map fresh-results second))
         (= fresh-rhs-ctxs (map fresh-results first))
         (= subst (unify-list found-types fresh-rhs-types))
         [(subst-on-ctx (ctx-merge-many
                         (cons (ctx-delete-many ctx2 v)
                               fresh-rhs-ctxs)))
          (subst-on-type body-type)])))))
```

> A comparison of our algorithm with the Curry-Hindley one for the
> lambda calculus [yelles 79] reveals that, as far as lambda terms are
> concerned, the second returns a single assumption for each free
> variable in the term while ours returns a different assumption for
> each occurrence of a free variable in the term. It is also not
> difficult to realise that, for an expression, `T` [`infer`] returns
> an assumption for each occurrence of a free variable in the let-free
> form of the expression.

[Yelles 79] "Type Assignment in the Lambda-Calculus: Syntax and Semantics.",
C.B. Yelles, PhD thesis, University of Wales, 1979.

> **Proposition 7** (Soundness of T).
>
> If `T(e)` succeeds with `(A, τ)` then there is a derivation of
>
>     A |- e: τ

## 1.7 Principal types and completeness of T

> **Proposition 8** (Completeness of T).
>
> Given an expression `e` if there is a type `ν` and a set of
> assumptions `B` such that
>
>     B |- e: ν
>
> holds, then
>
> (1) `T(e)` succeeds;
>
> (2) If `T(e) = (A, τ)` then there is a subset `B'` of `B` such that
> `B' |- e: ν` is an instance of `A |- e: τ`.

## 1.8 Type schemes, assumption schemes and type inference

> This section is concerned with providing a description of the set of
> types which can be inferred for an expression from a given set of
> assumptions. In fact we shall see that it is possible to define an
> algorithm which achieves that purpose for any expression and for a
> large class of sets of assumptions which can be described in a
> finite way.

下面就要引入带有局部类型变元的 polymorphic 类型了，
但是这里所描述的动机很有趣，
是说如果 `B |- e: τ`，那么对于那些在 `τ` 中出现，
但是不在 `B` 中出现的类型变量，
带入任何类型得到新的 `τ'`，
`B |- e: τ'` 依然成立。

> **Definition.** A _type scheme_ `η` is either a type `τ` or a term
> of the form
>
>     ∀ a1 ... an τ
>
> where τ is a type and `a1`, ..., `an` are type variables which will
> be called the _generic variables_ of `η`.

总结一下：

- monotype -- 不允许类型变量。
- polytype -- 允许出现全局类型变量。
- type scheme -- 允许出现全局类型变量和局部类型变量。

`[τ1, ..., τ2 /a1, ..., an] τ` 是 `∀ a1 ... an τ` 的 generic instance。

这里定义了一个 type 和 type scheme 之间的 instance 关系，论文中记录为 `ν < η`。
因此一个 type scheme 对应一个 type 的集合，
这个集合之间的 inclusion 关系又可以用来定义 type scheme 之间的序关系。

> Given a type scheme `η` we will let a substitution `S` act on `η`
> by acting on the free variables of `η` while renaming, if
> necessary, the generic variables of `η` to avoid clashes with
> variables involved in `S`.

## 1.9 Type assignment and overloading

> By overloading an identifier we will mean using that identifier to
> denote more than one value.

> The way the ambiguities that arise from overloading are handled is
> by using typechecking considerations to decide which value is
> intended by each occurrence of an overloaded identifier. This
> implies that each of the values represented by an overloaded
> identifier has type(s) distinct from those of the others. The
> process of identification just referred can be regarded as a
> transformation which produces a copy of the original program but
> with overloaded identifiers replaced by normal ones, e.g., each
> occurrence of `+` would be replaced by either `+int`. or `+real`.

这种方式不能用来实现 generic dispatching。
但是能够给如何实现带有类型的 generic dispatching 以启发？

TODO 实现了基础的 Hindley-Milner type system 之后再回来看这个功能。

# 2 A type scheme inference system

## 2.1 Introduction

> The theory supporting the polymorphic type discipline of ML, the
> metalanguage of the LCF system [Gordon et al 79], was studied in
> [Milner 78].

> Here we reformulate that theory by using quantifiers to make the
> _generic type variabLes_ of [Milner 78] explicit. This leads to a
> set of rules for inferring type schemes for expressions which is in
> contrast with chapter I where the inference system only dealt with
> types although type schemes were used to describe the set of types
> that could be derived for an expression.

> The main advantage of such a reformulation is that it enables us to
> prove both the completeness, conjectured in [Milner 78], of the type
> assignment algorithm W and the existence of principal type schemes
> of an expression under particular sets of assumptions. Those
> constitute the main results of this chapter. Finally we will also
> study the relation between the inference system of chapter I and the
> one defined here.

为什么不直接介绍这个版本的更好的类型系统？
目前所形成的唯一效果就是推迟引入 type scheme。
type scheme 并不是什么复杂的概念，
比 lambda abstraction 还简单，
没必要推迟引入。

## 2.2 Preliminaries

增加 type scheme：

```bnf
<type> ::= <type-var> | <primitive-type> | (-> <type> <type>)
<type-scheme> ::= <type> | (nu (<type-var>) <type-scheme>)
```

这里还提了一下 type-scheme 的 denotational semantics，
但是用到了 ideal 的无限 meet。
可以这么用吗？

## 2.3 Type inference

> From now on, and in contrast with chapter I, we shall assume that
> `A` contains at most one assumption about each identifier `x`.

由于增加 type scheme，
导致与第一章相比，增加了 rule INST 和 GEN，
并且 rule LET 变了，
不是给约束变元的每次出现都生成一个 type 保存在 ctx 中，
而是只把一个 type scheme 保存在 ctx 中。

需要的时候，可以通过 INST rule
把具体的 type 从 type scheme 通过代换找回来。

> Given a set of variables `V` and a type `τ` we define the _closure_
> of `τ` under `V` -- `(type-closure V τ)`, as being the type scheme
> `(nu (A1 A2 ...) τ)` there `(A1 A2 ...)` are all the type variables
> occurring in `τ` which are not in `V`.

> For any assumptions `A` we will use `(type-closure A τ)` to denote
> the closure of `τ` under the set of type variables which occur in
> `A`.

## 2.4 The type assignment algorithm W

与第一章不同的是，
这里 `infer` 带上参数 `ctx` 了，
返回值中的 `ctx` 也变成了 `subst`。

为了迎合 denotational semantics，
我把 `ctx` 当成 `infer` 的最后一个（第二个）参数，
这样 `infer` 就把 `exp-t` 映射成 `(-> ctx-t (tau subst-t type-t))`。

```scheme
(define-type ctx-t (list-t (tau var-t type-scheme-t)))
(define-type subst-t (list-t (tau type-var-t type-t)))
(claim subst-compose (-> (list-t subst-t) subst-t))
(claim subst-on-type (-> subst-t type-t type-t))
(claim subst-on-type-scheme (-> subst-t type-scheme-t type-scheme-t))
(claim subst-on-ctx (-> subst-t ctx-t ctx-t))

(claim infer (-> exp-t ctx-t (tau subst-t type-t)))
(define (infer exp ctx)
  (match exp
    ((the var-t v)
     [[] (type-scheme-gen (ctx-get ctx v))])
    ([(the exp-t e1) e2]
     (= [S1 target-type] (infer e1 ctx))
     (= [S2 arg-type] (infer e2 (subst-on-ctx S1 ctx)))
     (= ret-type (type-var-gen))
     (= U (unify (subst-on-type S2 target-type) ['-> arg-type ret-type]))
     [(subst-compose [U S2 S1]) (subst-on-type U ret-type)])
    (`(lambda (,v) ,e)
     (= arg-type (type-var-gen))
     (= [S ret-type] (infer e (ctx-update ctx v arg-type)))
     [S ['-> arg-type ret-type]])
    (`(let ((,v ,e1)) ,e2)
     (= [S1 rhs-type] (infer e1 ctx))
     (claim type-closure (-> ctx-t type-t type-scheme-t))
     (= rhs-type-scheme (type-closure (subst-on-ctx S1 ctx) rhs-type))
     (= body-ctx (subst-on-ctx S1 (ctx-update ctx v rhs-type-scheme)))
     (= [S2 body-type] (infer e2 body-ctx))
     [(subst-compose [S2 S1]) body-type])))
```

TODO 对比类型检查推导函数，与类型 inference rules，
看看能否从 inference rules 看出来算法应该如何实现。
bidirectional type checker 是可以做到这一点的。

> **Theorem 2** (Soundness of W). If `W(A, e)` succeeds with `(S, τ)`
> then there is a derivation of `(S A) |- e: τ`.

> Finally we note that since `(S A) |- e: τ` holds then
> `(S A) |- e: (type-closure (S A) τ)` also holds.
> We will refer to `(type-closure (S A) τ)`
> as the type scheme computed by W for `e`.

## 2.5 The completeness of W and principal type schemes

> In this section we prove the completeness of the type assignment
> algorithm W and use this result to prove the existence of principal
> type schemes.

## 2.6 Comparison with the inference system of chapter I

> We will close this chapter with a comparison of the inference system
> defined in this chapter with the one defined in chapter I.

> Proposition 2. ...

> It follows from the above result that the set of types that can be
> inferred for an expression, from a set of assumption schemes
> involving at most one assumption about each identifier, is the same
> for both inference systems.

> On the other hand the type discipline of chapter I can cope with
> sets of assumption involving more then one assumption scheme about
> each identifier and thus, that type discipline can be regarded as a
> proper extension of the one studied in the present chapter.

反而是第一章的类型系统更一般？

# 3 References to a store and type inference

# 3.1 Introduction

> In this chapter we extend the methods and results of chapter II to
> the case where the language semantics is no longer purely
> applicative but includes references to an updatable store as first
> class objects.

这里对 `ref` 的讨论让人想起 EOPL，
EOPL 中的 topics，应该是受到 Milner 的工作的启发的。

TODO

# 3.2 The language and its semantics
# 3.3 Types, type schemes and their semantics
# 3.4 Type inference
# 3.5 A type assignment algorithm
# 3.6 Weak polymorphism and programming examples

# 4 Conclusions and directions for further research

> The main objective of this dissertation as set out in the
> intoduction was to complete and extend the work of Robin Milner on
> polymorphic type assignment in programming. Here we examine the
> extent to which that objective was attained and also call the
> readers attention to some aspects which we think deserve a deeper
> study.

> The type inference system studied in chapter 1 can be seen as an
> extension of the system of Robin Milner to handle overloading. Apart
> from that it is interesting in itself because it provides a
> conceptually much more simple framework in which the theory of type
> assignment can be studied.

我一直认为带有局部类型变量的 type scheme 反而是更简单的。

> One of the interesting points made in that chapter is not only that
> typechecking can be supported by type assignment theory but also
> that derivation trees and hence type assignment itself can be used
> to formulate and justify the program transformations which need to
> be done by compilers to support overloading.

> We believe this use of derivation trees can be also made in relation
> with other possible extensions to the language such as those
> involving run-time type manipulation.

run-time type manipulation 正是我需要的。

> We should also point out that we have only presented a partial
> solution to the problem of handling user defined overloading and we
> think the way to overcome the limitation imposed is to design an
> algorithm that, like the one of chapter II, will go directly from a
> set of assumption schemes to the set of type schemes that can be
> inferred for the expression.

作者对 chapter 1 的某些处理也不甚满意。

> The reformulation of Milner's system presented in Chapter II
> provides a better insight on its features and in particular allowed
> us to give an affirmative answer to the questions left open in his
> work concerning the existence of principal types and the
> completeness of the type assignment algorithm.

作为 reformulation，主要包括引入 type context，
来代替 Milner 使用的 prefix 概念。

这里强调本论文的两个贡献：

- the existence of principal types.
- the completeness of the type assignment algorithm.

> Chapter III shows that type assignment and type polymorphism need
> not be confined to purely applicative languages. From a practical
> point of view it also provides a type discipline which is
> sufficiently powerful to handle a large class of programs involving
> references as can be judged by the examples provided there.

> Nevertheless we should point out that many of the features of that
> system were forced on us by technical considerations such as the
> existence of a semantics for types and of a type assignment
> algorithm. This may explain why we were unable to do for this system
> what chapter I does for Milner's system. One of the consequences of
> this fact is that the question of how to handle references in the
> presence of overloading is still left open, at least from a
> theoretical point of view.

> On the other hand we also should point that the technical problems
> associated with the definition of a semantics for types have nothing
> to do with polymorphism and would arise even if only monotypes were
> involved, so the methods and models presented there are also
> relevant to the study of the properties of typechecking for non
> purely applicative programming languages using the techniques of
> denotational semantics.

没能直接给 polymorphism 以所指，而是用了无限 meet。
我觉的这并不是什么好事。
应该直接给出所指，并且所给出的所指，
最好能增进我们对类型的理解。

> Finally the notion of weak polymorphism and the syntactical
> mechanisms used to enforce it, combined with program transformations
> based on derivation trees, seem to be promising in handling other
> extensions to the language such as those involving run-time type
> manipulation where those mechanisms could be used to decide what
> types needed to be dynamically passed to routines.
