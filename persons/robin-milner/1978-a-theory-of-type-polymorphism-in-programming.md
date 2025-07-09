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

# My Notes

## 「问题 + 算法 + 证明」的论文结构

[2025-06-10] 这篇论文与 luca 的
1993 年论文 "subtyping recursive types"
有相似的结构，即：

- 明确程序语言设计中的问题；
- 找出算法来解决这个问题；
- 构建理论来证明算法的正确性。

## 指称语义在实践中就是用 meta-language 实现 language 的过程

[2025-07-04] 抛开不可运行的代码不谈，
仔细看这里应用 denotational semantics
和 domain theory 的方式。

就会发现，所谓 denotational semantics，
就是用 meta-language 实现 language 的过程！
其中 meta-language 是数学没错，
但是一个具体的程序语言，
也可以被理解为是数学！

这是对我而言很重要的领悟，
因为它把抽象的理论，
变成了具体的，
我非常熟悉的过程，
即「写解释器」！

这么说来，我刚刚学会写程序的时候，
从 SICP 和 the little schemer 中，
学会写 meta circular interpreter 的时候，
我就已经是 denotational semantics 的大师了。

既然这样，我又想质疑 domain theory 是否实用了。
因为 domain theory 中，
人们引入拓扑概念，模仿无穷级数的分析，
费心去证明递归函数作为迭代的极限而存在。
而我在用 meta-language 来写解释器的时候，
已经在使用递归函数了，
那么这里的递归函数为什么存在呢？
这里的递归函数之存在，
是实现 meta-language 的人构造出来的，
这种「存在」显然更具体也更实用。

如果说「去证明这里的递归函数」确实是重要的问题，
那么这里通过构造与实现所证明的递归函数存在，
对于 denotational semantics 有什么启示呢？
是不是不应该去构造 domain theory，并且引入拓扑和极限？
而是应该像实现在机器中所存在的那样，
即潜在无穷的树，是通过带有圈的有向图中的路径来生成的。
有向图是有限的，是很容易构造的。
实际实现中，递归函数都是如此。

这里也是受到了 roberto amadio 和 luca cardelli 的论文
"subtyping recursive types" 的启发。
否则想不到去用带有圈的有限有向图来生成树的。

但是为什么要证明递归函数（还有递归定义的类型等等）之存在呢？
递归函数的理论不是和图灵机以及 lambda 演算等价吗？
都可以作为可计算性的定义。
递归函数与拓扑空间外加极限相比，谁是谁的基础？
数学分析中，无穷级数的收敛问题，
显然可以转化成递归函数的收敛问题。

学习 domain theory，绕了一个大圈子，
写一个 domain theory 中的解释器之后，
却发现自己又从「数学家」回到了写「解释器实现者」这个身份，
只不过更心安理得了。

或者从 peirce 的使用主义原则来看，
所绕的这个圈子也是有用的，
用处就在于让我能看透用 denotational semantics
和 domain theory 写论文的人，所做的本质究竟是什么了。

因为按照 peirce 的原则，
以 domain theory 为 meta-language 写解释器，
与用 lisp 为 meta-language 写解释器是等价的。

这并不是说 domain theory 无用。
domain theory 有大用。
这只是说用 domain theory
去理解递归函数与 lambda 演算，用处不大。

domain theory 的大用在哪里？
就在对 propagator system 的分析当中。
首先我们应该要求 domain 是 complete lattice，
这正式 Scott 起初所要求的。
只不过后面的学者弱化了这个要求，
把 domain 定义为了 cpo。

一旦要求 complete lattice，
就能用于 propagator system。
因为 propagator system 中的每个 cell 所能保存的都是 lattice，
cell 的 merge 是 lattice 的 join，
propagator 作为 cell 之间的多方向的函数，
是就 lattice 而言的单调函数。

有了这些理论基础，
就能分析 propagator system 的长期行为，
即给定 propagator system 的初始条件，
预测其静止时（quiescence）的稳态。

这就像分析微分方程的长期行为一样，
同属于对分析动力系统长期行为的研究。

这也和编程一样，
因为编程就是在一个计算模型内，
构造初始状态，初始状态就是所写的程序，
让程序运行，就是让动力系统演化，
动力系统的最终稳态就是计算结果。

当使用 propagator model 这个计算模型来编程时，
对最终稳态的分析尤为重要，因为那就是计算结果，
因此 lattice theory（domain theory）也就十分重要，
所以我说 domain theory 有大用处。

计算模型就是动力系统。
比如 lambda 演算的 Church-Rosser theorem
就可以看作是对动力系统长期行为的分析。

# Abstract

> The aim of this work is largely a practical one. A widely employed
> style of programming, particularly in structure-processing languages
> which impose no discipline of types, entails defining procedures
> which work well on objects of a wide variety.  We present a formal
> type discipline for such polymorphic procedures in the context of a
> simple programming language, and a compile time type-checking
> algorithm W which enforces the discipline.  A Semantic Soundness
> Theorem (based on a formal semantics for the language) states that
> well-type programs cannot "go wrong" and a Syntactic Soundness
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
> only as "procedure" (rather than say "integer to realprocedure") --
> but the flexibility was not uniform, and not sufficient.

就像有人在 typescript 中只用 `Function` 类型，
而不具体给出参数类型和返回值类型。

> An early discussion of such flexibility can be found in Strachey
> [19], who was probably the first to call it polymorphism. In fact he
> qualified it as "parametric" polymorphism, in contrast to what he
> called "adhoc" polymorphism.  An example of the latter is the use of
> "+" to denote both integer and real addition (in fact it may be
> further extended to denote complex addition, vector addition, etc.);
> this use of an identifier at several distinct types is often now
> called "overloading," and we are not doncerned with it in this
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
> either coercions or the "overloading" of identifiers. Our view is
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

> The types in Exp are just the hierarchy of purely functional types
> over a set of basic types. That is, the polymorphism in Exp is the
> natural outgrowth of a single primitive polymorphic operator,
> function application, together with variable binding.

> To add other primitive polymorphic operators, such as pairing and
> list-processing operators (as in ML), together with types built from
> basic ones x (Cartesian Product), list (list-forming), and +
> (disjoint sum) in addition to (function type), presents no extra
> difficulty in the two soundness theorems.

> Our work is a step towards solving the problem expressed by Morris
> [10] in his thesis as follows: "to design a language and a type
> system in which a programmer may define functions whose parameters
> may have different types for different calls of the function."  We
> recommend Chapter 4 of this thesis as a lucid introduction to the
> problem.

> Although Morris does not discuss the semantics of types formally, or
> give a polymorphic type system, he describes how a valid type
> assignment may be found for a term of the lambda calculus by solving
> a set of simultaneous linear equations; we take this idea further in
> the next section.

看起来值得一读，尤其是通过解方程来做类型推导的过程：

- [10] "Lambda-Calculus Models of Programming Languages",
  J. H. Morris, Ph.D. Thesis, 1968.

> After doing this work we became aware of Hindley’s [5] method for
> deriving the "principal type scheme" (which is what we call a
> polymorphic type) for a term in combinatory logic. Hindley appears
> to have been the first to notice that the Unification Algorithm of
> Robinson [14] is appropriate to this problem. Our work can be
> regarded as an extension of Hindley’s method to programming
> languages with local declarations, and as a semantic justification
> of the method.

Hindley-Milner type system 名字的由来。

也就是说 Hindley 没有给 lambda expression 增加 let，
就能形成类型推导吗？

# 2 Illustrations of The Type Discipline

> We illustrate our notion of polymorphism by means of some simple
> examples. They are written in a fragment of ML which we hope is
> self-explanatory; this fragment is indeed no more than Landins ISWIM
> [7], and we refer the reader to Burge’s book [l] in which he uses
> this style of programming almost exactly. We use no imperative
> constructs here (assignments or jumps).

也许值得一读：

- [1] "Recursive Programming Techniques",
  W. H. BURGE, 1975.

> Example 1. Mapping a function over a list.

```scheme
(define (map f m)
  (if (null? m)
    null
    (cons (f (car m)) (map f (cdr m)))))
```

> Intuitively, the function map so declared takes a function from
> things of one sort to things of another sort, and a list of things
> of the first sort, and produces a list of things of the second
> sort. So we say that map has type

```scheme
(claim map
  (nu (A B)
    (-> (-> A B) (list-t A)
        (list-t B))))
```

> How is this type determined from the bare declaration of map?
> First, the generic types (we discuss "generic" later) of the
> identifiers occurring free in the declaration are given by

```scheme
(claim null? (nu (A) (-> (list-t A) bool-t)))
(claim null (nu (A) (list-t A)))
(claim car (nu (A) (-> (list-t A) A)))
(claim cdr (nu (A) (-> (list-t A) (list-t A))))
(claim cons (nu (A) (-> A (list-t A) (list-t A))))
```

> that is, they are polymorphic, because their types contain one or
> more type variables, and our rule is: To every occurrence of such an
> identifier is assigned a type which is a substitution instance
> (substituting types for type variables) of its generic type.

> Now each of these identifiers occurs just once in the declaration,
> so if we denote by `<id>-t` the type assigned to an identifier
> `<id>` we must have for some types T1, ..., T5.

```scheme
(define null?-t (-> (list-t T1) bool-t))
(define null-t (list-t T2))
(define car-t (-> (list-t T3) T3))
(define cdr-t (-> (list-t T4) (list-t T4)))
(define cons-t (-> T5 (list-t T5) (list-t T5)))
```

> The other identifiers (map, f, m) each occur more than once, and our
> rules demand that each occurrence is assigned the same type. The
> rules also demand that the following equations are satisfied for
> some types X1, X2, ...

```scheme
(define (map f m)
  (if (null? m)
    null
    (cons (f (car m)) (map f (cdr m)))))

(= map-t (-> f-t m-t X1))
(= null?-t (-> m-t bool-t))
(= car-t (-> m-t X2))
(= cdr-t (-> m-t X3))
(= f-t (-> X2 X4))
(= map-t (-> f-t X3 X5))
(= cons-t (-> X4 X5 X6))
(= null-t X1) (= X6 X1)
```

> The first of these conditions relates the type of a function to
> those of its formal parameters; each of the other conditions arises
> from some subterm which is a function application, except the last,
> which is because a conditional expression has the same type as its
> two arms, and because the definiens and definiendum of a declaration
> have the same type.

根据函数的定义，列类型的方程。
解方程的过程可以有专门的算法，
也可以用一般的 propagator 来完成。

> Now these equations may be solved for the variables `Xn`, `Tn`, and
> `<id>-t`; Morris [10] discusses the solution of such equations.

这么看来 Morris [10] 是必须要读的了！

我记得在读 Milner 的 pi calculus book 时，process calculus 本身，
也是受到 regular expression 相关的代数方程的启发而来的。
可见代数的重要性。

> Indeed, the situation is entirely appropriate for the use of the
> Unification Algorithm of Robinson [14]; our well-typing algorithm is
> based upon this algorithm, and (since in this case nothing more than
> unification is needed) we may conclude from Robinson’s work that the
> most general type of map is obtained, i.e., any other type `map-t`
> which satisfies the equations must be a substitution instance of the
> type obtained. In fact, the solution of the above equations is

```scheme
(define map-t
  (nu (A B)
    (-> (-> A B) (list-t A)
        (list-t B))))
```

> So this is the generic type of map, that is, to any occurrence of
> map within the scope of this declaration must be assigned some
> substitution instance of this type.

> These instances need not be the same. Suppose that `token-t` is a
> basic type (a token being a sequence of characters) and that we have
> available the identifiers (with their types)

```scheme
(claim token (list-t token-t))
(claim length (-> token-t int-t))
(claim sqroot (-> int-t real-t))
```

> Then in the expression
>
>    (map sqroot (map length token))
>
> the two occurrence of `map` will have types

```scheme
(-> (-> token-t int-t) (list-t token-t) (list-t int-t))
(-> (-> int-t real-t) (list-t int-t) (list-t real-t))
```

> Similarly, if `null?`, for example, had occurred twice in the
> definition of `map`, its types could have been different instances
> of
>
>     (nu (A) (-> (list-t A) bool-t))
>
> but our rules demand that different occurrences of a formal
> parameter (f, for example), or of an identifier (map) being
> recursively defined, shall have the same type.

> It is clear from our example that the rules of typing need to be
> carefully framed. We leave this task until the next section, but
> here we look at one more example to illustrate what happens when
> `let` or `letrec` is used locally.

> Example 2. Tagging. Suppose we want a function `tag-pair`, such that
> `(tag-pair a)` is a function under which

```scheme
[b c] => [[a b] [a c]]
```

> Of course, we can easily write

```scheme
(define (tag-pair a)
  (lambda ([b c])
    [[a b] [a c]]))
```

> Now we can explain, without setting up equations, how our well-typing algorithm
> tackles this declaration. It first assigns "unknown" types (i.e., type variables)
> A, B, and C to a, b, and c. Then [[a b] [a c]] acquires type
> (tau (tau A B) (tau A C)) and the lambda-expression acquires

```scheme
(-> (tau B C) (tau (tau A B) (tau A C)))
```

> finally `tag-pair` acquires

```scheme
(-> A (tau B C) (tau (tau A B) (tau A C)))
```

> (no type equations have placed any constraint
> upon the types of A, B, and C).

> But consider another way of defining tagpair, using the `fn-pair` function

```scheme
(claim fn-pair
  (nu (A B C D)
    (-> (-> A B) (-> C D)
        (-> (tau A C) (tau B D)))))

(define (fn-pair f g [a c])
  [(f a) (g c)])
```

> and the pairing function `pair`

```scheme
(claim pair (nu (-> A B (tau A B))))
(define (pair a b) [a b])
```

> We could write `tag-pair`

```scheme
(define (tag-pair a)
  (let ((tag (pair a)))
    (fn-pair tag tag)))
```

> We might then expect the well-typing algorithm to proceed as follows.

```scheme
(check a A)
(check (pair a) (-> D (tau A D)))
;; two occurrences of `tag` in `(fn-pair tag tag)`
(check tag (nu (B A1) (-> B (tau A1 B))))
(check tag (nu (C A2) (-> C (tau A2 C))))
;; by the type equation for function application
(check (fn-pair tag tag)
  (-> (tau B C) (tau (tau A1 B) (tau A2 C))))
(check tag-pair
  (-> A (tau B C) (tau (tau A1 B) (tau A2 C))))
```

> compare it with:

```scheme
(-> A (tau B C) (tau (tau A B) (tau A C)))
```

> something has gone wrong; the second type is too general.

正确的处理方式是：

```scheme
(check a A)
(check (pair a) (nu (D) (-> D (tau A D))))
;; two occurrences of `tag` in `(fn-pair tag tag)`
(check tag (nu (B) (-> B (tau A B))))
(check tag (nu (C) (-> C (tau A C))))
;; by the type equation for function application
(check (fn-pair tag tag)
  (-> (tau B C) (tau (tau A B) (tau A C))))
(check tag-pair
  (-> A (tau B C) (tau (tau A B) (tau A C))))
```

实际实现的时候可以通过 A 和 A1， A2 的 unification 处理。

> From the examples it becomes clear that the rules for typing
> variables bound, respectively, by `let` (or `letrec`) and by lambda
> are going to be different. Thus, although our semantics for the two
> expressions

```scheme
(let ((x e)) e')
((lambda (x) e') e)
```

> may be (and are) equivalent, it may be possible to assign types
> correctly to the former but not to the latter. An example is the
> pair

```scheme
(let ((I (lambda (x) x))) (I I))
((lambda (I) (I I)) (lambda (x) x))
```

> A (partial) intuition for this is that a lambda-abstraction may
> often occur without an argument; the second expression above
> contains a special (and rather unnecessary) use of abstraction, in
> that an explicit argument -- `(lambda (x) x)` -- is present.
> Since the `let` construct (when translated) involves this restricted
> use of an abstraction, it is not unnatural that its rule for type
> assignment is less constrained. A compiler could, of course, treat
> all explicit occurrences of `((lambda (x) e') e)` in the less
> constrained manner.

就是说先把所有的 `((lambda (x) e') e)` 都翻译为 `let`，
然后再用 `let` 的 inference rule 来做类型检查。

TODO 我认为 Milner 在这里的论述并不清楚，
想要清楚的论述为什么后者不能 infer，
应该用 bidirectional type checking 中的概念。

```scheme
(let ((I (lambda (x) x))) (I I))
(infer (lambda (x) x) (nu (A) (-> A A)))
(check I (nu (A) (-> A A)))
(infer (I I) (-> (nu (A) (-> A A)) (nu (A) (-> A A))))
```

```scheme
((lambda (I) (I I)) (lambda (x) x))
(infer (lambda (x) x) (nu (A) (-> A A)))
(infer (lambda (I) (I I)) ?)
```

```
f: F
x: X
-------
F = (-> A B)
X = A
(f x): B
--------
F = (-> X B)
(lambda (x) (f x)): (nu (X) (-> X B))
```

```
I: B
-----
B = (-> C D)
B = C
(I I): D
-----
B = (-> B D)
(lambda (I) (I I)): (nu (B) (-> B D))
```

TODO 尝试用 bidirectional type checking，
把这里的问题论述清楚。

> The treatment of types in the interaction between lambda-bindings
> (i.e., formal parameter bindings) and let bindings is really the
> core of our approach.

> It gives a consistent treatment of the nonglobal declaration of a
> procedure which may contain, as a free variable, a formal parameter
> of an enclosing procedure. This appears to be one of the more
> crucial difficulties with polymorphism, and therefore we feel
> justified in presenting our analysis in terms of a simple language
> (Exp) which excludes as much as possible that is irrelevant to the
> difficulty.

> The reader may still feel that our rules are arbitrarily chosen and
> only partly supported by intuition. We certainly do not claim that
> the rules are the only possible ones, but the results given later
> demonstrate that they are semantically justified.

> In fact, we show that a program which admits a correct type
> assignment cannot fail at run-time due to a type error -- or more
> precisely, that type constraints encoded in its semantics are always
> satisfied. It follows from this that compile-time type checking
> (i.e., the attempt to discover a correct type assignment) obviates
> the need to carry types at run-time, with an obvious gain in the
> efficiency of implementation.

> We would like to give an independent characterization of the class
> of programs which can be well typed in our system, but we have no
> idea how to do this. However, we can give some pointers. At the
> suggestion of a referee we looked at Burge [1, Chapter. 3] concerning
> general functions for processing data structures. All of the
> functions there (with the exception of Section 3.11 which we did not
> examine) acquired the expected types from the ML type checker after
> they had been modified in two respects.

Burge [1] 看来也是必须要读的了。

> First, Burge leaves implicit the coercion functions between a
> disjoint sum type and its summand types; we needed to make these
> explicit (this point was mentioned in our Introduction).

implicit coercion 是否可以理解为是 union，
而不是 disjoint union？

> Second, we used the ML abstract type construct (see Section 5 for an
> example) to formulate the recursive type definitions used by
> Burge.

就是说 Burge 在用 structural type？

> In this construct, the isomorphism between a defined abstract
> type and its representation is made explicit and must be used
> explicitly. To see the need for this requirement consider the case
> of an A stream, which is defined to be a function which yields a
> pair consisting of an A and an A stream. The type equation
>
>     (stream-t A) = (-> (tau A (stream-t A)))
>
> cannot be solved by unification (unless we allow infinite type
> expressions). But by treating the equation as an isomorphism, and
> using two functions to convert back and forth between an abstract
> type and its representation, this difficulty is removed. We claim
> that this solution is in keeping with the notion of abstract type
> (see [8], for example).

> On the negative side, there are certainly useful expressions which
> we cannot well type, though we are not clear how burdensome it is to
> do without them. An obvious example is Curry’s Y combinator.

```scheme
(define (Y f)
  ((lambda (x) (f (x x)))
   (lambda (x) (f (x x)))))
```

> since self-application is ill typed for us. But letrec avoids the
> need for Y.

如果不考虑 infer，check 显然是可以支持 Y 的：

```scheme
(claim factorial-wrap (-> (-> Nat Nat) (-> Nat Nat)))
(claim (Y factorial-wrap) (-> Nat Nat))
(claim Y (nu (A) (-> (-> A A) A)))

(define factorial-wrap
  (lambda (factorial)
    (lambda (n)
      (if (zero? n)
        one
        (mul n (factorial (sub1 n)))))))
```

> More practically, consider

```scheme
(define (F f) (lambda ([a b]) [(f a) (f b)]))
```

> which -- it may be argued-should accept as argument a function which
> is polymorphic enough to operate upon a and b of different type. For
> example,

```scheme
(F reverse [x y])
```

> produces a pair of reversed lists of different type if x and y are
> lists of different type.  Our system rejects such a use of F (since
> it requires a and b to have the same type), but admits

```scheme
(define (reverse-pair [x y]) [(reverse x) (reverse y)])
```

> or any other specialization of the function argument of F.

说不支持的意思是不能 infer，
check 显然是简单的：

```scheme
(claim F
  (nu (A B)
    (-> (-> A B) (tau A A)
        (tau B B))))
(define (F f) (lambda ([a b]) [(f a) (f b)]))
```

> We feel that this example illustrates the main limitation of our
> system, but that we may have kept as much flexibility as is possible
> without the use of explicit type parameters.  When these are
> introduced, the problem arises of the type of types; Reynolds [12]
> has made some progress in solving this problem, but we were anxious
> to see how much could be done while avoiding it.

值得一读：

- [12] "Towards a Theory of Type Structure".
  J. C. Reynolds, 1974.

# 3 A Simple Applicative Language and Its Types

## 3.1 The Language Exp

> Then the expression language Exp is generated by the following grammar:

```bnf
<exp> ::= <var>
        | (<exp> <exp>)
        | (if <exp> <exp> <exp>)
        | (lambda (<var>) <exp>)
        | (fix (<var>) <exp>)
        | (let ((<var> <exp>)) <exp>)
```

> `(fix (x) e)` stands for the least fixed point of `(lambda (x) e)`.

也就是 `(Y (lambda (x) e))`。

> Constants are omitted; we can imagine instead some standard
> bindings for certain identifiers in an initial environment.

> We give an ordinary denotational semantics for Exp, in which we
> include a value "wrong," which corresponds to the detection of a
> failure at run-time.

> In this small language, the only failures are
>
> - the occurrence of a non-Boolean value as the condition of a
>   conditional;
>
> - the occurrence of a nonfunctional value as the operator of an
>   application.

[2025-06-13] 为了理解这里所用的 denotational semantics，
看完 Scott 的 1969-a-type-theoretical-alternative-to-ISWIM-CUCH-OWHY 回来了。

每个 domain 都有 bottom，并且对取 domain 的任意子集的最小上界封闭。

我用 `(undefined D)` 来表示 D 的 bottom。

> Take as given a set {Bi} of basic domains, with B0 = T, the three
> element truth value domain

```
  false    true
     \     /
      \   /
 (undefined bool)
```

我直接用 `bool-t` 来指代 B0。

> and we define recursively

```
V = B0 + B1 + ... + F + W -- disjoint sum of domains
F = V -> V                -- continuous functions from V to V
W = {·}                   -- error
```

注意 `W` 我们写作 `wrong-t`，是一个有单一元素 `wrong` 的集合，
补充元素 `(undefined wrong-t)` 成为 domain。

`wrong` 与 `undefined` 不一样。为什么需要有这个区分呢？
可以理解为 `undefined` 是 meta-language 中的 runtime error，
而 `wrong` 代表所实现的 language 的 runtime error。

```scheme
(claim wrong wrong-t)

(claim true bool-t)
(claim false bool-t)

(define-type value-t
  (union bool-t
         ...
         (-> value-t value-t)
         wrong-t))
```

> The solution (up to isomorphism) of such a set of domain equations
> is assured by Scott [15]. Although he worked with complete lattices,
> the solution also exists in cpos (see Plotkin [11]).

这里的两个引用：

- [15] "Lattice theoretic models for various type-free calculi", Scott, 1972.
- [11] "A power-domain construction", Plotkin, 1976.

> The semantic function is `E ∈ Exp -> Env -> V`,
> where `Env = Id -> V`, the domain of environments.

我用 `evaluate` 代表这里的 `E`。

```scheme
(define-type env-t (-> var-t value-t))
(claim evaluate (-> exp-t env-t value-t))
```

> In defining `evaluate`, and later, we use some familiar notation of
> Scott and Strachey [16], illustrated by these examples (where `D` is
> some summand of `V`):

> (1) If `d ∈ D`, then `d in V` is the image of `d` under the
> injection of `D` into `V`.

不声不响定义了一个叫做 `in` 的 infix operator，
这里不注意的话，读起后面的代码来，真是让人困惑。

我们的实现用 structural type，所以不需要这种 injection（cast）。

> (2) If `v ∈ V`, then
>
>     v E D = true                -- if v = d in V for some d ∈ D
>           = (undefined bool-t)  -- if v = (undefined value-t)
>           = false               -- otherwise

一个判断元素是否属于类型的函数：

```scheme
(claim in? (-> value-t type-t bool-t))
(define (in? v D)
  (cond ((element-of? v D) true)
        ((equal? v (undefined value-t))
         (undefined bool-t))
        (else false)))
```

> (3) If `v ∈ V`, then
>
>     v | D = d                   -- if v = d in V for some d ∈ D
>           = (undefined D)       -- otherwise

一个判断元素是否属于类型，并且返回这个元素本身的函数。
只要使用 structural type，就可以直接以给出的参数 `v` 为返回值。
如果不用 structural type，就需要 match 所给 datatype `D` 的 data constructor，
然后把 data constructor 的参数数据取出来。

参数顺序反过来，就是 the little typer 的 `the`：

```scheme
(claim the (pi ((D type-t) (v value-t)) D))
(define (the D v) (if (in? v D) v (undefined D)))
```

关于 env 的函数：

```scheme
(claim env-cons (-> env-t var-t value-t env-t))
(define (env-cons env name value)
  (lambda (x) (if (equal? x name) value (env x))))
```

## 3.2 Semantic Equations for Exp

这一节就是写解释器。

回顾一下语法：

```bnf
<exp> ::= <var>
        | (<exp> <exp>)
        | (if <exp> <exp> <exp>)
        | (lambda (<var>) <exp>)
        | (fix (<var>) <exp>)
        | (let ((<var> <exp>)) <exp>)
```

把语法定义成 structural type：

```scheme
(define-type exp-t (union var-t ap-t if-t fn-t fix-t let-t))
(define-type var-t (symbol-exclude 'if 'lambda 'fix 'let))
(define-type ap-t (tau exp-t exp-t))
(define-type if-t (tau 'if exp-t exp-t exp-t))
(define-type fn-t (tau 'lambda (tau var-t) exp-t))
(define-type fix-t (tau 'fix (tau var-t) exp-t))
(define-type let-t (tau 'let (tau (tau var-t exp-t)) exp-t))
```

解释器：

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
       (if (in? target function-t)
         ;; be strict about arg be wrong.
         (if (in? x wrong-t) wrong (target x))
         wrong)))
    (`(if ,e1 ,e2 ,e3)
     (let ((p (evaluate e1 env))
           (t (evaluate e2 env))
           (f (evaluate e3 env)))
       (if (in? p bool-t) (if p t f) wrong)))
    (`(lambda (,x) ,e)
     ;; use the lambda of meta-language instead of closure.
     (lambda (v) (evaluate e (env-cons env x v))))
    (`(fix (,x) e)
     (Y (lambda (v) (evaluate e (env-cons env x v)))))
    (`(let ((,x ,e1)) ,e2)
     (let ((v1 (evaluate e1 env)))
       ;; be strict about rhs be wrong.
       (if (in? v1 wrong-t)
         wrong-t
         (evaluate e2 (env-cons env x v1)))))))
```

注意，在 lambda 的情形，这里使用了 meta-language 中的函数，
即定义 domain 时的 `F = V -> V`。

这样看来，所谓 denotational semantics，
就是用 meta-language 实现 language 的过程！
其中 meta-language 是数学没错，
但是一个具体的程序语言，
也可以被理解为是数学！

> **Notes**

> (1) Y is the least fixed-point operation.
> In many languages the e in
>
>     (fix (f) e)
>
> would be restricted to be an abstraction
>
>     (lambda (y) e2)
>
> and then
>
>     (let ((f (fix (f) (lambda (y) e2)))) ...)
>
> might receive the syntax
>
>     (letrec (((f y) e2)) ...)

> (2) It is easy to see that `(let ((x e1)) e2)` has the same meaning
> under `evaluate` as `((lambda (x) e2) e1)`. But part of our aim is a
> type discipline which admits certain expressions in the first form
> and yet rejects their translations into the second form; this is
> because lambda-abstractions may in general occur without an explicit
> operand, and need more careful treatment.

> (3) The semantics for `(e1 e2)` corresponds to call-by-value, since
> the test `(in? arg wrong-t)` ensures that the meaning of `(e1 e2)`
> is `wrong` if the meaning of `e2` is `wrong`. The omission of this
> test gives a call-by-name semantics (a similar test may be omitted
> in the semantics of the let construct), and the Semantic Soundness
> Theorem goes through equally in this case.

## 3.3 Discussion of Types

> We now proceed, in outline, as follows. We define a new class of
> expressions which we shall call types; then we say what is meant by
> a value _possessing_ a type. Some values have many types, and some
> have no type at all. In fact "wrong" has no type. But if a
> functional value has a type, then as long as it is applied to the
> right kind (type) of argument it will produce the right kind (type)
> of result -- which cannot be "wrong"!

注意，这里是 value 有 type，而不是 expression 有 type。

> Now we wish to be able to show that -- roughly speaking -- an Exp
> expression evaluates (in an appropriate environment) to a value
> which has a type, and so cannot be wrong.  In fact, we can give a
> sufficient syntactic condition that an expression has this robust
> quality; the condition is just that the expression has a
> "well-typing" with respect to the environment, which means that we
> can assign types to it and all its subexpressions in a way which
> satisfies certain laws.

注意，最后这句话，"which means that we can assign types to it and all
its subexpressions in a way which satisfies certain laws."
看起来非常适合用 propagator model 实现，
因为每个 subexpression 都可以实现为一个 cell。

> So there are two main tasks,
> once the laws of type assignment are given.
>
> - The first -- to show that an expression (or program) with a legal
>   type assignment cannot "go wrong" -- is tackled in this section;
>   surprisingly enough, it is the easier task (at least for an
>   applicative language).
>
> - The second task is to _discover_ a legal type assignment, given a
>   program with incomplete type information.  This task is often
>   called _type checking_. Of course, this term can also mean just
>   verifying that a given type assignment is legal; in a practical
>   situation we probably require something between the two, since one
>   cannot expect a programmer to attach a type to every
>   subexpression.

> In Section 4 we look at the class of legal type assignments for a
> given program (the class is infinite in general, since we admit
> polymorphism), and we give an algorithm which, if it succeeds,
> produces a legal type assignment. We conjecture that if the latter
> exists then the algorithm finds one which is most general, in the
> sense that any other legal type assignment is a substitution
> instance of it (substituting types for type variables).

## 3.4 Types and their Semantics

> The syntax of types is as follows.

```bnf
<basic-type> ::= "one basic type for each basic domain"
<type> ::= <type-var> | <basic-type> | (-> <type> <type>)
```

> A _monotype_ is a type containing no type variables.
> A _polytype_ is a type may contain type variables.

变量使用的惯例：

- type variables -- α, β, γ
- monotype -- μ, ν, π
- polytype -- σ, τ

首先，给 monotype 以语义，
所谓类型的语义就是确定什么情况下，
一个值 `v ∈ V` 属于一个类型 `μ`，
值与类型之间的属于关系写作 `v: μ`，
定义如下：

- (1) 对于基础 domain `Bi` 所对应的基础类型 `ιi`，
  `v: ιi` 当且仅当下面一种情况成立：
  - `v = (undefined value-t)`；
  - `(in? v Bi)`。

- (2) 对于 monotype 所构成的函数类型 `μ -> ν`，
  `v: μ -> ν` 当且仅当下面一种情况成立：
  - `v = (undefined value-t)`
  - `(in? v F)` 且对于所有 `u: μ` 有 `(v u): ν`。

按照这里的定义，
有的 value 都没有类型，
比如 `wrong` 和返回值为 `wrong` 的函数。

有的 value 有很多类型，
比如 `(lambda (x) x)`（meta-language 中的函数），
对于任意 monotype `μ` 都属于类型 `μ -> μ`；
又比如 `(undefined value-t)` 属于任何类型，
它也是唯一一个属于任何类型的值。

> This notion of type is derived from Scott [17]. In fact, it is what
> Scott calls _functionality_ (after Curry), and is distinct from the
> notion of a _retract_.

[17] "Data types as lattices", Scott, 1976.

"retract" 指什么？
也许看上面这篇 Scott 的论文可以解惑。

> If we temporarily identify a type with the set of values which
> possess it, then it is easy to show that types are _downward_ closed
> and _directed complete_, that is
>
> (1) ∀ v, v' ∈ V . (v: μ and v' ⊑ v) -> v': μ
>
> (2) For each directed subset X of V, (∀ v ∈ X . v: μ) -> (⊔ X): μ

注意，这里讨论的序关系，还是 domain 中的序关系，
类型所对应的集合，是 domain 的子集。

这里的 `(⊔ X)` 也许应该用 domain 中的加法所形成的 sum，写作 `∑ X`。

> Retracts share the second property, but not the first.

> Recently Shamir and Wadge [18] have defined a type to be _any_ set
> with these two properties, and they investigate the consequences of
> identifying a value `v` with the type `{v' | v' ⊑ v}`.

[18] "Data types as objects", A. Shamir and W. Wadge, 1977.

看了 steven vickers 的 "topology via logic"，
我现在知道，可以说 Wadge 的处理方式，
就是去考虑 domain 经过 spatialization
所得到的 topological space。

monotype 的语义讨论完了，
下面讨论 polytype 的语义。
首先可以定义 substitute type variables，
所诱导的 polytype 之间的序关系：
`ρ ≤ σ` 定义为 `ρ` 可以通过对 `σ` 把中的 type variables
替换为 type 而得到。例如：

    μ -> μ  ≤  α -> α  ≤  β -> β  ≤  α -> β

但是下面的不等式不成立（除非 `α = β`）：

    α -> β  ≤  β -> β

然后定义 value 和 polytype 之间的属于关系：
`v: σ` 定义为 `∀ μ ≤ σ . v: μ`。

例如：`(lambda (v) v): α -> α`。

虽然用了真正意义上的全局自由变元，但是还是可以给出「属于」关系的定义。
但是由于定义中用了全称量词，所以判断这种「属于」关系是需要证明的。
更具体地说，是需要对所有 monotype 的集合，做归纳法证明。

也许其他 formal system 中，由全称量词构成的命题，
也都要针对 system 中递归定义的元素，做归纳法证明。

> Polytypes thereby also stand for subsets of V, and these are also
> directed complete. The reader may like to think of each type
> variable in a polytype as universally quantified at the outermost;
> for example,
>
>     α -> α "means" ∀ α. α -> α
>
> where the bound A ranges over monotypes. In fact, it is because A
> here ranges over monotypes (not all types) and because we do not
> admit expressions like
>
>     (∀ α. α -> α) -> (∀ α. α -> α)
>
> as types that we avoid the difficulties (and also some of the
> interest) of Reynolds [12] in his richer notion of type.

这里警告了 bound a variable ranges over monotypes，
而不能 ranges over polytypes，
也就是我们准备在 lisp 中引入的 `nu` -- `(nu (A) (-> A A))`。
为什么会如此？也许只有在具体的实现中才能体会到。

[12] "Towards a Theory of Type Structure", Reynolds, 1974.

`(∀ α. α -> α) -> (∀ α. α -> α)` 作为表达式，
可以被 evaluate 成 `(α1 -> α1) -> (α2 -> α2)`，
然后再把所有的自由类型变元，
都视为是被一个最外层的 `∀` 所约束。

> We need the following simple properties,
> which are immediate from our definitions.
>
> - Proposition 1. if v: σ and τ ≤ σ, then v: τ.
> - Proposition 2. If v: σ -> τ and v': σ, then (v v'): τ.
>
> In each case, a property of monotypes is lifted to polytypes.

注意，前面给出了 type 之间，
因 substitution type variable 而形成的序关系，
这个序关系不是子类型关系。
Proposition 1 说这与子类型关系的性质刚好相反。

## 3.5 Type Assignments

> To prepare the ground for the theorem that well-typed expressions
> cannot "go wrong," we need to define what is meant by _typing_ an
> expression. We need first some notion of a type environment to give
> types to the free variables in an expression.

> A _prefix_ `p` is a finite sequence whose members have the form
> `let x`, `fix x`, or `lambda x`, where x is a variable.

这里定义的 prefix 和 sub-prefix 概念，
看来是为了计算一个 sub-expression 的 scope。

> so a sub-pe [sub-prefixed-expression] is just a subexpression
> prefixed with all the variable bindings which enclose it.

这里的定义不是很令人满意，
在后续的论文中 prefixed-expression 的概念应该也被放弃了，
因为在 luis damas 1982 年的论文和 1985 年的 phd thesis 中，
都没有这个概念了。

下面进一步定义 well-typed prefixed-expression。
这看来就是 well-typed 的 inference rule。
这样看来 prefix 就可以理解为 `check` judgment 中个的 type context，
但是与后续论文中的 type context 不同的是，
prefix 不只记录了 var 到 type 的 mapping，
还记录了 var 被引入的方式 -- `let x`, `fix x`, `lambda x`。

## 3.6 Substitutions

> A substitution S is a map from type variables to types. S may be
> extended in a natural way to yield a map from types to types, from
> typed pe’s to typed pe’s, etc.

也许在命名上应该强调这里的 substitution 是 type-substitution。

"typed pe" 就是 type context。

> We need substitutions extensively in the second part of this paper,
> but for the present we need only one property relating substitutions
> and well-typed.

> **Proposition 4**. If S involves no generic variables
> of a well-typed `p | d`,  then `S(p | d)` is also well-typed.

## 3.7 Well-Typed Expressions Do Not Go Wrong

证明语法层面用 inference rule 定义的 well-typed，
可以保证语义层面定义的 `(claim in (-> value-t type-t))`，
即 semantic soundness。

这个证明论和模型论之间的定理，
在后续的 luis damas 1982 年论文中体现地更清晰，
因为有对 inference rule 的明确定义。

TODO 熟悉这个证明的细节。

# 4 A Well-typing Algorithm and Its Correctness

## 4.1 The Algorithm W

> In this section we tackle the question of finding a well typing for
> a prefixed expression.  We present an algorithm W -- for this. We
> would like to prove that W is both syntactically sound and (in some
> sense) complete. By syntactic soundness, we mean that whenever W
> succeeds it produces a wt; by completeness, we mean that whenever a
> wt exists, W succeeds in finding one which is (in some sense) at
> least as general.

> Although W is probably complete, it is difficult to find a simple
> proof. So we concentrate on soundness, and then comment on
> implementation of W and on extending it to deal with richer
> languages. Since a type-checking algorithm which simulates W has
> been working successfully for nearly 2 years in the context of the
> LCF metalanguage ML [2], we have evidence for its usefulness and
> even -- to some extent -- for its completeness.

`wt` well-typed 就是以根据推演规则而得到的，
类型检查 judgment `(check ctx exp type)`。

这里关于 W 的一致性和完备性，
是作为推演规则的关系，
和作为函数的 W 算法之间的关系。

这与推演系统的一致性和完备性不同，
后者是推演系统和模型的关系。

类型的模型就是值的子集，
到 Damas 的论文，才说类型的模型是 ideal。

这篇论文没有证明完备性。
相信完备性是因为有很多测试。
Damas 的论文才证明了完备性。

## 4.2 The Soundness of W
## 4.3 Implementation of W; a Simplified Algorithm J

> As it stands, W is hardly an efficient algorithm; substitutions are
> applied too often.  It was formulated to aid the proof of soundness.
> We now present a simpler algorithm J which simulates W in a precise
> sense.

把明显返回 subst 的算法，
变成带有一个 subst 全局变量的算法。

# 5 Types in Extended Languages

# 6 Conclusion

> We have presented a discipline of polymorphic programming which
> appears to be practically useful, and have given a rather simple
> type-checking algorithm. In a restricted language we have shown that
> this algorithm can be proved correct (the proof was factored into
> two Soundness Theorems). Though much work remains to be done, we
> hope to have made the point that the practice of type checking can
> and should by supported by semantic theory and proof.

这应该是 "the practice of type checking can be
supported by semantic theory and proof"
的第一个例子。
