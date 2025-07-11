---
title: a type theoretical alternative to ISWIM CUCH OWHY
author: dana scott
year: 1969
---

# My Motive

[2025-06-11] 在看 Milner 1978 年的论文
"a theory of type polymorphism in programming"，
想要学习 Hindley-Milner 类型系统。
但是第三章开始有 denotational semantics 了，所以需要补习一下。

说 denotational semantics 是用复杂的东西解释简单的东西一点没错，
因为 Hindley-Milner 类型系统本身是算法与具体实现的结合，
根本不必引入 lattices 之类的东西就能解释清楚。

这篇论文好像才是 lambda calculus 的
denotational semantics 最早的论文。

Milner 在 1973 年的论文 "models of LCF" 中说：

> The logic of computable functions proposed by Dana Scott in 1969, in
> an unpublished note, has since been the subject of an interactive
> proof-checking program designed as a first step in formally based
> machine-assisted reasoning about computer programs.

Wikipedia Lambda calculus 页面中说：

> The fact that lambda calculus terms act as functions on other lambda
> calculus terms, and even on themselves, led to questions about the
> semantics of the lambda calculus. Could a sensible meaning be
> assigned to lambda calculus terms? The natural semantics was to find
> a set D isomorphic to the function space D → D, of functions on
> itself. However, no nontrivial such D can exist, by cardinality
> constraints because the set of all functions from D to D has greater
> cardinality than D, unless D is a singleton set.

其实 lambda expression 的集合显然是可数的。

> In the 1970s, Dana Scott showed that if only continuous functions
> were considered, a set or domain D with the required property could
> be found, thus providing a model for the lambda calculus. [42]

其中 [42] 所引用的就是这篇论文。

既然是要解决悖论，
那么 Scott 引入过多数学概念就情有可原了。
毕竟悖论会令人疯狂。

另外，这篇论文引入 lattices，
可能是为了把离散的函数处理成连续函数。
这种意义上说，这篇论文也是 HoTT 的先驱。
在 HoTT 中，所处理的连续函数不是 lattice 意义上的连续，
这种连续对代数拓扑而言太间接了；
HoTT 所处理的连续函数是 cell complex
这种代数拓扑的直接对象之间连续函数。
连续在于保持边界，而不是在于（经典拓扑空间的公理下）保持开集。

另外注意，这里的 type theory 来自罗素，
其目的是避免朴素集合论的悖论，
而不是方便人们写程序。

# My Notes

[2025-06-13] 对我来说，从这篇论文中所学到的最重要的 idea 是，
想要把用程序实现的函数解释为数学函数的时候，
会遇到完全性问题，即数学函数全是完全的函数，
而程序所实现的函数是部分函数，可能对某些参数没有定义。
这表现为死循环或者报错。

而这里的 idea 是通过给函数的 domain 加上一个 undefined 值，
来重新回到数学意义上的完全函数。

并且这种添加 undefined 的行为，会引出函数之间的 lattice，
(less f g) 就定义为 (more-defined f g)。

这个 lattice 可以用来把递归函数解释为 lattice 中的极限。

另外重要的是，这里考虑的 lattice 非常平凡，
只有 undefined 和原来的 value 之间有实质的序关系。
但是 propagator model 也用到了 lattice，
并且给出了很多非平凡的序关系。

注意，尽管带有 undefined 的 domain 只有平凡的序关系，
但是如果把函数视为 record（mapping），
就可以引出丰富的 record 之间的序关系，
甚至更多，因为函数可以表示无穷的 record。

## 算术的等级

[2025-06-11] 这种 D 到 D -> D 之间的对应，
我感到很熟悉，因为在讨论「算术的等级」时有过类似的构造，
比如先通过皮亚诺公理定义 Nat，
加法这个二元运算 add: Nat -> Nat -> Nat,
其实是把 Nat 中的元素映射到一个类型为 Nat -> Nat 的函数的集合。

不同的是，为了构造加法，
add(Nat) 只是 Nat -> Nat 的子集，
而为了寻求 lambda 演算的指称语义，
需要找到 D 与 D -> D 之间的同构。

可以说函数作用是最一般的，
不满足任何运算律的二元运算。

## 格上的连续函数 vs. 拓扑空间上的连续函数

[2025-06-12] 这里的 lattice 上的连续性，
和拓扑学公理所定义的连续性有什么关系？

通过给 lattice 定义 Scott 开集，可以满足拓扑公理。

## propagator model vs. domain theory

[2025-06-12] 我想要把 propagator model 中使用 lattice 的方式，
和 domain theory 中使用 lattice 的方式统一起来。

以 domain theory 为主，因为这一方面的理论发展的更完备。

在 domain theory 中，X ≤ Y，
代表 Y is "more defined" than X，
也就是 propagator model 中的 "more informative"。

因此在 propagator model 中，
merge 是 lattice 的 join -- ⋁，
的效果是让 partial information 变得 more informative。

我统一用 richer 来表示 more informative 和 more defined。
相反的用 poorer 来表示 less informative 和 less defined。
也许应该用 more 和 less，这两个更简单的词。

- x ≤ y -- (less x y)
- x ≥ y -- (more x y)
- x ⋁ y -- (join x y)
- x ⋀ y -- (meet x y)

用一组无穷多函数去逼近递归函数，
就是取这组无穷多函数的 join。
lattice 需要对于无限 join 封闭。

问题：如果取 lattice 的 Scott 开集，
对无限 join 封闭是否对应于，
拓扑空间对开集的无限并的封闭？

# Abstract

> The paper (first written in 1969 and circulated privately) concerns
> the definition, axiomatization, and applications of the hereditarily
> monotone and continuous functionals generated from the integers and
> the Booleans (plus “undefined” elements). The system is formulated
> as a typed system of combinators (or as a typed lambda-calculus)
> with a recursion operator (the least fixed-point operator), and its
> proof rules are contrasted to a certain extent with those of the
> untyped lambda-calculus.

> For publication (1993), a new preface has been added, and many
> bibliographical references and comments in footnotes have been
> appended.

# Preface (1993)

> This particular paper has, of course, an odd historical role: in it
> the author argues against the type-free calculi of Church and Curry,
> Kleene and Rosser, and their later uses by Bohm and Strachey. But
> then in November of 1969, after writing this report, the author
> himself saw that the method of monotone continuous functions (which
> grew out of traditional recursive function theory in discussing
> certain kinds of functionals) could be applied to posets other than
> just those generated from the integers (with bottom) by the very
> simple type constructors. In particular, it was recognized that
> there were posets whose continuous function spaces of self-maps
> could be identified with the given posets themselves. And so there
> actually were “traditional” models of lambda-calculus that could be
> defined without first going through the proof theory of the formal
> system itself (and which could be related to many other
> mathematically meaningful structures and theories as well).

> After writing with tiresome sarcasm about the lack of meaning in the
> type-free lambda-calculus introduced only formally, the author
> himself found an interesting kind of semantical interpretation for
> the “type-free” language. This total shift of gears is the reason
> the present paper was not published: the foundational program being
> advocated had apparently been completely outmoded by the discovery
> of the more general lattice-theoretic models.  However, the
> _axiomatic_ program laid out here had much to recommend it, and it
> was continued and extended in many directions by Milner, Plotkin,
> and many others, to whom the paper had been circulated privately --
> often at nth hand.

> On the other hand, the type-theoretical approach has not died out at
> all, because it has been taken over and absorbed into the
> applications of category theory to semantics and computation
> theory. The author is fond of saying that a category represents the
> “algebra of types”, just as abstract rings give us the algebra of
> polynomials, originally understood to concern only integers or
> rationals. One can of course think only of particular type systems,
> but, for a full understanding, one really needs also to take into
> account the general theory of types, and especially translations or
> interpretations of one system in another. Category theory together
> with the notion of functor and natural transformation between
> functors has been proved over and over again in the last
> half-century to be the appropriate way to go about these studies.
> The author himself does not always like or enjoy the discipline of
> category theory, which seems ofttimes to carry along very, very
> heavy machinery and odd terminology, but he long ago came to the
> conclusion that it is quite _unavoidable_. The extremely active
> current research in semantics also shows that it is an especially
> fruitful way to think. The book of Gunter [9] with its wide-ranging
> historical comments and references is adequate proof of this
> assertion.

值得一读：

- [9] Semantics of Programming Languages - Structures and Techniques,
  C.A. Gunter, 1992.

> The strange title of this paper ought perhaps to be explained. In
> 1966, Landin published an influential paper [14] which introduced a
> syntactical design style for programming languages, one of which he
> called ISWIM, standing for “If you See What I Mean”.

也就是说 Landin 的这篇论文是关于语法设计风格的？

> Also Böhm in 1966 published the paper [3] which named a language of
> combinators called CUCH, standing for “Curry-Church”.

> The author hoped to stop some proliferation by suggesting a return
> to the logically standard type-theoret- ical framework and thereby
> deter the creation of programming languages of doubtful foundation
> called (as a group) OWHY, standing for “Or What Have You.”

> No one really understood the joke, and the effort was doomed to be
> of no avail. And history proved the author to be too conservative in
> any case.

# 0 Introduction

> No matter how much wishful thinking we do, the theory of types is
> here to stay.  There is _no other way_ to make sense of the
> foundations of mathematics.

> My point is that formalism _without eventual interpretation_ is in
> the end useless. Now, it may turn out that a system such as the
> lambda-calculus will have an interpretation along standard lines
> (and I have spent more days than I care to remember trying to find
> one), but until it is produced I would like to argue that its
> purposes can just as well be fulfilled by a system involving
> types. Indeed, as far as _proofs_ are concerned, the system with
> types seems to be much better.

> It is a pity that a system such as Zermelo-Fraenkel set theory is
> usually presented in a purely formal way, because the conception
> behind it is quite straightforwardly based on type theory. One has
> the concept of an arbitrary subset of a giuen domain and that the
> collection of all subsets of the given domain can form a new domain
> (of the next type!).

从罗素的类型论出发，而不是从程序语言的类型论出发。

> Starting with a domain of individuals (possibly empty), this process
> of forming subsets is then iterated into the transfinite. Thus, each
> set has a type (or rank) given by the ordinal number of the stage at
> which it is first to be found in the iteration.

> One advantage of this method is that the types are built into the
> sets themselves and need not be made manifest in the
> formalism. (Computer people might say that the type checking in set
> theory is done at runtime rather than at compile time.)

> For the purposes of understanding computation, however,
> set-theoretical formalism is not too helpful in any direct way.

其实 structural type 是非常接近集合论的。

> ... the axioms of set theory are meant to capture something
> essential of the idea of an arbitrary subset, while computation
> theory is more interested in the notion of an algorithmically
> dejined subset (or function). Of course, one can define in set
> theory such notions as that of a general recursive function, but
> such definitions do not emphasize enough what is special about
> algorithms.

> Nor is it generally clear when a defined function is recursive. So
> what we want is a “restricted” system that is specially designed for
> algorithms.

这里有一段注释：

> Unfortunately, the axiomatics of “synthetic domain theory” have not
> been completely clarified so that a convenient foundation for
> computation theory and semantics can be given in set-theoretic terms
> (see [12, 25] and the references therein).

也就是说人们尝试直接公理化 domain theory。
值得看看。

> What I shall present below is an independent system with its own
> axioms and rules; but, since I observe the canons of type theory, it
> can be (and indeed must be) read as a fragment of set theory so that
> its theorems can be recognized as valid. This is the main feature
> missing from the lambda-calculus.

# 1 Types

> We can reduce the notion of function to that of set, but it is not
> convenient to do so. A much better plan is to treat sets (and
> relations) as special functions (truth-valued) as Church does.

> These are the two logical types of the lowest order：

- `any-t` -- the largest data type;
  all other data types are to be “included” in it.

- `bool-t` -- the type of the truth values.

> The higher types are represented as follows: If `A` and `B` are
> types, then so is `(-> A B)`. What `(-> A B)` represents is the type
> of functions from objects of type A to those of type B.

> Aside from this collection of type symbols, we will also have a rich
> language of expressions.

> Each expression will have a (unique) type, and I shall write `X: A`
> to mean that the expression `X` is of type `A`.

> The logical constants are infinite in number (so are the types);
> I list them along with their types (A, B, C are arbitrary type symbols):

```scheme
;; two bottom elements
(claim any-omega any-t)
(claim bool-omega bool-t)

(claim true bool-t)
(claim false bool-t)

(claim ifte (nu (A) (-> bool-t A A A)))

(claim K (nu (A B) (-> A B A)))
(claim S (nu (A B C) (-> (-> A B C) (-> A B) (-> A C))))
(claim S (nu (A B C) (-> (-> A B C) (-> A B) A C)))
(claim Y (nu (A) (-> (-> A A) A)))
```

> As for the other expressions, suppose `X: A` and `F: (-> A B)` then
> we have `(F X): B`.

```
X: A
F: (-> A B)
-----------
(F X): B
```

> That is to say the standard function-value notation is the only way
> in which compounds can be made. Note that the type of X must& the
> type of `F` for `(F X)` to be well formed. Note, too, how
> complicated the types of our constants are. In particular, it is
> possible to form an expression of any type using only constants and
> no variables.

> By a _formula_, we understand either an atomic formula of the form
>
>     X ≤ Y,
>
> where X and Y are expressions, or a list (possibly empty!) of atomic
> formulae.

expressions 之间有顺关系？

> By an _assertion_, we understand a string of the form
>
>     C |- D
>
> where C and D are lists of atomic formulae. Intuitively lists of
> formulae are just conjunctions and |- gives an implication between
> conjunctions -- but this will all be clear when we find out in the
> next section the meanings of all our symbols. After that we will
> discuss _axioms_ and _rules of inference_ for generating the (or a
> good part of the) valid assertions in an “algebraic” way.

用 inference rule 所推导也能称作是 an “algebraic” way 了？

> We need a few abbreviations. If X and Y are expressions of the same
> type, then
>
>     X = Y
>
> stands for the list
>
>     X ≤ Y, Y ≤ X.

> If F is an expression, and if `F(X)(Y)(Z)` is well formed,
> then we abbreviate this as
>
>     F(X, Y, Z)
>
> Similarly, for more than three terms.

下面是很多年后的符号变更注释：

> Subsequently, the author’s notation changed because it was confusing
> (he felt) to use either ≤ or ⊆ for the information ordering within a
> domain. He thus adopted the “square” notation of ⊑ from lattice
> theory. This brought along ⊓ and ⊔. and took ⊥ for the bottom
> element instead of omega. It still seems to the author a better
> notation, but too many people prefer to write the easier ≤. However,
> in a system that might involve integers, sets (say as elements of
> power domains), and the information ordering, it seems cleaner to
> have different symbols for different notions.

# 2 Interpretation

> The classical way of viewing the theory of types is to assign to
> each type A a domain (D A). ... and each (D (-> A B)) is the domain
> of all functions from (D A), with values in (D B).

> This point of view is not convenient for our purposes. The reason is
> simple: classical type theory supposes total (everywhere defined)
> functions, while algorithms in general produce partial functions.
> We do not wish to reject a program if the function defined is
> partial -- because as everyone knows it is not possible to predict
> which programs will “loop” and which will define total functions.

> The solution to this problem of total versus partial functions is to
> make a “mathematical model” for the theory of partial functions
> using ordinary total functions. The idea is not at all original to
> the author (he has taken it from more “standard” versions of
> recursive function theory -- in particular, from the thesis of
> Platek [20]).

值得一读：

- [20] "Foundations of recursion theory",
  Richard Alan Platek, Ph.D. Thesis, 1966.

> What we do is to adjoin a “fictitious” element `any-omega`, to the
> domain `(D any-t)`, and an element `bool-omega`, to `(D bool-t)`. We call
> `any-omega` the “undefined” individual and `bool-omega`, the
> “undefined” truth value.

> However, we need to distinguish the new elements from the old. To do
> this we create a relation ≤ on (D any-t), and ≤ on (D bool-t), (same
> symbol -- different relations) meaning, roughly, “is less or equally
> defined as”. Thus, a reasonable assumption is that
>
>     any-omega ≤ x
>
> for all x ∈ (D any-t), but that
>
>     x ≤ y
>
> implies
>
>     x = y
>
> if x and y are not equal to `any-omega`.

这里有一段注释：

> [from the original text] I am sorry that I must use the same symbols
> in the metalanguage as some of those in the object language. It is a
> sad fact that there are just toofew symbols.

为什么不用清晰的而且可规模化的 lisp 语法呢？

> We make this assumption about ≤ on (D bool-t), but for the moment
> not about (D any-t). Thus, a “picture” of (D bool-t), could be:

```
  false    true
     \     /
      \   /
    bool-omega
```

omega 就是 propagator model 中的 undefined。

> We are therefore involved with a three-valued “logic” with the new
> value bool-omega, “in between” true and false but placed “a little
> lower down”.

> Kleene introduced such a three-valued logic in [13], and it has been
> discussed by many, many authors.

Kleene [13] 就是著名的 1952 年的 "Introduction to Metamathematics"。

既然 (D any-t) 和 (D bool-t) 都是篇序集，
那么 (D (-> A B)) 就是所有单调函数的集合，
这个集合是一个由 (D A) 和 (D B) 的篇序集所引出的篇序集。

> By _monotonic_ we understand a function, where
>
>     x ≤ y  ->  f(x) ≤ f(y).
>
> In words this means: the more you define an argument, the more
> you define its value under a “computable” function.

这与 propagator model 对 domain 的理解完全相同。

> we define
>
>     f ≤ g
>
> to mean
>
>     forall x ∈ (D any-t) f(x) ≤ g(x)

> Note that there is a natural “smallest” element among the
> elements of (D (-> any-t any-t)), the function omega:
>
>    (define (omega x) any-omega)

以 `nat-t` 和 `(D nat-t)` 为例，
注意这里的偏序关系，
`(D nat-t)` 是 `nat-t` 添加 `nat-omega` 而来的集合，
`nat-t` 的元素之间没有定义序关系，
只是在 添加 `nat-omega` 之后，要求：

    forall (n: nat-t) nat-omega ≤ n

> Now the monotonic functions f are almost like
> ordinary functions except we allow
>
>     (f n) = omega
>
> for certain arguments n if we so desire. If we read this equation
> as: f is undefined at n, then we agree that f is defined only for a
> subset of the natural numbers.

假设 f0 是对 nat-t 一个子集 S 有定义的，
那么可以定义单调（注意是就上面的序关系而言的）函数 f：

```scheme
(define (f x)
  (if (in x S)
    (f0 x)
    nat-omega))
```

对于函数的 domain 来说，
其序关系和 structural type 所定义的序关系类似，
只不过 structural type 中有有限的 record，
而一个函数是一个无限的 record（mapping）。

可数多个函数，在这种序关系下的最小上界，
显然可以为递归函数提供语义（用数学对象来解释）。

这「可数多个函数」可以是一个 chain：

    f0 ≤ f1 ≤ f2 ≤ f3 ≤ ...

TODO 这种带有 lattice 的语义理论，
可否扩展到 cell complex？
cell complex 用边界关系定义的连续性，
如何与拓扑学公理的连续性联系起来？
subdivision 的概念可否帮助我们形成这个联系？

```scheme
(claim ifte (nu (A) (-> bool-t A A A)))
(define (ifte p x y)
  (if (eq? p true)
    x
    (if (eq? p false)
      y
      omega)))

(claim K (nu (A B) (-> A B A)))
(define (K x y) x)

(claim S (nu (A B C) (-> (-> A B C) (-> A B) (-> A C))))
(claim S (nu (A B C) (-> (-> A B C) (-> A B) A C)))
(define (S f g x) (f x (g x)))

(claim Y (nu (A) (-> (-> A A) A)))
(define (Y f)
  ((lambda (x) (f (x x)))
   (lambda (x) (f (x x)))))

(define (Y f)
  (check f (-> A A))
  (check (x x) A)
  (check x (mu (B) (-> B A)))
  (check x (-> (mu (B) (-> B A)) A)) ;; expend mu once
  (check (f (x x)) A)
  (check g (-> (mu (B) (-> B A)) A))
  (check (g g) A)
  (check (f (g g)) A)
  (-> (mu (B) (-> B A)) A)
  (let ((g (lambda (x) (f (x x)))))
    (f (g g))))
```

```scheme
(claim pair (nu (A) (-> A A bool-t A)))
(define (pair x y p) (ifte p x y))

(claim x A)
(claim y A)
(check (pair x y) (-> bool-t A))
```

下面用极限来定义 (Y f)，
并且证明 (Y f) 是 f 的最小不动点。
这里的「最小」是就上面所定义的函数之间的序集而言的。

注意，要带入 f 的是 factorial-wrap，而不是 factorial。
这样看来 (f omega) 就有意义多了：

```scheme
(factorial-wrap omega)
(factorial-wrap (factorial-wrap omega))
(factorial-wrap (factorial-wrap (factorial-wrap omega)))
...
```

> Having now interpreted all of our constants, we can define the
> important notion of _validity_. Suppose X and Y are two expressions
> of the same type. In general, they contain variables; hence they do
> not denote, as they stand, anything in particular. But, if we assign
> values in the appropriate domain, to each of the variables, then all
> of the symbols in the expressions become meaningful and X and Y have
> values. Thus, under the assignment to the variables, the atomic
> formula X ≤ Y is either true or false.

> Now consider an assertion C |- D. It is said to be valid if under
> every assignment of values to variables that makes all the atomic
> formulae of the list C true, all the atomic formulae of Y are true
> also. That is, C “implies” D with the variables being universally
> quantified.

> Notice, however, how different our method is compared to the
> lambda-calculus. In the latter theory, validity of equations
> (interconvertibility) is defined in a purely formal manner.  Here we
> have dejned validity “semantically” and must _discover_ the formal
> properties of this notion.

# 3 Axiomatization

> In the first place there are some very general properties of |- that
> would be the same for any similar theory. We give “axioms” (quite
> self-evidently valid assertions) and “rules of inference” (simple
> deduction methods that clearly preserve validity).

本身 assertion 对应 Martin Lof 的 judgement，judgement 是推演规则的对象。
而 judgement 包含 expressions 代表人们对 expression 的看法。

最典型的例子是 bidirectional type checking 中的两个 judgements
-- check 和 infer。

而这里的 assertion（即 judgement），虽然形如蕴涵式（implies），
但是可以理解为是 `(valid premises conclusions)`，
其中 premises 可以看作是类似 check 和 infer 的 context。

这里的 assertion 的对象是 formula，
formula 都是形如 X ≤ Y。
这就使得 formula 类似于 judgement。

暂时使用正向的 inference rule，结论写在后面。

```
(include D C)
------------- [INCLUSION]
C |- D

C |- D
C' |- D'
-------------- [CONJUNCTION]
C, C' |- D, D'

C |- D
D |- E
------ [CUT]
C |- E

C |- D
---------------- [SUBSTITUTION]
C[X/x] |- D[X/x]
```

> Next the relation ≤ enjoys several useful properties:

```
-------- [REFLEXIVITY]
|- x ≤ x

--------------------- [TRANSITIVITY]
x ≤ y, y ≤ z |- x ≤ z

--------------------------- [MONOTONICITY]
x ≤ y, f ≤ g |- f(x) ≤ g(y)

C |- f(x) ≤ g(x)
x not in C
---------------- [EXTENSIONALITY]
C |- f ≤ g
```

> Note especially that we have stated these principles without type
> subscripts, This is a very convenient trick available in the
> metalanguage.  The point is that, say, |- x ≤ x is valid for
> variables x of all types. Similarly for the axiom of transitivity
> with the understanding that x, y, z are all of the same type --
> otherwise the formulae would not all be well formed.

在解释 EXTENSIONALITY rule 的时候，scope 已经乱了。
后面的规则 scope 更乱，比如 INDUCTION。

> By the way, remember that X = Y is short for X ≤ Y, Y ≤ X. Note that
> = is indeed an equality relation because we can now prove, as
> theorems from the axioms and rules we already have, that

```
              |- X = X
 X = Y, Y = Z |- X = Z
        Y = X |- X = Y
C[X/x], X = Y |- C[Y/x]
```

> Inasmuch as we have assumed no “non-logical” constants, there is in
>  general very little to say about individuals of type `any-t` except
>  for the “undefined” individual `any-omega`:

```
---------------- [MINIMALITY]
|- any-omega ≤ x
```

可以看出作者是想从上一章的数学推理中，
抽出来一个形式化的公理系统，
但是我不认为作者成功了。

而且我认为作者这样做本身就有点问题，
毕竟 denotational semantics 和 domain theory，
就是为了使用数学语言和工具，
这里就没有必要再回到证明论了。

后面讨论 induction 的时候有一个注释：

> In fact, there have been many subsequent studies of induction
> principles.  See for example [18, 19], but also compare the proofs
> in [5].

值得一读：

- [18] "A co-induction principle for recursively defined domains",
  A.M. Pitts, 1994.
- [19] "Relational properties of recursively defined domains",
  A.M. Pitts, 1993.

注意，在 HoTT 中，induction 也是大问题。

# 4 Completeness

作者讨论上面的形式系统的完备性，
读到这里我已经没有耐心了，
只是匆忙看完。

# 5 Conclusions

> It seems to me that the idea of a monotonic and continuous function
> is a very natural one for anyone thinking about computability.

这里有注释：

> The recursive function people have been considering monotonic
> functions for a long time, and Bekić recently came across the idea
> again in his study of automata theory [2].

值得一读：

- [2] "Definable operations in general algebras,
  and the theory of automata and flowcharts",
  Hans Bekić, 1969.

> What I have tried here is to give a logical calculus (or even:
> algebra) for the notion using type theory. The point is that the
> types are natural -- the higher-type functionals are useful -- and
> they have the advantage of possessing a semantical interpretation.

> It is not very surprising that there is a nice algebra since all we
> really need are the conditional expression and the possibility of
> explicit definition (S and K) and of recursive definition (Y).

> Now what about other data types? My present view is that all the
> data should be kept in type `any-t`.

这种观点，好像和我想要设计的 structural typed lisp 类似！

> Finally, we must agree that the study of lambda-calculus cannot
> replace the study of programming languages. It is true that the
> logical notation allows us to express many computations and that the
> system could be given the look of a programming language, but we
> have not built into the theory logical notions corresponding to the
> full glory of the assignment statement and to the idea of jumps and
> goto’s. Landin has tried to do this with ISWIM, but the personal
> view of the author is that the result is not quite
> successful. Landin does have a clean, regular, and powerful
> language, but in a certain sense it is just another language:
> evaluation still must be done on a machine.

operational semantics vs. denotational semantics！
