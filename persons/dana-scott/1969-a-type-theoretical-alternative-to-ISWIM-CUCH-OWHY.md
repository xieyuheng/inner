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

TODO 明确这里的 lattice 上的连续性，
和拓扑学公理所定义的连续性之间的关系。

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

> Already we should be forced to prove something in order to see that
> the above definitions “make sense”. In particular, we need to know
> that the functions do belong to the correct domains.

TODO

# 3 Axiomatization
# 4 Completeness
# 5 Conclusions
# 6 Afterthought (1993)
