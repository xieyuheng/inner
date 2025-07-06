---
title: principal type schemes for functional programs
authors: [luis damas, robin milner]
year: 1982
---

# My Motive

[2025-06-14] 在学习 Hindley-Milner type system，
与 Milner 的原始论文相比，这篇后期的论文可能更简单。
读来试试看。

# 1 Introduction

> This paper is concerned with the polymorphic type discipline of ML,
> which is a general purpose functional programming language, although
> it was first introduced as a metalanguage (whence its name) for
> constructing proofs in the LCF proof system.[4] The type discipline
> was studied in [5] where it was shown to be semantically sound, in a
> sense made precise below, but where one important question was left
> open: does the type-checking algorithm -- or more precisely the type
> assignment algorithm (since types are assigned by the compiler, and
> need not be mentioned by the programmer) -- find the most general
> type possible for every expression and declaration?

> Here we answer the question in the affirmative,
> for the purely applicative part of ML.

> It follows immediately that it is decidable whether a program is
> well-typed, in contrast with the elegant and slightly more
> permissive type discipline of Coppo. [1]

[1] "An extended polymorphic type system for applicative languages",
M. Coppo，1980.

> The discipline can be well illustrated by a small example.

```scheme
(define (map f s)
  (if (null? s)
    null
    (cons (f (car s)) (map f (cdr s)))))
```

> The type checker will deduce a type-scheme for map from existing
> type-schemes for `null?`, `null`, `cons`, `car` and `cdr`; the term
> type-scheme is appropriate since all these objects are
> polymorphic. In fact from

```scheme
(claim null? (nu (A) (-> (list-t A) bool-t)))
(claim null  (nu (A) (list-t A)))
(claim cons  (nu (A) (-> A (-> (list-t A) (list-t A)))))
(claim car   (nu (A) (-> (list-t A) A)))
(claim cdr   (nu (A) (-> (list-t A) (list-t A))))
```

> will be deduced

```scheme
(claim map (nu (A B) (-> (-> A B) (-> (list-t A) (list-t B)))))
```

> Thus, the main result of this paper is that the type-scheme deduced
> for such a declaration (and more generally, for any ML expression)
> is a principal type-scheme, i.e. that any other type-scheme for the
> declaration is a generic instance of it. This is a generalisation of
> Hindley’s result for Combinatory Logic [3].

[3] "The principal type-scheme of an object in combinatory logic",
R. Hindley, 1969.

注意，type-scheme 这个术语的使用，
是想要区分带有约束类型变量类型，
和不带约束类型变量的类型。
如今（dependent type 的时代）我们显然不应该再这样使用这个术语。

> ML may be contrasted with Algol 68, in which there is no
> polymorphism, and with Russell [2], in which parametric types appear
> explicitly as arguments to polymorphic functions. The generic types
> of Ada may be compared with type-schemes. For simplicity, our
> definitions and results here are formulated for a skeletal language,
> since their extension to ML is a routine matter. For example
> recursion is omitted since it can be introduced by simply adding the
> polymorphic fixed-point operator

[2] "Report on the programming language russell",
A. Demers and J. Donahue, 1979.

这里所说的
"parametric types appear explicitly
as arguments to polymorphic functions"
和没有 implicit argument 的 dependent type 一样。

```scheme
(claim fix (nu (A) (-> (-> A A) A)))
```

> and likewise for conditional expressions.

省略了 Milner 原始论文中的 if 表达式。

# 2 The language

> Assuming a set Id of identifiers x the language Exp of expressions e
> is given by the syntax

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

> Assuming a set of type variables α and of primitive types ι,
> the syntax of types τ and of type-schemes σ is given by

```bnf
τ ::= α | ι | τ → τ
σ ::= τ | ∀ασ
```

用 lisp 语法：

```bnf
<type> ::= <type-var> | <primitive-type> | (-> <type> <type>)
<type-scheme> ::= <type> | (nu (<type-var>) <type-scheme>)
```

`(-> A (-> B C))` 可以简写做 `(-> A B C)`；
`(nu (A1) (nu (A2) T))` 可以简写做 `(nu (A1 A2) T)`。

> A _monotype_ µ is a type containing no type variables.

# 3 Type instantiation

定义 type 和 type-scheme 由 substitution 引出的序关系，
后面定义值和类型之间的属于关系时要用到。

> If S is a substitution of types for type variables, often written
> [τ1/α1, ..., τn/αn] or [τi/αi], and σ is a type-scheme, then Sσ is
> the type-scheme obtained by replacing each free occurrence of αi in
> σ by τi, renaming the generic variables of σ if necessary. Then Sσ
> is called an _instance_ of σ; the notions of substitution and
> instance extend naturally to larger syntactic constructs [such as
> type context] containing type-schemes.

```scheme
(define-type subst-t (list-t (tau type-var-t type-t)))

(claim subst-on-type (-> subst-t type-scheme-t type-scheme-t))
(claim subst-on-ctx (-> subst-t ctx-t ctx-t))
```

注意，substitution 作为 map 的 key 是 type-var，
而 ctx 作为 map 的 key 是 var。

> By contrast a type-scheme σ = ∀α1...αm τ
> has a _generic instance_ σ' = ∀β1...βn τ'
> if τ' = [τi/αi]τ for some types τ1, ..., τm
> and the βj are not free in σ.

这里 "βj are not free in σ" 的条件，
也许应该直接说成是 β1...βn 是 α1...αm 的子集。

注意，generic instance 包含了 n = 0 的情况，
即 σ = ∀α1...αm τ 而 σ' = τ'。

> In this case we shall write σ > σ'.
> Note that instantiation acts on free variables,
> while generic instantiation acts on bound variables.
> It follows that σ > σ' implies Sσ > Sσ'.

# 4 Semantics

> The semantic domain V for Exp is a complete partial order satisfying
> the following equations up to isomorphism, where Bi is a cpo
> corresponding to primitive type ιi:

```
V = B0 + B1 + ... + F + W   (disjoint sum)
F = V → V                   (function space)
W = {·}                     (error element)
```

> To each monotype µ corresponds a subset V, as detailed in [5];
> if v ∈ V is in the subset for µ we write v: µ.
> Further we write v: τ if v: µ for every monotype instance µ of τ,
> and we write v: σ if v: τ for every τ which is a generic instance of σ.

注意，τ 想要作为 σ 的 generic instance，
τ 必定是不带有约束类型变元的。

例如，f: ∀β(β → β) 定义为对任意 T ∈ type-t，f: (T → T) 为真。

```scheme
(claim in (-> value-t type-scheme-t relation-t))
```

这里给出值的集合，并且以 monotype 为基础，
定义值和类型之间的属于关系。

另外注意，对于 monotype 来说，
这个属于关系是用语义层面的集合的属于关系来定义的，
而对于 type-scheme（polytype），
就是语义元素（值）与语法元素（type-scheme）之间的关系了。

这么看来，也许传统证明论的特点，
就是语法层面的 free variable 以及相关的量词，
这些语法元素本身是没有语义层面的对应的。
但是 dependent type 改变了这一点
（如果能找到令人满意的 dependent type 的指称语义的话）。

函数虽然是数学对象，
但是对函数的逻辑解读只能是：
`forall x: A, exists unique y: B`，
如果去掉了 unique
`forall x: A, exists y: B`，
就是一种对第一个参数都有定义的关系，
关系也是数学对象。

可否用一组数学对象，作为逻辑的组合子，
把所有的 forall 和 exists 的组合都穷尽呢？

> Now let `Env = Id → V` be the domain of environments η.
> The semantic function `evaluate: Exp → Env → V` is given in [5].
> Using it, we wish to attach meaning to assertions of the form
>
>     A |= e: σ
>
> where e ∈ Exp and A is a set of assumptions
> of the form x: σ, x ∈ Id.

注意，这里虽然还是用了 `e: σ`，但是不是：

```scheme
(claim in (-> value-t type-scheme-t relation-t))
```

而是：

```scheme
(claim in (-> exp-t type-scheme-t relation-t))
```

所以需要用 `evaluate` 才能定义这个 assertion form。

另外，这里 `|=` 是 model theory 意义上的 semantic entailment，
即所有使得前提（A）为真的语义赋值，
都会使得结论（e: σ）也（在数学意义上）为真。

「语义赋值」就是用 environment 来完成的。

如果忘记了 `|=` 的意义，
可以回顾一下其在（最简单的）命题演算中的定义。

```scheme
(define ctx-t (list-t (tau var-t type-scheme-t)))
(claim check (-> ctx-t exp-t type-scheme-t judgment-t))
```

> If the assertion is closed, i.e. if A and σ contain no free type
> variables, then the sentence is said to hold iff, for every
> environment η, whenever η[[x]]: σ' for each member x: σ' of A,
> it follows that evaluate [[e]] η: σ.

用对 environment 的全称量词来从语义上（数学意义上）定义 judgment。
即，对于任意 environment，如果 context 中的属于关系都成立，
那么结论中的属于关系在这个 environment 的 evaluate 下也成立。
这就是 model theory 中 semantic entailment -- `|=` 的定义。

> Further, an assertion holds iff all its closed instances hold.

> Thus, to verify the assertion
>
>     x: α, f: ∀β(β → β) |= (f x): α
>
> it is enough to verify it for every monotype µ in place of α.

用 lisp 语法：

```scheme
(check '((x A)
         (f (nu (B) (-> B B))))
  '(f x) 'A)
```

> This example illustrates that free type variables in an assertion
> are implicitly quantified over the whole assertion, while explicit
> quantification in a type scheme has restricted scope.

> The remainder of this paper proceeds as follows.
>
> - First we present an inference system for inferring valid assertions.
>
> - Next we present an algorithm W for computing a type-scheme
>   for any expression, under assumptions A.
>
> - We then show that W is _sound_,
>   in the sense that any type-scheme it derives
>   is derivable in the inference system.
>
> - Finally we show that W is _complete_,
>   in the sense that [any] derivable type-scheme
>   is an instance of that computed by W.

典型的 proof theory + model theory 的结论。

# 5 Type inference

这里首先从语义层面把 `check`
定义为了一个数学意义上的谓词，
或者说数学意义上的一个断言。

之所以强调说是数学意义上的（而不是程序意义上的），
是因为想要判断一个断言是否成立，
需要引入很多 forall 来做证明才能完成。

比如：

- 想要证明 `|=` 需要引入「对于任意赋值」，即 forall environment。
- 想要证明 `v: τ` 需要 for every monotype instance µ of τ.
- 想要证明 `v: σ` 需要 v: τ for every τ which is a generic instance of σ.

相比之下，the little typer 中没有这种语义分析，
直接给出了语法层面上的 judgment 和相关的 inference rules。
也许给出语义分析才是正确的，
毕竟 inference rule 只有在有 model 解释的情况下，
才是有用的。

> From now on we shall assume that A contains at most one assumption
> about each identifier x. `A[^x]` stands for removing any assumption
> about x from A.

> For assumptions A, expressions e and type-scheme σ we write
>
>     A |- e: σ
>
> if this instance may be derived from the following inference rules:

为了方便排版，我们反过来写推理树：

```scheme
(define ctx-t (list-t (tau var-t type-scheme-t)))
(claim check (-> ctx-t exp-t type-scheme-t judgment-t))

(define-rule TAUT (check A x σ)
  (equal? (lookup A x) σ))

(define-rule INST (check A e σ*)
  (check A e σ)
  (instance-of? σ* σ))

(define-rule GEN (check A e (nu (α) σ))
  (check A e σ)
  (not (include? A α)))

(define-rule COMB (check A (e e*) τ)
  (check A e (-> τ* τ))
  (check A e* τ*))

(define-rule ABS (check A (lambda (x) e) (-> τ* τ))
  (check (update A x τ*) e τ))

(define-rule LET (check A (let ((x e)) e*) τ)
  (check A e σ)
  (check (update A x σ) e* τ))
```

> The following example of a derivation is organised as a tree,
> in which each node follows from those immediately above it
> by an inference rule.

```
TAUT: --------------
      x : α |- x : α
ABS: -----------------
     |- (λx.x) : α → α
GEN: ---------------------
     |- (λx.x) : ∀α(α → α)
```

用 lisp 语法（省略语法元素上的 quote）：

```scheme
(define P1
  (prove (check [] (lambda (x) x) (nu (α) (-> α α))) GEN
    (prove (check [] (lambda (x) x) (-> α α)) ABS
      (prove (check [[x α]] x α) TAUT))))
```

展开应该是：

```scheme
(define (P1)
  (the (check [] (lambda (x) x) (nu (α) (-> α α)))
    (GEN (the (check [] (lambda (x) x) (-> α α))
           (ABS (the (check [[x α]] x α)
                  (TAUT)))))))
```

```scheme
(define (P2)
  (prove (check [[i (nu (α) (-> α α))]] (i i) (-> α α)) COMB
    (prove (check [[i (nu (α) (-> α α))]] i (-> (-> α α) (-> α α))) INST
      (prove (check [[i (nu (α) (-> α α))]] i (nu (α) (-> α α))) TAUT))
    (prove (check [[i (nu (α) (-> α α))]] i (-> α α)) INST
      (prove (check [[i (nu (α) (-> α α))]] i (nu (α) (-> α α))) TAUT))))
```

```scheme
(define (P3)
  (prove (check [] (let ((i (lambda (x) x))) (i i)) (-> α α)) LET
    (prove (check [] (lambda (x) x) (nu (α) (-> α α))) P1)
    (prove (check [[i (nu (α) (-> α α))]] (i i) (-> α α)) P2)))
```

> The following proposition, stating the semantic soundness of
> inference, can be proved by induction on e.

> **Proposition 1** (Soundness of inference).
>
> If A |- e: σ then A |= e: σ.

> We will also require later the following
> two properties of the inference system.

> **Proposition 2**.
>
> If S is a substitution
> and A |- e: σ
> then S A |- e: S σ.
> Moreover if there is a derivation of A |- e: σ of height n
> then there is also a derivation of S A |- e: S σ
> of height less [than] or equal to n.
>
> Proof. By induction on n.

> **Lemma 1**.
>
> If σ > σ' and `A[^x] ∪ {x: σ'} |- e: σ0`
> then also `A[^x] ∪ {x: σ} |- e: σ0`.
>
> Proof. We construct a derivation of the second assertion from a
> derivation of the first assertion by substituting each use of TAUT
> for x: σ' with x: σ, followed by an INST step to derive x: σ'.
> Note that GEN steps remain valid since if α occurs free in σ
> then it also occurs free in σ'.

# 6 The type assignment algorithm W

> The type inference system itself does not provide an easy method for
> finding, given A and e, a type-scheme σ such that A |- e: σ.

> We now present an algorithm W for this purpose. In fact, W goes a
> step further. Given A and e, if W succeeds it finds a substitution S
> and a type τ, which are most general in a sense to be made precise
> below, such that
>
>     S A |- e : τ.

> To define W we require the unification algorithm of Robinson [6].

值得一遍：

- [6] "A machine-oriented logic based on the resolution principle",
  J.A. Robinson. 1965.

substitution 是 var 到 type 的有限 record，
可以扩展到 type-scheme 到 type-scheme 的函数，
可以扩展到 type context 到 type context 的函数。

TODO 实现这里的算法。

# 7 Completeness of W

这里所说的 A' be an instance of A，
应该只是针对 type context 的 value 而言的。

> The detailed proofs of results in this paper, and related results,
> will appear in the first author’s forthcoming Ph.D. Thesis.

继续读 Damas 的博士论文，也许可以消除歧义。
