---
title: the principal type scheme of an object in combinatory logic
author: j roger hindley
year: 1969
---

# My Motive

[2025-06-26] 想要了解 Hindley-Milner 类型系统中 Hindley 的贡献。
看起来，Hindley 的论文更强调 combinatory logic 的传统（毕竟他是 Curry 的合作者），
而 Milner 更强调 Data Scott 的指称语义。

# Introduction

> In their book Combinatory Logic [1], Curry and Feys introduced the
> notion of "functional character" (here called "type-scheme") of an
> object of combinatory logic. Roughly speaking, each object of
> combinatory logic ("ob" for short) represents a function or an
> operator on functions; for instance the ob `I` represents the
> identity operator, and we have for all obs X, `I X = X`.

> One of the aims of combinatory logic is to study the most basic
> properties of functions and other concepts, with as few restrictions
> as possible; hence in the simplest form of combinatory logic there
> is nothing to stop a function being applied to itself; thus `X X`
> is an ob for all obs `X`.

> However it is also natural to look at the consequences of
> introducing type-restrictions, formulated by assigning types to the
> obs according to certain rules, to be described later.

> Each type is a linguistic expression representing a set of
> functions, and for any type a the statement "X has type α" is
> intended to mean that the ob `X` represents a member of the set
> represented by `α`. Given types `α` and `β`, the set of all
> functions from the set `α` into the set `β` is represented by the
> type `F α β` (using Curry's notation).

但是恒等函数 `I` 的类型，用 `F` 表示时，
可能有多个，`F α α` 和 `F β β` 等等。

因此需要引入类型变元 `a, b, c`，
称 `I` 的类型为 `F a a`，
并称带有类型变元的类型为 type-scheme
（模仿 formal system 中的 axiom-scheme）。

> We shall see later that all the types which the rules assign to `I`
> have the form `F α α`, so the types of `I` are just those obtained
> by substituting a type `α` for the variables in the type-scheme
> `F a a`.

> It will not be obvious from the rules that every ob `X` has a
> type-scheme with this property. If `X` does have a type-scheme from
> which all the types of `X` (and no extra types) can be obtained by
> substituting types for variables, it will be called a _principal
> type-scheme_ of `X`.

> In §3 of this paper it will be proved that every ob `X` that has a
> type at all, has in fact a principal type-scheme which can be
> effectively determined from the structure of the ob.

> Finally, in §6 it will be shown that if `α` is a principal
> type-scheme of an ob `X`, then any substitution-instance `β` of `α`
> is a principal type-scheme of some ob `Xβ` (which is reducible to
> `X` by the reduction-rules for combinators).

这个结果还挺令人惊讶的。

> Besides its intrinsic interest, this result goes part of the way
> towards justifying a conjecture of Curry's that the alternative
> system of combinatory logic with type-restrictions (in which an
> infinity of basic combinators is postulated, each with a unique
> type) can be defined in the system described here. This point will
> be explained in §5.

是要避免引入 infinity of basic combinators 吗？

# 1 Definitions

Hindley 强调后面要区分语法意义上的等价 `≡`（基础等价关系），
以及经过（方程）解释之后的等价关系 `=`。

> **Definition 1. Obs.**

其实说是 a presentation of magma 就可以了，
可以模仿 presentation of group，写作：

    < S, K | K x y = x, S f g x = f x (g y) >

用 lisp 的语法：

```scheme
(magma (S K)
  (= (K x y) x)
  (= (S f g x) (f x (g y))))
```

和 group 的元素一样，magma 的集合并不包含变元。

这里可以看出，如果想用 magma 来理解 lambda calculus，
之需要假设所有用来定义 generator 的方程都有解，
即形如 `(<name> <arg> ...) = <exp>` 的方程。

正是因为消除了变量，combinatory logic 作为 magma 的试试才显现出来。
如果一开始就研究 lambda calculus，由于其形式之复杂，
让人很难和代数结构联系起来。

> It is assumed that no compound ob is also an atom,
> and that if `(X Y) ≡ (U V)`, then `X ≡ Y` and `U ≡ V`.

> **Definition 2. Types and Type-schemes.**

```bnf
<type-scheme> := <atom-type> | <compound-type-scheme>
<atom-type> := <basic-type> | <type-variable>
<compound-type-scheme> := (F <type-scheme> <type-scheme>)
```

> A type is any type-scheme which contains no variables[3].

这里有一个注释：

> [3]: In [1], Curry finds it convenient to regard types as a
> particular kind of ob, called "F-obs"; the results of the present
> paper apply to this case.

也就是说 Curry 把 type 也理解为 value，
这对于 dependent type 来说是重要的。

> It is assumed that no compound type-scheme is a type-atom and that
> if `(F α β) ≡ (F γ δ)` then `α ≡ β` and `γ ≡ δ`.

> Greek letters denote arbitrary type-schemes, and lower case Roman
> letters from the start of the alphabet denote type-variables.

这里有犯了众多数学书的毛病，
开始规定和类型相关的使用字母的惯例。
这种惯例之使用，只在草稿本或者黑板上说得过去。
设计形式语言的时候，应该用更 scalable 的方式来处理。

也许在类型变量的时候，我可以模仿 lisp 的 lambda，
写成 `(nu (a) (F a a))` 来明确 bound variable 的 scope。

但是，这样写会违背 combinatory logic 的初衷！
即消除 bound variable。

TODO 如何设计组合子来消除那些不是由 lambda 所引入的 bound variable？
Church 发明 lambda 是想得到一种通用的引入 bound variable 的手段，
可以看一下 Church 为什么没有成功。

好像只需要定义 `(N f) = (nu (x) (f x))` 就可以解决问题。
`x` 的多次出现可以用 `S` 处理，
正如在 interaction nets 中使用 dup，
来处理 linear value。

TODO 可否用 formal system 之间的编译，把这两个理论联系起来？

比如，用 `nu` 所表达的 `I` 的类型 `(nu (x) (F x x))`：

```scheme
(same-as-chart
  (nu (x) (F x x))
  (nu (x) (F x (I x)))
  (nu (x) (S F I x))
  (N (S F I)))
```

> In the usual interpretations, the ob `(X Y)` represents the result of
> applying the function `X` to the argument `Y`.

> Each basic type represents a particular set of functions, depending
> on the interpretation one may have in mind.

注意，这里对 basic type 的理解方式。
不是向 SKI 的 magma 代数结构中增加元素，
而是代表其子集！这可以理解为是 structural typing。

> And `F α β` represents the set of all functions from `α` into
> `β`. Thus each type represents a particular set of functions.

> The variables are intended to represent arbitrary types, so that a
> type-scheme a containing variables represents a class of types, each
> type being obtained from a by substituting types for the variables.

如果没有 bound variables 和 scope，
那么 substitution 确实是简单的，
在 combinatory logic 中也常用。

> **Definition 3. Substitution.**

模仿分数的形式，来写 substitution。

> An expression consisting of a type-scheme α beside an ob X is called
> a _statement_, `α X`. The statement `β X` is called an instance of
> `α X` iff `β` is an instance of `α`.

为什么会有这种类似 C 语言的类型声明语法？
可能是 ALGOL 68。
不对！
应该是想说类型代表集合，因此代表谓词，
`α X` 就是谓词作用于 object 结果为真。

如果想用 lisp 语法写的话，可以用 the little typer 的 `(the α X)`。

> **Definition 4. Type-schemes of obs.**
>
> The type-scheme `α` is a type-scheme of `X` iff the statement `α X`
> can be deduced from the axioms below by the rule (F) below.

> Axiom-schemes:

```scheme
(claim (nu (A B C) (-> (-> A B C) (-> A B) A C)) S)
(define (S f g x) ((f x) (g x)))

(claim (nu (A B) (-> A B A)) K)
(define (K x y) x)

(claim (nu (A) (-> A A)) I)
(define (I x) x)
```

> Rule (F):

```scheme
(check (-> A B) X)
(check A Y)
--------------- rule-F
(check B (X Y))
```

> If `α` contains no variables, and `α X` is deduced from axioms
> containing no variables, we say that `α` is a type of `X`.

> **Definition 5. Deductions.**

就是 proof。

> Example 1:

```scheme
(prove (check (-> A A) (S K K)) rule-F
  (prove (check (-> (-> A B A) A A) (S K)) rule-F
    (prove (check (-> (-> A (-> B A) A) (-> A B A) A A) S)
        (type-of S A (-> B A) A))
    (prove (check (-> A (-> B A) A) K) (type-of K A (-> B A))))
  (prove (check (-> A B A) K) (type-of K A B)))
```

> **Definition 6. Instances of deductions.**

就是可以把 proof 中出现的全局类型变量，用 substitution 代换成任意类型。

> **Definition 7. Principal type-schemes.**

type-scheme 在 substitution 下形成序关系，
就是上面定义中的 "instance of"，
在这个序关系中最一般的类型就是 principal type-scheme。

> **Definition 8. Principal deductions.**

proof 在 substitution 下形成的序关系，
此时最一般的 proof 就是 principal deduction。

# 2 Lemmas on substitution

TODO

# 3 Existence of principal type-schemes
# 4 The principal type-scheme of [x].M
# 5 An alternative approach to typed combinators
# 6 Every type-scheme is a p.t.s (principal type-scheme)
