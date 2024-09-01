---
title: Logic Logic and Logic
author: George Boolos
year: 1998
---

# 学习动机

这是 Boolos 的论文集。
标题很吸引人，
读来领略逻辑学家的风采，
外加开拓眼界。

# studies on set theory and the nature of logic

> Georg Cantor, the founder of set theory, published neither explicit
> axioms nor an intuitive account of the conception of set he was
> assuming.

> Gottlob Frege, the founder of modern logic, did give explicit axioms
> in the logical system he proposed as a foundation for mathematics.
> Unfortunately, he assumed a naive conception of set, on which for
> any condition there is a set whose elements are all and only the
> things for which that condition holds; and such a conception leads
> to contradiction when applied to the condition "is a set that is not
> an element of itself."

为了解决悖论 Russell 和 Zermelo 分别给出了不同的类型论：

- Russell -- stratified
- Zermelo -- cumulative

> ... he [Zermelo] later gave an intuitive account of the underlying
> conception of set he was assuming, which also is widely accepted
> among set theorists today.

> Sometimes the temporal metaphor of sets formed at later and later
> stages is used in place of the spatial metaphor of sets lying at
> higher and higher levels, in which case one speaks of the iterative
> conception.

这个 iterative 我还是第一次听说。

希望 Boolos 的文章能解释 Zermelo 公理的直觉，
并因此让我轻松记住这些公理。

TODO Second-Order Logic
TODO Plural Quantification

## 1 The Iterative Conception of Set

### I. Naive set theory

首先批判康托对集合论的两个非形式定义：

> A set, according to Cantor, is "any collection ... into a whole of
> definite, well-distinguished objects ... of our intuition or
> thought."

> Cantor also defined a set as a "many, which can be thought of as
> one, i.e., a totality of definite elements that can be combined into
> a whole by a law."

这一节介绍朴素集合论，
以及其中的悖论，也就是展示朴素集合论的不一致性。
介绍的方式是直接给出形式语言的定义，
没有完全用散文描述。
非常不错。

- 注意，康托本身没有给出朴素集合论的形式定义，
  所以这里的形式定义只是对康托的解释，
  但是这样的解释显然是合理的。

已经熟悉了在类型论中叙述命题，
看这些用古典的谓词逻辑叙述的命题，
感觉很奇特，像是在一个动态类型语言中，
用谓词来限制变量的类型。

- 这种写命题时的不同感受本身，也很值得探讨。
  比如，在谓词逻辑中，引入变量与给变量类型是分开的。

但是不得不说，确实有些东西是很难在类型论的语言中表达的！
比如定义某个具体的集合存在的公理。
构造主义，没法直接用类型表达，
而是需要构造一个具体的集合出来。

不能说是没法表达，只能说是表达方式不同。
或者可以说，是某些存在型的语句的 pattern，
可以被实现为语言的基础功能。

朴素集合论的公理形式 -- 可以称作 extension axiom，
就是说任意一个谓词都有与之对应的集合
-- "any predicate has an extension"：

- 假设 y 没有在 <formula> 中自由出现。

```cicada
exists (y) And(Set(y), forall (x) Iff(BelongTo(x, y), <formula>))
```

或者假设 `P` 是谓词：

```cicada
exists (y) And(Set(y), forall (x) Iff(BelongTo(x, y), P(x)))
```

另外还有 extensionality axiom 或者说 extensional equality axiom：

```cicada
exists (x, y) And(
  Set(x), Set(y),
  (forall (x) Iff(BelongTo(z, x), BelongTo(z, y))) ->
    Equal(Set, x, y),
)
```

用类型论来表达 extension axiom，
我们可以把 `Set(y)` 和 `BelongTo(x, y)`
分别写在 `exists` 和 `forall` 两个量词中：

```cicada
exists (y: Set) forall (x: y) P(x)
```

但是我们发现，此时没法表达 `Iff` 中反向的箭头了，只能有正向的箭头。

假设我们想直接捕捉朴素集合论的基本公理形式，
可能需要能够对任意一个一元谓词，构造一个其参数的子类型。

```cicada
claim A: Type
claim P: (A) -> Type

claim Extension(P): Type

assert Subtype(Extension(P), A)

(x: A, P(x)) -> Extension(P)
(x: Extension(P)) -> P(x)
```

TODO 用类似 cicada 的语法叙述这里的命题的例子。
TODO 用类似 cicada 的语法叙述罗素悖论。

### II. The iterative conception of set

这一节要给出一个自然的公理体系来避免朴素集合论中的悖论！

> In order to see why a conception of set other than the naive
> conception might be desired even if the naive conception were
> consistent, let us take another look at naive set theory and the
> anomalousness of its axiom -- there is a set of all set.

> According to this axiom there is a set that contains all sets, and
> therefore there is a set that contains itself. It is important to
> realize how odd the idea of something's containing itself is.

对这一个命题的批判主要来自于，
集合不应该包含它自己为元素。
但是考虑在程序语言中实现 Set 的概念，
实现自我引用是很简单的，
集合包含它自己为元素也是可以实现的。

但是，回想康托对集合论的两个非形式定义，
这种批判也是有道理的。

TODO

## 2 reply to Charles Parsons' "sets and classes"
## 3 on second-order logic
## 4 to be is to be a value of a variable (or to be some values of some variables)
## 5 nominalist platonism
## 6 Iteration Again
## 7 introductory note to kurt Gödel's "some basic theoremson the foundations of mathematics and their implications"
## 8 must we believe in set theory?
# frege studies
## 9 Gottlob Frege and the foundations of arithmetic
## 10 reading the Begriffsschrift
## 11 Saving Frege from Contradiction
## 12 The Consistency of Frege's Foundations of Arithmetic
## 13 The Standard of Equality of Numbers
## 14 Whence the Contradiction?
## 15 1879?
## 16 The Advantages of Honest Toil over Theft
## 17 On the Proof of Frege's Theorem
## 18 Frege's Theorem and the Peano Postulates
## 19 Is Hume's Principle Analytic?
## 20 Die Grundlagen der Arzthmetik (with Richard Heck)
## 21 Constructing Cantorian Counterexamples
# various logical studies and lighter papers
## 22 zooming down the slippery slope
## 23 don't eliminate cut
## 24 the justification of mathematical induction
## 25 a curious inference
## 26 a new proof of the Gödel incompleteness theorem
## 27 on "seeing" the truth of the Gödel sentence
## 28 quotational ambiguity
## 29 the hardest logical puzzle ever
## 30 Gödel's second incompleteness theorem explained in words of one syllable
