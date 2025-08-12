---
title: proof nets and the identity of proofs
author: Lutz Straßburger
year: 2006
---

# My Motive

想要给 inet 加上依赖类型系统。
不能盲目地直接去加，
而是需要研究 inet 是什么 logic 的 term 语法，
当然就是 linear logic 的 term 语法，
但是如何形成对应关系我还不知道。

proof nets 是为了研究证明之间的等价关系而发展起来的，
在 lambda 演算中，等价关系在于 normalization，
可能证明之间的等价也在于此。
而 inet 之所允许有很简单的 normalize 算法，
就是因为其起源就是为了判断等价关系。

- 读到 2.2：

  注意，这里只介绍了 linear logic 的命题演算，
  甚至没有涉及到 linear logic 的谓词演算，
  因此可能没法直接对 inet + dependent type 的实现问题给出答案。

  - 但是，由于我有了实现 inet 的基础，
    所以这篇论文对于我理解 linear logic 的连词的语义还是有帮助的。

# 1 Introduction

## 1.1 The problem of the identity of proofs

> Although we are able to manipulate and transform proofs in various
> ways, we have no satisfactory notion telling us when two proofs are
> the same, in the sense that they use the same argument. The main
> reason is the lack of understanding of the essence of a proof, which
> in turn is caused by the bureaucracy involved in the syntactic
> presentation of proofs. It is therefore essential to find new
> presentations of proofs that are “syntax-free”, in the sense that
> they avoid “unnecessary bureaucracy”.

> Finding such presentations is, of course, of utter importance for
> logic and proof theory in its own sake. We can only speak of a real
> theory of proofs, if we are able to identify its objects.

## 1.2 Historical overview

> But even after Gentzen’s work, the natural question of asking for a
> notion of identity between proofs seemed silly because there are
> only two trivial answers: two proofs are the same if they prove the
> same formula, or, two proofs are the same if they are syntactically
> equal.

尝试定义 proofs 之间的等价，
尝试用 normalization 来定义，
在寻找合适的 normal form 时，
Prawitz 用了 cut elimination，
这等价于 inline 所有函数调用，
然后再通过 partial evaluation
化简为 normal form。

> Unfortunately, the problem of identifying proofs has not received
> much attention since the work by Prawitz and Lambek. Probably one of
> the reasons is that the fundamental problem of the bureaucracy
> involved in deductive systems (in which formal proofs are carried
> out) seemed to be an insurmountable obstacle. In fact, the problem
> seems so difficult, that it is widely considered to be “only
> philosophical”.

对于设计 dependent type system 与辅助证明系统的人来说，
证明之间的等价是核心的问题。

## 1.3 Proof nets

Proof nets 就是想要用图论中的结构来捕捉证明的语法本质。

正如实现 inet 时，真正的语法是 graph，
有一个 meta language 用来构造 graph。

# 2 Unit-free multiplicative linear logic

> Unit-free multiplicative linear logic (MLL-) is a very simple logic,
> that has nonetheless a well-developed theory of proof nets. For this
> reason I will use MLL- to introduce the concept of proof nets.

## 2.1 Sequent calculus for MLL

> When we define a logic in terms of a deductive system, we have to do
> two things. First, we have to define the set of well-formed
> formulas, and second, we have to define the subset of derivable (or
> provable) formulas, which is done via a set of inference rules.

| symbol  | name (Girard) | identifier |
|---------|---------------|------------|
| `A ⨂ B` | times         | `Both`     |
| `A ⅋ B` | par           | `Through`  |

> The set of formulas is defined via

```cicada
datatype Formula {
  Var(name: String): Formula
  NegativeVar(name: String): Formula
  Through(A: Formula, B: Formula): Formula
  Both(A: Formula, B: Formula): Formula
}
```

> The (linear) negation of a formula is defined inductively via

```cicada
function negation(formula: Formula): Formula {
  match (formula) {
    case Formula::Var(name) => Formula::NegativeVar(name)
    case Formula::NegativeVar(name) => Formula::Var(name)
    case Formula::Through(A, B) => Formula::Both(negation(B), negation(A))
    case Formula::Both(A, B) => Formula::Through(negation(B), negation(A))
  }
}
```

> Note that we invert the order of the arguments when we take the
> negation of a binary connective. This is not strictly necessary
> (since for the time being we stay in the commutative world) but will
> simplify our life when it comes to drawing pictures of proof nets in
> later sections.

> Here is a set of inference rules for MLL- given in the formalism of
> the sequent calculus:

```cicada
let Sequent: Type = List(Formula)

datatype Provable(Sequent) {
  Id(implicit A: Formula): Provable([A, negation(A)])

  Exchange(
    implicit Γ, ∆: Sequent,
    implicit A, B: Formula,
    Provable(Γ + [A, B] + ∆),
  ): Provable(Γ + [B, A] + ∆)

  Through(
    implicit Γ, ∆: Sequent,
    implicit A, B: Formula,
    Provable(Γ + [A, B] + ∆),
  ): Provable(Γ + [Formula::Through(A, B)] + ∆)

  Both(
    implicit Γ, ∆: Sequent,
    implicit A, B: Formula,
    Provable(Γ + [A]),
    Provable([B] + ∆),
  ): Provable(Γ + [Formula::Both(A, B)] + ∆)

  Cut(
    implicit Γ, ∆: Sequent,
    implicit A: Formula,
    Provable(Γ + [A]),
    Provable([negation(A)] + ∆),
  ): Provable(Γ + ∆)
}
```

> Note that the sequent calculus needs (apart from the concept of
> formula) another kind of syntactic entity, called sequent. Very
> often these are just sets or multisets of formulas. But depending on
> the logic in question, sequents can be more sophisticated structures
> like lists or partial orders (or whatever) of formulas. For us,
> throughout these lecture notes, sequents will be finite lists of
> formulas, separated by a comma, and written with a ⊢ at the
> beginning. Usually they are denoted by Γ or ∆.

> 2.1.1 Example:

```cicada
claim A, B: Formula

check [A, B]: Sequent
check [A, B, A]: Sequent
check [A, A, B]: Sequent
```

> We say a sequent ⊢ Γ is _derivable_ (or _provable_) if there is a
> _derivation_ (or _proof tree_) with ⊢ Γ as conclusion.  Defining
> this formally precise tends to be messy. Since the basic concept
> should be familiar for the reader, we content ourselves here by
> giving some examples.

其实定义很简单，一点也不 messy：

- `Provable` 就是一个 datatype。
- derivation 就是构造类型为 `Provable(sequent)` 的数据。

> 2.1.2 Example.
> The two sequents ⊢ !a, a⨂!b, b⨂!c, c
> and ⊢ ((a ⅋ !a) ⨂ b) ⅋ !b are provable:

```cicada
let a = Formula::Var("a")
let b = Formula::Var("b")
let c = Formula::Var("c")

let s1 = [
  negation(a),
  Formula::Both(a, negation(b)),
  Formula::Both(b, negation(c)),
  c,
]

let s2 = [
  Formula::Through(
    Formula::Both(Formula::Through(a, negation(a)), b),
    negation(b),
  ),
]


check Provable::Id(): Provable([negation(a), a])
check Provable::Id(): Provable([negation(b), b])
check Provable::Id(): Provable([negation(c), c])

// 也可以加上 implicit 参数：

check Provable::Id(implicit a): Provable([negation(a), a])
check Provable::Id(implicit b): Provable([negation(b), b])
check Provable::Id(implicit c): Provable([negation(c), c])


check Provable::Both(
  Provable::Id(implicit b), Provable::Id(implicit c),
): Provable([
  negation(b)
  Formula::Both(b, negation(c)),
  c,
])

// 也可以在每个子表达式上都把所证明了的命题带上，
// 把上面的证明也可以写成：

check Provable::Both(
  the(Provable([negation(b), b]), Provable::Id())
  the(Provable([negation(c), c]), Provable::Id()),
): Provable([
  negation(b)
  Formula::Both(b, negation(c)),
  c,
])

check Provable::Both(
  Provable::Id(implicit a),
  Provable::Both(
    Provable::Id(implicit b), Provable::Id(implicit c),
  ),
): Provable([
  negation(a),
  Formula::Both(a, negation(b)),
  Formula::Both(b, negation(c)),
  c,
])


check Provable::Id(implicit a): Provable([negation(a), a])
check Provable::Id(implicit b): Provable([negation(b), b])

check Provable::Through(
  Provable::Id(implicit a),
): Provable([
  Formula::Through(a, negation(b)),
])

// NOTE 下面照搬论文中的证明，其实错了，
// 还少了一个 Exchange：

check Provable::Both(
  Provable::Through(
    Provable::Id(implicit a)
  ),
  Provable::Id(implicit b),
): Provable([
  Formula::Both(Formula::Through(a, negation(a)), b),
  negation(b),
])

check Provable::Through(
  Provable::Both(
    Provable::Through(
      Provable::Id(implicit a)
    ),
    Provable::Id(implicit b),
  )
): Provable([
  Formula::Through(
    Formula::Both(Formula::Through(a, negation(a)), b),
    negation(b),
  ),
])
```

上面的表达式看起来太复杂了，
也许用后缀表达式会好一些？
感觉实现一个后缀表达式的辅助证明系统也是有必要的，
因为就像不同种类的算盘使用起来感觉很不同一样。

- 后缀表达式可以用连续的 `<value> <type> check` 来写证明，
  `check` 的类型为 `<value> <type> -> <value>`，
  与 `the` 类似。

> TODO Here are four different proofs of the sequent: ...
> three of them do not contain the cut-rule,
> and one does contain the cut-rule.

```cicada
TODO
```

上面这些对同一个命题的不同证明是关键的例子。

> Are these proofs really different? Or are they just different ways
> of writing down the same proof, i.e., they only seem different
> because of the syntactic bureaucracy that the sequent calculus
> forces upon us? In the following, we will try to give a sensible
> answer to this question, and proof nets are a way to do so.

> TODO 2.1.3 Exercise. Give at least two more sequent calculus proofs
> for the sequent ...

## 2.2 From sequent calculus to proof nets, 1st way (sequent calculus rule based)

> Although, morally, the concept of proof net should stand
> independently from any deductive formalism, the proof nets
> introduced by Girard very much depend on the sequent calculus. The
> ideology is the following:

> 2.2.1 Ideology. A proof net is a graph in which every vertex
> represents an inference rule application in the corresponding
> sequent calculus proof, and every edge of the graph stands for a
> formula appearing in the proof.

- "every vertex represents an inference rule application"

  在我们上面的形式语言中，等价于 data constructor application，
  而在 inet 的实现中，应该等价于 node 的 application，
  node 能是 cons 也可能是 elim，
  因此这对应于所有的 function application
  加 data constructor application。

- "every edge of the graph stands for a formula"

  一个 formula 就是一个类型。
  edge 可以理解为带有类型的参数。
  在 inet 中，可以理解为带有类型的 HalfEdge。

由此可以看出，inet 中 port 根本就不应该有 sign，
即不应该区分 input port 和 output port 两种，
这些信息应该在类型中，
就像 `Provable::Id` 所连接的元素的类型
-- `A` 与 `negation(A)`。

如果想象给一个 stack-based 后缀表达式语言加类型系统，
就可以把 sequent 的结构以 implicit 的方式实现在语言里，
而在上面定义 `Provable` 的时候，sequent 的结构是需要 explicit 写出来的。

- 别忘了 Sussman 说过，
  设计新语言的意义就在于，
  把之前 explicit 的东西变成 implicit。

注意，这里之所以会形成 graph 而不是 tree，
就是因为 `Provable::Id` 有两个返回值，
多返回值导致了而 graph。

- 如果只有单返回值，并且也可以使用局部变量，就也可以形成 graph，
  毕竟多个返回值是可以用一个 tuple 来模拟的。
- 如果只有单返回值，并且禁止局部变量，就只能形成 tree。
- 如果有多返回值，就算禁止局部变量，也可以形成 graph。

之所以出现（3）与（4）两个例子在 proof net 的形式下等价，
是因为 stack-based 后缀表达式可以揭示出来一些新的等价关系。
正如 de Bruijn notation 相比 lambda calculus 一般的语法，
可以揭示出来 lambda term 之间更多的的等价关系。

> 2.2.3 Theorem Two sequent calculus proofs using the rules in (1)
> translate into the same proof net if and only if they can be
> transformed into each other via trivial rule permutations.

这里所说的 "trivial rule permutations"，
正如在实现 inet 时，
在 meta language 中有多种方式可以构造出来同一个 net，
这种差异显然是平凡的。

- 但是别忘了 inet 还有 interaction！
  interaction 应该可以被视为没有改变一个证明的本质，
  interaction 之前和之后是相同的证明。
  这应该就是这篇文章后面要讲的。

TODO

## 2.3 From sequent calculus to proof nets, 2nd way (coherence graph based)
## 2.4 From deep inference to proof nets
## 2.5 Correctness criteria
## 2.6 Two-sided proof nets
## 2.7 Cut elimination
## 2.8 *-Autonomous categories (without units)
## 2.9 Notes

# 3 Other fragments of linear logic

## 3.1 Multiplicative exponential linear logic (without units)
## 3.2 Multiplicative additive linear logic (without units)
## 3.3 Intuitionistic multiplicative linear logic (without unit)
## 3.4 Cyclic linear logic (without units)
## 3.5 Multiplicative linear logic with units
## 3.6 Mix
## 3.7 Pomset logic and BV

# 4 Intuitionistic logic

# 5 Classical Logic

## 5.1 Sequent calculus rule based proof nets
## 5.2 Flow graph based proof nets (simple version)
## 5.3 Flow graph based proof nets (extended version)
