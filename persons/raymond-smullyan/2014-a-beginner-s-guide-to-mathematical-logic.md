---
title: a beginner s guide to mathematical logic
author: raymond smullyan
year: 2014
---

# 学习动机

[2025-01-05] 想要读 Yves Lafont 的 2017-linear-logic-pages，
但是感觉需要复习一下命题逻辑和谓词逻辑。

[2025-01-06] 这绝对是一本隐藏成教科书的谜题书。

# Part I General Background

## Chapter 1 Genesis

> Just what is Mathematical Logic?
> More generally, what is Logic,
> whether mathematical or not?

> Many people have asked me what mathematical logic is, and what its
> purpose is. Unfortunately, no simple definition can give one the
> remotest idea of what the subject is all about. Only after going
> into the subject will its nature become apparent. As to purpose,
> there are many purposes, but again, one can understand them only
> after some study of the subject. However, there is one purpose that
> I can tell you right now, and that is to make precise the notion of
> a proof.

不能很快给出证明论意义上的 proof 的定义。
但是对 syllogism 而言，可以简单讨论一下 valid 和 sound。

> It is important to understand the difference between a syllogism
> being valid and a syllogism being sound. A valid syllogism is one in
> which the conclusion is a logical consequence of the premises,
> regardless of whether the premises are true or not. A sound
> syllogism is a syllogism which is not only valid, but in which, in
> addition, the premises are true.

> I am amused by syllogisms that appear to be obviously invalid,
> but are actually valid!

```
Everyone loves my baby.
My baby loves only me.
----------------------------
Therefore, I am my own baby.
```

哈哈。

提到了布尔在 "An Investigation of the Laws of Thought" 中，
尝试分析斯宾诺莎的推理的有效性，而不在乎合理性。
感觉布尔这本书也值得看看，因为我也喜欢斯宾诺莎。

> The beginnings of mathematical logic went pretty much hand in hand
> with the nineteenth century development of set theory --
> particularly the theory of infinite sets founded by the remarkable
> mathematician Georg Cantor. Before discussing infinite sets, we must
> first look at some of the basic theory of sets in general.

要讲命题演算和谓词演算就必须介绍集合论。

- 对于 linear logic 来说，扮演集合论位置的是什么呢？
  我希望对 linear logic 之类的非古典逻辑，
  我也能获得和古典逻辑一样稳定的直觉。

在介绍 Complementation 时提出了 the universe of discourse 的概念：

> We now consider as fixed for the discussion a set I,
> which we will call the universe of discourse.

这对于区分类型论和谓词演算而言很重要。

> What the set I is will vary from one application to another. For
> example, in plane geometry, I could be the set of all points on a
> plane. For number theory, the set I could be the set of all whole
> numbers. In applications to sociology the set I could be the set of
> all people.

就 proof as program 而言，
这种需要约定 context 的情形，
类似于使用 dynamic scope 的 variable。

> The operations of union, intersection and complementation are the
> fundamental Boolean operations on sets. Other operations are
> definable by iterating these fundamental operations.

> For example, the set denoted A – B [the so called difference of A
> and B], which is the set of all elements of A that are not in B, can
> be defined in terms of the three fundamental operations, since A – B
> = A ∩ B~.

但是其实 difference 的定义并不依赖 context，
而 complementation 的定义依赖 context。

下面讨论 boolean equations，
首先要定义 boolean term，
这就是我们在写解释器时最熟悉的 Exp。

> We will use the capital letters A, B, C, D, E, with or without
> subscripts, to stand for arbitrary sets (just as in algebra we use
> the lower-case letters x, y, z to stand for arbitrary numbers).  We
> call these capital letters (with or without subscripts) _set
> variables_.  By a _term_ we mean any expression constructed
> according to the following rules:

> - (1) Each set variable standing alone is a term.

> - (2) For any terms t1 and t2,
>   the expressions (t1 ∪ t2), (t1 ∩ t2),
>   and t1~ are again terms.

> A Boolean equation is called _valid_ if it is true no matter what sets
> the set variables represent.

这里甚至提出了一个新颖的判断 equal 的方案，
首先把目标集合分解为不相交的子集，
以这些子集的 union 为 normal form 来判断 equal。

- 类似 disjunctive normal form。

这里给出了这个一般方法之后，
让学生自己去证明很多 boolean 等式。

- 由于计算的本质就是化归到正规形式，
  然后比较相等或者比较大小。
  所以这里的例子也可以让学生体验一下，
  在不同于数字的领域做计算的感觉。

看到 "Solutions to the Problems of Chapter 1"，
让人感觉这又是 Ray 的一本谜题书。

- 我可否模仿这种风格给 linear logic 和 inet 写一本书？
- 如果能在写 solution 的时候，用 "how to solve it" 的风格，
  把所有用到的定义都列出来，也是不错的。
  也就是把解题思路也列出来。

> Speaking of logic, here is a little something for you to think
> about: I once saw a sign in a restaurant which read, “Good food is
> not cheap. Cheap food is not good.”

> Problem 1. Do those two statements say different things, or the same thing?

> Solution 1. Logically they say the same thing, namely that no food
> is both good and cheap, but psychologically they convey quite
> different images. The statement “good food is not cheap” tends to
> create the image of good expensive food, whereas “cheap food is not
> good” makes on think of cheap rotten food.

也就是说，一句话的主语会成为人们想象的主体。

## Chapter 2 Infinite Sets

> A complete mathematical revolution occurred in the late nineteenth
> century with Georg Cantor’s theory of infinite sets. Just what does
> it mean for a set to be finite or infinite? The basic idea behind
> this is that of a 1-1 (one to one) correspondence.

关于 denumerable sets：

> Imagine that you and I are immortal. I write down on a piece of
> paper some positive integer, and tell you that each day you have one
> and only one guess as to what the number is. I offer you a grand
> prize if you ever guess my number. Is there a strategy you can take
> that will guarantee that you will get the prize sooner or later?
> Obviously there is: On the first day, you ask if the number is 1; on
> the second day, whether it is 2, and so forth. If my number is n,
> then you will win the prize on the nth day.

> Next, I make the problem a wee bit harder. This time I write down
> either a positive whole number 1, 2, 3, ..., n, ... or a negative
> whole number –1, –2, –3, ..., –n, ... Again you are allowed only one
> guess a day.

通过假设 "a grand prize" 来鼓励读者思考这个问题。
用一个读者和作者之间的游戏，
来鼓励读者思考数学问题。

> We have just seen that the set of all finite sets of positive
> integers is denumerable.  What about the set of all sets of
> integers, whether finite or infinite? Is that set denumerable?
> Cantor’s great discovery is that the set of all sets of positive
> integers is not denumerable! We now turn to the proof of this
> crucial fact, which started the whole mathematical field known as
> set theory.

下面 Ray 再次把技巧性很高的证明，变成了有趣的谜题。

> To illustrate the proof, let us consider a book with denumerably
> many pages -- Page 0, Page 1, Page 2, ..., Page n, .... On each page
> is written down a description of a set of natural numbers. If every
> set of natural numbers is listed somewhere in the book, then the
> book wins a grand prize. But without even looking at the contents of
> the book, one can describe a set of natural numbers that cannot
> possibly be listed on any page!

关于 Continuum Problem：

> Now, there are those mathematicians known as formalists who regard
> this as a sign that the continuum hypothesis is neither true nor
> false in its own right, but depends on what axiom system is
> used. Then there are others, called Platonists (and this includes
> the present author) who believe, quite independent of any axiom
> system, that the generalized continuum hypothesis is either true or
> false in its own right, but we simply don’t know which is the case.

原来 Ray 是 Platonists，
那么关于构造主义逻辑学的证明论，
Ray 会是什么观点呢？
Ray 知道类型论吗？

- 从后面一章可以知道，
  Ray 至少知道罗素意义上的型论。

对 The Bernstein Schroeder Theorem 的证明太有趣了！

## Chapter 3 Some Problems Arise!

关于 The Paradoxes，先叙述了一个关于基数的悖论。
然后是罗素悖论，即关于属于关系的悖论。
从罗素悖论到理发师悖论，
也是一个形成 puzzle 的例子。

> Problem 1. The solution to the barber paradox is really very simple!
> What is it?

> Solution 1. Suppose I told you that there is a man who is more than
> six feet tall and also less than six feet tall. How would you
> explain that?  The obvious answer is that I must be either mistaken
> or lying! There obviously couldn’t be such a man. Similarly, there
> couldn’t be a barber, given the contradictory information given
> about him. Thus the answer to the paradox is that there was no such
> barber.

罗素自己给出的类型论，也是排除带有悖论的集合。
所以说，悖论并不可怕，只是说明了所描述的东西不存在而已。
需要重新审视朴素集合论对集合的定义，
因为这个定义方式允许了带有悖论的集合被定义出来。

但是好像也不用修改定义，
能描述出来带有悖论的集合又怎样呢？
我们知道发现悖论的时候，所描述的集合不存在就好了。

- 在类型检查器中，如果悖论存在，
  并且导致可以证明任意命题，
  也就是有漏洞可以让人绕过类型检查器的检查，
  那就是需要修复的漏洞了。

在 Ray 的语境中，好像每一个悖论，都有一个答案。

> Solution 4. If Hypergame were well-defined, we would have a
> contradiction; hence it is not well-defined. Yes, given a set of
> games not containing Hypergame, one can well define a Hypergame for
> the set, but one cannot well define Hypergame for a set that already
> contains Hypergame.

这是在排除 self-reference 的定义。

假设有一个 normal game 的集合，
此时 hypergame 的定义是每问题的，
然后证明 hypergame 是 normal game，
把它加入可选的集合（副作用），
hypergame 的定义就变了（因为所引用的集合变了），
此时 normal game 的集合就失去 normal 属性了。

用 hypergame 可以给出 Cantor’s theorem 的另一个证明。

- 好像任何一个证明，都可以被 Ray 变成一个谜题。

Two Systems of Set Theory 指：

- 罗素的类型论
- Zermelo 的公理集合论

注意，罗素的类型论是给集合以类型，来限制构造集合的方式。

```
element ∈ set ⊆ type-1
```

type 本身也描述这集合，因此有：

```
type-0 = set of elements
type-1 = power-set(type-0) // any set of elements of type-0
type-2 = power-set(type-2)
...
```

因此有：

```
type-0 ∈ type-1 ∈ type-2 ...
```

罗素的类型论排除了一个集合包含自己的情况。

> The sad thing is that only after Frege completed his monumental work
> on set theory did Russell discover and communicate to Frege the
> inconsistency of his system! Frege was totally crestfallen by this
> discovery and regarded his whole system as a total failure! In
> actuality, his pessimism was quite unjustified, since all his other
> axioms were quite sound and of great importance to mathematical
> systems of today. It was merely his abstraction principle that
> needed modification, and this modification was carried out by
> Zermelo, who replaced the abstraction principle by the following
> axiom:

> - Z0 [Limited abstraction principle].
>   For any property P and any set S, there exists
>   the set of all elements of S having property P.

这是说一个 property 必须要就一个给定的集合来定义。
类型论中更进一步，说引入每个变量的时候都应该指定其所在的集合
（避免了类似于全局变量的 "the universe of discourse"）。

- 我不记得看 Boolos 关于 ZFC 的论文时，
  有看到这个 Limited abstraction principle，
  需要回顾一下 Boolos 关于 ZFC 的论文。

- "For any property P" 中的 property 的定义是什么？
  可以理解为一个可以用程序实现的谓词吗？

  - 我觉得这是需要讨论的。
    毕竟，下面的段落中自由地使用了等词（Equal）来定义 property。
    而 Equal 本身是非常复杂的概念。

  「可用程序实现」这个定语可能太局限了，可能应该改成：
  有良好的定义，使得我们可以判断一个「x 满足 P」的证明是否正确。

  - 「有良好的定义」就是说，有程序可以实现对这种证明的自动检查。
  - 不能用程序实现出来谓词，就是说，不是所有的 property 都有证明生成器。

> Here are some other axioms of Zermelo’s system:

> - Z1 [Existence of the empty set].
>   There is a set that contains no elements at all.

> Before stating other Zermelo axioms, let me tell you a funny
> incident concerning Z1. As a graduate student, I had a course in set
> theory. When the professor introduced the axioms, the first was the
> existence of the empty set. At that point, brash student that I was,
> I raised my hand and said, “The empty set must exist, since if it
> didn’t the set of all empty sets would be empty, and we would then
> have a contradiction.” The teacher then explained that without some
> axioms to the effect, the existence of the set of all empty sets
> could not be proved – indeed, one could not prove the existence of
> any set at all! Of course he was right.  The interesting thing,
> though, is that if instead of Z1, Zermelo had taken the axiom,
> “There exists a set,” the existence of the empty set would follow
> (by using the limited abstraction principle Z0).

即便是在这种老生常谈的公理中，
Ray 还是能发现一些新东西。

> Here is another axiom of Zermelo’s system:

> - Z2 [Pairing Axiom].
>   For any pair of sets x and y,
>   there is a set that contains both x and y.

下面对比了 Zermelo 和 John Von Neumann 用集合构造自然数的两种方案。

- John Von Neumann 的方式可以推广到 infinite ordinals。

## Chapter 4 Further Background

> Before embarking on the subject of mathematical logic proper,
> some more basic mathematical topics are in order.

先导知识还没有结束。
看来想要开始讲数理逻辑还是很难的。

本章介绍的概念：

- tuple
- relation
- function
- mathematical induction
- complete induction
- the least number principle

虽然上面三个是等价的，但是 dependent type
好像是以 mathematical induction 为公理最方便。

还有：

- principle of finite descent

> König’s Lemma. A finitely generated tree with infinitely many
> points must have an infinite path.

> Problem 12. Prove König’s lemma. [Hint: Recall that if a point x
> has only finitely many successors, and if each of the successors has
> only finitely many descendants, then x has only finitely many
> descendants.]

> Solution 12. Call a point rich if it has infinitely many
> descendants, and poor if it does not.  We recall that if a point x
> has only finitely many successors, if each of those successors is
> poor, then x must be poor. Thus if x is rich and has only finitely
> many successors, then it cannot be that all its successors are
> poor. At least one of them must be rich. Thus in a finitely
> generated tree, each rich point must have a rich successor. [A rich
> point with infinitely many successors doesn’t necessarily have to
> have at least one rich successor. For example, in the tree of the
> solution of Problem 10, the origin is rich, but all its successors
> are poor.]
>
> Now, we are considering a tree with infinitely many points and such
> that each point has only finitely many successors. Since there are
> infinitely many points, and all points other than the origin are
> successors of the origin, the origin must be rich, hence must have
> at least one rich successor x1 (as we have seen), which in turn has
> at least one rich successor x2, which in turn has at least one rich
> successor x3, and so forth. We thus generate an infinite path.

注意，Solution 12 不是 constructive 的证明，
因为一般情况下，判断 rich 和 poor 是 undecidable 的。

> Discussion. Brouwer’s Fan Theorem is in fact the contrapositive of
> the König lemma. [By the contrapositive of an implication “p
> implies q” is the proposition “not q implies not p.”] Now, the
> logic we are using in this book is known as classical logic, a
> fixture of which is that any implication is equivalent to its
> contrapositive. There is a weaker system of logic known as
> intuitionist logic, of which Brouwer was a major representative, in
> which not all implications can be shown to be equivalent to their
> contrapositives. In particular, the Fan Theorem can be proved in
> intuitionist logic, but König’s Lemma cannot. Another proof of the
> Fan Theorem will be given shortly, one which is intuitionistically
> acceptable.  Unfortunately, we will not be able to say more about
> intuitionist logic in this work.

Ray 是知道 intuitionistic logic 的。

> Generalized Induction Theorem. A sufficient condition for a
> component relation C(x, y) on a set A to obey the generalized
> induction principle is that all descending chains be finite.

这就是 dependent type 中对递归定义的类型进行归纳证明的基础。

这一章关于 inductive 证明技巧的讨论很精彩。
值得读多次。

> Problem 17. The converse of the Generalized Induction Theorem also
> holds, that is, if the component relation obeys the generalized
> induction principle, then all descending chains must be
> finite. Prove this.

TODO 后面的部分有点难，之后需要了再看。

# Part II Propositional Logic

## Chapter 5 Beginning Propositional Logic

> The so-called logistic program, which is the one we are following,
> is one that is designed to develop all mathematics from just a few
> principles of logic. Thestarting point is Propositional Logic, which
> is the basis of First-Order Logic, as well as of higher order
> logics.

### Formulas

关于括号的讨论，让人想到 lisp 和 forth 的简洁性。

> A way people often write complex formulas to help them to be
> understandable as well as to help checking that they are
> well-formed, is to use different kinds of parentheses within the
> formula, i.e. ordinary parentheses “(and)”, square brackets “[
> and ]”, curly brackets “{ and }”, or even ordinary parentheses of
> different sizes.  These can make matching parentheses more apparent.

正如 scheme 中同时使用 () 和 []。

### Compound Truth Tables

从最简单最稳定的命题逻辑开始，
把形式逻辑中的各种概念理解透彻。

重点是要能用一个故事把整个理论的发展串起来的。
Truth Table 就是最容易理解的开端。

### Tautologies

> In general, by an _interpretation_ of a formula is meant an
> assignment of truth values (T’s and F’s) to all the propositional
> variables in the formula. As we have seen, for a formula with n
> variables, there are 2^n interpretations, each corresponding to a
> row in its truth table. A formula is called a _tautology_ if it is
> always true, that, is, true under all its interpretations.

### Logical Implication and Equivalence

> A formula X is said to logically imply a formula Y if Y is true in
> all cases in which X is true, or what is the same thing, if X ⊃ Y is
> a tautology.

这其实是很有趣的情况。
即 meta theory 中的一些概念，
可以用 theory 中的 expression 来捕捉！

> Given a set S offormulas and a formula X, we say that X is logically
> implied by S if X is true in all cases in which all elements of S
> are true.

上面包括证明论中所写的各种推演规则。
比如：

```
S1
S2
---
X
```

> Two formulas X and Y are called logically equivalent (sometimes just
> equivalent for short) if they are true in just the same cases, or
> what is the same thing, if X ≡ Y is a tautology.

### Finding a Formula Corresponding to a Truth Table

用 disjunctive normal form
-- disjunction of conjunctions。

### Formulas Involving t and f

> Any formula X involving t or f or both, is equivalent either to a
> formula involving neither, or to t alone, or to f alone.

这其实是一种 partial evaluation。

### Liars, Truth Tellers and Propositional Logic

> Before continuing with a formal study of Propositional Logic, I
> would like to interject some problems that illustrate how
> Propositional Logic is related torecreational logic puzzles about
> lying and truth-telling of the type considered in many of my earlier
> puzzle books.

魔术师要揭秘魔术背后的秘密了！

背后的秘密就在于发现如何根据问题，列 boolean 方程。

### Interdependence of the Logical Connectives

这里有趣的等式是：

```
(or p q) = (if (if p q) q)
```

因为：

```
(or (not (or (not p) q)) q)
(or (and p (not q)) q)
(and (or p q) (or (not q) q))
(and (or p q) true)
(or p q)
```

### Joint Denial

> All the five logical connectives, ∼, ∧, ∨, ⊃, ≡, are definable
> from ∼ and ∧ alone (since we have seen that we can first get ∨,
> then ⊃, and hence ≡). Likewise it follows from the last ten
> problems that ∼ and ∨ is a basis for the rest, and so is ∼ and ⊃,
> as well as ⊃ and f.

> Now there is a single logical connective p ↓ q, called joint denial
> from which all the five connectives ∼, ∧, ∨, ⊃, ≡, can be
> defined. p ↓ q means “p and q are both false,” and is thus
> equivalent to ∼ p ∧ ∼ q.

### Alternative Denial

> There is another single connective from which all the others can be
> obtained. It is known as alternative denial, and also as the Sheffer
> Stroke. p|q is read “at least one of p, q is false, or as “p is
> false or q is false.” It is equivalent both to ∼ p ∨ ∼ q and to
> ∼(p ∧ q).

### The Sixteen Logical Connectives

系统地给出所有可能的二元 Logical Connectives。

> A set of connectives is sometimes called a basis (for all the
> connectives) if all sixteen connectives are definable from
> them. Now, we have seen that given any truth table, a formula can be
> found that has it for its truth table – moreover, as seen in the
> solution of Problem 2, a formula using just the connectives ∼, ∨,
> and ∧, so that these three connectives clearly form a basis for all
> the others. But these three are derived in turn, either from ∼ and
> ∧, or from ∼ and ∨. Thus both ∼, ∧ and ∼, ∨ are also
> bases. Other bases are ∼ and ⊃; ⊃ and f; ⊃ and ~⊃; ↓ alone; and |
> alone.

⊃ and f 是 basis，正好是类型系统里有的两个 type。

## Chapter 6 Propositional Tableaux

TODO
