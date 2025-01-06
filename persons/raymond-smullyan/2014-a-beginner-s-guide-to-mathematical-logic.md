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

TODO
