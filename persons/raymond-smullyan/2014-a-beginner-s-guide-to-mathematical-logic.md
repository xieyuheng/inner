---
title: a beginner s guide to mathematical logic
author: raymond smullyan
year: 2014
---

# 学习动机

[2025-01-05] 想要读 Yves Lafont 的 2017-linear-logic-pages，
但是感觉需要复习一下命题逻辑和谓词逻辑。

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

TODO
