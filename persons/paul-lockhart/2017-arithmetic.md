---
title: Arithmetic
author: Paul Lockhart
year: 2017
---

# 学习目的

看了《一位数学家的挽歌》之后想进一步看看，
Paul 会如何展示数学的艺术。

# Dear Reader

> It’s fun to count and arrange things. We like doing it, and we have
> even developed it into something of a folk art. This art is called
> _arithmetic_. Arithmetic is the skillful arrangement of numerical
> information for ease of communication and comparison.  It is a fun
> and enjoyable activity of the mind and a relaxing and amusing
> pastime -- a kind of "symbol knitting," if you will.

这本书讲 Counting，与 Paul 的前一本书 Measurement 相互补充。

> My hope is that by reading this book you will be inspired to try it
> out for yourself and to experience firsthand the simple pleasure and
> satisfaction that comes with numerical fluency.

如果想像 Paul 一样写书来介绍我所爱的数学领域，
关键词可能是 Computing -- 定义为 directed change 的 computing。

# Things

注意论述中的 Why！

> Why exactly do we bother counting things? The truth is, we usually
> don’t. Most situations do not really call for any careful counting,
> just a sense that “we have plenty enough” or “we’re way short.”
> The main reason why we count is to _compare_.

"to compare" 的模型内没法解释所有的 counting 行为，
所以有下面的 "curiosity counting"。

> Then there is curiosity counting
> -- counting for the sake of pure wonder.

> Whatever the purpose or rationale, we occasionally find ourselves
> wanting to know how many of something there are.  This is where
> arithmetic begins, with _desire_. There’s no point counting
> something you don’t care about. Don’t ever do that.

> The point is that there are things, and we sometimes want to count
> them. Actually, there are some quantities that we don’t so much
> count as _measure_. What we do when we measure something -- and this
> could be an amount of milk, or someone’s height, or a piece of land
> -- is we take some agreed-upon amount, known as a _unit_ (e.g., a
> cup, an inch, or an acre), and we count the number of those. This
> has the effect of turning something smooth and continuous into
> something discrete and lumpy. So let’s confine ourselves to
> thinking about count- ing distinct individual items.

Paul 的前一本书的主题就是 Measurement。

> One problem is that we are a little too good at pattern
> recognition. Our brains are quite good at storing patterned
> information; we don’t do so well with random spatters. Here are two
> examples of sixness:

```
    o     o
       o                 o o
  o           o          o o
                         o o
       o
```

为什么只有摆整齐了才方便人们比较？
因为人们擅长识别模式，
所谓「整齐」就是具有模式。

> OK, so we don’t have instant number perception past five or so, and
> we have trouble with chaos and disorder. Also, we are careless
> daydreamers who can’t be trusted to perform one simple mind-numbing
> and repetitive task. So what do we do?  We come up with art and
> science, that’s what. We develop a creative and entertaining craft
> of counting well, using our pattern-seeking minds.

对于为什么我们发明算术，
给出了浪漫的，并且符合直觉主义的解释。

# Language

用 representation 的功能来解释 language 的起源。

> In particular, when we start designing arithmetic systems, it will
> be crucial to keep clear the distinction between a number and its
> representation. A lot of people are walking around in a muddle about
> this, and I don’t want you to be one of them.

也就是设计程序语言的时候，要区分语法和语义。

> The history of language (and arithmetic in particular) is a history
> of increasingly abstract and portable substitutions: from actual
> antelope to drawings of antelope, to rocks, sticks, or fingers, to
> scratches on bones, to verbal utterances (i.e., words), to abstract
> symbols.

> My guess is that each of these developments in the evolution of
> language arose from the need to explain and communicate information.
> Mathematics, in particular, is an explanatory art form, and
> ultimately all of its structures arise as “information carriers”
> for the purpose of explaining a pattern or idea.  Numbers are simply
> the most basic kind of mathematical information.

# Repetition

讲最简单的，无进位制的自然数编码。

```cicada
datatype Nat {
  zero: Nat
  add1(prev: Nat): Nat
}
```

在发展出进位制之前，人们先发展出了 grouping 的技术，
比如中文用「正」字来计数，就是五个一组。

> However we wish to communicate numbers -- by words, gestures, or in
> written form -- repetition, grouping, and abbreviation are the
> natural linguistic means. It’s up to the users of such languages to
> decide on a convenient grouping size and how they wish to indicate
> both groups and leftovers.

"repetition, grouping, and abbreviation"
这里总结出了三种语言中的工具。
程序语言设计中可以总结出类似的工具吗？

- repetition 是自然数最自然的结构。
- grouping 和 abbreviation 是为了解决人类对 repetition 的认知问题。
  就像设计程序语言是，考虑的复杂度不是算法的复杂度，
  而是就人类认知而言的复杂度。

在程序语言的语法设计中，
从最底层的角度看，
我们在设计 token 的序列来代表语法树。
语法树本身是工具还是被表达的对象呢？
可能二者都是，在不同的层次有不同的理解。

- 就像编程领域的 pattern language 中，
  有 class 层次的 design pattern，
  也有 app 层次的 app pattern。

- 也许形成程序语言设计的科学，
  就在于总结这些不同层次的语法工具。

"repetition, grouping, and abbreviation"
这三种工具再进一步就是进位制了，
进位制本身也是一种语法工具。

# Three Tribes

非常有趣的故事。
构拟历史。
假设了三个原始部落，用不同的 grouping size -- 5、4、7。

不同的 grouping size 会产生不同的「文明」。
十进制确实让人们对「十」有特殊的认知与感情。

> As humble as it may appear, the Piles of Rocks system is actually a
> very powerful calculating device. To use it, you simply rearrange
> the rocks into whatever different grouping sizes you wish.

然后石子可以作为他们之间数字换算的计算器。

> This is what is known as “doing arithmetic.” You have taken
> numerical information presented in one form, and you have
> reorganized it into another. Congratulations.  Arithmetic doesn’t
> actually get much more complicated than this, so if rearranging
> piles of rocks into different-sized groups makes sense to you, then
> it’s probably going to be pretty smooth sailing from here on out.

从一个最简单的例子，给出了 "doing arithmetic" 的定义。

> These three tribal number languages are examples of what are called
> _marked-value systems_, meaning that each word or symbol has a
> fixed, definite meaning. In particular, for representation systems
> like these, the order of the symbols doesn’t matter.

原始部落在设计语言，顺序无关是语言的重要属性。
进位制就需要位置相关了。

与 marked-value 相对的是 place-value。

其他的 marked-value systems 的例子有：

- 钱。
- 扑克牌的手牌。

|      | Set  | Bag  |
|------|------|------|
| 顺序 | 无关 | 无关 |
| 重复 | 不可 | 可   |

顺序无关，因此有多种表示方式，
为了方便比较，需要整理到一种表示方式
-- 就是 normal form 的概念了，
用 normal form 可以实现等价的判断。

> ... nothing could be simpler than to compare two numbers that are
> both nicely packed into groups and organized by size.

> This way of organizing and comparing is called _lexicographic ordering_.

- [Wikipedia / Lexicographic order](https://en.wikipedia.org/wiki/Lexicographic_order)

# Egypt

> One of the earliest and simplest examples of a marked-value system
> is the one used by the ancient Egyptians from about 3000 bc.

> Also, as with most numerical representation systems used by humans,
> the grouping size is ten.

> In particular, ten is well beyond the limit of most people’s number
> perception abilities. So in order to design a convenient
> marked-value system, the Egyptians first needed to solve the
> perception problem.

> Another wonderful innovation, which replaces Piles of Rocks as a
> calculation tool, was the introduction of counting coins. The idea
> is simply to make objects (usually wooden or ceramic chips of some
> kind) that are marked so as to indicate their value.

类似某些桌游中的硬币。

> An abacus is simply a manual representation system; that is, a way
> of representing numbers by things that can be held and manipulated.

有了一个对「算盘」的一般定义。

# Rome

> The Roman system provides a rather different solution to the
> problems of repetition and perception.  The grouping size is still
> ten, and being a marked-value system, it has special names for a
> group, a group of groups, and so on.

> In the Roman system these are represented by various letters of the
> Roman alphabet. Thus, leftovers were denoted by the letter I, a
> group of ten was represented by an X, and a group of groups by the
> letter C. A group of ten hundreds was written M.

> What we are doing here is making a trade. We are eliminating the
> need for stacking but at the cost of having more symbols to get used
> to. Once again, the new symbols (and their English equivalents) are:

| I | one          |
| V | five         |
| X | ten          |
| L | fifty        |
| C | one hundred  |
| D | five hundred |
| M | one thousand |

罗马 tabula abacus 用一个纸质棋盘就可以玩，
比中国算盘要简单一些，可能更适合给小孩子学习用。

这里提到的 trade-off 很有趣，
与 marked-value 的硬币相比，
tabula abacus 的状态很容易被打翻。
类似选择数据结构和算法时的 trade-off，
但是这里真就是物理意义上的 fragility。

# China and Japan

把中国和日本放到一起说已经很搞笑了，
因为从历史发展来看，日本根本不值一提。

说中国发明数字是 200 BC 也很搞笑，
因为那个时候已经西汉了，
其实至少在 1600 BC 到 1000 BC 的商朝，
就有甲骨文对数字的记载了。

还说汉字是任意的，
其实每个字都有从甲骨文开始有趣的故事和演变。

算盘介绍的是传到日本的一四珠算盘。
其实中国在有算盘之前就有算筹了，作者也不知道。

> The main difference between the Japanese soroban and the Roman
> tabula is that we don’t need to carry around a bowl of calculi; the
> stones (in the form of beads) are built in. It’s just a question of
> indicating which of them “count.”

> No matter what plan you come up with, it will have advantages and
> disadvantages, pros and cons, and will undoubtedly require a fair
> amount of practice before you get really comfortable.

# India

这章算是介绍到了人们从小学习到的算术了，
但是，经历了对算术历史的了解，
算术与计数所用的语言是密不可分的，
并且现在我们能在比较不同语言下算术的不同性质，
从而认识到我们现代算术的优越性。

> In fact, getting stuck (say on 7 + 8, for instance) is one of the
> best things that can happen to you because it gives you an
> opportunity to reinvent and to appreciate exactly what it is that
> you are doing: you are rearranging numerical information for
> comparison purposes.You have an amount of things, seven of them
> added to eight of them, and that is a complete and unambiguous
> description of what you have. There is no need to do anything.The
> number 7 + 8 is not a problem requiring a solution, nor a question
> seeking an answer. It is a number, that’s all. Oh, what’s that you
> say? You want to compare it with something else? Well, all
> right. That’s another matter.

重申了算术是为了比较。
回顾开篇所说的：

> Arithmetic is the skillful arrangement of numerical
> information for ease of communication and comparison.

算术是为了比较，
一般的计算是否也是呢？
就函数式计算和 lambda 演算而言，
确实是为了比较，因为计算的目的就是为了得到 normal form。

我经常说，计算被定义为「有方向的变化」，
但是计算是为了什么？
rewrite system 一类的计算模型显然是为了比较，
turing machine 一类的计算模型是在模仿人类的计算，
因此也是为了比较。

「我想知道计算的结果」的前提是，
计算的对象编码了我感兴趣的信息，
计算的方法代表了我感兴趣的变化。

在学到高等的数学和计算机科学时，
还记得这些最初的动机吗？

> When we say that seven plus eight is fifteen what we are really
> saying is that seven combined with eight can be rearranged into a
> group of ten and five leftovers. In fact, the very word fifteen is
> simply an abbreviation for five and ten.

这里介绍了一种计算加法的方式，
可以不用太在意数字的对齐。

|   | 1 | 8  |    | 4  |
|   |   | 6  | 9  | 7  |
|---|---|----|----|----|
|   | 1 | 14 | 9  | 11 |
|---|---|----|----|----|
|   | 2 | 4  | 10 | 1  |
|---|---|----|----|----|
|   | 2 | 5  |    | 1  |

相比需要对齐的方法：

```
  18 4
   697
------
```

> So the Hindu arithmeticians came up with a wonderful, revolutionary,
> and hilarious solution: create a new symbol to stand for
> _nothing_. We simply expand our number language to include a
> “blank” or placeholder symbol, so we can tell if a column is
> intended to be empty (as opposed to just being a space between
> consecutive symbols).

其中空格的个数也是要精确控制的，
这可能就是印度人发明 0 的原因。

```
  1804
   697
------
  2501
```

计算 453 + 866 - 395 时，
可以不用急着化为 normal form，
先得到 12 11 9，然后再减 395。

> I guess my real point here (and with this book in general) is that
> there are many good strategies for encoding and manipulating
> numerical information, and you can use them in any way you see
> fit. Instead of thinking in terms of systems and rules, think of it
> more as options and tools at your disposal.  There’s no rule saying
> I can’t have forty-seven in the tens column if I want to; the only
> issue is whether I know what I mean and what I want. So play around!

# Europe

TODO

# Multiplication
# Division
# Machines
# Fractions
# Negative Numbers
# The Art of Counting
