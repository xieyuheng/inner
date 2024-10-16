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

TODO

# Egypt
# Rome
# China and Japan
# India
# Europe
# Multiplication
# Division
# Machines
# Fractions
# Negative Numbers
# The Art of Counting
