---
title: Arithmetic
author: Paul Lockhart
year: 2017
---

# 学习目的

看了《一位数学家的挽歌》之后想进一步看看，
Paul 会如何展示数学的艺术。

# 读后感

这本书对算术的介绍，
会让人对什么是计算有很多反思。
这对我来说是很有用的，
因为我要设计语言来实现各种计算模型。

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

> As a mathematician, I tend not to think of numbers symbolically, or
> even necessarily as quantities.To me, numbers are _creatures_ that
> exhibit behavior, and I occupy my time observing, studying, and
> trying to understand that behavior.

> All I’m trying to say is that we need to be wary and not let our
> familiarity with a particular system blind us to its arbitrariness.

对于程序语言也是如此，要小心。

米制系统起源于法国大革命。

> The only thing that really matters is that we understand what we
> mean by our symbols and notations -- that we own our language and
> feel comfortable and free to play and invent.

对程序语言设计也适用。

# Multiplication

> There is one situation, however, that occurs rather frequently in
> arithmetic, where place-value representation possesses a clear and
> overwhelming advantage: _making copies_.

纯粹的进位制适合用来做乘法，
因为可以用 shift 来做基数幂的乘法，
比如在指令集中有整数 shift 相关的指令。

这里是用几何的方法来证明乘法交换律，
如何在辅助证明系统中形式化这种证明？
好像是不可能的，必须要先用递归函数定义乘法，
然后用归纳法根据递归定义来做证明。
这种形式证明完全没有这里的几何证明的优雅的对称性。

> This picture of multiplication as a rectangle of rocks also clearly
> pertains to the measurement of areas such as a rectangular plot of
> ground, or a wall that needs to be plastered and painted. Usually,
> such areas are sectioned off into a grid, and then the grid squares
> can be counted using multiplication.

几何的证明也可以适用于计算面积，
但是注意 "usually, such areas are sectioned off into a grid"，
是几何的算术化。
可能穷竭法也算是一种算术化。

> [to compare 6 x 8 and 50] One way to proceed would be to stick with
> ten as our grouping size and try to rewrite six groups of eight in
> terms of tens, then see how it compares to five groups. This is what
> most people mean when they say they are multiplying: converting a
> quantity conveniently expressed in one grouping size into piles of
> ten and leftovers.

计算的目的是比较，计算的本质是划归到正规形式，然后做比较。

计算可以定义为指定方向的变化。
所谓变化是一个对象从一种形态变化为等价的形态，
在所有的可能的等价形态所构成的空间中，
一个对象的变化方向有很多，
指定方向就是指定了「计算规则」，
就形成了计算。

这种定义所引起的想象，
适用纯粹的 rewrite system，
比如 lambda calculus 和 interaction net。
而不适用于带有副作用的系统，
比如 turing machine 和冯诺依曼构架。
纯粹的 rewrite system 是计算的本质，
而 turing machine 只是在模拟人类做 rewrite 的过程。

> While we’re on the subject of numbers in the abstract, especially
> their behavior with respect to multiplication, let me just mention
> an extremely important, though patently obvious fact. When we say
> that two plus three is five, what we mean is that two of _anything_,
> together with three more of those same things, whatever they may be,
> makes a total of five of them.Two cows added to three cows makes
> five cows, two years added on to a prison sentence of three years
> makes five years in the slammer, and so on.

用直来证明乘法对加法的分配律。

介绍这些定律，是为了进一步介绍十进制下乘法的算法。
可以想象，作者是从这个目的出发，来安排这一章的内容的。

> The real power of this method comes when we combine it with a
> place-value representation system that uses a consistent grouping
> size, such as the Hindu-Arabic system.  The reason is that now there
> is at least one number that is always easy to multiply by: _the
> grouping size itself_. This is such an important discovery that it
> pays to look at it a bit more generally.

我们从小就学的十进制下的乘法计算，其实是有很多条件才能成立的：

- 必须是 place-value system 而不能是 marked-value system。
- 还必须有 consistent grouping size。

在小时候学印度-阿拉伯表示数的方式时，
没有意识到其实这种表示其实就是一种像算盘一样的计算器，
只是必须在纸上以 append-only 的方式运行。

> I could come up with several arguments in favor of the Hindu-Arabic
> system over its marked-value cousins -- light-weight portability,
> low cost, and so forth—but the truth is that it is this
> place-shifting business that is the real advantage. No transmutation
> of coins or symbols, no sliding rocks around from one line to
> another. It’s just symbols lightly dancing on the page, leaping
> from one column to the next.  That’s the ultimate reason why the
> Roman system had to make way and why pretty much the whole world now
> uses the Hindu-Arabic system.

甚至还能引出和乘方相关的运算律。

```
30 × 200 =
(3 × 10) × (2 × 10 × 10) =
(3 × 2) × (10 × 10 × 10) = 6000
```

> So it’s particularly easy to multiply these sorts of single-digit
> numbers: just multiply the digits and add the shifts.

抽象代数起源于算术相关的运算律，
对于其他代数系统，我们很少有这种熟悉与直觉了。

就像介绍十进制加法的格式一样，
Paul 再次在介绍最终的计算格式之前，
发明了一种稍微有些啰嗦，
但是更容易理解的中间格式。

甚至解释了自己的这种行为：

> Of course, once we have a purely symbolic algorithm like this,
> especially if it makes good sense and we understand why it works,
> then naturally we get lazy and impatient and we want to streamline
> the procedure to make it even easier.  One possible first step is to
> eliminate the imaginary rectangle altogether.

然后 generalize a little：

> This is what we humans always do once we’ve mastered something --
> look for shortcuts.

这里一点一点一字不漏地解释简单例子的样子，
很像 "the little schemer" 的风格。

> While we’re on the subject, an extreme case of small grouping size
> would be a _binary_ system, with a grouping size of two.  There
> would be only two symbols needed, 0 and 1. The only thing to
> remember is that 1 + 1 = 10; there are no pesky times tables at all.

我才发现二进制没有乘法表！

还介绍了科学记数法：

> While we’re on the subject of decimal representation, place
> shifting, and estimation, it often happens (especially in scientific
> work) that one is presented with numbers -- usually measurements of
> some kind -- that are both astronomically large (or microscopically
> small) as well as approximate. In these cases it becomes convenient
> to take advantage of the place-shifting apparatus in order to save
> space. Instead of writing the number four billion six hundred
> million as 4 600 000 000, I could use the more succinct version 4.6
> [9], meaning 4.6 with nine shifts. The approximate number of carbon
> atoms in a 12-gram sample is 6.02 [23] and the radius of a proton in
> meters is about 8.5 [–16] (meaning sixteen antishifts). This is a
> quick and convenient way to encode large-scale information in a
> compact form.

还随手设计了一个 后缀 + 边缀 的 shift 函数。

注意，计算机用的 float，就是根据科学记数法设计的。

使用科学记数法的一个原因是适合带有误差的测量，
为什么这样计数适合误差？

# Division

> Let’s step away from the issues of representation and symbolic
> manipulation for a moment and think about what we are really doing
> when we are doing arithmetic. As I’ve said before (probably too
> many times), arithmetic is the art of arranging quantities for ease
> of comparison.

> The modern view of arithmetic is that it consists of two categories
> of objects: _numbers_ and _operations_. Numbers are the actors;
> operations are the actions.

类似 lambda calculus 的计算模型，
的意义就在于比较 operations 本身。

> ... we often find ourselves asking questions about numbers that
> concern their interplay, and it is the behavior of these various
> arithmetic activities (i.e., operations) that attracts our interest
> and curiosity. At any rate, this is the modern viewpoint -- what
> matters is not the actors but the acting.

> Addition, for instance, is an operation in the abstract (namely,
> pushing piles together) that does what it does independent of
> anyone’s language or representation scheme. Of course, once you
> have adopted such a scheme, then it certainly behooves you to
> investigate what the operation of addition “looks like” in that
> system and to develop algorithms (and mnemonic devices such as
> needles) to make things quick and convenient. That’s what we’ve
> been talking about this whole time. Be that as it may, addition is
> not columns of symbols and carrying; it’s pushing piles together.

上面这种观点也许可以理解为，
先有自然数的最朴素递归定义，
add 也是最朴素的递归函数，
然后再寻找等价的，
但是有更好属性的表示方式与算法。

> Multiplication, on the other hand, is a bit more sophisticated. As
> an operation, we are taking two piles and using one of them to
> dictate how many copies to make of the other.

这里是用群作用（一元函数的迭代）的观点去看二元运算。
这种观点可以处理很多自然二元运算，
比如加法也可以是 how many add1 to add to the other。

> Or, if you prefer, the two piles refer to the number of rows and
> columns of a rectangle of rocks, the total of which is their
> product.

> In any case, multiplication is a perfectly nice operation -- another
> verb in the grammar of arithmetic.

下面就是数与运算的交替扩张过程。

> The thing about verbs is that whenever we have one, we always seem
> to get two. ... Actions that can be done almost always need to be
> undone. And this is especially true in mathematics, where symmetry
> is so highly prized and where the imaginary nature of the place
> allows us the freedom to reverse our actions so easily.

想要自然地引出除法运算。

> So we can view subtraction, for example, as an almost necessary
> linguistic construct. Once we can speak of addition, we then have
> the ability to ask questions: What do I add to this to get that? The
> desire to unadd follows immediately.

> It so happens that making copies (multiplication) has a very natural
> inverse that arises quite frequently in arithmetic: _sharing_.

> The interesting aspect of such a sharing scenario is that we have no
> idea how many each person should receive, only that we want the
> amounts to be equal. That is, whatever this mystery quantity is,
> seven copies of it should make the original total. This is the way
> that sharing is the inverse of multiplication.

用 distributing 来理解人们是如何发明除法运算的具体算法的。
用带有 "Amount Given to Each" 和 "Number Remaining" 的 table，
作为一个中间阶段，来引出最终的除法算法。

以 248 / 7 为例：

```
   Each     Remaining
-----------------------
                 248
  + 10          - 70
               -----
                 178
  + 20         - 140
               -----
                  38
   + 5          - 35
  ----         -----
    35             3
```

> There always seems to be a kind of "conservation of
> energy" principle with things like this. We can do a lot of
> simple arithmetic over and over, or we can work a little
> harder mentally but get it over with faster.

类似设计算法时的 tradeoff。

关于乘法表：

> ... we really are talking about a dictionary -- we’re translating
> from groups of six to groups of ten and back again. That’s what the
> times tables really are: a dictionary from decimal to other grouping
> sizes.

> Truth be told, I’m not actually all that big a fan of the more
> efficient version. I don’t really want to work that hard to ensure
> that I’m always giving away the greatest possible amount each time.
> I’d rather just give out whatever I want to whenever I want to. In
> fact, sometimes I will even give away too much (that is, more than I
> can afford) and then take a little back later just to make my life
> easier with the estimation.

每次都要估算出来最大的商，
让计算不能有中间状态，
因此容易出错。

# Machines

> The simplest mechanical arithmetic device would be a _counter_.

> The trick to getting a machine to do something for us is to find a
> way to remove comprehension and understanding from the process; to
> reduce it to a purely mechanical activity, where not only is it
> unnecessary to understand what is going on, but it is _essential_
> that there be no understanding needed, else how are we going to get
> a bunch of mindless gears and springs to do it?

> So the first step in the construction of any machine is to break
> down the procedure you wish it to perform into a sequence of
> mindless, rote mechanical activities.

TODO

# Fractions
# Negative Numbers
# The Art of Counting
