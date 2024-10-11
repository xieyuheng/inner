---
title: A Mathematician's Lament
author: Paul Lockhart
year: 2009
---

# 学习目的

想找回一点小时候对数学的热爱。

# 读后

正如前言所说，
作者的文笔很好，
读起来是一种享受。

读了之后发现，
可以从这本书里学到的东西太多了，
是可以常读常新的书。

# Lamentation

让人汗毛倒立的比喻，
因为很贴切，让人看到了现代数学教育的荒谬，
所以让人汗毛倒立。

这种感觉像是看庄子文章中的比喻。

数学之为想象的艺术，与音乐与绘画相同，
教数学的形式而不教数学的实践，
正如教音乐而不歌唱，
正如教绘画而不作画。

## Mathematics and Culture

> The first thing to understand is that mathematics is an art.

这里以平面几何的想象为例子，
对计算模型的想象是否也简单而美呢？
比如 inet 和 pnet。

也许在计算模型之中，简单而美的是：

- lambda calculus
- stack machine
- inet 应该也算

在语言设计中简单而美的是：

- scheme
- forth

以三角形面积公式为例子，
对比了从简单的形发现这个公式的证明，
与学校的背公式的教学方式。

我想小学的时候，
在介绍这个公式的时候，
老师应该讲过直觉的证明。
但是我对这个公式的记忆并不带有这里的证明。

> But it's not the fact that triangles take up half their box that
> matters. What matters is the beautiful idea of chopping it with the
> line, and how that might inspire other beautiful ideas and lead to
> creative breakthroughs in other problems -- something a mere
> statement of fact can never give you.

三角形面积公式，
其实代表了对一个问题的探索，
在教学的时候，应该介绍这个探索的过程。

> By removing the creative process and leaving only the results of
> that process, you virtually guarantee that no one will have any real
> engagement with the subject.

欧拉在论文，经常介绍自己探索一个问题的过程。

> By concentrating on what, and leaving out why, mathematics is
> reduced to an empty shell. The art is not in the "truth" but in the
> explanation, the argument. It is the argument itself that gives the
> truth its context, and determines what is really being said and
> meant.

考虑到辅助证明系统，以及写程序就是写证明。
在用形式语言写证明的时候，
如何表达 the explanation 和 the argument 呢？
用形式语言写程序也是如此，
也许 literal programming 是对的，
比如 Knuth 说，唯一能让他还理解他之前代码的方式，
就是 literal programming。

> Mathematics is _the art of explanation_.

## Mathematics in School

> There is surely no more reliable way to kill enthusiasm and interest
> in a subject than to make it a mandatory part of the school
> curriculum.

> Mathematics is _the music of reason_.  To do mathematics is to
> engage in an act of discovery and conjecture, intuition and
> inspiration; to be in a state of confusion -- not because it makes
> no sense to you, but because you _gave_ it sense and you still don't
> understand what your creation is up to; to have a breakthrough idea;
> to be frustrated as an artist; to be awed and overwhelmed by an
> almost painful beauty; to be _alive_, damn it.

写程序也是如此，不是想清楚了一切才开始写，
而是在写的过程中会遇到 a state of confusion。
要通过创造力从疑惑的状态中走出来。

举了一个代数中简洁问题的例子，
用来批判学校教的所谓「应用题」：

> Suppose I am given the sum and difference of
> two numbers. How can I figure out what the
> numbers are themselves?

我好像才意识到，
在 inet 和 pnet 中，
用变量和函数作用来构造二分图的想法，
其实来自代数学中对变量的使用。

变量在于形成连接，
变量通常代表图中的节点，
从对偶图的角度看，也是这些节点连接了相关的边。

> In any case, do you really think kids even want something that is
> relevant to their daily lives? You think something practical like
> compound interest is going to get them excited? People enjoy
> fantasy, and that is just what mathematics can provide -- a relief
> from daily life, an anodyne to the practical workaday world.

Raymond Smullyan 的谜题书是 fantasy 的代表，
其中有魔法树林，其中的鸟儿有神秘鸣叫规律，哈哈。

相比之下，学校教授数学的方式也是荒谬而愚蠢的。

这一章的例子让我回忆起了自己所受的应试教育，
那个漫长的过程是煎熬的，就像在监牢中，只是刑期结束的那天。

幻想着自由之后会是多么快乐：
在做学校作业的疲倦之余，我在读斯宾诺莎的书信集，
看到他与同时代的哲学家和科学家的讨论与探索。
但是我总觉得自己不应该读这些，因为我要考试，
我不应该分散时间与精力到与考试无关的地方。
我梦想高考结束的那天，我不需要进入一个好大学，
只需要有一个图书馆，和自由。

在大学的时候，一位老师说，
你们不应该反感应试教育，
因为你们是应试教育的最大受益者。
所受的「益」是名校的标签吧？
但是在人的成长过程中，
有漫长的时间创造力被抑制，
思想的自由被禁锢，
失去的是什么呢？

其实我知道不应该抱怨，
因为个人没有能力改变这种社会环境，
但是可以像 Lockhart 一样写书，
用优美的行文揭示出来荒谬愚蠢的东西，
然后再写书去展示美的东西。

然而在大学之后，
我并没有做到珍惜时间去学习，
现在知道还来得及吧。

> A good problem is something you don't know how to solve. That's what
> makes it a good puzzle, and a good opportunity. A good problem does
> not just sit there in isolation, but serves as a springboard to
> other interesting questions. A triangle takes up half its box. What
> about a pyramid inside its three dimensional box? Can we handle this
> problem in a similar way?

用 propagator 实现类型系统，
对我来说就是这样一个好问题。

类型系统作为逻辑系统，
与各种不同计算模型的程序运行时的关系，
也是很好的问题。

## The Mathematics Curriculum

> As a result, we have a math curriculum with no historical
> perspective or thematic coherence, a fragmented collection of
> assorted topics and techniques, united only by the ease with which
> they can be reduced to step-by-step procedures.

我们介绍数学的方式有问题，
「阶梯神话」阻止了数学被视为一个有机的整体。
为什么历史很重要，因为在历史中你能读到前人创造的过程。
重点是要思考某些数学是如何被发现的，
因为前人发现的过程可以帮助我们学习如何获得新的发现。

## High School Geometry: Instrument of the Devil

> A proof, that is, a mathematical argument,
> is a work of fiction, a poem.
> Its goal is to _satisfy_.

甚至暗合了构造主义四原则中关于证明的定义。

> A beautiful proof should explain, and it should explain clearly,
> deeply, and elegantly.  A well-written, well-crafted argument should
> feel like a splash of cool water, and be a beacon of light -- it
> should refresh the spirit and illuminate the mind.  And it should be
> _charming_.

# Exultation

TODO
