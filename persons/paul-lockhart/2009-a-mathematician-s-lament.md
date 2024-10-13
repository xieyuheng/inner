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

> Instead of a witty and enjoyable argument written by an actual human
> being, and conducted in one of the world's many natural languages,
> we get this sullen, soulless, bureaucratic form-letter of a proof.

上面这个段也构成对辅助证明系统的批判。
用类型来编码命题，用函数来写证明的过程，
所得到的证明应该也可以被称为是 bureaucratic。

> The effect of such a production being made over something so simple
> is to make people doubt their own intuition. Calling into question
> the obvious, by insisting that it be "rigorously proved" (as if the
> above even constitutes a legitimate formal proof), is to say to a
> student, "Your feelings and ideas are suspect. You need to think and
> speak our way."

在纯粹的形式证明中，如何才能避免这种现象？

> Now there is a place for formal proof in mathematics, no question.
> But that place is not a student's first introduction to mathematical
> argument. At least let people get familiar with some mathematical
> objects, and learn what to expect from them, before you start
> formalizing everything.

也许只是如上面所说，
不应该在教学的开始就介绍形式证明，
应该先介绍例子，让直觉先行。

其实下面马上就有更好的为形式证明的辩护：

> Rigorous formal proof only becomes important when there is a crisis
> -- when you discover that your imaginary objects behave in a
> counterintuitive way; when there is a paradox of some kind.

> But such excessive preventative hygiene is completely unnecessary
> here nobody's gotten sick yet! Of course if a logical crisis should
> arise at some point, then obviously it should be investigated, and
> the argument made more clear, but that process can be carried out
> intuitively and informally as well. In fact it is the soul of
> mathematics to carry out such a dialogue with one's own proof.

关于 proof，数学家心中应该有什么样的 dialogue 呢？
可能可以总结为，应该有让 the argument more and more clear 的 dialogue。

作者总是说数学在于数学家想象中的 pattern，
的确如此，pattern 的概念可以进化到范畴论，
也可以进化到计算机语言方向的 design pattern 与具体的 class，
实际上在辅助证明系统中，就是用 class 来表达抽象的数学结构的，
而抽象的数学结构，就是最典型的数学家想象中的 pattern。

> This is what comes from a misplaced sense of logical rigor:
> _ugliness_. The spirit of the argument has been buried under a heap
> of confusing formalism.

又遇到可以用来批判辅助证明系统的句子了。
不得不说设计这种系统的人是心虚的，
形式证明确实不如古典的证明直观，
那么有直观的形式证明吗？
有好的形式证明的例子吗？

# Exultation

> To me the important step is not the move from rocks to symbols, it's
> the transition from quantity to _entity_ -- the conception of five
> and seven not as amounts of something but as _beings_, like
> hamsters, which have features and behavior.

确实，以数为实体本身已经是一个认知上的飞跃了。
可以简单的认为是等价关系的概念吗？以等价类为实体。
我想不可以这样简单地认为，
因为这种飞跃同样适用于从数到群等抽象数学结构，
以及从群等抽象数学结构到范畴论。

也许，与相对复杂的 propagator network 相比，
interaction network 更简单，更适合作为数学的研究对象。

- 如果把 interaction network 中的无向图改成二分图会怎样？
  注意，二分图、超图、关系、形式语境等等结构是等价的。

我实现了 inet，但是却没有研究很多具体的 inet。
这就像是发现了一个数学领域，
确满足于在门口站着，
而不进去探索。

> ... Specifically, after expanding our notion of collection sizes to
> include zero (the size of the empty collection), we can then define
> new numbers like '-3 ' to be "that which when added to three makes
> zero." And similarly for the other negative numbers. Notice the
> philosophy here -- a number is what a number _does_.

这么说来，
原来皮尔士实用主义格言，
就是数学中的基本常识之一。

> In particular, we can replace the old-fashioned notion of
> subtraction by a more modern idea: _adding the opposite_.
> Instead of "eight take away five," we can (if we wish) view this
> activity as "eight plus negative five." The advantage here is that
> we have only one operation to deal with: adding. We have transferred
> the subtraction idea away from the world of operations and over to
> the numbers themselves. So instead of taking off my shoe, I can
> think of it as putting on my "anti-shoe." And of course my
> anti-anti-shoe would just be my shoe. Do you see the charm in this
> viewpoint?

anti-shoe 真是太搞笑了，哈哈。

> What are so often presented to students as a cold, sterile set of
> facts and formulas are actually the exciting and dynamic results of
> these new creatures interacting with each other -- the patterns they
> play out as a result of their inborn linguistic "nature."

在讲述数与数的概念的扩张的时候说是
new creatures interacting with each other。
在实现 inet 时，
也有很迫切的创造 new creatures 的需要，
就是 dependent type 要求：
- type 中可以包含任意 node，
  因此 type constructor 本身也应该被实现为 node。
- 类型检查中 type 之间等价的判断的需求，
  要求可以判断任意两个 net (rooted net) 是否相等。

这大概要求我们在 data constructor 之外实现 type constructor，
可能不需要区分，都可以用 node 来实现。

> The bold idea was to add new points to the classical Euclidean
> plane. Specifically, we create one new point "at infinity" for each
> direction in the plane. All the parallel lines in that direction
> will now "meet" at this new point. We can imagine the new point to
> be infinitely far away in that direction. Of course, since every
> line goes off in two opposite directions, the new point must lie
> infinitely far away in both directions!  In other words, our lines
> are now infinite loops. Is that a far out idea, or what?

与扩展数系相似的过程是构造射影平面。
这里对射影平面的构造有一个很好的描述，
解释了为什么要添加无穷远点，
因为要让任意两个平行线也能相交在一点（像非平行线一样），
因此摄影平面才有了拓扑意义上 cross-cap 的样子。
有的教科书并不解释为什么。

> Make up anything you want, so long as it isn't boring. Of course
> this is a matter of taste, and tastes change and evolve. Welcome to
> art history! Being a mathematician is not so much about being clever
> (although lord knows that helps); it's about being aesthetically
> sensitive and having refined and exquisite taste.

确实如此，因为在设计数学结构，
或者对已有的数学结构做扩展的时候，
可能方向和选择太多了，
因此必须要用美学来限制。

> This is especially an issue when making extensions or improvements
> to existing structures. We are of course free to do as we wish, but
> usually we want to extend a system in such a way that the new
> patterns do not conflict with the old ones. (Such is the case with
> the arithmetic of negative numbers and fractions, for instance.)

做数学与设计程序是如此相似。

> Occasionally, this compels us to make decisions we might otherwise
> not want to make, such as forbidding division by zero (if a number
> such as '1/0' were to exist, it would conflict with the nice pattern
> that multiplication by zero always makes zero).  Anyway, as long as
> you are consistent, you can pretty much have whatever you want.

甚至需要做妥协的情况都相似。

注意，上面这种扩展而不影响已有系统的性质，
只有在带有 generic function 的程序语言中才能实现。

- inet 好像天生就是 generic 的！

> So the mathematical landscape is filled with these interesting and
> delightful structures that we have built (or accidentally
> discovered) for our own amusement.  We observe them, notice
> interesting patterns, and try to craft elegant and compelling
> narratives to explain their behavior.

写程序很多时候也是为了娱乐，
比如模拟物理世界的就称作是「游戏引擎」。

> This is the Frankenstein aspect of mathematics -- we have the
> authority to define our creations, to instill in them whatever
> features or properties we choose, but we have no say in what
> behaviors may then ensue as a _consequence_ of our choices.

这种说法不一定对，因为为了某种 consequence
而设计数学结构的例子也是有的，
比如 inet 就是为了获得更好的 normalization 相关的属性。

如何研究 inet 呢？
可能开始的时候，就是以 functional programming 中，
简单的数据结构与程序为例子，尝试去实现它们，
然后在这个过程中总结经验。

> I am drawn in by the possibility of a _connection_ -- a new,
> unforeseen relationship that will improve my intuition and perhaps
> permanently change the way I think about these objects. I suppose
> that's really a key part of it for me: I want to be _changed_. I
> want to be affected in a fundamental way.

浪漫。

> Math is not about a collection of "truths" (however useful or
> interesting they may be). Math is about reason and understanding. We
> want to know _why_. And _not_ for any practical purpose.

这里就是要引出「证明」这个概念了，
但是注意这里提到的 why，
why 在于用一个模型去解释，
而对证明而言，这个模型就是逻辑系统。

TODO
