---
title: Measurement
author: Paul Lockhart
year: 2012
---

# 学习目的

看了《一位数学家的挽歌》之后想进一步看看，
Paul 会如何展示数学的艺术。

书中很重要的一点就是习题都是相对自然的，
习题也在故事之中，并且习题没有答案。
自己去思考这些问题很重要。

这本书里的内容，
作为高中的教程内容足够了。

geometry 叫大地测量学，
是真的和 measurement 有关，
所有有趣的概念和定理都可以由测量引出。

- 跟近代的几何学，
  比如高斯曲律，
  和黎曼几何中的度量，
  等等概念，怎么说？
  也可以用 measurement 来自然地引出来吗？
  我想是可以的，毕竟黎曼几何是特殊的度量空间，
  而度量空间的定义就在于度量。

# Reality and Imagination

> I want to talk about a different sort of place. I’m going to call
> it “mathematical reality.” In my mind’s eye, there is a universe
> where beautiful shapes and patterns float by and do curious and
> surprising things that keep me amused and entertained. It’s an
> amazing place, and I really love it.

首先是 physical reality v.s. mathematical reality。

> The thing is, physical reality is a disaster. It’s way too
> complicated, and nothing is at all what it appears to be. Objects
> expand and contract with temperature, atoms fly on and off.  In
> particular, nothing can truly be measured. A blade of grass has no
> actual length. Any measurement made in this universe is necessarily
> a rough approximation. It’s not bad; it’s just the nature of the
> place. The smallest speck is not a point, and the thinnest wire is
> not a line.

> Mathematical reality, on the other hand, is imaginary. It can be as
> simple and pretty as I want it to be. I get to have all those
> perfect things I can’t have in real life. I will never hold a
> circle in my hand, but I can hold one in my mind. And I can measure
> it. Mathematical reality is a beautiful wonderland of my own
> creation, and I can explore it and think about it and talk about it
> with my friends.

> The former is important to me because I am in it,
> the latter because it is in me.

> My idea with this book is that we will design patterns. We’ll make
> patterns of shape and motion, and then we will try to understand our
> patterns and measure them. And we will see beautiful things!

比如在 Coxeter 关于多面体的书中，
开始就是列举多面体，然后做度量。
希望从这本书中，我能学会欣赏各种数学结构的方式。

计算机科学和编程的意义就在于，
在欣赏数学结构的同时，
我们还能用程序去模拟。

> But I won’t lie to you: this is going to be very hard work.
> Mathematical reality is an infinite jungle full of enchanting
> mysteries, but the jungle does not give up its secrets easily.  Be
> prepared to struggle, both intellectually and creatively. The truth
> is, I don’t know of any human activity as demanding of one’s
> imagination, intuition, and ingenuity. But I do it anyway.  I do it
> because I love it and because I can’t help it. Once you’ve been to
> the jungle, you can never really leave. It haunts your waking
> dreams.

# On Problems

> What is a math problem? To a mathematician, a problem is a probe --
> a test of mathematical reality to see how it behaves. It is our way
> of “poking it with a stick” and seeing what happens.

第一次听到这种说法，
提出数学问题本身也是一个艺术。
对于我所关心的数学问题

> So what exactly is the mystery here? The mystery is why.
> Why would a triangle want to do such a thing?
> ... What we are looking for is an explanation.

> Now here’s where the art comes in. In order to explain we have to
> create something. Namely, we need to somehow construct an argument
> -- a piece of reasoning that will satisfy our curiosity as to why
> this behavior is happening.

"The Book of Why" again!

> How will we ever know anything about a purely imaginary realm? The
> point is, it doesn’t matter so much what is true. It matters why
> it’s true.  The why is the what.

> We have discovered what we think may be a beautiful truth, and now
> we need to prove it. This is what mathematicians do, and this is
> what I hope you will enjoy doing yourself.

"A proof explains why"!
这与 "The Book of Why" 中的
"a model explains why" 有什么关系？
如果 logic 只是观察，
那么在数学证明中，
为什么 logic 可以给出关于 why 的最让人满意的解释？
也许在这里，logic 只是工具，
实际上对 why 给出满意解释的还是 model。

> There is no systematic way to create beautiful and meaningful
> paintings or sculptures, and there is also no method for producing
> beautiful and meaningful mathematical arguments.

这前一句话已经被生成式 AI 反驳了，
AI 应该也可以用来生成证明，
不是简单的生成，而是搜索，
因为机器本身就可以验证所搜索到的证明是否正确。

给定 Type 去找证明的搜索，
简单的逻辑式编程都可以完成，
但是人们给出证明通常需要先构建一个辅助的 model，
对于 AI 来说这是比较难的了。

> Also, try to always have five or six problems you are working on. It
> is very frustrating to keep banging your head against the same wall
> over and over. (It’s much better to have five or six walls to bang
> your head against!) Seriously, taking a break from a problem always
> seems to help.

并且像 V.V. 所说，这样放在脑子里的问题，叙述一定要清晰。

对于，三角形的三中线为什么交于一点的问题，
作者对 why 的解释确实构建了强大的 model，
而不只是用 logic。

证明 1：首先是以全等三角形为基础例子，
然后用对称性在这个特例中做证明，
然后再用摄影变换扩展到所有三角形。

> Which brings me to another piece of advice: improve your
> proofs. Just because you have an explanation doesn’t mean it’s the
> best explanation. Can you eliminate any unnecessary clutter or
> complexity? Can you find an entirely different approach that gives
> you deeper insight? Prove, prove, and prove again. Painters,
> sculptors, and poets do the same thing.

Polya 的《怎样解题》也有类似的建议。
对于写程序而言也是如此。
但是对于写程序而言更难，
因为一个项目可能要投入很多时间才能完成，
但是如果能保持实现简单，就能更轻易地重写。

证明 2：连接中线形成一个小三角形，
不停地构造小三角形，可以收敛到一个点。

# Part One: Size and Shape

- In which we begin our investigation of abstract geometrical figures.
- Symmetrical tiling and angle measurement.
- Scaling and proportion.
- Length, area, and volume.
- The method of exhaustion and its consequences.
- Polygons and trigonometry.
- Conic sections and projective geometry.
- Mechanical curves.

## 1

> What are all the different ways to make symmetrical mosaic designs
> using regular polygons?

这与 John Conway 的 "The Symmetries of Things" 的主题类似。
John Conway 的书也契合这里的精神。

## 2

> What is measuring? What exactly are we doing when we measure
> something? I think it is this: we are making a comparison. We are
> comparing the thing we are measuring to the thing we are measuring
> it with. In other words, _measuring is relative_.

measure 的是 proportion，
在 scaling 下不变的 proportion。

> Whenever you create or define a mathematical object, it always
> carries with it the blueprint of its own construction -- the
> defining features that make it what it is and not some other thing.

正如 create 一个 class 的 instance。

> ... one thing to always be aware of is whether you’ve pinned down
> your objects enough to get any information out of them.

> We need to be clear about the extent to which we have specified our
> objects so that we can ask well-posed, meaningful questions.

"pin down an object" 可以理解为所构造出来的 instance，
在所关心的等价关系意义上是等价的。

> Once a shape (or any structure in mathematics) is specified enough,
> the “forces of mathematical nature” then dictate all of its
> behavior. We can certainly try to find out what is true, but we no
> longer have any say in the matter.

## 3

关于面积。

## 4

> The study of size and shape is called **geometry**.

size 包括各个维度的 size -- length、area、volume。

关于 measure the diagonal of a square：

> The problem is not with the diagonal; it’s with our language.

程序语言的设计也是如此，我们需要能表达更多。
也许我不应该说程序语言设计，而应该说类型系统设计，
因为程序语言可以只关心运行时的行为，
而我所关心的是把 program 视为 proof 时的行为。

> Numbers like this that cannot be expressed as fractions are called
> irrational (meaning “not a ratio”).

不是 ir-rational 而是 ir-ratio-nal，
所以不应该依照 rational 来翻译，
而是应该翻译成「非比率数」。

# 5

勾股定理。

# 6

> The answer to the question about the diagonal of a square is not
> √2; it’s the mosaic design.

> The solution to a math problem is not a number; it’s an argument,
> a proof. We’re trying to create these little poems of pure reason.

# 7

计算正五边形的对角线长度。

# 8

> The tangling and untangling of numerical relationships
> is called **algebra**.

要解上面求得的正五边形的对角线长度的代数方程。

# 9

> In general, the main task of the geometer is to translate geometric
> information into algebraic information, and vice versa. This is not
> so much a technical problem as it is a creative one.

# 10

已知圆的周长，求面积。

与这里的证明相比，
小时候学到的证明更好，
即把圆裁开成两个半圆，
然后裁开成锯齿，
然后再拼起来。

> Show that if two points are connected to the same
> arc, the resulting angles must be the same.

中学的圆周角定理。
都忘记如何证明了。

# 11

> So that’s the story. The ratio of circumference to diameter is pi,
> and there’s nothing we can do about it. We’ll simply have to
> expand our language to include it.

哈哈。

# 12

the method of exhaustion 中文是穷竭法。

对于柱体来说，不论是什么形状的底面，
体积等于底面积乘以高，这是需要证明的吗？
我认为不需要。

这里需要证明是因为假设了体积之前只对长方体有定义，
但是别忘了长方体的体积的定义本身就是底面积乘以高。

用穷竭法还可以证明，
将平面按照某个方向拉伸，
图形的面积会根据拉伸的比例变化。
这个现象需要证明吗？可能是需要的。

> Since boxes behave well under dilation so will anything.

用拉伸和体积的关系，
可以证明金字塔的体积是其包围立方体的三分之一。

# 13

用横向切割的穷竭法证明圆锥的体积，
也是其包围圆柱的三分一。
这个是绝对需要证明的了，
并且穷竭法的证明很精妙。

先从特殊的金字塔开始，
然后想办法推广到更一般的图形。
这种先处理特殊，再解决一般的过程，已经出现好几次了。
在我所关心的问题领域（证明论）有类似的例子吗？

TODO

# Part Two: Time and Space

- Containing some thoughts on mathematical motion.
- Coordinate systems and dimension.
- Motion as a numerical relationship.
- Vector representation and mechanical relativity.
- The measurement of velocity.
- The differential calculus and its myriad uses.
- Some final words of encouragement to the reader.
