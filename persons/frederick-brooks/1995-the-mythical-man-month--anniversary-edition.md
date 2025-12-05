---
title: the mythical man-month (anniversary edition)
year: 1995
---

# My Motive

[2025-12-01] 在实现 x-lisp 这个编译器项目的时候，
想学一下项目管理方面的知识。

这本书主要是在总结大型软件项目中的失败的经验。

# Preface to the First Edition

> Although written as separable essays, there is a central argument
> contained especially in Chapters 2-7. Briefly, I believe that large
> programming projects suffer management problems different in kind
> from small ones, due to division of labor.  I believe the critical
> need to be the preservation of the conceptual integrity of the
> product itself. These chapters explore both the difficulties of
> achieving this unity and methods for doing so.  The later chapters
> explore other aspects of software engineering management.

# 1 The Tar Pit
# 2 The Mythical Man-Month

> First, our techniques of estimating are poorly developed. More
> seriously, they reflect an unvoiced assumption which is quite
> untrue, i.e., that all will go well.

> Second, our estimating techniques fallaciously confuse effort with
> progress, hiding the assumption that men and months are
> interchangeable.

## Optimism

> So the first false assumption that underlies the scheduling of
> systems programming is that _all will go well_, i.e., _that each
> fask will fake only as long as if "oughf" fo fake_.

> For the human makers of things, the incompletenesses and
> inconsistencies of our ideas become clear only during
> implementation. Thus it is that writing, experimentation, "working
> out" are essential disciplines for the theoretician.

强调 implementation 就像《实践论》中强调实践。

> In many creative activities the medium of execution is
> intractable. Lumber splits; paints smear; electrical circuits
> ring. These physical limitations of the medium constrain the ideas
> that may be expressed, and they also create unexpected difficulties
> in the implementation.
>
> Implementation, then, takes time and sweat both because of the
> physical media and because of the inadequacies of the underlying
> ideas. We tend to blame the physical media for most of our
> implementation difficulties; for the media are not "ours" in the way
> the ideas are, and our pride colors our judgment.

遇到 bug 时，有时会质疑编译器或硬件。
但是绝大多数时候问题处在自己的代码上。

## The Man-Month

> The second fallacious thought mode is expressed in the very unit of
> effort used in estimating and scheduling: the man-month. Cost does
> indeed vary as the product of the number of men and the number of
> months. Progress does not. _Hence the man-month as a unit for
> measuring the size of a job is a dangerous and deceptive myth_. It
> implies that men and months are interchangeable.

这本书让 man-month 这个词更流行了，
人们知道这个词，但是并不知道这本书是批评这个词的。

> Men and months are interchangeable commodities only when a task can
> be partitioned among many workers _with no communication among
> them_. This is true of reaping wheat or picking cotton; it is not
> even approximately true of systems programming.

man 和 month 之间的关系，
取决于 task 在分割之后的 communication 属性，
对于具体 task 要具体分析其分割后对 communication 的需要程度。

> The added burden of communication is made up of two parts, training
> and intercommunication. Each worker must be trained in the
> technology, the goals of the effort, the overall strategy, and the
> plan of work. This training cannot be partitioned, so this part of
> the added effort varies linearly with the number of workers.

> Intercommunication is worse. If each part of the task must be
> separately coordinated with each other part, the effort increases as
> n(n-l)/2.

> Since software construction is inherently a systems effort -- an
> exercise in complex interrelationships -- communication effort is
> great, and it quickly dominates the decrease in individual task time
> brought about by partitioning. Adding more men then lengthens, not
> shortens, the schedule.

# 3 The Surgical Team

> The conclusion is simple: if a 200-man project has 25 managers who
> are the most competent and experienced programmers, fire the 175
> troops and put the managers back to programming.

这篇文章对 team 结构的描述，
可以看出来 1960 年编程工作的情况和 2025 年有着翻天覆地般的差异。
因此这本书的所有结论都应该谨慎对待。

模仿外科手术团队：

- the surgeon -- 主程序员 -- 类比外科医生，是就这个项目而言团队唯一的程序员。
- the copilot -- 副手 -- 给主程提建议、研究方案，很少写代码，如果写也对代码不负责。
- the administrator -- 负责帮助主程处理人事工作，但是所有的人事决策都有主程拍板。
- the editor -- 帮助编辑主程所写的文档。
- two secretaries -- 服务于 administrator 和 editor。
- the program clerk -- 帮助记录和整理程序的输入输出文件。
- the toolsmith -- 专门帮助主程写辅助工具类程序，包括 libraries。
- the tester -- 帮助主程做测试。
- the language lawyer -- 帮助主程使用程序语言，比如研究程序语言的新特性。

> This, then, is how 10 people might contribute in well-differentiated
> and specialized roles on a programming team built on the surgical
> model.

这看起来简直荒谬绝伦。
但是不能因此摒弃这本书，
因为所有古代的书都有类似的问题。

## Scaling Up

> So far, so good. The problem, however, is how to build things that
> today take 5000 man-years, not things that take 20 or 30.

既然已经说过了 man-month 不能作为 progress 的度量单位，
这里却又在用 man-month 作为项目规模的度量单位。
但是对项目的规模度量，其实就是对项目 progress 完结的度量。
这不是互相矛盾吗？

# 4 Aristocracy, Democracy, and System Design

> I will contend that conceptual integrity is the most important
> consideration in system design. It is better to have a system omit
> certain anomalous features and improvements, but to reflect one set
> of design ideas, than to have one that contains many good but
> independent and uncoordinated ideas.

> Because ease of use is the purpose, this ratio of function to
> conceptual complexity is the ultimate test of system design.
> Neither function alone nor simplicity alone defines a good design.
> This point is widely misunderstood. Operating System/360

因为一个系统的易用程度与其功能成正比，
但是与其复杂度成反比。

不要为了简单的功能设计复杂的系统，
尽量设计简单的系统来实现复杂的功能。

但是这一章很虚伪，
作者不能接受金字塔状的团队不民主的事实，
也不能接受团队的底层实现人员被剥夺了创造性工作这个事实。

作者发现了生产力方面的问题，
但是没有考虑生产关系上的创新，
而是选择了抑制人们创造性的生产关系方案，
来保证产品在设计上的一致性得以维系。

zmq 团队的 C4，是从生产关系上创新来解决生产力问题的例子。

# 5 The Second-System Effect

TODO

# 6 Passing the Word
# 7 Why Did the Tower of Babel Fail?
# 8 Calling the Shot
# 9 Ten Pounds in a Five-Pound Sack
# 10 The Documentary Hypothesis
# 11 Plan to Throw One Away
# 12 Sharp Tools
# 13 The Whole and the Parts
# 14 Hatching a Catastrophe
# 15 The Other Face
# 16 No Silver Bullet - Essence and Accident
# 17 "No Silver Bullet" Refired
# 18 Propositions of The Mythical Man-Month: True or False?
# 19 The Mythical Man-Mollth after 20 years
# Epilogue
