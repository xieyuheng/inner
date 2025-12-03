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
# 4 Aristocracy, Democracy, and System Design
# 5 The Second-System Effect
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
