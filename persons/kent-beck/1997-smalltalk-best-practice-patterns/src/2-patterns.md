---
title: 2. Patterns
---

> Developers don’t ever develop from scratch. Many problems recur in
> various guises throughout development. Mature engineering
> disciplines capitalize on the repetitive nature of development by
> collecting handbooks of adequate solutions to recurring problems.

# WHY PATTERNS WORK

> Here is the Big Assumption: There are only so many things objects
> can do. Not in the sense that there are a limited number of
> applications, because there will always be new domains to model, but
> in the sense that the same structures of objects keep appearing over
> and over, regardless of the application.

数学结构作为和证明相关的 pattern 也是如此。

> The problems in the construction of objects are universal. You have
> to name classes, relate classes via inheritance and delegation,
> relate methods in the same class and different classes, name
> variables, and so on. Patterns record these problems and how to
> approach solving them.

注意，这里提到的都是 class + methods 风格中的 patterns，
那么 record + functions 是否需要总结出新的 patterns 呢？

> When you want to improve communication, you have two choices; either
> increase the bandwidth so you can communicate more bits or increase
> the context shared between sender and receiver so the same number of
> bits mean more. The first is impractical, especially in the presence
> of reuse (one developer can only be sliced into so many pieces), so
> we must find some way to make our words mean more.

pattern 是 context shared 的一部分。

同时这也解释了 pair programming 为什么实用。

> When you want to optimize any process, the first strategy to employ
> is to find how the common case is simpler than the general case. For
> software engineering, the general case is the programming
> language. Commonly, though, we don’t go all the way back to the
> programming language to solve problems.  We look at what we did last
> week and last year, and what our friends did last week and last
> year, and do something similar.

> Patterns are a literary form for capturing and transmitting common
> practice.  Each pattern records a recurring problem, how to
> construct a solution for the problem, and why the solution is
> appropriate.

这是很好的对 pattern 的定义，
前一句讲了 pattern 的属性，
后一句讲了如何使用。

- 也许应该说是对 design pattern 的定义，
  而这里只是省略了 design，
  因为 pattern 的概念本省更广。
