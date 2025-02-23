---
title: Is Parallel Programming Hard, And, If So, What Can You Do About It?
author: Paul E. McKenney
created-year: 2005
updated-year: 2024
source: "https://www.kernel.org/pub/linux/kernel/people/paulmck/perfbook/perfbook.html"
---

# My Motive

[2025-02-23] 为了实现 inet-lisp 而学 parallel programming。
开始是读 2024-shared-memory-synchronization，
但是感觉读地有点累，感觉需要更多的例子才能更好理解。
因此看看这本看起来更轻松的书。

# Chapter 1 How To Use This Book

> The purpose of this book is to help you program shared-memory
> parallel systems without risking your sanity.
>
> - Or, perhaps more accurately, without much greater risk to your
>   sanity than that incurred by non-parallel programming. Which, come
>   to think of it, might not be saying all that much.

# Chapter 2 Introduction

> Parallel programming has earned a reputation as one of the most
> difficult areas a hacker can tackle. ... However, new technologies
> that are difficult to use at introduction invariably become easier
> over time.

## 2.1 Historic Parallel Programming Difficulties

> As indicated by its title, this book takes a different
> approach. Rather than complain about the difficulty of parallel
> programming, it instead examines the reasons why parallel
> programming is difficult, and then works to help the reader to
> overcome these difficulties.

> The earlier restricted availability of parallel hardware is the real
> reason that parallel programming is considered so difficult.

作者的观点是，复杂的技术只要有广泛的市场，
能被大众消费得起，就会在演化中变得简单。

这本书就是在演化的过程中，
推动 parallel programming 变简单的因素之一。

## 2.2 Parallel Programming Goals

> The three major goals of parallel programming (over and above those
> of sequential programming) are as follows:
>
> - 1. Performance.
> - 2. Productivity.
> - 3. Generality.
>
> Unfortunately, given the current state of the art, it is possible to
> achieve at best two of these three goals for any given parallel
> program. These three goals therefore form the _iron triangle of
> parallel programming_, a triangle upon which overly optimistic hopes
> all too often come to grief.

从 inet-lisp 的角度看这三个问题。
首先要保证 performance，
在这个前提下 inet-lisp 的主要特点是 generality，
需要解决的主要问题是 linear variable 带来的 productivity（易编程）问题。
注意，如果处理不好 linear variable 也会损失 performance。

### 2.2.1 Performance

> Performance is the primary goal behind most parallel-programming
> effort.  After all, if performance is not a concern, why not do
> yourself a favor: Just write sequential code, and be happy? It will
> very likely be easier and you will probably get done much more
> quickly.

### 2.2.2 Productivity

硬件贵的时候，程序员的工资不是花钱的大头；
硬件便宜，如果程序员的开发效率低下，很容易就被暴露出来了。

### 2.2.3 Generality

这里从经济学的角度对软件开发中的某些现象给出了不错的分析：

> One such tradeoff is depicted by the green “iron triangle”5 shown
> in Figure 2.3, which shows how productivity becomes increasingly
> important at the upper layers of the system stack, while performance
> and generality become increasingly important at the lower layers of
> the system stack.

> - The huge development costs incurred at the lower layers must be
>   spread over equally huge numbers of users (hence the importance of
>   generality), and performance lost in lower layers cannot easily be
>   recovered further up the stack.

在 stack 的底层，人们更关心 generality 和 performance。

- 因为越是底层越 general，花费可以被均摊。
- 越是底层 performance cost 也会被继承，
  所以 performance 更重要。

> - In the upper layers of the stack, there might be very few users
>   for a given specific application, in which case productivity
>   concerns are paramount. This explains the tendency towards
>   “bloatware” further up the stack: Extra hardware is often
>   cheaper than extra developers.

这里对 "bloatware" 现象的解释很令人信服。
在 stack 的上层，人们更关心 productivity；

- 因为用户少，花费没法被均摊。

> This book is intended for developers working near the bottom of the
> stack, where performance and generality are of greatest concern.

比如程序语言的设计者。

> It is important to note that a tradeoff between productivity and
> generality has existed for centuries in many fields. For but one
> example, a nailgun is more productive than a hammer for driving
> nails, but in contrast to the nailgun, a hammer can be used for many
> things besides driving nails.

对于 productivity 和 generality 之间的 tradeoff，
又给出了「铆钉枪和锤子」的例子。

关于 productivity 比 generality 重要的例子：

> This is eminently achievable. The cellphone is a computer that can
> be used to make phone calls and to send and receive text messages
> with little or no programming or configuration on the part of the
> end user.
>
> This might seem to be a trivial example at first glance, but if you
> consider it carefully you will see that it is both simple and
> profound. When we are willing to sacrifice generality, we can
> achieve truly astounding increases in productivity. Those who
> indulge in excessive generality will therefore fail to set the
> productivity bar high enough to succeed near the top of the software
> stack. This fact of life even has its own acronym: YAGNI, or “You
> Ain’t Gonna Need It.”

TODO
