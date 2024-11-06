---
title: PROGRAMMING A PROBLEM-ORIENTED-LANGUAGE
author: Charles H. Moore
year: 1970
---

# 学习动机

[2024-11-07]

在实现 c 版本的 inet 时，
不知道如何设计 rule 编译出来的 bytecode 了。
也许 forth 的 inner interpreter 可以给一些启发。

比如，如何命名这些数据类型？

- word？
- opcode？

我很久之前就读过这本书，但是现在又忘记了。

readonly.link 整理的版本（有些排版没整理完）：
- <https://readonly.link/books/https://books.xieyuheng.com/programming-a-problem-oriented-language/book.json/-/1-introduction.md>

# Preface

> This is an unpublished book I wrote long ago. Just after I'd written
> the first versions of Forth. Perhaps it explains the motivation
> behind Forth. There is some interest in it, and publishing is no
> longer relevant since I can post it on my website.

# 1 Introduction

## 1.1 The Basic Principle

> **Basic Principle: Keep it Simple.**

> As the number of capabilities you add to a program increases, the
> complexity of the program increases exponentially. The problem of
> maintaining compatibility among these capabililties, to say nothing
> of some sort of internal consistency in the program, can easily get
> out of hand.  You can avoid this if you apply the Basic Principle.
> You may be acquainted with an operating system that ignored the
> Basic Principle.

> It is very hard to apply. All the pressures, internal and external,
> conspire to add features to your program. After all, it only takes a
> half-dozen instructions; so why not? The only opposing pressure is
> the Basic Principle, and if you ignore it, there is no opposing
> pressure.

> **Corollary 1: Do Not Speculate!**

> **Corollary 2: Do It Yourself!**

> Before you can write your own subroutine, you have to know how.
> This means, to be practical, that you have written it before; which
> makes it difficult to get started. But give it a try. After writing
> the same subroutine a dozen times on as many computers and
> languages, you'll be pretty good at it. If you don't plan to be
> programming that long, you won't be interested in this book.

> But suppose everyone wrote their own subroutines? Isn't that a step
> backward; away from the millenium when our programs are machine
> independent, when we all write in the same language, maybe even on
> the same computer? Let me take a stand: I can't solve the problems
> of the world. With luck, I can write a good program.

## 1.2 Preview

> I'm going to tell you how to write a program. It is a specific
> program; that is, a program with a specific structure and
> capabilities. In particular, it is a program that can be expanded
> from simple to complex along a well-defined path, to handle a wide
> range of problems, likewise varying from simple to complex. One of
> the problems it considers is exactly the problem of complexity. How
> can you control your program so that it doesn't grow more
> complicated than your application warrants?

"how to write a program. It is a specific program ..."
是说要用一个例子来介绍 forth 是如何在解决问题的过程中，
被自然而然地逐步设计出来的。

> First I'll define "input", and mention some general rules of
> programming that apply to all programs, whether they have input or
> not. Actually we will be almost exclusively concerned with input, so
> I've not much to say about programs lacking input.

这里的带有 "input"，就是 forth 的外部解释器。

> By admitting input, a program acquires a control language by which a
> user can guide the program through a maze of possibilities.
> Naturally, this increases the flexibility of the program, it also
> requires a more complex application to justify it. However it is
> possible to achieve a considerable simplification of the program, by
> recognising that it needs a control language as a tool of
> implementation.

这里说的 "control language" 就像给用户提供 forth words，
让用户能自己通过 compose words 来灵活地解决问题。

> The next step is a problem-oriented-language. By permitting the
> program to dynamically modify its control language, we mark a
> qualitative change in capability. We also change our attention from
> the program to the language it implements. This is an important, and
> dangerous, diversion. For it's easy to lose sight of the problem
> amidst the beauty of the solution.

"dynamically modify its control language"
就是用户能定义新的 forth word。

> In a sense, our program has evolved into a meta-language,
> which describes a language we apply to the application.

# 2 Programs without input

TODO

# 3 Programs with input
# 4 Programs that grow
# 5 Programs with memory
# 6 Programs with output
# 7 Programs that share
# 8 Programs that think
# 9 Programs that bootstrap
