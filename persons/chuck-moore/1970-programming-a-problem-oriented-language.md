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

> The simplest possible program is one that has no input. That is a
> somewhat foolish statement, but if you'll give me a chance to
> explain we can establish some useful definitions.

> First consider the word "input". I want to use it in a specific sense:
>
> - Input is information that controls a program.

这里的 input 的定义，看起来就是函数的参数。

让用户用调用函数的方式使用程序，看来是不合理的。
但是让用户使用后缀表达式的 word 来使用程序看来是合理的，
比如后缀表达式的计算器。
可以以这种计算器为典型例子，来理解这里提到的 input。

> In particular, I do not consider as input:
>
> - Moving data between media within the computer. For instance,
>   - copying tape onto disk, or disk into core.
> - Reading data into the computer. This is really a transfer between media:
>   - from card to core.

> In order to sharpen your recognition of input, let me describe a
> program that has input. Consider a program that fits a smooth curve
> through measured data points. It needs a lot of information in order
> to run: the number of data points, the spacing between points, the
> number of iterations to perform, perhaps even which function to fit.
> This information might be built into the program; if it is not, it
> must be supplied as input. The measured data itself, the object of
> the entire program, is not input; but must be accompanied by input
> in order to to intelligible.

这个例子非常适合 "The Little Learner"，
因为 learner 就是在 fits curve，
而在 learner 中区分了 parameters 和 arguments，
这里的 "input" 就类似 parameters。
"The measured data itself,
the object of the entire program, is not input;"
那应该就是 arguments 了。

> A program that has no input may be extremely complex. Lacking input
> simply means the program knows what to do without being told.
> That built into the code is all the information needed to run. If
> you are willing to re-compile the program, you can even modify it
> without input.

从这段来看，input 又像是 dependency injection。

> But I'll be viewing programs from the input side. I'll be ranking
> programs according to the complexity of their input and I plan to
> demonstrate that a modest increase in the complexity of input can
> provide a substantial decrease in the complexity of the program.
> From this point of view, a program with no input is simple.

## 2.1 Choosing a language

> ... we won't be talking about problems at the language level.

> I want to talk about problems common to all programs in a
> machine-independent and language-independent manner. I will leave to
> you the details of implementation. I am not going to write a
> program, I am going to show you how to write a program.

> Now let's look at the major failing of higher-level languages.  In
> attempting to achieve machine-independence and to be applicable to a
> wide range of applications, they only give you acess to a fraction
> of the capabilities of your computer. If you compare the number of
> loop control instructions on your computer to the number of loop
> constructs in your language, you'll see what I mean.

批评了高阶语言隐藏了 computer 的能力。

后面还批评了复杂的 calling convention，比如需要保存寄存器之类的。
相比之下简单的 calling convention 就是用一个传递 arguments 的 stack。

> You will have to code in assembler! Not the whole program, if you
> insist, but the important parts that we'll be concentrating on. You
> might be able to do some of these in FORTRAN, but it simply isn't
> worth the effort.

这里说的是 FORTRAN，但是对 C 而言也是如此。

## 2.2 Choosing a computer

TODO

# 3 Programs with input
# 4 Programs that grow
# 5 Programs with memory
# 6 Programs with output
# 7 Programs that share
# 8 Programs that think
# 9 Programs that bootstrap
