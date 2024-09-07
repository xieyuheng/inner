---
title: Refactoring
subtitle: Improving the Design of Existing Code
authors: [Martin Fowler, Kent Beck]
year: 2018
---

# 学习动机

Refactoring 这本书里所讲的，是现代程序员最基本的知识。
书中的大部分我都很熟悉了，可以经常参考这本书来温故知新。

# Chapter 1: Refactoring: A First Example

## Decomposing the statement Function

> When refactoring a long function like this, I mentally try to
> identify points that separate different parts of the overall
> behavior. The first chunk that leaps to my eye is the switch
> statement in the middle.

这么说来，也许读 lisp/scheme 代码的时候，
更容易区分不同部分的代码。

主要是因为 lisp 习惯使用 "rigid body indentation"，
这相当于是给代码的 layout 增加了一个 constraint，
在一位代码的读者尝试去区分不同部分时，
人脑的模式匹配机制可以利用到这个 constraint。

> The way to put that understanding [about different parts of the
> overall behavior] into code is to turn that chunk of code into its
> own function, naming it after what it does -- something like
> `amountFor(aPerformance)`.

> When I want to turn a chunk of code into a function like this, I
> have a procedure for doing it that minimizes my chances of getting
> it wrong. I wrote down this procedure and, to make it easy to
> reference, named it Extract Function (106).

这就是程序员们和 Christopher Alexander 所学到的，
「用 pattern language 来总结知识」的技巧了。

这里 Martin Fowler 介绍了两个他自己的 coding style。

第一个是 `result` 这个变量名的用法：

> It’s my coding standard to always call the return value from a
> function “result”.  That way I always know its role.

第二个是，在没有更有意义的名字的情况下，
用「不定冠词 + 类型」来命名变量。

> Again, this is following my coding style. With a dynamically typed
> language such as JavaScript, it’s useful to keep track of types --
> hence, my default name for a parameter includes the type name. I use
> an indefinite article with it unless there is some specific role
> information to capture in the name. I learned this convention from
> Kent Beck [Beck SBPP] and continue to find it helpful.

我这次重读 "Refactoring" 就是为了体验这种命名变量的方式，
因为我觉得这很时候在一个有静态类型的 lisp/scheme 中使用。

原来这一点是和 Kent Beck 学的！
来自 "Smalltalk Best Practice Patterns"（SBPP） 这本书。

> The great benefit of removing local variables is that it makes it
> much easier to do extractions, since there is less local scope to
> deal with. Indeed, usually I’ll take out local variables before I
> do any extractions.

在 Forth 中，这一点有了极端的体现，
因为 Forth 根本就没有 local variables，
所以对任意一段代码我们都可以 do extraction。

TODO

# Chapter 2: Principles in Refactoring
# Chapter 3: Bad Smells in Code
# Chapter 4: Building Tests
# Chapter 5: Introducing the Catalog
# Chapter 6: A First Set of Refactorings
# Chapter 7: Encapsulation
# Chapter 8: Moving Features
# Chapter 9: Organizing Data
# Chapter 10: Simplifying Conditional Logic
# Chapter 11: Refactoring APIs
# Chapter 12: Dealing with Inheritance
