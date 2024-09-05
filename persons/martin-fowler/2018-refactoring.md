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
