---
title: Smalltalk Best Practice Patterns
author: Kent Beck
year: 1997
---

# 学习动机

我是从 Martin Fowler 的 "Refactoring" 中，
了解到「用不定冠词来命名变量」这个 ieda 的，
我想用这种 naming convention 来写带有静态类型系统的 lisp/scheme。

而 Martin Fowler 在书中指出了，
他是从 Kent Beck 的这本书中了解到
「用不定冠词来命名变量」这个 ieda 的。

所以我也来学习一下，希望能学到更多好而编程风格。

但是我不会直接用 Smalltalk 本身，因为我只喜欢编辑纯文本的代码。

- 也许可以尝试一下 squeak，感觉跑起来还挺简单的。
- 也许我可以用想像中的 lisp 来写这本书里代码的例子。

[2025-02-09] 用冠词命名并不是个好主意，
应该反过来用 -t 后缀来命名类型。

# Method Protocol

Smalltalk 有 method protocol 的概念，
其实在动态类型语言中，就是简单地给 method 分组。

> Put Default Value Methods in a method protocol called “private.”

但是作为学习这本书的方法，
也许可以总结一下书中所提到的所有 method protocols，
以及每个 method protocol 中的函数有什么。

其实在长期写代码的过程中，
每个人都会形成自己的 method protocol，
Smalltalk 明显地把它作为一个语言中的概念，
还是很不错的。
