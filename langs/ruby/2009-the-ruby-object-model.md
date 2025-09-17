---
title: the ruby object model
year: 2009
---

在这个演讲中 [The Ruby Object Model](https://www.youtube.com/watch?v=X2sgQ38UDVY)，
Dave Thomas 介绍了 ruby 是如何实现 OOP 的。

重点在于如何实现 `self` 或者别的 OOP 语言的 `this`。

有一个 stack of self，只有两个操作会影响这个 stack：

- method call
- class/method definition

这可以使得 definition 也是简单的 procedural code。
class definition 可以理解为 open up a class and modify it。
class 也是 object 就是 meta object protocol。
真是不错的语言实现课程，让人感觉 ruby 的设计真是优雅。

两个 ruby 的规则：

- instance variables: look up in self
- methods: look up in self's class

另外通过在 class chain 中增加匿名 class 实现了很多功能，
比如 singleton method 和 inlcude。
