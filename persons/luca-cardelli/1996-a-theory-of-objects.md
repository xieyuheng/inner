---
title: A Theory of Objects
authors: [martin abadi, luca cardelli]
date: 1996
---

# My Notes

## Object 不是函数

注意不能用 `(message) -> method` 来理解 object 或者 record，
因为我们需要明确 object 的 properties 的名字，
才能获得 object 之间的 subtype relation。

# Prologue

## 为什么要为程序语言发展相对形式化的理论？

因为理论可以帮助程序语言的实现者，理解在实现中所遇到的问题。

## 本书作者的主要目的是什么？

函数式和过程式语言已经有相对完备的理论了。
对象式还没有被广泛认可的理论。

作者断言，如果使用函数来构建「对象」的理论，
在无类型时还是可以接受的，
但是当考虑类型系统时就会遇到阻碍。

- 具体的阻碍是什么呢？

因此作者的目的在于，不用函数去编码「对象」，
而是是直接处理「对象」这个概念，来形成理论。

理论的基础讲被称为「object calculus」，有待解释的概念包括：

- self
- dynamic dispatch
- classes
- inheritance
- prototyping
- subtyping
- covariance and contravariance
- method specialization

# 1 Object Orientation

# 2 Class-Based Languages
