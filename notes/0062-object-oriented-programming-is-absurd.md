---
title: OOP 是荒谬的
date: 2025-10-12
---

# 归谬法

第一次发现 OOP 有大问题，
是在尝试设计新的 OOP 语言的时候，
发现的一个归谬法证明。

首先，OOP 的特点是：

- class 不是纯的 interface（record type），
  class 中的某些 attribute 不只之给出了 type，
  还要给出 value，这些就是 class 中所定义的 method。

- object 中除了包含作为数据的 attribute，
  还要包含处理数据的 method。

这种写程序的方式是荒谬的。

假设这种写程序的方式是正确的，
那么它应该适用于各种数据类型，
但是对 nat 或者 int 来说，
这显然是荒谬的。

设想 nat 和 int 被定义为两个 class，
它们应当包含那些 method？
可能说要包含核心的处理函数。
但是人们想要针对 nat 和 int 实现的处理函数，
可能有成千上万个，哪些是核心的，哪些不是？
任何划分都是完全主观的，是任意的。

哪些 function 应该被作为这两个 class 的 method，
其实只有一个客观准则，那就是不应该有任何 method。
所有的 nat 和 int 的处理 function 都应该是独立的。

这在 OOP 中经常说的「接口分离原则」中已经有所暗示了。
所有的接口都分离，就是所有的 function 都独立。

# 继承与代码复用

用继承的方式来重用代码是错误的。

EOC 的课程代码就是最好的反面例子，
为了课程渐进地进行，
老师实现了一系列相互继承的解释器 class。

本身一个好端端的解释器递归函数，
被写到了支离破碎的很多 class 中。

# 关于 simulation

OOP 起源于对世界的 simulation 是错误的。

人们经常这么说，因为 OOP 起源于 simula 语言，
而 simula 语言设计支出是用来做物理 simulation 的。

但是这是错误的，
OOP 的 feature 并不能帮助 simulation，
也不能帮助写 GUI。

真正能帮助 simulation 和 GUI 的，
是 reactive programming。

vue 和类似的前端 UI 框架，都是例证。

# 结论

这对于设计新语言的人来说是好事，
因为大多数已有的语言都是 OOP 的，
都是荒谬的。

这也是坏事，因为它证明了人群是荒谬的，
这么多年下来，人们没能对 OOP 给出充分的批判。
