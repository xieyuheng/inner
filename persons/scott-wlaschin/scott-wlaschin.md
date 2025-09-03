---
title: scott wlaschin
---

主页：https://scottwlaschin.com

scott wlaschin 有一个关于 F# 的演讲，
介绍了如何在 F# 中，用 pipe + bind 来使用 monad，
重点是 bind 的参数交换了位置（也许应该被称为 lift）。

这种使用 monad 的方式与 hashell 的 do notation 不同，
并不需要专门的语法，更接近组合子演算的简单用法。

[2025-09-03] 在设计 occam-lisp 的时候，
我想起了 F# 的这种 monad 用法。
觉得是值得学习的。

我也给别人讲过函数式编程与 monad，
对比 wlaschin 用 railway track 来讲解 monad 之间的复合，
我的讲解真差劲。

演讲：

- [F# for C# programmers](https://www.youtube.com/watch?v=KPa8Yw_Navk)
  通过一点一点把 C# 示例代码翻译成 F# 代码，来介绍 F#。
  这是很好的向旧语言用户介绍新语言的方法，
  可以很直观地展示新语言的优势。

- [The Power of Composition - Scott Wlaschin](https://www.youtube.com/watch?v=WhEkBCWpDas)
  用 lego 积木和玩具火车中的 railway track 来类比函数。

  在类比中可以强调函数的属性：

  - 函数生来就是为了被复合与重用。
  - 函数是独立的，使用函数不需要额外的依赖与条件。

  上面的描述中，「函数」换成是「lego 积木」也是成立的。
