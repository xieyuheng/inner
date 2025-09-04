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

- [The Power of Composition](https://www.youtube.com/watch?v=WhEkBCWpDas)

  用 lego 积木和玩具火车中的 railway track 来类比函数。

  在类比中可以强调函数的属性：

  - 函数生来就是为了被复合与重用。
  - 函数是独立的，使用函数不需要额外的依赖与条件。

  上面的描述中，「函数」换成是「lego 积木」也是成立的。

- [Moving IO to the edges of your app: Functional Core, Imperative Shell](https://www.youtube.com/watch?v=P1vES9AgfC4)
  这里用了极限思维，先用一个简单的带有副作用的例子，
  来说明核心应该是纯函数，副作用应该完全与核心分离。

  例子就是输入两个数字，然后打印两个数字的和。

  然后论证如果简单的例子应该这样写，
  那么复杂的 app 也应该这样写。

  提出 core 与 shell 的概念，
  写代码的时候要明确你写的是 core 还是 shell，
  如果写的是 shell 就副作用随便用，不用搞复杂的依赖注入。
