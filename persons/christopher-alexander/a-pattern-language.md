---
title: A Pattern Language
subtitle: Towns, Buildings, Construction
authors: [Christopher Alexander, Sara Ishikawa, Murray Silverstein]
year: 1977
---

# 记

读这本书的动机来自构建程序与构建家园的比喻。
也来自程序员对建筑工程师，对构建家园者的敬仰。

关于写程序的书 -- 《设计模式》，
和这里描述模式的方式（模式）也是相同的。
对于不同的问题领域以及子领域，可以有不同的模式书，格式也将类似。

在我所关心的领域 -- 程序语言设计中，
也有解决问题的各种模式（或者说这些解决方案中蕴藏着更一般的模式）：

- 用 closure 解决 lexical scope 的问题。
- 用 explicit substitution 来解决 Exp 与 Value 分裂为两个 type 的问题。
- 用带有自己的 stack 的 lambda machine 来突破解释器受到 host 语言调用栈的限制。
- 用 NbE 来做 partial evaluation 来得到 normal form 以判断等价。
- 用 elaboration 来从当前的 Exp 和 context 中获得更多信息，以形成更容易处理的新 Exp（称为 Core）。
- 在实现逻辑式语言时，使用的 unification 和 substitution 来解等式形成的方程组。
- 在搜索解决方案的的 tree 时使用 queue 来记录当前所搜索到的 tree 的轮廓，
  并且用对 queue 的不同处理方式来实现不同的搜索策略 -- 深度优先和广度优先。
- 用 reify 来处理带有 logic variable 的 value 之间的 alpha equivalence，
  正如 lambda term 之间的 alpha equivalence。

等等等等，还有很多。
也许也可以整理为一本模式书，
我相信在整理的过程中，我们能学到很多，
并且整理的成果也将称为实用的工具，
增益未来更进一步的探索。

总结来说，就是在解决某个领域问题的过程的实践中，
把想到的解法加以抽象，并且以相对固定的格式总结出来。

也许我们还可以进一步分析所总结出的 pattern 的属性，
并且做 formal concept analysis，来发现更多的概念与 pattern。
重点在与分析 pattern 之间的关系，使得整个 pattern book 成为一个整体，
而 formal concept analysis 是系统地分析 pattern 之间关系的方法。

也许一个程序员的职业生涯所解决的各种问题都可以写成自己的 pattern book。
很惭愧这么多年之后，我会解决的问题还很少。
