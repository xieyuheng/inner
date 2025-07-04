---
title: Exp vs. Value
date: 2024-04-01
---

我想要探索的主要问题是如何降低语言设计实验的成本。
主要的矛盾在于如何降低语言实现的复杂度。

我的印象是，很大一部分复杂度来源于 Exp vs. Value 这两个类型的分离。
前者为语法，后者是解释的结果，为语义。
此时 evaluate 作为函数是从 Exp 到 Value 的。

如果能将二者合并为一个类型，实现的复杂度就会降低很多。

我获得这个印象的线索有如下几条：

- 一、Vladimir Voevodsky 在一次演讲中畅想未来的数学时，
  用人们现在对多项式的操作，来类比人们未来对证明的操作。
  而多项式是简单的表达式，当区分 Exp 与 Value 时，
  操作就会变复杂。
  在没有程序语言辅助的情况下，
  当人们用纸和笔来操作证明时，
  很难想象人们会发展出来 Exp 与 Value 分离的系统。

- 二、在 "The Little Typer" 中，"everything is expression"，
  evaluate 作为函数是从 Exp 到 Exp 的。
  这样没有 Exp 和 Value 之分，是十分易于理解的。
  但是假设真就这么实现，真想不用 closure 和 Value，
  那么在做 substitute 时就要注意实现效率，
  因为古典的实现有很多重复的计算与生成新表达式。

  - 我现在知道，可以用 explicit substitution 来解决实现效率的问题。

- 三、所有论文中的 inference rule 都只会用 Exp
  而不会用 Value 和 closure。

- 四、在 inet 中，语法是用来以线性的方式构造 graph 的，
  而 reduce 作为函数是从 graph 到 graph 的。
  此时和上面的情况一样，在关键的地方仍然没有两个类型之分，
  仍然是易于理解的，并且此时没有实现效率的问题。

我首先想到用 graph 来实现 lambda calculus，
然后探索了 inet 的实现方式。
最后我才发现，原来 explicit substitution
就是要将 Value 合并到 Exp 中。

既然 "The Little Typer" 的书中想要 "everything is expression"，
而实现中还是用到了 Exp 与 Value 分离的实现方式，
为了探索这个问题，首先我就应该尝试用 explicit substitution
来实现 "The Little Typer" 的 Pie。
