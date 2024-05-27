---
title: Primitives, Combination and Abstruction
date: 2024-05-28
---

Sussman 经常说 Language 就在于三点：

- primitive elements
- means of combination
- and means of abstruction

比如在这个演讲中：["Gerald Sussman Teaches Mechanical Watch Ideas at MIT"](https://www.youtube.com/watch?v=TWQN8Yf1g70)

正如在 Scheme 语言的设计中，
以及在 SICP 的教学中所展示的那样，
语言确实在于这三点。

现代数学中的范畴论也能佐证这一点，
好像掌握了各个范畴的 morphism 与 composition，
就掌握各个数学结构。
尽管范畴论中的 composition 是更狭义的 combination。

但是，说语言由这三点组成，还忽视了一些事实。

比如，有一次，我给一位朋友介绍 Forth 语言，
我介绍了 primitives -- 比如 `dup` 和 `mul` 等等，
又介绍了 combination -- 即用后缀表达式写的函数复合 `dup mul`，
最后介绍了 abstruction -- 即定义新函数的方式 `: square dup mul ;`。
我说，这就是这个语言的全部了，你已经掌握了这个语言的大部分。
但是我的朋友并不能使用 Forth 语言，
因为我还缺少了一大部分，
即大量的解决实际问题的 Forth 代码的例子。

这一大部分可能更重要，
单单从例子中，
我的朋友可能能自己分析出来 primitive elements，
means of combination
和 means of abstruction 分别是什么。

正如机器学习中，从大量的例子中，
机器好像学到了更深刻的概念。

通过把语言分解成三个组成部分来学习语言，
与通过大量的使用语言的经验来学习语言，
正是分析与综合这个认知方式的二分法所描述的。

Quine 批判过分析与综合，而强调整体，
灵活的使用语言来解决问题，就是整体。

但是 Quine 的批判很深刻，
他说根本就没法定义什么是分析，
这个二分根本就不好用，
或者说迷信这个二分的教条，
对人们的认知而言弊大于利。

例子和经验是更重要的，
分析可以被看成是更高一层次的经验。
要知道，当 Sussman 想要证明 Language 就在于上述三点时，
也是要据很多 Language 的例子来证明的，
比如上面提到的演讲中所说的电子电路的语言和机械表的语言，
当然也包括 Scheme 和 Forth 这种程序语言。
正是从这些语言的例子中，Sussman 总结出了语言的分析式的定义，
即 Language 就在于三点：

- primitive elements
- means of combination
- and means of abstruction
