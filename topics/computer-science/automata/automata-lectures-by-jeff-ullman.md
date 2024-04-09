---
title: Automata Lectures by Jeff Ullman
references:
  video: "https://www.youtube.com/playlist?list=PLEAYkSg4uSQ33jY4raAXvUT7Bm_S_EGu0"
  site: "http://infolab.stanford.edu/~ullman/ialc.html"
---

# 学习 automata 的动机

看了 Jonathan Blow 的 ["Discussion: Making Programming Language Parsers"](https://www.youtube.com/watch?v=MnctEW1oL-E)，
我发现我应该放弃 parser generator 项目，
比如 [Earley parser](https://en.wikipedia.org/wiki/Earley_parser)，
我应该直接手写语法解析器。

- 在当前程序语言之外，不依赖于额外的工具（哪怕工具是自己设计的）。
- 方便实现更好的语法解析报错。

为此我想学到如下知识：

- 语法复杂度，automata，正则表达式这三者之间的关系。
- 用递归下降，手写语法解析的基本 pattern。
- 递归下降与 [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator) 之间的关系。

进而我想到，手写 automata 可能就是程序员可以做的基本练习之一。
有如下线索：

- 写程序时，大部分的函数都是转换系统（transition system）。
  - 甚至大多数计算模型都是转换系统。
- 利用 automata 经常能把复杂的 if else 用 finite automata 理清。

# 有限状态机 -- Finite Automata

有限状态机很简单，因为只有有限多个状态。

有良好的性质才叫简单：

- 有方法可以判断两个 automata 是否等价。
- 可以正规化 automata 到 normal form。
- 可以判断是否存在输入使得 automata 进入错误状态。

有限状态机非形式化的描述：

- 一个 automata 所在的状态（state）就是他能记住的全部历史。
  - 或者说他基本记不住什么历史信息。
- 状态根据输入（input）而变化，比如字符或事件（event）。
- 描述状态如何根据输入而变化的规则，称作转移（transition）。
- 有一个状态是起始状态，有某些状态可以被规定为是结束状态。

对比其他计算模型：

- 有限状态机在某一时刻只能占有一种状态，
  而 Petri net 作为有限状态机的推广，
  在某一时刻可以占有多种状态。

- 一个有限状态机可以被想像为 graph 或者 table。
  当描述为 graph 时，点代表状态，边代表转移。
  这与 flowchart 刚好相反，
  在 flowchart 的 graph 中点代表转移，或者说操作（action）。
  Petri net 即是 finite automata 的推广，
  也是 flowchart 的推广。

有限状态机器的结束状态所对应的输入字符串，可以用来定义语言。
这样就得到了有限状态机器与语言之间的对应关系。
注意这里的方向，一个有限状态机可以确定语言，但是反过来不行。

# 正则语言 -- Regular Language

TODO 在 Lecture 1.3 中，
用证明两个集合相等的方式，
展示了语言与所有限状态机的等价。
用 dependent type 将这个证明形式化，会是很好的练习。

有趣的是这个证明在用归纳法之前，
首先加强了结论，并且结论带有两个命题，
两个命题同时归纳才能完成证明。

- 所有的有限状态机作为一个 interface，其中的一个作为 class 的实例。
  - 可以针对限状态机作的 interface 定义一般的由它定义的语言。
- 可以用一个谓词定义代表语言的集合。

能够被确定性有限状态机器识别的语言，可以用来定义正则语言。

- 未来还可以用生成语法来给出正则语言的另外的定义。

  - 这样就像在 "function as proof, type as proposition" 中，
    每个逻辑系统对应于某个一个语言的类型系统一样，
    这里每个生成语法也对应一个可以接受它的机器。

一个直觉是，一个有限状态机器可以数数，但是数不过某个固定的数。

因此有如下非正则语言的例子：

- 有相同数量 0 和 1 的字符串。
- 左右括号匹配的语言。
