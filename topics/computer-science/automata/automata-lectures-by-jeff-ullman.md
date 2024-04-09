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
因为编程中大部分函数都有一个 state 作为 context，
根据这个 state 的不同，执行不同的操作，
可能是对其他数据的操作，也可能是对 state 本身的操作。
automata 除了根据 state 不同，还有一个 input 参数，
这个参数不同也会有不同转移（transition）。

也就是说大量的函数都是这种类型的：

```cicada
automata: (state: State, input: Input) -> State
f: (state: State, input: Input) -> State
```

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
