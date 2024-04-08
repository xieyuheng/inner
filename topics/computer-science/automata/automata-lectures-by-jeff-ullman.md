---
title: Automata Lectures by Jeff Ullman
references:
  videos: "https://www.youtube.com/playlist?list=PLEAYkSg4uSQ33jY4raAXvUT7Bm_S_EGu0"
  site: "http://infolab.stanford.edu/~ullman/ialc.html"
---

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
