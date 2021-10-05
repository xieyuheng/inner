---
title: Domain Driven Design -- Talks
---

# Graph theory as model

2020-11-09

- What is DDD - Eric Evans - DDD Europe 2019
  https://www.youtube.com/watch?v=pMuiVlnGqjk&ab_channel=Domain-DrivenDesignEurope

这个演讲所列举的建模过程中，是以图论为模型的。

|-----------|--------|
| model     | graph  |
|-----------|--------|
| itinerary | path   |
| leg       | edge   |
| stop      | vertex |
|-----------|--------|

通常我们建立模型是以 graph theory 为基础的，
只不过会给 graph theory 中的概念以不同名字。

# Game theory as model

2020-11-14

- Eric Evans - Good Design is Imperfect Design
  https://www.youtube.com/watch?v=lY54TmmEllY

当某些函数缺少结合律的时候，
可以考虑组合博弈论等离散数学结构，
而不要强行使用代数结构中的概念。
