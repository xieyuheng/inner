---
title: SolidJS and Propagator Model
date: 2024-09-16
---

solidjs 很明显就是一个 propagator model，
可以理解为 cell 保存的东西是 history -- time 到 value 的 k-v-map，
在前端 view 组件中显示的，只是 history 中最新的 entry。

下面的描述和 inet 中用表层语言来构造计算模型的对象类似：

> Components have a lifecycle that defines how they are created,
> updated, and destroyed. A Solid component's lifecycle is different
> from other frameworks, as it is tied to the concept of reactivity.

> Where frameworks may re-run components on every state change, a
> Solid component's lifecycle is tied to its initial run. What this
> means is that a Solid component is only run once, when it is first
> rendered into the DOM. After that, the component is not re-run, even
> if the application's state changes.

> When the Solid component renders, it sets up a reactive system that
> monitors for state changes. When a state change occurs, the
> component will update the relevant areas without re-running the
> entire component. By bypassing the full component lifecycle on
> every state change, Solid has a more predictable behavior compared
> to frameworks that re-run functions on every update.

但是这里的 "re-run functions on every update"
还是 expression 思维，或者说是 functional 思维。
