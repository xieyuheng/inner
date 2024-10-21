---
title: Lattice about "more informative"
date: 2024-09-04
---

```typescript
export const merge = defineGeneric()

export function implies<A, B>(x: A, y: B): boolean {
  return merge(x, y) === x
}
```

merge 的效果是让 partial information 变得 more informative。

- merge 是 lattice 的 meet。
- implies 是 ordered set 中的 "less or equal to"。
  虽然 "more informative" 一词中有一个 "more"，
  但是其实就序集而言它是更小。

注意术语所在的领域：

- merge 是就 partial information 而言的术语。
- implies 是就命题之间的蕴含而言的术语。

最好用例子来理解 "more informative"：

- 对集合来说，越小的集合越具体，包含的信息更多。
  比如考虑 CLP(FD) -- Constraint Logic Programming over Finite Domains，
  在求解 constraint 问题的过程中，domain 作为一个集合会变小。

- 对于命题来说，蕴含式前项的命题比后项的命题包含更多信息。
  因此 more informative 就是 implies。
  集合之间的「包含于」关系，就对应于命题之间「蕴含关系」。

- 对区间来说，越小的区间就更精确，就包含更多信息。
  毕竟，区间是特殊的集合。

注意，对于 Belief 来说，merge 所定义的 implies，
与之后定义 beliefSystemMerge 时所用到的 stronger，
对 reasons 集合的理解方式是相反的。
