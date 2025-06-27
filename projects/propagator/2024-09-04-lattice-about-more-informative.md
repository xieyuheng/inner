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

- merge 是 lattice 的 meet -- ⋀。
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

# 用 join 而不是 meet

[2025-06-12] 也许应该用 join 而不是 meet，
以使得 propagator model 对 lattice 的使用方式，
和 domain theory 对 lattice 的使用方式保持一致。

并且统一用 "richer" 来表示 propagator model 中的 "more informative"，
和 domain theory 中的 "more defined"。
注意，还需要反方向的词，如果用 richer，反过来就是 poorer，这不太好。
也许应该直接用 more 和 less，这两个更简单的词。

# 用 meet 而不是 join

[2025-06-27] 也许 domain theory 的选择是错误的，就是应该用 meet。
因为这样更符合集合论的直觉，
即更小的集合代表了更小的范围，
因此是 "more informative" 的。

符合集合论的直觉很重要，
因为逻辑中的 "A imples B" 所使用的也是这个直觉，
类型论中类型的大小也是这个直觉。

domain theory 术语选择的问题在于，
想要用 "more defined" 来表达一个函数所能处理的参数范围更大，
但是其实一个函数所能处理的参数范围更大，这个函数的类型就更小。
