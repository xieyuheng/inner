---
title: gradual typing for functional languages
authors: [jeremy g. siek, walid taha]
year: 2006
---

# My Motive

[2026-05-09] 要给 meta-lisp 增加 `any-t`，
所以来看一下 gradual typing 的论文。

在读论文之前，deepseek 对论文的解释如下。

`any-t` 的语义选择有几种情况。

情况一：只作为顶类型（? = Top）

- 允许从任意类型隐式向上转型到 ?（即 S <: ? 对所有 S 成立）。
- 但不允许从 ? 隐式向下转型到其他类型（即 ? <: T 一般不成立）。
- 这是多数面向对象语言中的做法（如 Java 的 Object，C# 的 object）。
- 效果：你可以把任何值放进 ? 类型的容器里，
  但从 ? 取出来使用时需要显式向下转型（可能失败）。
  这失去了动态类型那种“把 ? 当作任何类型来用”的便捷性。
  - 我在 c 中，编码语言的 runtime `value_t`，
    用的就是这种语义，`value_t` 需要被 cast 到具体的 subtype。

情况二：只作为底类型（? = Bottom）

- 允许从 ? 隐式向下转型到任意类型（? <: T 对所有 T 成立）。
- 但不允许从任意类型隐式向上转型到 ?。
- 这在类型系统中很少见，因为底类型通常不包含实际的值（例如 Nothing）。
  真要把某个值赋给 ?，你必须显式包一层。这不符合动态类型的使用习惯。

渐进类型中的 `any-t` 必须把 ? 同时当作顶和底，
因为动态类型的核心是：你既可以把任何值赋给一个未注解的变量（? 作为顶），
也可以把一个未注解的变量当作任何类型去使用（? 作为底）。比如：

```python
x: ? = 3        # ? 被当作 top type
y: int = x + 3  # x 不需要被 cast 就能被当作 int
```

但是如果直接把这种同时作为 top 和 bottom 的元素添加进子类型格中，
子类型格就会坍缩为平凡格。

这正是 Jeremy Siek 的核心发现：
不把 ? 放入子类型格，而是定义一种新的关系，
即一致性（consistency），它不是传递的。
因此 ? 可以在定义上与所有类型双向兼容，
同时又不会因为传递性导致 int 和 bool 被当作彼此的子类型。

typescript 的 unknown 和 never 分别是 top 和 bottom，
而 any 类似渐进类型的 ?，但是不完全一样。
差异在于，渐进类型要求在编译时插入运行时的 cast，
把 ? 安全地转化到更具体的类型，
而 typescript 的 any 不需要插入这种 cast。

自动插入检查不是很复杂且在运行时开销很大？
毕竟 type 可能不是 int 或 bool 一类的 atom type，
还可能是 list of int 之类的 compound type。
尤其是考虑到函数类型的时候，需要把函数包起来，
会影响尾部递归调用，所以这是完全不能接受的。

有很多类似的对 gradual typing 的批评：

- 2017-sound-gradual-typing-is-nominally-alive-and-well.pdf
- 2018-collapsible-contracts--fixing-a-pathology-of-gradual-typing.pdf
- 2024-typed-and-confused--studying-the-unexpected-dangers-of-gradual-typing.pdf
- 2025-an-argument-against-gradual-type-systems-in-programming-language.pdf

还有对批评的回应：

- 2015-refined-criteria-for-gradual-typing.pdf

因此，虽然读 jeremy 的论文，
但是实现的时候可能还是要用 typescript 的语义，
甚至直接照搬 typescript 的 any、unknown 和 never。

大概知道如何实现 any-t 了。
我可以在 subtype 关系之外，实现论文中所说的 consistency 关系。
但是我还不知道的是：
- 如何定义 any-t 在 subtype 关系中的行为？
  - deepseek 说 typescript 是用子类型关系来处理 any 的，
    也就是直接把 any 视为 top 和 bottom。
- 以及如何定义 any-t 在 unification 中的行为？

在 typescript 中，是鼓励尽量少用 any，
而是更多用 unknown + narrowing 吗？

- 好像是这样的。
  即便是我们要使用的 `(define-generic)` 和 `(define-handler)`，
  也都是这种 unknown + narrowing 的情况。

- 注意，如果考虑子类型作为集合的语义，any 既是全集也是空集。

但是，我发现在我的类型检查器代码中，
并没有用到子类型关系的传递性的地方。
如果说传递性会引发塌缩呢？

- 也许可以通过函数复合的构造，来是的传递性出现？
  在类型检查中，传递性并不需要作为一个“算法步骤”显式实现。
  它更多是推理规则组合的自然结果。

scala 和别的带有 any type 的语言，其 any 的语义是 top，
而不是类似 ts 的 top + bottom 吗？

- 对，ts 的类型系统是特例，为了兼容 js。

注意，typescript 的 unknown 是要与 narrowing 配合使用的。

- typescript 的 narrowing 是否不只要处理 any，
  还要要用到 intersection type？
- narrowing 好像很复杂，还需要处理 and 和 or。
  也许不应该支持 narrowing，而应该用 explicit cast。
- TypeScript 的类型收窄（Narrowing）本质上就是集合运算，
  其中交集类型 (&) 是核心机制，而 any 则是一个需要特殊对待的边界情况。
  而我们的类型系统是 hindley-milner 风格的，
  不是 set-theoretical 的，没有 union 和 intersection。


# Abstract

TODO
