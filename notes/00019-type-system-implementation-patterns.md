---
title: Type system implementation patterns
date: 2021-07-02
tags: [cicada]
---

# 描述实现技术的准则

每段实现技术的说明要清晰简洁，
并且每段说明都应该能清晰地被相应的代码表达出来，
这样代码本身才算是清晰的。

# Patterns

我们通过描述数据类型，以及它所承担的「职责」，来描述所用到的实现技术。

> 数据类型的职责，说明着数据类型之间的关系。

> 一组相对固定的关系，用来解决某个领域中某类问题，就称作 pattern。

# Open-closed principle

为了将诸「职责」清晰地表达在具体的代码里，
我们选择使用了 OOP 中的 abstract class（或 interface）来实现数据类型。

- 这并不是本质的，另外的实现方式是用函加模式匹配来实现每个职责。

这样做好处是，每当需要增加一个新的 Exp 子类型时，不需要修改涉及到其他 Exp 的函数。

- 这是 OOP 中的 [open-closed principle]。

[open-closed principle]: https://en.wikipedia.org/wiki/Open-closed_principle

# Exp

首先从 Exp 开始说。

> Exp 的职责是被 check，实现 check 的同时，还要实现 infer。

```
check(ctx: Ctx, exp: Exp, t: Value): void
infer(ctx: Ctx, exp: Exp): Value
```

这个技术叫做 **bidirectional type checking**。

使用这个技术是为了把 type system 中声明式的 inference rule 改写为函数，
这在于，在无方向的 inference rule 中，通过明确输入与输出来指出方向，有了方向就成了函数。

"bidirectional" 一词中所说的「双向」就在于：
- 在 check 时，type 是输入
- 在 infer 时 type 是输出

需要被 check 的 Exp 对应 intro rule，即 data constructor，
需要被 infer 的 Exp 对应 elim rule。

典型的 intro rule 是 Fn，典型的 elim rule 是 Ap。

# Core

我们需要修正一下 Exp 的职责：

> Exp 在其基本职责 check 与 infer 的同时，还要返回 Core。

```
check(ctx: Ctx, exp: Exp, t: Value): Core
infer(ctx: Ctx, exp: Exp): { t: Value, core: Core }
```

Core 与 Exp 结构类似，差异在于，它补充了一些信息，
这些信息是在编译时期 -- 即 check 和 infer 时期，就可以确定的。

这个技术叫做 **elaboratation**。

- 「问题」为什么需要 elaboratation 与 Core？如果没有会遇到什么问题？

  - 不止是为了省略参数。

> Core 的职责在于能够被 evaluate 成为 Value。

```
evaluate(env: Env, core: Core): Value
```

虽然 Exp 与 Core 这两个数据类型结构相似，
但是区分这两个数据类型可以帮助我们区分类型检查的不同阶段，
这不同的阶段体现于 Exp 与 Core 的不同职责。

# Value

Value 这个数据类型的结构也与 Exp 和 Core 类似，
其主要差别在于 Value 只包含顶层是 constructor 的表达式，
包括 data constructor 与 type constructor。

> Value 的职责是能够被 readback 回到 Core。

```
readback(ctx: Ctx, t: Value, value: Value): Core // Normal form
```

readback 之后的 Core 已经是 normalize 的了。

用 evaluate 加 readback 来获得 normal form，
这种技术叫做 **normalization by evaluation**，简称 NbE。

关于 readback，有一个需要注意的要点，
那就是 readback 必须有两个参数，一个是 value，一个是 type，
因为 readback 是为了获得 normal form，
而 normal form 是就某个 type 而言的，
这个要点叫做 **typed directed readback**。

因为 normal form 是用 equivalence 来定义的，
而 equivalence 是就某个类型而言的。

我们获得 normal form，是为了判断 Value 之间的等价关系，
这个等价关系同时涵盖了 definitional equivalence 与 computational equivalence，
而不涵盖 propositional equivalence，
propositional equivalence 是需要给出额外证明的。

- definitional equivalence 与 computational equivalence 就像数学归纳法中的基础步骤。
  可用数学归纳法证明 propositional equivalence。

# NotYetValue

Value 中有一类特殊的叫做 NotYetValue，
它包含一个 Neutral -- 即 elim rule 所对应的表达式，和相应的 type。

```
NotYetValue {
  t: Value
  neutral: Neutral
}
```

在 evaluate Core 时候，表达式就被分成了两类，
其中与 intro rule 对应的，被 evaluate 成了 Value，
而与 elim rule 对应的，被 evaluate 成了 Neutral，
这些 Neutral 和其 type 被「嵌入」在 NotYetValue 中，而重新成了 Value，

- 「问题」为什么 NotYetValue 中要包含 Neutral 的 type？

  - 因为 readback 需要 type。
  - 所以 Neutral 所包含的子表达式必须是 Normal（带有 type 的 Value），
    而不是单纯的 Value，这样 Normal 的 readback 才能带有 type。
  - 如果 NotYetValue 中不包含 Neutral 的 type，
    就没法给出 Normal 的 type。

NotYetValue 使得我们的 evaluate 可以进行 partial evaluation。

# eta-expansion during readback

我们也用 Value 作为 check 与 infer 中出现的 type 参数或返回值的类型。

因此 Value 就有了另一个职责，即用来 eta-expansion Exp。

> Value 作为 type 时，可能有的职责是 引导 eta-expansion。

```
// t: Value
t.readback_eta_expansion(ctx: Ctx, value: Value): Core
```

在转化为 normal form 的时候，还需要施行 beta-reduction 与 eta-expansion。

eta-expansion 的例子：

- Pi -- `f => λx. f(x)`
- Sigma -- `x => cons(car(x), cdr(x))`

- 「问题」是否可以在 NotYetValue 中处理 eta-expansion？

- 「问题」为什么 在 normalization 的过程中，
  我们用了 eta-expansion，其方向是从 f 到 λx. f(x)，
  而不用 eta-reduction，其方向是 从 λx. f(x) 到 f？

  - 可能与 partial evaluation 有关。
