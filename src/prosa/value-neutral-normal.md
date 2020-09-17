# Value, Neutral & Normal

2020-09-17

在上一个实现 cicada 的尝试中，
尝试实现了 structural typing 与 fulfilling type system。

其中的错误在于，对 `Value, Neutral, Normal` 三者的理解不对。
当 `Neutral` 想要递归引用到 `Value` 时，
应该通过带有类型的 `Value`，即 `Normal` 来引用，
因为 `Value.readback` 必须是带有类型的 readback，
其类型不应是 `(ctx: Ctx, value: Value) => Exp`，
而应该是 `(ctx: Ctx, t: Ty, value: Value) => Exp`。

除了纠正上面的错误，在再次尝试实现时，我们还要注意：
- 用 curry 的 typed lambda 而不用 church 的。
- 简化语言 -- 简化表达式，简化 telescope 的结构。
