---
title: stack is not enough for linear type system
date: 2025-01-14
---

如果要实现这里所提到的语义：https://protovision.github.io/#!/documents/blog/linear-logic.html
就不能用 stack 来处理 linear type。

这里的语义的特点是，每个 linear sequent 被认为是一个 transition 的类型。
inputs 必须全部到齐 transition 才能开始；
当有多个 outputs 时，不必同时给出，可以随时给出任意一个 output。

但是返回到 stack 中的数据必须被认为是一起返回到 stack 中的，
而上面提到的语义却要求函数可以在不同时间给出多个返回值。
这要求输入输出是用 graph 的 wire 来传递的，而不能是用 stack 来传递的。

因此 stack is not enough for linear type system，
除非 stack 的返回值是可以返回到很多 named channels 中。

可以回去看 frank pfenning 所给出的 linear logic 的 term syntax。
因为当时没有仔细看是因为认为 stack 对于捕捉 linear logic 语义而言是足够的，
而现在知道了是不足够的。
