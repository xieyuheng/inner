# Implementing Structural Typing Again

2020-10-06

## Value, Neutral & Normal

在上一个实现 cicada 的尝试中，
尝试实现了 structural typing 与 fulfilling type system。

其中的错误在于，对 `Value, Neutral, Normal` 三者的理解不对。
当 `Neutral` 想要递归引用到 `Value` 时，
应该通过带有类型的 `Value`，即 `Normal` 来引用，
因为 `Value.readback` 必须是带有类型的 readback，
其类型不应是 `(ctx: Ctx, value: Value) => Exp`，
而应该是 `(ctx: Ctx, t: Ty, value: Value) => Exp`。

## 简化

除了纠正上面的错误，在再次尝试实现时，我们还要注意：
- 用 curry 的 typed lambda 而不用 church 的。
- 简化语言 -- 简化表达式，简化 telescope 的结构。

## Normal Form in fulfilling type system

我们也可以尝试在 fulfilling type system 中定义 Normal Form 来加深理解。

``` cicada
@datatype Exp {
  v(name: String): Exp
  type: Exp
  pi(name: String, arg_t: Exp, ret_t: Exp): Exp
  fn(name: String, ret: Exp): Exp
  ap(target: Exp, arg: Exp): Exp
  obj(map: Array([String, Exp])): Exp
  dot(target: Exp, name: String): Exp
  the(t: Exp, exp: Exp): Exp
}
```

## `obj` 而非 `cls` 与 `obj`

上面只写了
  obj(map: Array([String, Exp])): Exp
而不是
  obj(map: Array([String, Exp])): Exp
  cls(tel: Array([String, Exp])): Exp

因为 `obj` 如果能与 `cls` 共用一种 `Value.obj`，
就能简化 `Value.conversion` 的实现。

并且 `obj` 与 `cls` 可以共用语法 `{ k: T, ... }`，
将 `{ k = T, ... }` 留给 `suite`。

## 限制 fulfilling 的方向

在再次尝试实现时，我们还要注意：
- 需要实现 fulfilling type，因为形式化范畴论之类的数学结构需要这个功能，
  但是可以限制 fulfilling 的方向必须是从前向后。
- unnamed type constructor application syntax 此时也很容易实现了。

## `:` 与 `<:` 是否应该统一？

在 fulfilling type system 中 `:` 与 `<:` 是否应该统一？

如果只看 class 与 object，统一看来是可行的。
因为 `:` 可以被理解为 fully fulfilling，
而 `<:` 可以被理解为 partly fulfilling。
并且，一个 obj 作为类型，所描述的，可以是与其相等的元素。
这么说，函数也可以作为类型，它所描述的，也是与其相等的函数。

但是我们还是分两个 judgement 来实现，
以避免太多的变化要考虑。

## 如何理解「等价关系是就类型而言的」？

"在对两个 value 之间的等价形成判断 judgement 时，需要指明类型。" 其道理并不显然。
但是，"在对两个 obj 之间的等价形成判断 judgement 时，需要指明类型。" 其道理看来是显然的，
例如，`x = { a = a1, b = b1 }`，而 `y = { a = a2, b = b2, c = c2 }`，
当用来形成等价判断的类型是 `T1 = { a: A }` 时，二者可能是相等的，
但是当使用 `T2 = { a: A, b: B }` 来做判断时，二者可能就是不相等的了。
