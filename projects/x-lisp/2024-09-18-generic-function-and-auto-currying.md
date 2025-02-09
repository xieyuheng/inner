---
title: Generic Function and Auto Currying
date: 2024-09-18
---

注意，generic function 会让 auto currying 变复杂。

如何处理 generic function 的 auto currying？

多个 handlers 的定义使得 auto currying 有歧义的例子：

```scheme
(define (f (a A) (b B) (c C)) ...)
(define (f (a A) (b B)) ...)
```

在上面的定义下，`(f a b)` 可能代表：

- （1）匹配到第一个定义并且做 auto currying。
- （2）匹配到第二个定义。

模仿 prolog 用 `/n` 后缀，
来明显地把所能匹配到的函数的范围限制在 arity 为 n 的函数。

当没有加后缀的时候，
我们还是要从上面的（1）（2）中选择一种作为默认行为，
哪一个是合理的呢？

优先匹配短的比较合理，
因为带有更多参数的定义，
可以被看作是同一个函数带有 optional arguments 的版本。

注意，我们不能禁止 generic function 对 arity 的 overload，
因为对于同一个函数名字，不同模块可能会定义 arity 不同的函数。

考虑到模块之间是相互独立的，
还有可能出现的情况是后 import 的函数，覆盖前面定义的函数。
此时我们应该以后 import 的函数为准（也就新 import 的函数为准）。

但是，这样就意味着一个表达式的意义依赖于前面定义的顺序。
由于类型检查的过程中可能会调用函数，
所以这将导致在前面类型检查不通过的表达式，
在某些定义之后类型检查通过了（或者反过来）。

我想我们应该没有更好的方式来避免啊这种情况了。

------

要注意的是必须保持模块之间相互独立这个约束条件。

不能像在 JS 中嵌入 generic function 时那样，
在 import 一个模块时，利用这个模块的副作用，
来把新的 handler 定义到某个 generic 上。

正确的处理方式是，在 import 的时候，
把相关的 handler patch 到当前 module 的，
代表 generic function 的 cell 中。

------

[2025-02-09] 还有一种方案是禁止函数的 auto currying。
或者设计新的语法来明显表示 currying。

注意，inet-lisp 中，
node constructor 是带有 auto currying 的，
并且其语义与 function 的 auto currying 不同。

如果允许不同类型的 value 有不同的 auto currying，
- 禁止 generic auto currying。
- 允许 function auto currying。
就可以了。
