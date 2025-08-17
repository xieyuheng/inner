---
title: forth-like threaded code
date: 2025-08-17
---

applicative language 的核心语法是 application，
`(f x y)` 或 `f(x, y)`。
其组成部分可以称为 target 和 arguments，
applicative 语言可以根据不同类型的 target value，
做不同的 application 操作，
相当于对函数作用（apply）做了 overload。

concatenative language 的核心语法一般理解为是 composition，
但是具体实现是可以发现 overload 的是 name lookup，
或者说 word lookup 或者说 call 或者说 reference。
每一个 word 都可以理解为对整个 world 的 effect，
所谓 overload lookup，就是根据 word 下保存的不同类型的东西，
对 world 做不同的 effect。

相比之下，application language 的 lookup 是很简单的，
就是取出 name 所对应的 value。
