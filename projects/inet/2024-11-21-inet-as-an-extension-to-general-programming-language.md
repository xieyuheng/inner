---
title: inet as an extension to general programming language
date: 2024-11-21
---

看现在 inet 的实现，其实是在一个 forth-like 上扩展出来的。
只要给增加两个数据类型 node 和 wire，外加一个 @interact builtin。
execute a node 要被理解为构造 graph。

不只是 forth-like 和 concatenative language 可以这样扩展出来 inet，
applicative language 也可以，并且也有很好的性质，可以 auto curry。
此时 apply a node 要被理解为构造 graph。

我想问的问题是，
一个 general programming
具有什么属性才方便这种扩赞？

- 要支持 generic function，
  因为一个函数作用于 wire 和 primitive value 是不同的。

- 要可以扩展语法，不需要复杂的 macro system，
  只要能增加 top-level statement 就可以。

可能不需要的属性是：

- GC -- 因为 inet 不需要 GC。
