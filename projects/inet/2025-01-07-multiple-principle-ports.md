---
title: multiple principle ports
date: 2025-01-07
---

我感觉只有支持了 multiple principle ports，inet 才算是能实用。
比如 primitive int 之间的 add，就是要等待两个参数都到齐才能作用。

考虑之前对图的分析，感觉有多个 principle ports 也不影响并行运行：

```
     \   |   /
  .-------------.
  |    \ | /    |
  |   (.....)   |
  |      |      |
  |   (.....)   |
  |    / | \    |
  `-------------`
     /   |   \
```

为了实现 multiple principle ports，
在 connect 的时候，就需要判断所连接的局部 net 是否可以 interact，
然后找到对应的 rule，构造一个 activity 到 vm 的 activity list 中。

如果每个 node 只有 single principle port，
对于局部 net 是否可以 interact 的判断就非常简单。
想要支持 multiple principle ports 就是要推广这个判断。

在类 lisp 的语法中可以模仿 prolog 的 logic variable，
用 pattern matching 中重复出现的 variable 来标记 connection 的位置。

```scheme
(define-rule*
  [(add target! addend! result)
   (int x target!)
   (int y addend!)]
  (connect result (int-add x y)))
```

也许 primitive add 并不是一个好例子，
实施 nat-max 的例子：

TODO

```scheme
TODO
```

```forth
define-rule
  TODO
then
  TODO
end
```
