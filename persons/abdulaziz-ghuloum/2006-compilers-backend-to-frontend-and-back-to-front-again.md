---
title: "compilers: backend to frontend and back to front again"
author: abdulaziz ghuloum
year: 2006
---

# My Motive

[2025-06-06] 这个 tutorial
是 2006-an-incremental-approach-to-compiler-construction 的补充。
这种写法的特点是 incremental 或者说 TDD，但是没有 nanopass。

在读 jeremy siek 的 essentials of compilation（EOC）时，
作者是用 ADT 来定义 program、c-program 还有 exp 的。

这里是如何处理的？
好像没有 ADT 也没有 class。
难道就是直接用 sexp？

就是直接用 sexp！

长时间使用静态类型语言之后，
我甚至已经忘记了还可以直接用 sexp。

缺点：

- 直接用 sexp 的一个严重问题是没有良好的报错。

优点：

- 这才是 lisp 的本质，
  所谓设计的好的 lisp 语法，
  可能就是能够被当作列表处理的 lisp 语法。

# Preface

TODO
