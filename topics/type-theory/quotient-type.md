---
title: Quotient type
---

quotient type 要通过改变 equivalence relation 来定义，
只要给每个 datatype 属于它自己的 equivalence relation 就行了，
但是，函数的单值性检查怎么办？

方案 A：

- 允许不单值的函数
- 单值性成为 class 的 axiom 的一部分，比如：
  - Category -- compose
  - Group -- mul inv
    由于单值函数的复合还是单值的，
    这样对于所发展的抽象理论而言，
    就不需要证明单值性了。

我们需要设计语言使得表达这些变容易。

# quotient type and CPL (constraint logic programming)

TODO

# 关于 quotient type 的现象

如果结合律与 List 这个数据类型对应，
结合律 + 交换律 与 DupSet (Set With Duplication) 这个数据类型对应，
结合律 + 交换律 + 幂等 与 Set 这个数据类型对应，
那么交换律与 什么 数据类型对应？
是否有一般的理论来描述这种对应？

注意这些运算律都是对二元运算而言的，
所以其实是在 二叉树 让引入不同的等价关系，
用 二叉树的图形 以及对 二叉树的图形 的形变（Tree rotation）（即 考虑 形变所形成的 等价关系），
可以发现 结合律 总是可以让我们把 二叉树 变为等价关系中一种特殊的代表形式（等价类的代表元） -- 即 List。

也就是说，有了 形变（等价关系）之后，找到 代表形式，
然后限制数据类型的定义，使得其只包含这种 代表形式 中的信息，
这就是我们所观察到的「现象」。

# unorder-ness

如何在数据结构中表示 unorder-ness？

如果和用 ordered pair 来表示 unordered pair？
如果元素之间有全序关系，可以通过排序来解决，
如果元素之间没有全序关系，好像没法化归到某种 代表形式。
如果没法化归到 代表形式 就只能通过实现非平凡的 等词 来处理了。
