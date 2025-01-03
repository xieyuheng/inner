---
title: the plan of x-forth
date: 2025-01-03
---

xvm 很难让人满意，所以回到类 forth 语言。

目的是实现一个自己用起来舒服的通用语言，
来代替目前在 c 中使用的 scalable-c 编程风格。

要能够用 x-forth 来方便地写一些实验性程序，比如：

- lambda 演算的解释器
- explicit subst 解释器
- linear logic 等等类型系统的实现

这些看来需要一个简单类型系统才能让人用起来满意。

如果能用 costack 来简化类型检查过程，就可以，
否则类型检查还是太复杂了。
