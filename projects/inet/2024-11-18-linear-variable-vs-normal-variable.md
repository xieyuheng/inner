---
title: linear variable v.s. normal variable
date: 2024-11-18
---

想要支持 primitive types，
一个重要的问题是区分 linear variable 和 normal variable。

保存 wire 的是 linear variable，
因为每个 wire 只能连一次。

但是保存 primitive value 的不是 linear variable，
比如实现 list 的 map 时，函数要被引用多次。
其他 int 之类的 primitive value 也不能用 linear variable。

但是这种区分有些问题，
因为假设 `(map)-fn` 保存函数，
这个变量也可能连接一个有待计算出来的函数，因此是 wire，
而不是具体函数的 primitive value。

根据 linear logic，
可能正确的处理方式是，
根据类型来判断一个变量中的值，
是否取出来之后就不能再用了。
