---
title: cell complex vs. cobordism
date: 2024-09-27
---

几何建模需要以拓扑建模为基础，
而拓扑建模有很多中方案。

最广为人知的是 cell complex，
在很多人的认识中 cell complex 就是代数拓扑的研究对象本身。

最近我发现 cobordism 也可以用于构造代数拓扑的研究对象，
并且与 cell complex 相比，
cobordism 可以简化两个元素粘连（glue）相关的 API。

注意，在实现 glue 时，
如果已知 glue 之前两个元素的边界，
要能够求出 glue 之后所得元素的边界。

cell complex：

```scheme
(check boundary
  (-> (element n)
      (spherical (sub1 n))))

(check glue
  (-> (element n) (element (sub1 n)) (element n)
      (element n)))
```

cobordism：

```scheme
(check boundary
  (-> (element n)
      (list (spherical (sub1 n)))))

(check glue
  (-> (element n) (element n)
      (element n)))
```

为了能够像 cell complex 构造拓扑元素，
在 cobordism 的语境下，
只需要 `(list (spherical (sub1 n)))`
能够确定唯一一个 `(element n)` 就可以了。
这显然是满足的。
