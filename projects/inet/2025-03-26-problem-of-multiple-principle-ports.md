---
title: problem of multiple principle ports
date: 2025-03-26
---

问题不在于计算的不确定性，
而是在于 multiple principle ports
必定导致 subgraph pattern matching，
而后者很难并行：

- thread 1 -- 开始 match 一个子图 A
- thread 2 -- 开始 match 的子图 B 与 A 相交
- thread 2 -- match 到 B 与 A 相交部分的点 a（此时没有被标记为 match）
- thread 1 -- match 成功，并且标记 A 中的 node 为 matched
- thread 1 -- 把 match 的结果作为 task 返回
- thread 3 -- 接收到上面的 task，处理 task 的过程中删除 A
- thread 2 -- 继续处理点 a，但是其实 a 已经被删除了

一个补救的方案是使用 embarrassingly parallel 的方案：

- 分批给所有 worker thread 分配 task
- 每次所有 task 处理完之后再分配下一批 task

这样在处理某一批 task 的时候，
在同一批中被 matched 点不会被删除，
在处理下一批的时候，可能被删除的点都已经被标记为了 matched。

[2025-03-27] 备选方案：

如果在 inet-lisp 中 embarrassingly parallel 方案失败，
就回到 inet-forth 中来实现 parallel，如果成果，
可以从 inet-lisp 中删除 multiple principle ports 的功能。

因为我还是觉得 lisp 的语法相比 forth 有优越性。

主要是因为：
- 在实现 forth 时，是要对不同的 definition 实现 call；
- 而在实现 lisp/scheme 时，是要对不同的 value 实现 apply。

forth 必须要同时有 value 和 definition，
而 lisp 可以只有 value。
或者可以说，applicative 的函数作用语法，
把 call 进一步 factor 成为了 lookup + apply。

这使得在 lisp 中更方便实现 list-map 之类的高阶函数：

```scheme
(define-node list-map target! fn result)

(define-rule (list-map (null) fn result)
  (null result))

(define-rule (list-map (cons head tail) fn result)
  (= fn fn* (fn-dup fn))
  (cons (fn head) (list-map tail fn*) result))
```

或者可以说，在 forth 中引用每一个 name 时只有一种解释方式，就是 call；
而在 lisp 中有两种引用 name 的方式 -- `name` 和 `(name ...)`。

可能说 lisp 有优越性并不对，
因为 forth 的函数复合语义也有很大优点。

到底应该如何分析这二者的优劣呢？
