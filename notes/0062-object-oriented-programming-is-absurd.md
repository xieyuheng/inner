---
title: OOP 是荒谬的
date: 2025-10-12
---

# 归谬法

第一次发现 OOP 有大问题，
是在尝试设计新的 OOP 语言的时候，
发现的一个归谬法证明。

首先，OOP 的特点是：

- class 不是纯的 interface（record type），
  class 中的某些 attribute 不只之给出了 type，
  还要给出 value，这些就是 class 中所定义的 method。

- object 中除了包含作为数据的 attribute，
  还要包含处理数据的 method。

这种写程序的方式是荒谬的。

假设这种写程序的方式是正确的，
那么它应该适用于各种数据类型，
但是对 nat 或者 int 来说，
这显然是荒谬的。

设想 nat 和 int 被定义为两个 class，
它们应当包含那些 method？
可能说要包含核心的处理函数。
但是人们想要针对 nat 和 int 实现的处理函数，
可能有成千上万个，哪些是核心的，哪些不是？
任何划分都是完全主观的，是任意的。

哪些 function 应该被作为这两个 class 的 method，
其实只有一个客观准则，那就是不应该有任何 method。
所有的 nat 和 int 的处理 function 都应该是独立的。

这在 OOP 中经常说的「接口分离原则」中已经有所暗示了。
所有的接口都分离，就是所有的 function 都独立。

# 继承与代码复用

用继承的方式来重用代码是错误的。

EOC 的课程代码就是最好的反面例子，
为了课程渐进地进行，
老师实现了一系列相互继承的解释器 class。

本身一个好端端的解释器递归函数，
被写到了支离破碎的很多 class 中。

# 关于 simulation

OOP 起源于对世界的 simulation 是错误的。

人们经常这么说，因为 OOP 起源于 simula 语言，
而 simula 语言设计支出是用来做物理 simulation 的。

但是这是错误的，
OOP 的 feature 并不能帮助 simulation，
也不能帮助写 GUI。

真正能帮助 simulation 和 GUI 的，
是 reactive programming。

vue 和类似的前端 UI 框架，都是例证。

# 结论

这对于设计新语言的人来说是好事，
因为大多数已有的语言都是 OOP 的，
都是荒谬的。

这也是坏事，因为它证明了人群是荒谬的，
这么多年下来，人们没能对 OOP 给出充分的批判。

# 再反思

[2025-11-01]

我发现，虽然没有使用 OOP 的 dot syntax，
也没有把 method 放到 class 里，
但是我依然践行着 Sandi Metz 所教授的 OOP 技巧中的优点。

只不过从 a lot of little objects and methods，
变成了 a lot of little functions。

以 x-lisp 中实现的 graph API 为例：

```scheme
(graph? vertex-p value)
(graph-edge? vertex-p edge)
(make-graph vertices edges)
(make-empty-graph)
(graph-copy graph)
(graph-vertices graph)
(graph-vertex-count graph)
(graph-empty? graph)
(graph-edges graph)
(graph-equal-edge? lhs rhs)
(graph-equal-edges? lhs rhs)
(graph-neighbors vertex graph)
(graph-add-vertex! vertex graph)
(graph-has-vertex? vertex graph)
(graph-delete-vertex! vertex graph)
(graph-add-vertices! vertices graph)
(graph-add-edge! edge graph)
(graph-has-edge? edge graph)
(graph-delete-edge! edge graph)
(graph-add-edges! edges graph)
(graph-adjacent? source target graph)
(graph-degree vertex graph)
(graph-max-degree graph)
```

从 OOP 的角度，上述简单 API 看来应该属于 graph 这个 class。
可是下面的 coloring 却比较复杂，不应该作为 graph 的 method。

```scheme
(graph-coloring! coloring vertices graph)
(graph-coloring graph)
```

我可以写一个 `graph-coloring` module，
不必使用 OOP 的技巧定义新的 class，
直接实现相关的函数。

作为 `graph` module 的用户，
所实现的 `graph-coloring!` 和 `graph-coloring` 函数，
与 graph 本身的处理函数是同等级的。

如果是 OOP，那么作为用户所实现的 method 就是次一级的 method 了，
可能需要定义新的 class，不能直接用 graph 的 dot syntax。

Sandi Metz 所讲授的 OOP 技巧还有一个重点是，
"send message" 的时候不必知道 object 的具体 class，
只要知道 object 可以接受这个 message，
并且其行为符合某个 interface。

在 x-lisp 中，大部分函数是需要知道 target 的具体类型的，
但是其实也是可以做到 interface 的效果的，
并且也需要 interface 的功能。
实现这一效果的语言 feature，
是 sussman 经常说 的 generic dispatching。
比如在实现 propagator model 时，
大量使用了 generic function。

因此 Sandi 所讲授的技巧是完全适用的。
只不过在 x-lisp 中，可以在大部分时间使用普通的函数，
推迟实现 interface 的决定。

# 抵抗 OOP API 的诱惑

[2025-11-18]

考虑 [honojs](https://hono.dev/docs/getting-started/basic) 的 context API：

```js
app.get('/posts/:id', (c) => {
  const page = c.req.query('page')
  const id = c.req.param('id')
  c.header('X-Message', 'Hi!')
  return c.text(`You want to see ${page} of ${id}`)
})
app.post('/posts', (c) => c.text('Created!', 201))
app.delete('/posts/:id', (c) =>
  c.text(`${c.req.param('id')} is deleted!`)
)
```

也就是上面通过 c (context) export 的 API function：

```
c.req.query
c.req.param
c.header
c.text
```

这类似于把 callback function 的参数当作模块系统来用，
这种 API 看似很简洁，但是其实有问题。

主要在于 library 和 framework 的设计者，不能穷尽所有的处理函数。
这里想要处理的是 request 和 response，
用户想要处理这些数据的方式是无穷的，
把其中一部分实现为特殊的 context API，
会使得用户所实现的处理函数为「二等公民」。

这与对 nat class 所用的「归谬法」类似。

上述 API 完全可以实现为多带一个参数的函数：

```
req-query(req)
req-param(req)
req-header(req)
req-text(req)
```

这样用户所实现的处理函数，
就和 library 作者所实现的处理函数同级别了。
