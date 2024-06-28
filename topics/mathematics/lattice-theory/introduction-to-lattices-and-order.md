---
title: Introduction to lattices and order
subtitle: Second Edition
authors: [B. A. Davey, H. A. Priestley]
year: 2002
---

# 动机

读这本书的目标有两个：

- 一个是熟悉 FCA；
- 一个是看 propagator 中 cell 保存的东西，
  能否以某种 lattice 为 interface。

# 1. Ordered sets

## Ordered sets

一个 ordered set 就在于 reflexivity，antisymmetry 与 transitivity。

- (1) x == y -> x <= y & y <= x
- (2) x <= y & y <= x -> x == y
- (3) x <= y & y <= z -> x <= z

与 ordered set 等价的名字有 partially ordered set 和缩写 poset。

不要求 antisymmetry 称为 pre-order 或 quasi-order。

等价关系是退化的 order 称为 discrete order。

如何命名这个关系是个问题，
less than or equal to 太长了，也许 lteq 可以，
单个的英文词有 below 或 under，
这两个词暗示了画图描述 lattice 而方式，
也许 below 好一些。

最典型的 order：

- 数字而小于等于 x is less than or equal to y
- 集合的包含关系 x is included in y
- 命题的蕴含关系 x implies y

## Examples from social science and computer science

注意这里定义 interval order 的方式不是 interval 之间的包含关系，
而是两个 interval 的数字之间的大小关系：

```
x <= y := x.right <= y.left
```

- 后面也会提到用 interval 之间的包含关系来定义的 order。

这一章提到的 social choice function 非常有趣。
这是关于投票，或者集体意志和集体偏好的。

假设有 n 个人，给 m 个东西排序，
每个排序代表一个人对这 m 个东西的喜好程度，
如何获得这个集体对这个 m 个东西的喜好程度呢？

可能人们的喜好一样，但是不同的标准可以给出不同的集体喜好。
在社会学意义上，这个结果就很有意思了，
因为可以解释很多社会现象。

另外举了组织工业生产和组织会议等例子，
在设计解决方案的过程中，要求解 constraints，
而 constraints 通常是用 order 关系表达的。

可以说哪里有排序，哪里就有比较，也就有序理论。

> ..., concept analysis builds an ordered set which reveals inherent
> hierarchical structure and thence natural groupings and dependencies
> among the objects and the attributes.

formal concept analysis 也被算到 humanities 这一节了，
其实将其看作是计算机科学，甚至是纯数学也可以。

[propagator] 在 propagator 的 cell 中，
保存 supported value 时，
support 的集合越小，代表信息越多，
也许应该被理解为 FCA 中的 attributes。

将要讲序理论在计算机领域的应用了，
这与我们的目的 propagator，息息相关了。

> In each case, a relation ⩾ serves to capture the notion of "is at
> least as informative as", with the precise interpretation depending
> on the context.

[propagator] 也就是说，就在 propagator 的 cell 中保存 interval 而言，
interval A 被包含在 B 中，反而代表 A 更精确，包含的信息更多，
因此 A ⩾ B -- A is at least as informative as b。
propagator 用的正是这种意义上的序关系，
merge 两份信息，会得到更多的信息，
及在 ⩾ 的意义上更大。

这里定义程序的时候，其实只要以 record 为 value，
就可以将处理输入 state 和输出 state 的 program，
理解为处理输入 record 和输出 record 的 function 了。

这里 partial maps 显然是关于递归函数的，
因为我们可以将递归函数视作是由无穷多个 partial maps 的极限。

## Diagrams: the art of drawing ordered sets

和图论一样，ordered sets （的有限局部）可以被画出来。
由于 below 这个关系中间可能有别的元素，
所以为了画图，需要重新定义一个中间没有元素的关系。
比如称作 beneath 或者 cover。

根据 order 的公理，所画出来的是无圈的有向图。

显然任何有向无环图都可以画成
[Hasse diagram](https://en.wikipedia.org/wiki/Hasse_diagram)，
可以理解为将有向图化成所有的箭头的指向下方的样子。

- 以 `below(low, high)` 为关系，
  用不等号表示就是 `low <= high`，
  为了好记忆，把不等号看成是有向图中的箭头，
  这样箭头就是指向下方的。

在 Hasse diagram 可以更直观地通过高低看出 below 关系，
而不用跟随有向图的 arrow。

- 比如在 FCA 中，所引出的中间概念，
  就出现在 Hasse diagram 的中间。

## Constructing and de-constructing ordered sets

**dual ordered set**，就是 Hasse diagram 在上下方向的镜面反射。

**top 和 bottom** 元素：

> In the context of information orderings, ⊥ represents "no
> information", while ⊤ corresponds to an over-determined, or
> contradictory, element.

[propagator] 这与 propagator 中的 nothing 和 contradictory 是否一致？

top 和 bottom 元素也可以视为构造新 ordered set 的方法，
因为对于没有 top 或 bottom 的 ordered set，
可以添加一个，获得新的 ordered set。

对于一个 ordered set 的子集，
可以定义 **maximal 和 minimal 元素**，
注意与 top 和 bottom 不同，
maximal 和 minimal 可能不唯一。

> ... when an order relation models information, we might expect a
> correlation between maximal elements and totally defined elements.

**disjoint sum** 在图上看，就是把两个 Hasse diagram 并列摆在一起，中间没有连线。

**linear sum** 在图上看，就是把两个 Hasse diagram 上下摆在一起，
并且让上图的所有极小元都连接到下图的所有极大元。

给 ordered set 增加 top 和增加 bottom 元素，
都可以被看成是特殊的 linear sum。

disjoint sum 和 linear sum 显然都满足结合律。

[propagator] 在 propagator 中，
一个 cell 可以保存多种类型的元素，
这些元素的并，作为更大的 ordered set，
之间的 order 有时是用 disjoint sum 来定义的，
比如 String 和 Number 之间；
但有时是用分层次的 linear sum 来定义的，
比如 Number <= Interval <= Supported <= BeliefSystem。

另外有两个构造 primitive ordered set 的函数：

- 一个是 `Chain(n)` -- `Nat` 的子集合，继承序关系；
- 一个是 `Antichain(n)` -- `Nat` 的子集合，带有离散的序关系。

用上面的两个 sum 和两个 primitive 函数，
已经可以构造出来很多有趣的有限 ordered sets 了。

另外有 `PowerSet(set)` 这个 promises 函数也很常用。
比如 `PowerSet({1, 2, 3})` 是 cube。
结构化的群也可以给出 ordered sets，
比如群的 `SubgroupOrder` 和 `NormalSubgroupOrder`。

- 也许这里的命名不对，因为根据后面两个命名，
  `PowerSet` 应该叫 `SubsetOrder`。
  但是好像没有更好的命名了。

**product** 一组 ordered set 的笛卡尔积，
可以被视为一个 ordered set，
其中 order 可以逐坐标地定义
-- **product by coordinatewise order**。
这很严格，只有每个坐标维度上的值都对应满足 below 时，
两个乘积元素才满足 below。

另外一个重要的 product
是 **product by lexicographic order**，
即编程中常见的字典序乘积。
也是有笛卡尔积构成，
但是定义 order 的方式不一样，
就像在英文字典中给单词排序而方案一样，
乘积前面的元素占主导地位，
只有前面的 prefix 相等的时候，
才需要比较后面的元素。

[propagator] Supported value 也是由笛卡尔积构成的，
中包含一个 order set `T`，和一个 `PowerSet(String)`，
它用的是 字典序 还是 逐坐标序 呢？
可能是类似 逐坐标序 但是又有一些变化，
我们之后要用 order theory 的数学语言把这一点明确下来。

> Informally, a product P * Q is drawn by replacing each point of a
> diagram of P by a copy of a diagram for Q, and connecting
> "corresponding" points.

"corresponding" points 就是第二个维度元素相等的点。
注意，画出来的 Hasse 图还要满足 Hasse 图需要的几何条件。
在想象 product 的 Hasse 图时，
可以想象沿着 P 中箭头复制与平移整个 Q。

有趣的例子：

```
Chain(2) * Chain(2) * Chain(2) =
PowerSet({1, 2, 3}) =
Cube
```

显然 `Chain(2)` 的 `n` 次方，
与 `PowerSet({1, 2, ..., n})` 是同构的，
因为前者有 `n` 个坐标维度，
每个维度可以记录 `n` 各元素是否出现。

## Down-sets and up-sets

**down-set** 是子序集向下的闭包，
又称 **decreasing set** 和 **order ideal**；

- 当考虑 lattice 中的 meet 和 join 运算时，
  这个定义可能与环的理想的定义类似，
  所以也叫理想。

**up-sets** 是子序集向上的闭包，
又称 **increasing set** 和 **order filter**。

我们可以把 `DownClosure` 和 `UpClosure` 视为两个函数
（我们不用 `UpSet` 因为听起来不好），

`DownClosure({x})` 与 `UpClosure({x})` 也称为 principal。

- 也许类似环中的主理想。

给定一个序集 `P`，
考虑它的所有 `DownClosure` 构成的 ordered set
-- 称为 `PowerDownClosure(P)`。

因为我们可以用一个 DownClosure 中的极大值来代表这个 DownClosure，
所以显然可以到到一个计算（表示） `PowerDownClosure` 的方式。

注意，有趣的是：

- 如果 `P` 是 `Antichain(n)`，
  那么 `PowerDownClosure(P) == PowerSet(P)`；

- 如果 `P` 是 `Chain(n)`，
  那么 `PowerDownClosure(P) == Chain(n+1)`
  （因为带有空集所以加一）。

`PowerSet(P)` 代表了全量的搜索，
`PowerDownClosure(P)` 是否代表根据 `P` 的序关系进行的剪枝呢？

从书中给出的一些例子可以看出，
`P` 的对称性会以有趣的方式出现在 `PowerDownClosure(P)` 中。

连接序关系和 `DownClosure` 的引理：

> Let P be an ordered set and x, y ∈ P . following are equivalent:
> (1) x <= y;
> (2) DownClosure(x) ⊆ DownClosure(y);
> (3) (forall Q ∈ PowerDownClosure(P)) -> (y ∈ Q -> x ∈ Q).

也就是说，元素之间的序关系，被转化为了集合之间的包含关系。

同时，ordered set 之间的 dual，也可以被转化为集合之间的补：

> Q is a down-set of P if and only if P\Q is an up-set of P
> (equivalently, a down-set of Dual(P)).

有范畴论意义上的自然同构（Natural transformation）：

```
PowerDownClosure(Dual(P)) ≅ Dual(PowerDownClosure(P))
```

书中还列举了很多 `PowerDownClosure`
和构造 ordered set 的运算（各种 sum 和 product）之间的自然同构。

TODO 练习 1.32 Proposition 中的证明和运算。

## Maps between ordered sets

TODO

## Exercises

# 2. Lattices and complete lattices
# 3. Formal concept analysis
# 4. Modular, distributive and Boolean lattices
# 5. Representation: the finite case
# 6. Congruences
# 7. Complete lattices and Galois connections
# 8. CPOs and fixpoint theorems
# 9. Domains and information systems
# 10. Maximality principles
# 11. Representation: the general case
# Appendix A: a topological toolkit
# Appendix B: further reading
