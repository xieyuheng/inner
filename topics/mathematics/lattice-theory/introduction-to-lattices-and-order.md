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

- (1) x == y -> x <= y & y <= x -- reflexivity
- (2) x <= y & y <= x -> x == y -- antisymmetry
- (3) x <= y & y <= z -> x <= z -- transitivity

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

[propagator] 在实现 propagator 时，
可能应该实现一个 generic 的 compare(a, b) 函数，返回四种可能：

- < -- a < b
- = -- a = b
- > -- a > b
- | -- a 与 b 不可比较

但是这可能并不合理，
因为需要调用 below(a, b) 和 below(b, a) 两次，
所以应该实现的 generic 是最基础的 below。

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

- 以 below(low, high) 为关系，
  用不等号表示就是 low <= high，
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

## Exercises

关于练习，对于一般的数学书，我以前经常没有耐心去做练习。
但是 Dan 的 little books 也带有很多练习，
为什么对与 little books 我就有耐心做练习呢？

可以在做一般的数学书中的练习时，把它们想象成 little book 吗？

练习就像写程序过程中的测试一样，
是为了检验自己是否真的掌握了知识。
对于定义一类的知识，特别适合用 mimor 来辅助记忆。

# 2. Lattices and complete lattices

> Many important properties of an ordered set P are expressed in terms
> of the existence of certain upper bounds or lower bounds of subsets
> of P.  Two of the most important classes of ordered sets defined in
> this way are lattices and complete lattices.

Lattice 中的二元运算可以理解为，
两个元素的集合的 upper bound（可能不在集合之内了）。

虽然 Lattice 有二元运算，类似代数结构，
但是以 upper bound 和 lower bound 为基础，
可以以统一的方式去理解 complete lattices。

## Lattices as ordered sets

> It is a fundamental property of the real numbers, R, that if I is a
> closed and bounded interval in R, then every subset of I has both a
> least upper bound (or supremum) and a greatest lower bound (or
> infimum) in I. These concepts pertain to any ordered set.

join(a, b) 和 meet(a, b) 存在的 ordered set 称为 **lattice**。

sup 是 supremum 的缩写 least upper bound；
inf 是 infimum 的缩写 greatest lower bound。

sup({a, b, c, ...}) 和 inf({a, b, c, ...})
存在的 ordered set 称为 **complete lattice**。

lattice 要求 sup 和 inf 对于任意有限子集存在；
complete lattice 要求 sup 和 inf 对于任意子集存在。

在 lattice 中，当 x ⩽ y 时，
join(x, y) = y 且 meet(x, y) = x。

并不是对所有的 ordered set，这两个运算都存在。

> In an ordered set P, the least upper bound join(x, y) of {x, y}
> may fail to exist for two different reasons:
> (a) because x and y have no common upper bound, or
> (b) because they have no least upper bound.

想要求 join 或 sup，
要先找到所有 upper bound 的集合，
然后再找到集合中的唯一最小元。

TODO 2.6 Examples.

TODO 2.7 Lattices of subgroups.

## Lattices as algebraic structures

∨, ∧ 与 ⩽ 之间的系：

- (1) a ⩽ b;
- (2) a ∨ b = b;
- (3) a ∧ b = a.

a ⩽ b -> a ∨ b = b

如果 a ⩽ b，即 a 在 b 之下，那么显然二者的上确界就是 b。

a ⩽ b <- a ∨ b = b

如果 a ∨ b = b，即 b 是 a 与 b 的上确界，那么作为上界，我们就已经有 b >= a。

在 ordered set 中定义的 join 和 meet 满足运算律：

- associative laws
- commutative laws
- idempotency laws
- absorption laws

反过来从代数结构的角度看，
带有 join 和 meet 运算，
并且满足上述运算律的代数结构，
就是 lattice。

假设 join 和 meet 满足上述运律，
那么 join(a, b) = b 等价于 meet(a, b) = a，
可以中这个等式来定义 below(a, b) 关系。

也许 join 和 meet 可以比喻为爬山，
join 意思是在上山的路上汇合，
meet 意思是在下山的路上相遇。
尤其是对于带有 top 的 lattice 来说，
这个比喻尤其贴切。

TODO 反过来证明，由 join 和 meet 定义的 ordered set，
满足从 ordered set 出发的两个元素的集合的 上确界 sup 和 下确界 inf 的定义。

如果一个 lattice 中 top 和 bottom 存在的话，
那么它们满足代数结构中单位元的属性：

- forall a, a = join(a, bottom)
  join 类似加法，bottom 类似 0；

- forall a, a = meet(a, top)
  meet 类似乘法，top 类似 1。

虽然类比了加法和乘法，但是其实差异很大，
因为 lattice 中两个运算是完全对称的。

其实，下面的三个等价的关系，
说明 join 与 meet 更类似于 max 与 min。

```
a <= b
join(a, b) = b
meet(a, b) = a
```

类比 Number 中的关系：

```
a <= b
max(a, b) = b
min(a, b) = a
```

带有 top 和 bottom 的 lattice 称为 **bounded lattice**，
因为 join 与 meet 总是存在，
所以有限 lattice 中 top 和 bottom 一定存在。

还有一个典型的 bounded lattice
是 Nat 上的整除（divisible）所给出的序关系，此时：

- below(a, b) := divisor(a, b) 或 multiple(b, a) 或 factor(a, b)
- join(a, b) := lcm(a, b) -- least common multiple
- meet(a, b) := gcd(a, b) -- greatest common divisor
- bottom := 1 -- 1 是任何自然数的因子
- top := 0 -- 任何数乘以 0 都得 0

## Sublattices, products and homomorphisms

一个 lattice 的，
对 join 和 meet 运算封闭的子集，
是这个 lattice 的 sublattice。

关于 homomorphisms，
保持运算 join 与 meet 的 lattice homomorphism，
与保持 order 的 ordered set homomorphism 不同。

- (1) The following are equivalent:

  - (a) f is order-preserving;
  - (b) (∀a, b ∈ L) f (a ∨ b) ⩾ f (a) ∨ f (b);
  - (c) (∀a, b ∈ L) f (a ∧ b) ⩽ f (a) ∧ f (b).

  也就是说，order-preserving
  只能给出 join 与 meet 相关的不等式，
  而不能给出等式。

  In particular, if f is a homomorphism,
  then f is order-preserving.

- (2) f is a lattice isomorphism
  if and only if it is an order-isomorphism.

## Ideals and filters

一个 lattice 的 **ideal** 是对 join 封闭的 DownClosure 子集。
DownClosure 要求小于子集中任意一个元素的元素，还在子集中。
DownClosure 使得 meet 也在子集中，因此子集也是 sublattice。

对偶地：

一个 lattice 的 **filter** 是对 meet 封闭的 UpClosure 子集。
UpClosure 要求大于子集中任意一个元素的元素，还在子集中。
UpClosure 使得 join 也在子集中，因此子集也是 sublattice。

DownClosure({x}) 总是 ideal，并且称为 principal ideal；
UpClosure({x}) 总是 filter，并且称为 principal filter。

在有限 lattice 中，每个 ideal 和 filter 都是 principal，
即都是由一个元素的 DownClosure 和 UpClosure 生成的。

对一一个保持 top 和 bottom 的 lattice homomorphism 而言，
ideal 和 filter 分别是两个 kernel：

- ideal 是 0（bottom）的逆像；
- filter 是 1（top）的逆像。

对于拓扑空间而言，
一个点的邻域（neighbourhood）
是 PowerSet order 意义上的 filter。

- 对 meet 封闭，就是对集合的交封闭，
  因为是同一个点的邻域，所以显然满足；
- UpClosure 显然满足。

## Complete lattices and intersection–structures

有限 lattice 都是 complete lattice，
所以我先跳过这一节，之后需要用到相关概念了再来探索。

TODO

## Chain conditions and completeness

TODO

## Join-irreducible elements

lattice 中不可分解的元素，
类比的是自然数中，
素数在乘积意义上是不可分解的元素。

> The Fundamental Theorem of Arithmetic says that every natural number
> is a product of prime numbers. Since prime numbers are just the
> product-irreducible natural numbers (other than 1) an analogous
> result for lattices would state that every element is a meet of
> meet-irreducible elements or, dually, a join of join-irreducible
> elements. This will not be true in general but will hold provided we
> impose an appropriate finiteness condition. We prefer to build from
> the bottom up rather than the top down and consequently focus on
> joins rather than meets.

lattice 中的元素 x 是 **join-irreducible**，定义为：

- (1) x != bottom -- 如果 lattice 有 bottom 的话。
- (2) forall a, b: x = join(a, b) -> x = a 且 x = b。
- 与 (2) 等价的 (2') forall a, b: a < x 且 b < x -> join(a, b) < x，
  即任意两个元素的 join 都在 x 之下。

在 hasse diagram 中，
所有尽下方（beneath）只有一个元素的元素都是 join-irreducible。
注意 bottom 不算，因为 bottom 尽下方没有元素了。

在自然数的 lcm 和 gcd 中的，
形状为 p^r 的素数的幂，都是 join-irreducible。

在 PowerSet 中，只有单元素集合才是 join-irreducible。

TODO 这一节还有一些先不看了，先探索 FCA。

# 3. Formal concept analysis

开篇说要用 ordered set 和 lattice 来研究层级（Hierarchy）。
但是我觉得对于 FCA 来说，层级还是次要的，
主要是概念这一概念本身。
因为画出 concept lattice 是为了找出更多有意义的概念。

## Contexts and their concepts

几句话讲清楚 FCA 的理论如何处理 concept：

> What is a concept?  This would appear to be a question for
> philosophers rather than for mathematicians. Indeed, traditional
> philosophy's answer provides us with the basis for our formal
> definition.  A concept is considered to be determined by its extent
> and its intent: the extent consists of all objects belonging to the
> concept while the intent is the collection of all attributes shared
> by the objects.

一个 concept 由 extent 和 intent 组成。

解释为什么要有 formal context：

> As it is often difficult to list all the objects belonging to a
> concept and usually impossible to list all its attributes, it is
> natural to work within a specific context in which the objects and
> attributes are fixed.

concept 需要被限制在某个 context 中才有意义，
context 限制 objects 和 attributes 的范围，
并且给出它们之间的 incidence 关系。

而集合论中一个集合只有 extent。
但是 concept 的形式化定义还是依赖集合论，
即 concept 是一个二元组 -- `(extent, intent)`；
且 context 是一个三元组 -- `(objects, attributes, incidence)`。

尽管 concept 由两部分组成，但是其中的一部分可以确定另一部分。

> To demand that the concept (A, B) is determined by its extent and by
> its intent means that B should contain just those attributes shared
> by all the objects in A and, similarly, the objects in A should be
> precisely those sharing all the attributes in B.

这一节的开头表明，
可以完全从一个具体的例子，
来把 FCA 中的所有核心概念介绍清楚。

形式定义：

```cicada
class Context {
  Objects: Type
  Attributes: Type
  incidence: Relation(Objects, Attributes)

  commonAttributes(objects: Set(Objects)): Set(Attributes)
  commonObjects(attributes: Set(Attributes)): Set(Objects)

  isConcept(extent: Set(Objects), intent: Set(Attributes)): Boolean {
    setEqual(commonAttributes(extent), intent) &&
    setEqual(commonObjects(intent), extent)
  }

  // TODO
  // Concept 本身作为对象（如果想不依赖），Context 存在，
  // 就需要固定 Objects 和 Attributes 这两个 Type。
  // 也许 Concept 本身根本就没必要作为一个 class 存在。
}
```

如何实现？

- 方案 A：

  corss-table = array of rows

  我们可以用 JSON 来表示 corss-table 中的一行 `{ "@id": "", ... }`，
  用 JSON 的缺点是，一个 attributes 之存在，
  不能直接像是 XML 的 attributes 一样单单写出 attribute，
  而是要写 `{ "@id": "", "attribute": true, ... }`。
  也许可以专门给 FCA 设计一个 DSL，来解决这个问题。

- 方案 B：

  corss-table = map from an entity (an object) to a set of attributes

  这样可以保证 object 的唯一性。
  实际实现的时候我们用 entity 一词，而不用 object，
  因为和程序语言中的 OOP 混淆了。
  entity 也能让人想起 EAV 这种三元组的数据库。

> The framework within which we are working -- a pair of sets, G, M,
> and a binary relation I linking them -- is extremely general, and
> encompasses contexts which might not at first sight be viewed in
> terms of an object-attribute correspondence.

毕竟任何二分图都有这样的结构。

一个关于程序的，非平凡的例子：

> Consider, for example, a computer program modelled by an
> input-output relation R between a finite set of initial states X and
> a finite set of final states Y with xRy if and only if the program
> when started in state x can terminate in state y.  Then (X, Y, R) is
> the context for what is known as a (non-deterministic) transition
> system.  Here A' (for A ⊆ X ) is to be interpreted as the set of
> final states in which the program can terminate when started from
> any one of the states in A.

定义好 Context 之后，`commonAttributes` 和 `commonObjects` 都有很常用的解释。

- 学习 Galois connection 之后，对于这两个函数，
  可能可以给出更抽象的名字。

TODO 证明 3.6 Proposition，
即 concept lattice 是 complete lattice，
并且找出合适的 join 和 meet。

## The fundamental theorem of concept lattices

TODO 我跳过了很多证明与推导，
一个有意义的练习是尝试从头开始独立给出这些推导。

TODO 看完第五章之后回来推导相关的证明。

已知 concept lattices 是 complete lattices。
所谓 concept lattices 基本定理，
就是对于任意一个 complete lattices，
构造一个与它同构的 concept lattices。

构造方式类似于在线性空间中选择一组基，
这里要选择两组基，
分别作为 concept context
的 objects 和 attribute。

于线性空间不同的时，这里不要求基相互独立。

- concept lattices 之间同构，
  但是 concept context 却有可能不同。

## From theory to practice

TODO

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
