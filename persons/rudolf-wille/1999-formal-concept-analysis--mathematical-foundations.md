---
title: Formal Concept Analysis
subtitle: Mathematical Foundations
authors: [Bernhard Ganter, Rudolf Wille]
year: 1999
---

# O. Order-theoretic Foundations

## 0.1 Ordered Sets

二元关系（binary relation）。

序关系（order relation）。

下邻居（lower neighbour），上邻居（upper neighbour）。

层级图（line diagram，Hasse diagram）。

可比较（comparable），不可比较（incomparable）。

链（chain），反链（anitchain），长度（length），宽度（width）。

区间（interval），主理想（principal ideal），主滤子（principal filter）。

保序映射（order-preserving map），
保序嵌入（order-embedding），
序同构（order-isomorphism）。

直接积（direct product），不交并（disjoint union，tagged union）。

对偶序（dual order），对偶陈述（dual statement）。

下界（lower bound），上界（upper bound），
最大下界infimum），最小上界（supremum），
取下（meet），取上（join）。

## 0.2 Complete Lattices

格（lattice），完全格（complete lattice）。

顶（top，largest element，unit element），
底（bottom，smallest element，zero element）。

用取上的等式定义序关系，
用取下的等式定义序关系。

取上与取下满足结合律与广义结合律。

对偶格（dual lattice）。

为了证明一个序集是完全格，只需要证明下确界存在，
因为一个子集的上界的集合的下确界，就是这个子集的上确界。
反之证明上确界存在也可以。

取上不可约（join-irreducible）：不可被分解为另外两个元素的取上，有唯一下邻居；
取下不可约（meet-irreducible）：不可被分解为另外两个元素的取下，有唯一上邻居。

取上稠密（join-dense），
取下稠密（meet-dense）。

一个格的所有取上不可约元素的集合，是取上稠密的，
也许可以称作取上基，可以通过取上来生成所有元素；
一个格的所有取下不可约元素的集合，是取下稠密的，
也许可以称作取下基，可以通过取下来生成所有元素。

总结来说：
取上不可约集是取上基：最小取上稠密集；
取下不可约集是取下基：最小取下稠密集。

底的上邻居都是取上不可约的，称为格的原子（atom）；
顶的下邻居都是取下不可约的，称为格的对偶原子（coatom）。

一个格的原子构成取上基，称这个格具有原子性（atomistic）。

取上子半格（join-subsemilattice），
取下子半格（meet-subsemilattice），
子格（sublattice）；
上确界子半格（supremum-subsemilattice），
下确界子半格（infimum-subsemilattice），
完全子格（complete sublattice）。

保取上映射（join-preserving map），
保取下映射（meet-preserving map），
格同态（lattice homomorphism）；
保上确界映射（supremum-preserving map），
保下确界映射（infimum-preserving map），
完全格同态（complete lattice homomorphism）。

问题：
一个序集上如果可以定义取上与取下而成为一个格，这个格是唯一的吗？
也就是说，一个序集可以有不同的格结构吗？
假设我们按照元素个数不同来分类，画出所有序集的 Hasse 图，
可以看出，所有满足格条件的 Hasse 图，是所有的序集的子集。
那么对于有限情况而言，一个序集的格结构就是唯一的了。

## 0.3 Closure Operators

注意，这里定义的闭包系统（closure system），
就是 The Order Book 中定义的交集结构（intersection-structure），
但是在那本书里没有强调闭包算子。

闭包结构（closure structure）也是一个十分重要的数学结构，
它出现在拓扑、代数、逻辑等各个领域。

常见的闭包算子（closure operator），
是就某个集合 A 的幂集 PowerSet(A) 而言的。

```cicada
claim A: Set

closure: (PowerSet(A)) -> PowerSet(A)
extensive: (X: PowerSet(A)) -> SubsetOf(X, closure(X))
increasing: (X, Y: PowerSet(A), SubsetOf(X, Y)) -> SubsetOf(closure(X), closure(Y))
idempotent: (X: PowerSet(A)) -> Equal(closure(closure(X)), closure(X))
```

PowerSet(A) 是一个特殊的 OrderedSet，
闭包算子的定义也可以推广到一般的 OrderedSet。

```cicada
claim order: OrderedSet

closure: order.Element -> order.Element
extensive: (X: order.Element) -> order.Below(X, closure(X))
increasing: (X, Y: order.Element, order.Below(X, Y)) -> order.Below(closure(X), closure(Y))
idempotent: (X: order.Element) -> Equal(closure(closure(X)), closure(X))
```

闭包系统是说一个集合的某个子集的集合对于交封闭，
这些子集被称为闭集（closed set），
而闭包算子是一个集合的幂到自身的映射。

闭包系统与闭包算子之间的联系在于如下定理：

- 如果已有一个闭包算子，它的像空间就定义了一个闭包系统的闭集的集合；

- 如果已有一个闭包系统，对于任意一个子集，
  取所有包含它的闭集，然后求交（按闭包系统的定义封闭），
  就得到一个新的子集，这样就可以定义一个闭包算子。

TODO 证明这里的断言。

我们已经知道闭包系统对于形式概念分析非常重要，
因为在概念格中定义取上和取下运算的时候，
分别要用到内涵的交与外延的交。

乍一看，这好像是不对称的，
为什么用到的是交而不是并？
也许闭包算子可以帮助理解这一点。

这方面最重要的定理是：

- 任意一个闭包系统都是一个完全格，
  其中，序关系是集合的包含关系
  （看参数的位置，应该说是「包含于」关系），
  取下就定义为集合的交，
  取上就定义为集合的并的闭包。

- 任意一个完全格，它的上闭包（或下闭包）的集合，
  可以看成是一个闭包系统。

  - 序集的上闭包（up closure）与下闭包（down closure），
    就是在 The Order Book 中提到的 up-set 和 down-set，
    它可以把对任意序关系的研究，
    转化为对某个集合的特殊子集之间的包含关系的研究。
    特殊的序关系被转化为了特殊的子集加一般的序关系（集合的包含关系）。

TODO 这里有一个问题，
一个闭包系统给出一个完全格，
但是一个完全格却可以分别用上闭包与下闭包给出两个闭包系统！

- 注意，上闭包与下闭包是可以对任意序集定义的，
  但是书中的定理的对象却不是序集，而是完全格，
  也许这个问题的答案就在这里。

很多重要的数学结构的子结构的集合，都是闭包系统。
也就是说，对于这样的每个具体的数学结构，
都可以获得一个子结构的完全格：

- 向量空间的子空间
- 群的子群和正规子群
- 拓扑空间中的闭集
- 欧氏空间中子集的凸包
- 多面体的面的集合 TODO ！？
- 等价关系 TODO ！？

注意，对于这些子空间构成的完全格来说，取下就是交，
但是，取上的计算带有闭包算子，与具体数学结构有关，
是值得仔细考察的东西。

TODO 看看群论中取上如何计算。

如果一个完全格满足下面的等式，
就称它为分配的（distributive）：

```
meet(x, join(y, z)) = join(meet(x, y), meet(x, z))
join(x, meet(y, z)) = meet(join(x, y), join(x, z))
```

类比乘法对加法的分配律：

```
x * (y + z) = x * y + x * z
```

如果满足下面的性质，
就称它为模格（modular lattice）：

```
below(x, z) -> join(x, meet(y, z)) = meet(join(x, y), z)
```

可以认为这个性质描述了
join 和 meet 之间可以使用结合律的条件。

在这个简单的定义之外，
diamond isomorphism theorem
可以让人更直觉地认识模格。

> Distributivity and modularity are **self-dual** properties:
> if they hold for a lattice V, they also hold for the dual of V.
> All above-mentioned properties transfer to complete sublattices.
> Power-set lattices are completely distributive,
> subspace lattices of vector spaces are modular.

子结构的格的属性，竟然能用来区分不同的数学结构。
可能类似于，拓扑空间的拓扑不变量可以用来区分空间。

TODO 在格论的背景下，学习一下伽罗瓦理论，应该是很棒的。

## 0.4 Galois Connections

一对满足如下性质的反向保序映射，
被称为伽罗瓦对应（Galois connection）：

```cicada
claim f: P -> Q
claim g: Q -> P

f 反向保序: (p1, p2: P, below(p1, p2)) -> below(f(p2), f(p1))
g 反向保序: (q1, q2: Q, below(q1, q2)) -> below(g(q2), g(q1))

函数复合在 P 中递增: (p: P) -> below(p, g(f(p)))
函数复合在 Q 中递增: (q: Q) -> below(q, f(g(q)))
```

f 与 g 被称为对偶伴随（dually adjoint），
应该和范畴论中的伴随函子（adjoint functor）有关。

上面的四个性质可以简化为一个等价的性质：

```cicada
below(p, g(q)) <-> below(q, f(p))
```

伽罗瓦对应满足如下属性：

```cicada
f == f ∘ g ∘ f
g == g ∘ f ∘ g
```

TODO 下面的定理，
给出了判断一个序集之间的映射 f: P -> Q，
是否具有 dual adjoint 的条件，
并且给出了构造它的 dual adjoint 的方式，
并且证明了如果 dual adjoint 存在，就是唯一的。

条件可以叙述于主理想与主滤子，也可叙述如下：

```cicada
(p1, p2: P) -> f(join(p1, p2)) == meet(f(p1), f(p2))
```

这就很直观了很好记了。

两个集合之间的伽罗瓦对应，
定义为两个幂集之间的伽罗瓦对应。

集合之间的伽罗瓦对应与闭包算子之间的关系在于如下定理，
给定对偶伴随 f: P -> Q 和 g: Q -> P，

- g ∘ f: P -> P 是 P 上的闭包算子，
- f ∘ g: Q -> Q 是 Q 上的闭包算子，
- 并且 f 和 g 构成 P 和 Q 作为闭包系统之间的对偶同构。

下面描述两个集合之间的伽罗瓦对应，
与两个集合上的二元关系之间的关系。

- 注意，一个二元关系就是一个形式概念语境，
  所以这里的结论也可以延伸到形式概念分析中。

  比如，首先这里就定义了形式概念分析中的两个衍生算子，
  但是是就二元关系来定义的。

下面对于任意一个二元关系，
给出一个集合之间的伽罗瓦对应。

- 已知一个二元关系，
  得到伽罗瓦对应的方式很简单，
  两个衍生算子本身就是对偶伴随。

- 已知一个集合之间的伽罗瓦对应，
  有两个等价的定义二元关系的方式，
  即 R(x, y) := x in g(y)，
  或 R(x, y) := y in f(x)。

如果不用反向保序映射，
而是用保序映射和对偶序集来叙述伽罗瓦对应，
就得到 residuated map 和 adjoint 的概念（不是 dual adjoint 了）。

## 0.5 Hints and References

介绍与格论相关的教科书与文献。

比如，关于上面的 residuated map 有 residuation theory 相关的引用。

# 1. Concept Lattices of Contexts

## 1.1 Concept Lattices of Contexts

先看一下简单的实现方式。

首先是类似 `String` 的 `Entity` 和 `Attribute`：

```cicada
class Entity extends String
class Attribute extends String
```

其次是 `Context`：

```cicada
class Context {
  entities: Set(Entity)
  attributes: Set(Attribute)
  entityAttributeIndex: Map(Entity, Set(Attribute))
  attributeEntityIndex: Map(Attribute, Set(Entity))
}
```

`Concept`：

```cicada
class Concept {
  context: Context
  extent: Set(Entity)
  intent: Set(Attribute)
}
```

重要的 API：

```cicada
entityHasAttribute(context: Context, entity: Entity, attribute: Attribute) -> boolean

entitiesOf(context: Context, attribute: Attribute) -> Set(Entity)
attributesOf(context: Context, entity: Entity) -> Set(Attribute)

commonEntities(context: Context, attributes: Set(Attribute)) -> Set(Entity)
commonAttributes(context: Context, entities: Set(Entity)) -> Set(Attribute)

closureEntities(context: Context, entities: Set(Entity)) -> Set(Entity)
closureAttributes(context: Context, attributes: Set(Attribute)) -> Set(Attribute)
```

我们可以用到前一章学的知识，来总结概念格的性质。

首先是伽罗瓦对应：

```cicada
claim context: Context

commonEntities(context): (Set(Attribute)) -> Set(Entity)
commonAttributes(context): (Set(Entity)) -> Set(Attribute)
```

构成 `context.entities` 和 `context.attributes` 这两个 Set 之间的伽罗瓦对应。
注意，集合之间的伽罗瓦对应就是集合的幂集作为序集之间的伽罗瓦对应。

其次是闭包算子：

```cicada
claim context: Context

closureEntities(context: Context) -> (Set(Entity)) -> Set(Entity)
closureAttributes(context: Context) -> (Set(Attribute)) -> Set(Attribute)
```

构成 `context.entities` 和 `context.attributes` 这两个 Set 上的闭包算子，
其中的闭集，将会作为某个 `concept` 的 `concept.extent` 和 `concept.intent`。

TODO

## 1.2 Context and Concept
## 1.3 Context and Concept Lattice
## 1.4 Many-valued Contexts
## 1.5 Context Constructions and Standard Scales
## 1.6 Hints and References

# 2. Determination and Representation

## 2.1 All Concepts of a Context
## 2.2 Diagrams
## 2.3 Implications between Attributes
## 2.4 Dependencies between Attributes
## 2.5 Hints and References

# 3. Parts and Factors
# 4. Decompositions of Concept Lattices
# 5. Constructions of Concept Lattices
# 6. Properties of Concept Lattices
# 7. Context Comparison and Conceptual Measurability
