---
title: from formal concept analysis to contextual logic
authors: [Frithjof Dau, Julia Klinger]
year: 2005
---

# 1 Overview

# 2 Formal Concept Analysis

> The aim is to reach a structured theory which unfolds the formal
> thoughts according to meaningful interpretations allowing a broad
> communication and critical discussion of the content.

Lattice 经过 restructuring 而得到 FCA。
强调 learnable 的理论，让更多的人能理解，能对更多的人有用。

这里提到的所谓 many-valued contexts，
其实就是把 FCA 的数据库的 entity-attribute 的结构，
扩展为 entity-attribute-value 的结构，
因为这个结构才是人们自然地记录数据的方式。

但是 FCA 的方法只能用于 entity-attribute，
所以需要把 value 进一步离散化 -- 所谓 scaling。

这确实是非常重要的问题。
这里说的不能 auto scaling，
必须由领域专家根据领域知识来 scaling，
其实也是错的，auto scaling 正是一个实用的 app 的方向。
即，用机器学习的方法，以奥卡姆的剃刀为原则，
找出合理的离散化方式，使得概念数量最少。

# 3 Logic

> The purpose of predicate logic has never been to model or support
> human reasoning, but to provide an instrument which shall explain
> and contribute to the structure of mathematical argumentations only.

谓词逻辑是为了数学，但是数学还是为了人们对世界的认识。
所以就算是把谓词逻辑作为间接的工具，也不应该有此评价。
再说了，看谓词演算发展的历史，
比如 Frege 的动机，也是为了理解人类的认知的，
看莱布尼兹和亚里士多德的动机，更如如此。

> In view of this problematic situation [of rational argumentation] it
> is more obvious not to give up reasoning at all, but to break with
> the concept of reasoning which is orientated by the pattern of
> logic-mathematical proofs.

也许说的没错，数学的核心在于想像与建模，而不在于证明。
但是想否定证明 -- 这个数学的核心概念，的价值，还是有点过头了。

# 4 Conceptual Graphs

这里所描述的 CG，看起来像是
high order logic programming with a type system。

想要做的是在 CG 的基础上，
以 FCA 的 concept 为类型，
以 object 为类型的元素，
模仿谓词逻辑构造一个新的逻辑。
与一般的谓词逻辑和 dependent type 相比，
这里的 type 是 concept 不是 set，
因此，除了带有 extent 之外，还带有 intent。

进一步我们可以发现，class 就是带有 extent 和 intent 的，
所以这里想要的其实是一个以 class 为 type 的语言，
可能比一般的 class 的表达能力要强一些，
比如不光可以限制某个属性的类型，
还可以限制某个属性的值，或者值的范围
（比如已经（用谓词）给所有的 value 分出了不同的范围）。

# 5 Concept Graphs and Contextual Logic

这一节应该讲的是这篇论文的主要 idea，
描述从上一节引出的，想要实现的语言及其类型系统。

这里用 power context family 做的构造，
不如直接用程序语言中的 class 来做构造，
context 的乘积所带来的以 tuple 为 object 的乘积 context，
可以用 class 的 composition 来处理。

这样我们只需要一个 context，即可嵌套的 record 的集合，
而不需要 family of contexts。

另外可能需要对 class 的表达能力做的扩展是，
class 可以带有谓词来限制其属性中的元素的范围，
可以以结构化的等词为基本的谓词。
如果允许谓词带有计算，就是带有 class 的 dependent type。
