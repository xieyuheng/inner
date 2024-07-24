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
