---
title: revisiting iso-recursive subtyping
authors: [yaoda zhou, jinxu zhao, bruno c.d.s. oliveira]
date: 2022
---

# My Motive

[2026-02-14] 这可以看作是 1993-subtyping-recursive-types 的后继论文。
iso-recursive 和 equi-recursive 在 cardelli 之前的论文中还没有明显地区分。

# 1 Introduction

> Recursive types come in two flavours: equi-recursive types and
> iso-recursive types [Crary et al. 1999]. With equi-recursive types a
> recursive type is equal to its unfolding. With iso-recursive types,
> a recursive type and its unfolding are only isomorphic. To convert
> between the (iso-)recursive type and its isomorphic unfolding,
> explicit folding and unfolding constructs are necessary.

引用：

- [Crary et al. 1999]
  Karl Crary, Robert Harper, and Sidd Puri. 1999.
  What is a Recursive Module?.
  In Proceedings of the ACM SIGPLAN 1999

> The main advantage of equi-recursive types is convenience, as no
> explicit conversions are necessary. However, a disadvantage is that
> algorithms for languages with equi-recursive types are quite
> complex. Furthermore, integrating equi-recursive types in type
> systems with advanced type features, while retaining desirable
> properties such as decidable type-checking, can be hard (or even
> impossible) [Colazzo and Ghelli 1999; Ghelli 1993; Solomon 1978].

引用：

- [Colazzo and Ghelli 1999]
  Dario Colazzo and Giorgio Ghelli. 1999.
  Subtyping recursive types in kernel fun.

- [Ghelli 1993]
  Giorgio Ghelli. 1993.
  Recursive types are not conservative over F≤.
  In International Conference on Typed Lambda calculi and Applications.
  Springer, 146–162.

- [Solomon 1978]
  Marvin Solomon. 1978.
  Type definitions with parameters.

> The Amber rules are well-known and widely used for subtyping
> iso-recursive types. They were briefly and informally introduced in
> 1985 by Cardelli in a manuscript describing the Amber language
> [Cardelli 1985]. Later on, Amadio and Cardelli [1993] made a
> comprehensive study of the theory of recursive subtyping for a
> system with equi-recursive types employing Amber-style rules. One
> nice result of their study is a declarative model for specifying
> when two recursive types are in a subtyping relation. In essence,
> two (equi-)recursive types are subtypes if their infinite unfoldings
> are subtypes.

> Amadio and Cardelli’s study remains to the day a standard reference
> for the theory of equi-recursive subtyping, although newer work
> simplifies and improves on the original theory [Brandt and Henglein
> 1997; Gapeyev et al. 2003]. Since then variants of the Amber rules
> have been employed multiple times in a variety of calculi and
> languages, but often in an iso-recursive setting [Abadi and Cardelli
> 1996; Bengtson et al. 2011; Chugh 2015; Duggan 2002; Lee et
> al. 2015; Swamy et al. 2011]. Perhaps most prominently the seminal
> work on “A Theory of Objects” by Abadi and Cardelli [1996] employs
> iso-recursive style Amber rules.

引用：

- [Gapeyev et al. 2003]
  Vladimir Gapeyev, Michael Levin, and Benjamin Pierce. 2003.
  Recursive Subtyping Revealed.

- [Lee et al. 2015]
  Joseph Lee, Jonathan Aldrich, Troy Shaw, and Alex Potanin. 2015.
  A Theory of Tagged Objects.
  In European Conference on Object-Oriented Programming (ECOOP).

> This paper aims to revisit the problem of subtyping iso-recursive
> types. We start by introducing a novel declarative specification for
> Amber-style iso-recursive subtyping. Informally, the specification
> states that two recursive types are subtypes if all their finite
> unfoldings are subtypes.

这个定义好像就是 cardelli 所指出的错误定义。

# 2 Overview

## 2.1 Applications of Iso-Recursive Types

这里说所有带有递归的语言，
都用到了 implicit 的 fold 与 unfold，
所以算是 iso-recursive type 的应用，
太牵强了。

## 2.5 The Amber Rules

cardelli 的论文中是需要先实现类型之间的等价，
然后再用等价来帮助实现类型之间的子类型关系的。

类型之间的等价被这里称为 reflexivity rule。

所谓 iso-recursive 就是不要先实现类型之间的等价，
或者说把 reflexivity rule 简化为语法上的等价。
