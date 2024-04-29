---
title: Constraint processing
author: Rina Dechter
year: 2003
---

# 学习动机

（1）增进对类型检查器的理解。

Constraint Programming 的知识可以用来加深人们对程序语言中类型检查器的理解。

一个变量所满足的类型被记录在 context 中，
就代表了这个变量所需要满足的 constraint。

简单的类型检查算法，只要用 unification
就可以解决变量类型的 constraint 问题，
但是复杂情况下，比如 dependent type 和 path type，
想要让实现更容易理解，就需要更一般的理论了。

（2）实现实用的 logic programming 语言。

> That embedding also allows us to see that logic programming, as
> embodied in Prolog, is just a special case of constraint logic
> programming with unification as the constraint solver.

我要能够实现实用的 logic programming 来像 prolog 一样解决数独等等问题。

# 问题

（1）constraint programming 是否可以被理解为一个计算模型？
或者说 hypergraph rewriting 可以不可以？
如果可以，如何用这个计算模型编程？

注意，一般的 constraint programming 只是描述问题，
而没有程序语言中的控制流。
而传统的计算模型好像在于控制流，
而不应该把控制交给搜索与调度算法。

（2）一系列 constraints 在某个系统下的变化，
可以看作是一个整体在某个动力系统下的运动。
描述变化的规则就是运动的“物理”定律。

# Preface

> A constraint is a restriction on a space of possibilities; it is a
> piece of knowledge that narrows the scope of this space.  They can
> be used to formulate regularities that govern our computational,
> physical, biological, and social worlds.  They identify the
> impossible, narrow down the realm of possibilities, and thus permit
> us to focus more effectively on the possible.

# Chapter 1 Introduction

## 1.1 Basic Concepts and Examples

> First, every constraint problem must include **variables**: objects
> or items that can take on a variety of values. The set of possible
> values for a given variable is called its **domain**.

> The second component to every constraint problem is the set of
> constraints them selves. **Constraints** are rules that impose a
> limitation on the values that a variable, or a combination of
> variables, may be assigned.

因此一个 constraint 就是一个 relation。

> Note that there is often more than one way to model a problem. In
> the previous example, we could just as logically have decided to
> call the guests our variables and their domains the set of chairs at
> the table.


和 Formal concept analysis 中一样，object 和 attribute 是对偶的。

> A model that includes variables, their domains, and constraints is
> called a **constraint network**, also called a **constraint
> problem**.

> A **solution** is an assignment of a single value from its domain to
> each variable such that no constraint is violated. A problem may
> have one, many, or no solutions. A problem that has one or more
> solutions is **satisfiable** or **consistent**. If there is no
> possible assignment of values to variables that satisfies all the
> constraints, then the network is **unsatisfiable** or
> **inconsistent**.

## 1.3 Mathematical Background

We can represent a relation as a table.

Consider object and attribute (property)
-- like in formal concept analysis.

API:

```
Table = { schema: Schema, objects: Array(Object) }

tableSelection(table, record): Table
tableProjection(table, names): Table
objectProjection(table, names): Table
tableJoin(table, table): Table
```

# Chapter 2 Constraint Networks

# Chapter 3 Consistency-Enforcing and Constraint Propagation

# Chapter 4 Directional Consistency

# Chapter 5 General Search Strategies: Look-Ahead

# Chapter 6 General Search Strategies: Look-Back
