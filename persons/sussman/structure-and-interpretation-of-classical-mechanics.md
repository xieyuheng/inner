---
title: Structure and Interpretation of Classical Mechanics
authors: [Gerald Jay Sussman, Jack Wisdom]
date: 2000
---

# 引用

- Hans Freudenthal,
  Didactical Phenomenology of Mathematical Structures,
  Kluwer Publishing Co., Boston, 1983.

- P. E. B. Jourdain,
  The Principle of Least Action,
  The Open CourtPublishing Company, Chicago, 1913.

# 笔记

## 学习动机

SICM 的目的在与，利用函数式编程中的严格的符号系统，来介绍经典力学和微分几何。

我学习 SICM 的动机是学会经典力学，同时也学习用 Scheme 表达知识的方式。

## 经典力学的产生

数世纪的对星体运动的观察中，
人们积累了相当多的关于星体运动的规律的经验性知识，
这使得人们有能力预测很多天文现象。

为了理解运动，
而把这些知识表达地符合人类对事物的理性的认识方式，
人们就创造出了一种描述运动的一般规律的数学语言，
即经典力学。

初步的观察，
使人们区分出空间与空间中的物体，
并用一种几何（度量关系等抽象数学结构）去描述空间，
而物体被抽象为空间中的点集。

人们观察到物体在运动，
人们通过引入参考系来描述点在空间中的位置，
而运动就被描述为点在空间中的位置的变化，
需要引入时间这个参数才能描述变化这个概念。

人们观察到物体之间有相互作用，
这被描述为力，它以物体为作用对象。

人们观察到的相同的力对不同物体的作用效果不同，
人们引入质量这个物体万有的参数来度量这种不同的作用效果。

# 1 Lagrangian Mechanics

> The motion of a system can be described by giving the position of
> every piece of the system at each moment. Such a description of the
> motion of the system is called a _configuration path_; the
> config-configuration path specifies the configuration as a function
> of time.

说 configuration 而不是说 position，
是因为想要描述刚体，除了 position 之外还需要 orientation。

Newton 的方程是一个方程，
满足运动条件的 configuration path 需要让方程等于 0。

> However, there is a alternate strategy that provides more in-insight
> and power: we could look for a path-distinguishing functionthat has
> a minimum on the realizable paths -- on nearby unreal-unrealizable
> paths the value of the function is higher than it is on the
> realizable path. This is the _variational strategy_: for each
> physical system we invent a path-distinguishing function that
> distinguishes realizable motions of the system by having a
> stationary point foreach realizable path. For a great variety of
> systems realizable motions of the system can be formulated in terms
> of a variational principle.

而在变分法中，之需要 configuration path 为 stationary point。

## 1.1 The Principle of Stationary Action

尝试通过我们对运动的直觉，推导出 path-distinguishing function。
能熟练地重复这里的推导过程，就算是学会这一节了。
推导的结论是：

> We will consider actionsthat are integrals of some local property of
> the configuration pathat each moment.

TODO

## 1.2 Configuration Spaces

状态空间
状态空间的维度
状态轨迹 状态路径

## 1.3 Generalized Coordinates

In order to be able to talk about specific configurations
we need to have a set of parameters
that label the configurations.

Xie:
这种对座标系的直觉理解
也指明了推广座标系这个概念的方向

Xie:
上面这句话也可以拆解成如下
- In order to be able to talk about something
  we need to have name them.
- In order to be able to talk about specific configurations
  we need to label the configurations.
- we use a set of numerical parameters
  to label the configurations.

Xie:
这里很多篇幅是用来作 座标系无关 讨论的
我想如果加入一个限制的话会更好
比如 限制流形 为 代数方程的零点

## 1.4 Computing Actions

## 1.5 The Euler-Lagrange Equations

## 1.6 How to Find Lagrangians

## 1.7 Evolution of Dynamical State

## 1.8 Conserved Quantities

## 1.9 Abstraction of Path Functions

## 1.10 Constrained Motion

# 2 rigid bodies

# 3 hamiltonian mechanics

# 4 phase space structure

# 5 canonical transformations

# 6 canonical perturbation theory
