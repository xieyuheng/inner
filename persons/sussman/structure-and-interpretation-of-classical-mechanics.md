---
title: Structure and Interpretation of Classical Mechanics
authors: [Gerald Jay Sussman, Jack Wisdom]
date: 2000
---

# reference

- Hans Freudenthal,
  Didactical Phenomenology of Mathematical Structures,
  Kluwer Publishing Co., Boston, 1983.

- P. E. B. Jourdain,
  The Principle of Least Action,
  The Open CourtPublishing Company, Chicago, 1913.

# notes

## 經典力學的產生

- 數世紀的對星體運動的觀察中
  人們積累了相當多的關於星體運動的規律的經驗性知識
  這使得人們有能力預測很多天文現象
- 爲了理解運動
  而把這些知識表達地符合人類對事物的理性的認識方式
  人們就創造出了一種描述運動的一般規律的數學語言
  即經典力學
- 初步的觀察
  使人們區分出空間與空間中的物體
  並用一種幾何[度量關係等抽象數學結構]去描述空間
  而物體被抽象爲空間中的點集
- 人們觀察到物體在運動
  人們通過引入參考系來描述點在空間中的位置
  而運動就被描述爲點在空間中的位置的變化
  需要引入時間這個參數才能描述變化這個概念
- 人們觀察到物體之間有相互作用
  這被描述爲力 它以物體爲作用對象
- 人們觀察到的相同的力對不同物體的作用效果不同
  人們引入質量這個物體萬有的參數來度量這種不同的作用效果

## sicm 的目的

利用函數式編程中的嚴格的符號系統來介紹
經典力學 和 微分幾何

## 術語

- configuration path
  相曲線 相空間中的時間參數化曲線
- invent some mathematical function
  that allows us to distinguish realizable motions
  from among all conceivable motions
- a path-distinguishing function
  that has a minimum on the realizable paths
- on nearby unrealizable paths the value of the function
  is higher than it is on the realizable path

# intro

- good mathematics must be precise, clear and computationally effective.

- functional notation -- without irrelevant letters.

# 1 lagrangian mechanics

## intro

- newtonian formulation v.s. variational formulation

## 1.1 The Principle of Stationary Action

- Let us suppose that for each physical system
  there is a path-distinguishing function
  that is stationary on realizable paths.

- 路徑積分 與 變分問題

- 分解被積函數

## 1.2 Configuration Spaces

- 狀態空間
  狀態空間的維度
  狀態軌跡 狀態路徑

## 1.3 Generalized Coordinates

- In order to be able to talk about specific configurations
  we need to have a set of parameters
  that label the configurations.

- Xie:
  這種對座標系的直覺理解
  也指明了推廣座標系這個概念的方向

- Xie:
  上面這句話也可以拆解成如下
  - In order to be able to talk about something
    we need to have name them.
  - In order to be able to talk about specific configurations
    we need to label the configurations.
  - we use a set of numerical parameters
    to label the configurations.

- Xie:
  這裏很多篇幅是用來作 座標系無關 討論的
  我想如果加入一個限制的話會更好
  比如 限制流形 爲 代數方程的零點

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
