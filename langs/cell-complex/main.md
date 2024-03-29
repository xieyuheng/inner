---
title: Main
newline: preserve
---

# [todo-stack]

## marks of 3-torus

## the boundary-rule of product-space

## the boundary-rule of quotient-space

## spaces described in detail syntax

- the building blocks that we can use
  to form product-spaces, quotient-space and so on,
  are spaces described in detail syntax

## natural syntax of product-space and quotient-space

- use product-space and quotient-space to describe
  torus := glue the opposite edge of a square
  3-torus := glue the opposite face of a cube

# [note]

## chain

- we have higher data constructors
  but no  higher type constructors
  because type of higher data is chain
  just like arrow, chain is a special form of type

## 同倫理論

### 抽象公理

- 首先我們可以用抽象的公理 來明確同倫理論
  所研究的具體對象 所需要滿足的性質
  比如
  - 一個幾何體有維度的屬性
  - 多個幾何體可以連接成鍊
  - 有一個邊界算子 可以作用於幾何體 以得其邊界
    並且邊界的維度比原集合體小

### 具體構造

- 其次要給出一些具體的滿足同倫理論的構造
  來作爲同倫理論具體的研究對象
  - 我們要設計一個語言來描述如何構造幾何體
  - 同時要給出方法來分析所構造的幾何體的各種屬性
    比如維度和邊界算子的作用
    這些都是同倫理論的抽象公理所要求具備的

### 理論的擴展

- 我們還可以給出從舊的幾何體構做新的幾何體的方法
  比如 積空間 與 商空間

- 在給出這些方法的同時
  我們又一次需要明確如何分析和計算所得空間的各種屬性

- 我們需要這些新的構造方法
  是因爲 具體的構造語法太過繁複
  只有用了這些新的方法
  才能讓形式表達在人們的認知複雜度之內

- 給出 積空間 商空間 子空間 等構造方法
  並不能說是所設計的形式語言中的新創造
  因爲在自然的數學語言中 這些概念都是既有的
  設計形式語言的初步工作 多是把這些概念形式化

## 分析與綜合

- 給出構造方法的同時
  還要給出如何[由成分之屬性]分析所造之複合物的屬性

- 當給出某些所期望的屬性
  而要找到滿足這些屬性的構造之時
  就需要構造者發揮其創造力
  來把已有之物綜合

- 與其說綜合所需要的是創造力
  不如說綜合所需要的是細緻的觀察與經驗

- 細緻的觀察能讓我們識別出所求屬性的特徵
  而經驗能讓我們想到已習得的綜合之術 來接近所求特徵

- 我們觀察以分解問題
  然後給出部分解法
  測試我們的解法
  再改進我們的解法

## 形式語言與自然語言

- 我們的目標是設計形式語言
  但是在這個過程中 我們也大可部分使用自然語言
  因爲設計形式語言的目的是要有效地交流想法
  舊有的自然語言之交流可能是低效的
  可能只需要將純粹的自然語言中的某些關鍵部分形式化
  就能達到高效的交流

- 正式因爲此
  我們已有的很多形式化程序語言的語法和語義的細節才各不相同
  這些差異都是不緊要的
  就一般的程序語言來說
  需要被形式化的關鍵只是
  - 函數之作用與複合
  - 命名新的函數
  - 遞歸與分支

- [問]
  就同倫理論來講
  所需要被形式化的關鍵概念又是什麼呢?

# 已有的語言之問題

- 已有的語言雖然能描述 AT 中的某些現象
  但是所使用的語義並不經濟
  描述簡單的現象時也需要複雜的語法

# 定義空間

- 定義 type 時使用高階元素
  是爲了能夠檢驗函數的連續性

- 引入 n + 1 階元素的條件是
  聲明一個 n 階邊界
  n 階鍊爲邊界的條件是
  1. 邊界爲空
  2. 可定向

- n 階鏈包含了粘合方式的信息
  1. 對於一階鏈來說
     就是 arrow 的複合
  2. 對於二階鏈來說
     明顯地說明符合方式 已經太過複雜了
     需要 mark-language

# product-space

# quotient-space

# 2-torus

``` cicada
(+type 2-torus-t : type-tt
  (-> -- 2-torus-t))

(+space 2-torus-t
  (1 )
  (2 ))
```

# 3-torus

``` cicada
(+type 3-torus-t : type-tt
  (-> -- 3-torus-t))

(+space 3-torus-t
  (1 b0 b1 b2 : (=> 3-torus-c -- 3-torus-c))
  (2 c0 : (=> b0 b1 (- b0) (- b1))
     c1 : (=> b0 b2 (- b0) (- b2))
     c2 : (=> b1 b2 (- b1) (- b2)))
  (3 d0 : (=> (-> c0 c1 c2
                  [0 1] [2 0]
                  [0 2] [1 0]
                  [1 1] [2 1])
              (-- c0 c1 c2
                  [0 0] [1 2]
                  [0 3] [2 1]
                  [1 3] [2 0]))))

(+space 3-torus-t
  (1 b0 b1 b2 : (=> a0 -- a0))
  (2 c0 : (=> b0 b1 -- b1 b0)
     c1 : (=> b0 b2 -- b2 b0)
     c2 : (=> b1 b2 -- b2 b1))
  (3 d0 : (=> c0 c1 c2 -- c0 c1 c2)))

(note using this expression
  we can compute
  (1) orientability
  (2) homology group
  for homotopy group my view is that
  the objects built by these expressions
  form a higher algebraic structure
  of which the homotopy group
  is just a sub algebraic structure)
```

# nat-u

``` cicada
(+union nat-u : type-tt
  zero-c : (-> -- zero-t)
  succ-c : (-> nat-u -- succ-t)
  nat-egde-c : (-> n : nat-u -- (=> n -- n succ-c)))
```

# 邊界算子

# 連續函數
