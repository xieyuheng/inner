---
title: Fibration
newline: preserve
---

# note

- 現在的想法來源於 理解 fibration 時的困難
  但是解決這個困難的前提是先在不考慮 fibration 的前提下完善語言

# [old] the formalization of fibration

- fibration 對應於 programming language 中的 dependent type

- fibration 的形式化公理將來源於 section 的連續性條件

- section 是特殊的到 totel space 的函數
  (x : B -> x P)
  一般的到 totel space 的函數之連續性條件
  可能劃歸於它 (A -> x : B, x P)
  - pullback bundle

- 有很多特殊的 fibration 可以從中尋找有很強幾何直覺的例子
  - vector bundle
  - principle bundle
  - fiber bundle
  - covering space
  - hopf fibration

- fibered category 作爲 cat 的概念
  可以用來融合 AT AG DG 中的例子
  - 還有 pullback 與 pushout

- grothendick construction
  由 F : (C -> CAT)
  可構造 一個 CAT
  object : (c, x) : (C, c F)
  arrow  : (f, g) : (c1, x1 -> c2, x2) : (C, c1 F -> C, c2 F)
  -------: f : (c1 -> c2) : (C -> C)
  -------: g : (x1 (f F)) -> x2 : (c2 F -> c2 F)
  - F 可作用於 f (F 是 functor)
    定義一個 type-family F 時
    就要給出 C 中 1-level element 到 CAT 中 arrow type 的對應

- 在形式化的處理中 fibration 有兩種
  一種是 非引入的 有自然高階元對應的 : (x f g == x)
  一種是 需要指定高階元對應方式的 : (hopf)

# the 'transport' of hott

## [old] note

``` scheme
;; fibration
F := ((:x : A) -> :x P)
;; 每個 fibration 必須有 tp
(* (F tp) : ((:x = :y) -> (:x P ~> :y P)))
;; section
(* k : ((:x : A) -> :x P))
;; k 和 (F tp) 的作用 出現在 (k apd) 的類型中
(* (k apd) : ((:p : (:x = :y)) -> (:x k (:p F tp) = :y k)))
;; (k apd) 的函數體的存在性 就是限制 k 的函數體的方程
;; 所謂 限制
;;   即 x k 與 y k 不能在 x P 與 y P 中任意取值
;;   即 有邊相連的點 x 與 y 其值 x k 與 y k 受邊所生成的方程的限制
```

- 而古典理論中 描述 fibre bundle 的方式是
  projection : total-space -> base-space
  這裏 total-space 的定義本身就描述了全局結構
  - total-space must be a dependent-pair space

- fibre bundle 中 fibre 是固定的空間
  而 fibration 中 fibre 是可依賴 x 的
  此時 fibre 不必是相等的空間
  但是必須是同倫等價的
  這就是 要求 tp 存在的原因

- 而在形式理論中 描述 fibration 的方式是
  F := (x : base-space -> x P)
  此時 total-space 是構造出來的 F
  需要額外用 tp 給出 fibration 的全局結構
  - 所謂 全局結構 或 全局信息
    其實也是局部的 只不過不是就 point 來描述
    而是就 path 或高階幾何元素來描述而已

- apd 的類型中 有 tp 的作用 可能是爲了
  能夠以線性的方式表示封閉的高階 path

## tp

- the 'transport' of hott
  while the 'apd' is expressed by
  overloading function name to levels of elements by numbering

``` scheme
(def P (-> [A] [space]))
(def k0 (-> [(0 A) %:x] [:x P]))

(def tp1 (-> [:x P (1 :x :y)]
             [:y P]))
(def k1 (-> [(1 :x :y) %:p]
            [(1 :x k0 :p tp1
                :y k0)]))

(def tp2 (-> [:x P (2 :p :q)]
             [(1 :x k0 :p tp1
                 :x k0 :q tp1)]))
(def k2 (-> [(2 :p :q) %:m]
            [(2 :p k1
                :x k0 :m tp2
                :q k1)]))

(def tp3 (-> [:x P (3 :m :n)]
             [(2 :x k0 :m tp2
                 :x k0 :n tp2)]))
(def k3 (-> [(3 :m :n) %:o]
            [(3 :m k2
                :x k0 :o tp3
                :n k2)]))
```

## tr

- tr1 是 (x f g == x) 與 (y f g == y) 這兩個邊界空間之間的共軛變換

- tr2 與 tr3 也可以理解爲邊界空間之間的變換
  並且這種變換是通過把兩個邊界連接爲一個封閉的邊界來描述的

``` scheme
(def P (-> [A] [space]))
(def k0 (-> [(0 A) %:x] [:x P]))

(def tr1 (-> [:x P (1 :x :y)]
             [:y P]))
(def k1 (-> [(1 :x :y) %:p]
            [(1 :x k0 :p tr1 :y k0)]))

(def tr2 (-> [(1 :x k0 :p tr1 :y k0) (2 :p :q)]
             [(1 :x k0 :q tr1 :y k0)]))
(def k2 (-> [(2 :p :q) %:m]
            [(2 :p k1 :m tr2 :q k1)]))

(def tp3 (-> [(2 :p k1 :m tr2 :q k1) (3 :m :n)]
             [(2 :p k1 :n tr2 :q k1)]))
(def k3 (-> [(3 :m :n) %:o]
            [(3 :m k2 :o tp3 :n k2)]))
```

# [old] fibration & fiber-bundle

## 區分 fibration 與 fiber-bundle 這兩個術語

- 描述 fibration 的方法
  古典方法 與 形式化方法 有別
  或者說
  古典方法 與 構造性的方法 有別
  古典者
  設一對象 在說明此對象作爲 fibration 所需要滿足的條件
  構造者
  設計一種構造方式 使得依此方式所構造出來的對象都爲 fibration
  構造方式中有種種限制 並且這些限制可以被機器檢查

- 可細分 fibration 與 fiber-bundle 兩個術語

- 古典者 用 projection-map 來代表 fibration
  記 pi : E -> B
  其中 total-space E 已經存在
  即 先給出空間 E
  再觀察如何用 projection-map 將其分解爲 fiber-bundle

- 構造者 用函數空間來代表 fiber-bundle
  記 ((x : B) -> x P)
  與古典相比 P 就是 pi^{-1}
  此時的術語有
  - type-family ::
       P : (B -> type)
  - total-space ::
       ((x : B), x P) [as dependent-pair]
       看來 pi 就是 drop : ((x : B), x P -> x P)
       但是 如若如此 total-space 就是 production-space 了
       所以 pi 應該不能是單純的 drop
  - a section ::
       k : ((x : B) -> x P)
       且 (x k pi == x)

- ><><><
  如上這種 構造性地 定義 fiber-bundle 的想法 並不完善
  因爲 並沒有明確構造 fiber-bundle 的各種限制條件
  尤其是與函數連續性有關的條件

## cover & nerve

- 如上是所謂 構造性地 定義 fiber-bundle
  古典的處理 fiber-bundle 連續性條件 的方式是
  [或者說 古典的定義 fiber-bundle 的方式是]
  是利用 space 的 cover 之間的相容性

- cover 中的兩個 u1 u2 相交的部分之相容
  就是指定一個 ((u1 交 u2) * F) 上的一個變換
  - 這裏 F 就是 fiber 或稱爲 fiber-space
    也就是說 這裏的 type-family 其實不依賴與 x
    x P == F
  即所謂的
  局部平凡乘積空間上的變換

- F 到自身的變換之全體是 Aut(F)
  因此
  爲了構造一個 fiber-bundle
  在 cover 相交的部分指定 Aut(F) 中的元素即可

- 相交部分的每一點都要指定 Aut(F) 中的元素
  這就是 transition-function
  t(i,j) : (u (i) 交 u(j) -> Aut(F))

- 而 古典的 fibration
  pi : E -> B
  則不是指定 Aut(F) 中的元素
  而是在一個對 E 的劃分中
  發現一系列 Aut(F) 中的元素
  這個劃分就是 pi^{-1}(B)

- cover 顯然可以得到 complexes
  u1 u2 之交非空 就是一邊
  u1 u2 u3 之交非空 就是一面
  這種 complexes 被稱爲 cover 的 nerve
  - 如此代數化就得到 infi-groupoid
    此時 每個邊就要給出一個 transition-function

- cocycle condition
  t(i,k) = t(i,j) t(j,k)
  對於形式化[代數化]後的理論來說就是
  infi-groupoid 中的 2-level 元素 應該給出 1-level 元素之間的等價
  - 有趣的是 想象空間中的四個球相交時
    以如上的條件是不足的

- 把點對應 fiber 改爲 open set 對應 fiber
  就得到 presheaf
  fiber-bundle 的公理是就 (u1 交 u2) 敘述的
  而 sheaf 的公理是就 (u1 < u2) 敘述的
  - 之所以有這種非對稱
    可能是因爲 cat 的語法
    沒法以對稱的方式處理 infi-groupoid 中高階元素的乘法

## uncurry 能力有限

- 我的規則可以用 uncurry 來處理 低階到高階的映射
  但是 古典的定義 fibration 的方式是用一個 projection-map
  proj : (total -> base)
  這是一個 高階到低階的映射
  uncurry 如何處理這種映射

- ><><><
  是否 extension 與其逆問題能對解決這個問題有所啓發

# >< 對稱的處理方式能更好地理解高階元的乘法

# >< hott 對 fibration 的非對稱的處理方式 與 AT 的直覺相左

- 在想高階元素在 fibration 中的 lifting 時
  這種衝突就體現出來

# >< 用 bridge 與 logic-language 將 fibration 做對稱化處理
