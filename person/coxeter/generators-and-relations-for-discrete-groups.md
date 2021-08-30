---
title: generators and relations for discrete groups
author: coxeter
date: 1957
---

# 1 cyclic, dicydic and metacyclic groups

## cyclic group

- 用 AT1 的語言
  ``` scheme
  (type space
    (: a (-1 <>))
    (: b (0 a a))
    (: c (1 b b b ... b)))
  ```

## isomorphic

- example of isomorphic
  ``` scheme
  (define R
    (type space
      (: a (-1 <>))
      (: r (0 a a))
      (: c (1 {r} 6 times))))

  (define S
    (type space
      (: a (-1 <>))
      (: [s, t] (0 a a))
      (: d0 (1 s s s))
      (: d1 (1 t t))
      (: d2 (1 s rev t s t))))

  (define f
    (lambda (-> R S)
      (with (-> (-1 R) (-1 S))
        (-> R/a S/a))
      (with (-> (0 R/a R/a) (0 S/a S/a))
        (-> (1 r) (1 s t)))
      (with (-> (1 {r} 6 times)
                (1 {s t} 6 times))
        (-> (2 c)
            (2 d0
               d0 (1 s s s s s s) as-remained-boundary
               d2 (1 s t s t s s s s) as-remained-boundary
               d2 (1 s t s t s t s t s s) as-remained-boundary
               d2 (1 s t s t s t s t s t s t) as-remained-boundary)))))

  (define g
    (lambda (-> S R)
      (with (-> (-1 S) (-1 R))
        (-> S/a R/a))
      (with (-> (0 S/a S/a) (0 R/a R/a))
        (-> (1 s) (1 {r} 4 times))
        (-> (1 t) (1 {r} 3 times)))
      (with (-> (1 {s} 3 times)
                (1 {r} {4 times} 3 times))
        (-> (2 d0)
            (2 c
               c (1 {r} {4 times} 3 times) as-remained-boundary)))
      (with (-> (1 t t)
                (1 {r} {3 times} 2 times))
        (-> (2 d1)
            (2 c)))
      (with (-> (1 s rev t s t)
                (1 {r} -4 times
                   {r} 3 times
                   {r} 4 times
                   {r} 3 times))
        (-> (2 d2)
            (2 c)))))

  (note
    is this the end of the proof of the iso between the two space?
    do not need to prove
    (~ [f g] (identity-map-of R)) and (~ [g f] (identity-map-of S))?)

  (= [f g]
     (lambda (-> R R)
       (with (-> (-1 R) (-1 R))
         (-> a a))
       (with (-> (0 a a) (0 a a))
         (-> (1 r) (1 {r} 7 times)))
       (with (-> (1 {r} 6 times)
                 (1 {{r} 4 times {r} 3 times} 6 times))
         (-> (2 c)
             (2 {c} 7 times)))))

  (note
    each rewriting by relation
    must be recorded by homotopy)

  (= [g f]
     (lambda (-> S S)
       (with (-> (-1 S) (-1 S))
         (-> a a))
       (with (-> (0 a a) (0 a a))
         (-> (1 s) (1 {s t} 4 times))
         (-> (1 t) (1 {s t} 3 times)))
       (with (-> (1 {s} 3 times)
                 (1 {s t} {4 times} 3 times))
         (-> (2 d0)
             (2 c
                c (1 {r} {4 times} 3 times) as-remained-boundary)))
       (with (-> (1 t t)
                 (1 {r} {3 times} 2 times))
         (-> (2 d1)
             (2 c)))
       (with (-> (1 s rev t s t)
                 (1 {r} -4 times
                    {r} 3 times
                    {r} 4 times
                    {r} 3 times))
         (-> (2 d2)
             (2 c)))))
  ```

## note equality of group

- x -
  equality of group is simpler than equality of space?

- k -
  要知道
  就算 '空間的同倫'
  被劃歸到了 '空間的所有同倫羣同構'
  我們還是沒有給出有效的 判別 simple-space 與 mapping-space 等價的方式
  因爲 我們還沒有 判別同倫羣同構的方式

- x -
  可能
  '計算' 空間的同倫羣
  就不是 證明兩個空間同倫[這需要用一個遞歸函數來證明]
  而是 給出兩個滿足條件的函數
  這樣遞歸定義就有了基礎步驟

- k -
  有可能只是二維的空間之間的等價比較特殊?

- x -
  不一定
  因爲與 AT1 中的空間相比
  羣的結構要簡單很多
  因爲
  我們已經知道了
  用 generators 和 relations 定義的羣
  能給出 AT1 中的空間
  而 這個羣就是這個空間的基本羣
  我們在上面的嘗試中 是想要通過證明 空間等價 來證明 羣同構
  但是 空間等價 比 羣同構 更強
  不同的空間 可能有相互同構的基本羣
  但是空間並不等價

- k -
  就此 你能給出具體的例子嗎
  兩個 用 generators 和 relations 定義的羣 G1 G2
  它們對應 AT1 中的 A1 A2 兩個空間
  並且我們知道 G1 G2 作爲羣 是同構的
  但是 A1 與 A2 作爲空間不同倫等價

- x -
  (1) 嘗試直接在 AT1 中給以論證
  (2) 嘗試在古典的語言中論證
  我想先試試 (2)

------

- x -
  AT1 中 generators and relations 表述的羣
  是二維空間的基本羣
  基本羣之間的等價 並非空間的等價
  觀察羣同構的定義
  可以發現它忽略了空間的高維結構
  也就是說
  在證明羣同構時
  我們不需要證明 (~ [f g] (identity-map-of R))
  而是可以忽略 [f g] 的二階映射
  而只考慮其一階映射
  ``` scheme
  (= [f g]
     (lambda (-> R R)
       (with (-> (-1 R) (-1 R))
         (-> a a))
       (with (-> (0 a a) (0 a a))
         (-> (1 r) (1 {r} 7 times)))
       (with (-> (1 {r} 6 times)
                 (1 {{r} 4 times {r} 3 times} 6 times))
         (-> (2 c)
             (2 {c} 7 times)))))
  ```
  要知道 R 與 S 的基本羣同構
  並非是說 (~~ R S)
  也並非是說 (~~ (-> I R) (-> I S))
  而是說 (-> I R) (-> I S) 作爲羣 是同構的

- k -
  也就是說
  不用考慮
  ``` scheme
  (= [f g]
     (lambda (-> R R)
       (with (-> (-1 R) (-1 R))
         (-> a a))
       (with (-> (0 a a) (0 a a))
         (-> (1 r) (1 {r} 7 times)))
       (with (-> (1 {r} 6 times)
                 (1 {{r} 4 times {r} 3 times} 6 times))
         (-> (2 c)
             (2 {c} 7 times)))))
  ```
  而只用考慮
  ``` scheme
  (= [f g]
     (lambda (-> R R)
       (with (-> (-1 R) (-1 R))
         (-> a a))
       (with (-> (0 a a) (0 a a))
         (-> (1 r) (1 {r} 7 times)))))
  ```

- x -
  如何論證這一點呢
  如何描述 (-> I R) (-> I S) 作爲羣的同構呢

- k -
  函數空間並非比空間本身更難
  比如 (-> I R)
  其實就是 R 的一階元素生成的
  而以二階元素爲關係的[以二階元素來定義等詞的]
  更簡單的結構

- x -
  也就是說
  對於空間而言
  只有相鄰的兩個階級的元素之間的相互影響才重要
  比如說 三階元素對於判斷一階元素之間的相等是沒有用的

- k -
  這不就是 n-cat 中的 weak structure?

- x -
  先擱置 n-cat 中的 weak structure 不談
  看來爲了定義羣同構
  (a) 我們需要能定義空間之間 忽略某級以上之高維的 映射
  (b) 然後我們要能定義這些映射之間的等價關係
  ``` scheme
  (define R
    (type space
      (: a (-1 <>))
      (: r (0 a a))
      (: c (1 {r} 6 times))))

  (define S
    (type space
      (: a (-1 <>))
      (: [s, t] (0 a a))
      (: d0 (1 s s s))
      (: d1 (1 t t))
      (: d2 (1 s rev t s t))))

  (define f
    (lambda (-> R S)
      (with (-> (-1 R) (-1 S))
        (-> R/a S/a))
      (with (-> (0 R/a R/a) (0 S/a S/a))
        (-> (1 r) (1 s t)))))

  (define g
    (lambda (-> S R)
      (with (-> (-1 S) (-1 R))
        (-> S/a R/a))
      (with (-> (0 S/a S/a) (0 R/a R/a))
        (-> (1 s) (1 {r} 4 times))
        (-> (1 t) (1 {r} 3 times)))))

  (= [f g]
     (lambda (-> R R)
       (with (-> (-1 R) (-1 R))
         (-> a a))
       (with (-> (0 a a) (0 a a))
         (-> (1 r) (1 {r} 7 times)))))

  (= [g f]
     (lambda (-> S S)
       (with (-> (-1 S) (-1 S))
         (-> a a))
       (with (-> (0 a a) (0 a a))
         (-> (1 s) (1 {s t} 4 times))
         (-> (1 t) (1 {s t} 3 times)))))
  ```

## factor groups

- In other words,
  the effect of adding new relations
  to the abstract definition of a group G,
  is to form a new group G' which is a factor groupof G.

## direct products

- every finite Abelian group
  is a direct product of cyclic groups.

## automorphisms

# 2 systematic enumeration of cosets

# 3 graphs, maps and cayley diagrams

- the diagram that shows us how to get a torus by gluing a square,
  is the no a cayley diagram,
  because its points are not regular.

- the cayley is the universal covering space of torus.

- in the construction of universal covering space of a space,
  different pathes are distinct as different points.

- a group defined by generators and relations,
  is the fundamental group of the corresponding space in AT1,
  since an element of the group is a loop in the space with fixed base point.

# 4 abstract crystallography

# 5 hyperbolic tessellations and fundamental groups

# 6 the symmetric, alternating, and other special groups

# 7 modular and linear fractional groups

# 8 regular maps

# 9 groups generated by reflections
