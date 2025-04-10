---
title: holding
---

# todo

可以先設計語言與語法以處理 有限生成羣 與 二維連續體

整理關於各種由羣獲得的空間的理論
判斷目前獲得的認識是否可靠
[尤其是有關 dual-cayley-graph 的]
[covering-space 與 '賦以流形幾何' 密切相關 [thurston]]

[hopf-fibration]
I need mapping-class-group of type (-> torus torus)
to express the re-gluing of (* sphere-2 sphere-1)

[about Van Kampen Theorem]
Van Kampen Theorem of groupoid
the Generalised Van Kampen Theorem

- an algebra in which compositions are of a geometric type
  reflecting the geometrical properties of the spaces.

syntax for weak equivalent

to develop a homotopy theory in AT1

fix the syntax of homotopy group and relative-homotopy group

cylinder torus and MCG of dehn

# leaning path

fiber-space -> hopf-fibration

hopf-fibration -> cutting-and-regluing -> dehn-surgery -> lens-space ->
thurston-s-teaching -> hyperbolic geometry -> projective geometry

hopf-fibration -> GA and SO(3) -> matrix group

classical-background -> cobordism

classical-background -> dimension-down

dimension-down -> infinitesimal -> non-standard-analysis

infinitesimal -> leibniz euler newton

hopf-fibration -> H-space -> hopf-invariant -> hopf-algebra

# about classical construction

There are many ways to do classical constructions in algebraic topology.
One who wishes to design a formal language for algebraic topology,
must choose carefully which to implement,
which concept to include in, and which concept to hold still,
while waiting for a better understanding of
how constructive the concept is.

# immersion and embedding

map of type (-> A B)
is an immersion of A in B.

with the 'no self-intersection' condition,
the map is an embedding,
B is the ambient space.

If the embedding is to be expressed at all,
the sphere-3 defined as with no non-trivial path
will be not enough for embedding.

Thus distinction between constructions of the same space
must be maintained.

Or we must be able to intro subdivision locally.
for example, to intro a path in a surface,
which connects two point on the boundary of the surface.

# the plan

Inspired by Bishop's plan
and the treatment of algebraic-topology by sze-tsen-hu.

A concept is defined once we explain how to implement it in our language.
- Just like in physics, a concept is operationally defined,
  once we explain how to measure it.

To define property of object
by the solvability of problem fromed around the object.

# same-position-self-gluing, cancel or not cancel?

same-position-self-gluing
[for example: p * p^{-1}]

non-same-position-self-gluing
can only happen,
when there is at least one part of the boundary is repeating.
[for example: p * p]

[cancel or not cancel?]
when building a cylinder,
the boundary of the surface must be a circle,
thus part of the same-position-self-gluing of a path
can not be canceled !

# geometry

How to intro geometry back to a language for AT?
by giving path a length? surface an area?
or by a general structure for all the measurement?

# classical mechanics

引入幾何的同時
可否像利用微分流形來處理經典力學一樣
利用 cell-complex-structure 來處理經典力學?
利用 cell-complex-structure 描述空間的同時
還要描述空間中的運動

如何在 cell-complex-structure 上引入座標系?
是否應該引入座標系?
這對描述運動而言是否是必要的

# adjunction-space

是否在 at1 中引入 adjunction-space?
作爲一種構造新空間的方式?

注意
cell-complex 中的 characteristic-map 就是 adjunction-space

# local-subdivision

With the syntax for local-subdivision,
maybe we can prove the mapping-class-group of cell is trivial.

可以利用 local-subdivision 來恢復 topology structure 嗎?

continuum 的一個直觀特徵是
可以被潛在無限地細分
local-subdivision 就是爲了捕捉這個特徵

they may be divided anywhere,
though they cannot be divided everywhere at the same time.

# dehn 的方法

- x -
  dehn 的方法在於
  在曲面上給出一個座標系
  使得密布曲面的嵌入曲線得以代數表示
  因爲曲線是密布曲面的
  所以這個代數結構的變換羣就是曲面的映射類羣

# 映射類羣

- x -
  disk[cell] is special,
  for its mapping-class-group is trivial.
  is this the reason why cell-complex is built by cells?

- k -
  首先我們需要知道如何在 at1 中定義映射類羣
  並且要知道我們的定義是良好的

- x -
  也就是說 我們必須給我們的形式語言以古典基礎
  - 古典的定義是開端 它們定義了研究對象
    爲了探究這些對象的某些性質
    我們發現略去古典定義中的大部分信息
    就可以簡化我們所使用的語言
  - 我們想要直接使用這個更簡的語言
    但是這需要辯護
    我們需要證明這個更簡的語言是對原語言的有效簡化
    而不會有不一致的地方
  - 我們並不棄幾何而只看拓撲
    也不棄拓撲而只看代數
    我們要明確這些不同的語言之間的關係
    作爲語言的設計者和理論的提出者
    我們還要不斷地對比不同語言 就解決問題而言之優劣

- k -
  古典定義之所以爲 '古典'
  是因爲人們已經如此習以爲常
  以至於略去了這些概念的啓發性例子
  和這些概念之形成的歷史語境

# 非標準分析

- x -
  我們開始學習 非標準分析
  嘗試用它來解決 形式化的同倫論中的維數變化問題

- k -
  並且 要知道
  非標準分析 也是爲古典的領域設計新語言的一個例子

- x -
  我想 看看古典的著作中作者是如何依直覺使用無窮小量的
  也許會有益於我們的學習

# embedding 與 homotopy

- x -
  MCG 中的映射是 X 到自身的 embedding
  但是 把面积挤压到邊界的映射並非 embedding
  但是我必須要允許這種映射
  因爲在我的形式化語言中 我想象不出別的選擇

- k -
  這個被擠壓的映射 與某個不被擠壓的映射同倫
  因此只有 在 homotopy 下 我們才能完成形式化

# 關於邊界

- x -
  同一個拼接起來的形狀
  可能有多種不同的 描述其邊界的方式
  比如
  cylinder 的 mapping-class-group 涉及到了同一個 space 的不同邊界描述

# the use of fibration in homotopy group

- x -
  這應該是被優先考慮的
  畢竟 fiber-space 的形式處理 一定要能夠捕捉這種應用

# classical view cell-complex 的映射

cell-complex 之間的映射是一次指定一個 cell 的
因爲在 homotopy equivalent 下
固定邊界後 cell 之間的映射只能有一種情況

# >< how to define function in synthetic geometry?

# a new plan

a synthetic homotopy theory with axioms;
and a implementation as its model.

calculation of many different types of (co)homotopy and (co)homology,
might be done in an intrinsic way.

to calculate
mapping-class-group
homotopy-group
homology-group
dRham-co-homology-group

# x 對數學的奇怪印象

- x -
  數學本應該是最清晰的藝術於學科
  在以綜合法學習歐氏幾何或者射影幾何的時候
  我能感受到這一點
  甚至在用無窮小分析來研究物理現象的時候
  我也能感受到這一點
  但是爲什麼在學習現代數學的時候
  我確恰恰有相反的印象呢?
  爲什麼我總覺得所學到的前人的著述如此晦澀
  以至學來無樂呢?

- k -
  欲究其因
  方向有二
  其一在己
  其二在乎他人

- x -
  於己 可謂不勤乎
  置之且不論
  於他人之著述
  其有別於先賢者甚矣
  先賢之著 以公理爲本 實例豐富
  每舉例者 必察簡單圖形之有趣性質
  由簡至繁 引人入勝
  而今人所著者
  仿先賢之公理方法 而鮮舉實例
  其法尚一 而不辨細節

- k -
  對於想要設計的 AT1 來說
  我們能給出良好的類似歐式幾何的 '原本' 嗎?

- x -
  首先 我們有簡單的圖形 即 cell-complex
  其次 我們要找出一些有趣的關係
  最簡單的關係是 映射之間的同倫
  其次是 空間之間的同倫等價
  在這個模型下
  我們可以形成一些定理 來以綜合的方式證明 這兩個關係
  而不用再用定義驗證
  注意 對這些關係的證明都是構造性的
  一個具有一般性的命題 其實就是一個函數
  所謂 綜合的證明 就是用舊的函數構造新的函數

- k -
  我想首先我們需要
  證明我們的理論是對經典的 CW-complex 理論的有效簡化[公理化]

- x -
  在這之前 我也可以先發展關於 bundle-space 於 fiber-space 的理論
  並且大致想清楚如何計算 homology-group

# cell-structure of mapping-space

mapping-space might not be reduced to [be viewed as] simple-space,
because a path can not be mapped to a point.

but we do not need to,
for the structure of simple-space
is to make us be able to check continuity of function.

# about equality

equality between two elements (: [x0, x1] X),
is established by a extension-problem
```scheme
(lambda (-> I X)
  (extend-from
    (lambda (-> (list i0 i1) X)
      (-> i0 x0)
      (-> i1 x1)))
  ...)
```

equality between two elements (: [f, g] (-> A X)),
is established by a extension-problem
```scheme
(lambda (-> (* A I) X)
  (extend-from
    (lambda (-> (* A (list i0 i1)) X)
      (-> (* :a i0) [:a f g])
      (-> (* :a i1) :a)))
  ...)
```

we know the equality between two spaces (~~ A B)
```scheme
(: f (-> A B))
(: g (-> B A))
(~ [f g] [A id])
(~ [g f] [B id])

(lambda (-> (* A I) A)
  (extend-from
    (lambda (-> (* A (list i0 i1)) A)
      (-> (* :a i0) [:a f g])
      (-> (* :a i1) :a)))
  ...)

(lambda (-> (* B I) B)
  (extend-from
    (lambda (-> (* B (list i0 i1)) B)
      (-> (* :b i0) [:b g f])
      (-> (* :b i1) :b)))
  ...)
```

but how about equality between two mapping-spaces
(~~ (-> A X) (-> B Y))?

the algebraic structure of (-> I X) is given by a function of type
```scheme
(-> [(-> I X) (-> I X)] (-> I X))
```
what is the equality between algebraic structures?

first, we know that the definition of (~~ A B)
is to make sure that
homotopy(n) (A, a0) = homotopy(n) (B, b0)
[for every definition, we must ask why we intro it this way.]
thus, the definition of (~~ (-> A X) (-> B Y))
must also make sure that
homotopy(n) ((-> A X), a0) = homotopy(n) ((-> B Y), b0)
there (: a0 (-> A X)) and (: b0 (-> B Y))

子曰
視其所以
觀其所由
察其所安
人焉廋哉
人焉廋哉

let's see why (~~ A B) implies
homotopy(n) (A, a0) = homotopy(n) (B, b0)
we know that
```scheme
homotopy(n) (A, a0) :=
(lambda (-> I A)
  (extend-from
    (lambda (-> (list i0 i1) A)
      (-> i0 a0)
      (-> i1 a0)))
  ...)
;; and
homotopy(n) (B, b0) :=
(lambda (-> I B)
  (extend-from
    (lambda (-> (list i0 i1) B)
      (-> i0 b0)
      (-> i1 b0)))
  ...)
```
and the equality between elements of the group
is defined as the equality between functions.

if we have
```scheme
(: f (-> A B))
(: g (-> B A))
(~ [f g] [A id])
(~ [g f] [B id])

(lambda (-> (* A I) A)
  (extend-from
    (lambda (-> (* A (list i0 i1)) A)
      (-> (* :a i0) [:a f g])
      (-> (* :a i1) :a)))
  ...)

(lambda (-> (* B I) B)
  (extend-from
    (lambda (-> (* B (list i0 i1)) B)
      (-> (* :b i0) [:b g f])
      (-> (* :b i1) :b)))
  ...)
```

then we can use (: f (-> A B)) to map
elements in (-> I A) to elements in (-> I B)
and use (: g (-> B A)) to map
elements in (-> I B) to elements in (-> I A)

we must then prove these two maps
1. are well defined w.r.t. the equality of the group.
   [this can be proved by homotopy-extension-property]
2. respect the group production.
   [this can be proved by ><><><]
3. are revers of each other as group homomorphism.
   ```scheme
   (: [x, x f g] (-> I A))
   (: (~ x [x f g]) (-> (* I I) A))

   ;; ------------

   (define f
     (lambda (-> A B)
       ...))

   (define g
     (lambda (-> B A)
       ...))

   ;; (~ [f g] (identity-map-of A))
   (define h
     (lambda (-> (* A I) A)
       (extend-from
         (lambda (-> (* A (list i0 i1)) A)
           (-> (* :a i0) [:a f g])
           (-> (* :a i1) :a)))
       (with (-> (* (-1 A) (-1 I)) (-1 A))
         ...)
       (with (-> (* (-1 A) %:a (0 i0 i1))
                 (0 (* :a i0) <>
                    (* :a i1) <>))
         (-> (* a0 (1 i01))
             (:> (0 (* a0 i0) <>
                    (* a0 i1) <>)
                 (0 a0 f g
                    a0)))
         ...)
       ...))

   ;; (~ [g f] (identity-map-of B))
   (define k
    (lambda (-> (* B I) B)
      (extend-from
        (lambda (-> (* B (list i0 i1)) B)
          (-> (* :b i0) [:b g f])
          (-> (* :b i1) :b)))
      ...))

   (define x
     (lambda (-> I A)
       (extend-from
         (lambda (-> (list i0 i1) A)
           (-> i0 a0)
           (-> i1 a0)))
       ...))

   (define (~ [x f g] x)
     (lambda (-> (* I I) A)
       ;; (extend-from
       ;;   (lambda (-> (* I I) A)
       ;;     (with (-> (* (-1 I) (-1 I)) (-1 A))
       ;;       (-> (* i0 i0) [i0 x f g])
       ;;       (-> (* i1 i0) [i1 x f g])
       ;;       (-> (* i1 i1) [i1 x])
       ;;       (-> (* i0 i1) [i0 x]))
       ;;     (with (-> (* (0 i0 i1) (-1 I) %:i)
       ;;               (0 (* i0 :i) <>
       ;;                  (* i1 :i) <>))
       ;;       (-> (* (1 i01) i0) [(1 i01) x f g])
       ;;       (-> (* (1 i01) i1) [(1 i01) x]))))
       (extend-from
         (lambda (-> (* I (list i0 i1)) A)
           (-> (* :i i0) [:i x f g])
           (-> (* :i i1) [:i x])))
       (with (-> (* (-1 I) %:i (0 i0 i1))
                 (0 (* :i i0) <>
                    (* :i i1) <>))
         (-> (* i0 (1 i01))
             [(* [i0 x] (1 i01)) h] (=> [(* a0 (1 i01)) h])
             (:> (0 i0 x f g
                    i0 x)
                 (0 a0 f g
                    a0)))
         ;; how can an element of the above type
         ;; be given by 'h' and 'k'?
         (-> (* i1 (1 i01))
             [(* [i1 x] (1 i01)) h] (=> [(* a0 (1 i01)) h])
             (:> (0 i1 x f g
                    i1 x)
                 (0 a0 f g
                    a0))))
       (with (-> (* (0 i0 i1) %:p0
                    (0 i0 i1) %:p1)
                 (1 (* (1 :p0) i0) <>
                    (* i1 (1 :p1)) <>
                    (* (1 :p0) i1) <> rev
                    (* i0 (1 :p1)) <> rev))
         (-> (* (1 i01) (1 i01))
             [(* [(1 i01) x] (1 i01)) h]
             (:> (1 (1 i01) x f g
                    (* i1 (1 i01)) <>
                    (1 i01) x rev
                    (* i0 (1 i01)) <> rev)
                 (1 (1 i01) x f g
                    (* [i1 x] (1 i01)) h
                    (1 i01) x rev
                    (* [i0 x] (1 i01)) h rev))))))

   (define (~ [x f g] x)
     (lambda (-> (* I I) A)
       (extend-from
         (lambda (-> (* I (list i0 i1)) A)
           (-> (* :i i0) [:i x f g])
           (-> (* :i i1) [:i x])))
       (with (-> (* (-1 I) %:i (0 i0 i1))
                 (0 (* :i i0) <>
                    (* :i i1) <>))
         (-> (* i0 (1 i01))
             [(* [i0 x] (1 i01)) h])
         (-> (* i1 (1 i01))
             [(* [i1 x] (1 i01)) h] ))
       (with (-> (* (0 i0 i1) %:p0
                    (0 i0 i1) %:p1)
                 (1 (* (1 :p0) i0) <>
                    (* i1 (1 :p1)) <>
                    (* (1 :p0) i1) <> rev
                    (* i0 (1 :p1)) <> rev))
         (-> (* (1 i01) (1 i01))
             [(* [(1 i01) x] (1 i01)) h]))))
   ```

for (~~ (-> A X) (-> B Y))
1.
2.
3.

# to develop a homotopy theory in AT1

- x -
  不要着急把這些理論和語法固定下來
  我們還有時間

# about dim

(* A B) dim = A dim B dim add

(-> A X) dim = X dim A dim sub

(* A B) dim = (-> (* A B)) dim

(-> (-> A B) C) dim
= C dim (-> A B) dim sub
= C dim B dim A dim sub sub
= C dim A dim add B dim sub
= (-> B (* A C)) dim

# erlangen program

- x -
  從 erlangen program 的角度來看
  就一個 cell-complex 而言
  我們考慮的只能是那些在空間自身的連續變換下不變的性質
  連續映射的類型爲 (-> X X) 並且要是滿射
  而空間自身的連續變化 甚至可以改變其部分的維度
  高維元素可以映射到低維元素
  反過來 低維元素是不是也應該可以映射到高維元素?

# sad fact

- x -
  我發現想要拋棄 topological structure
  就必須要先非常熟悉它
  而我對它的學習是爲了讓後人不再學習它
  讓它在我這裏終止

# begin

- x -
  I begin here.
  And I won't begin in the way
  in which I can ensure to you I won't fall to error.
  I just begin.

# 解決問題

- x -
  人們被謎題所吸引
  謎題引人思考
  當大家聲稱一個問題被解決了
  人們就不在乎當初人們爲什麼提出這個問題
  也很少在乎解決這個問題的人的動機與方式
  而只在乎這個問題已經被給以了否定或肯定的答案

- k -
  那麼 AT1 的目的是什麼呢?
  不是爲了解決代數拓撲中的問題嗎?

- x -
  我要提供一種新的語言來解釋這些問題
  解決與否並不重要
  最好的情況是 把舊的問題用更好的方式描述來吸引更多人思考
  我想 薩繆里安 說的非常對
  如果 歐幾里得 不是用這種給以每個問題以確切證明的方式來呈現一個幾何學
  而是把這些問題寫成一本有趣的謎題書
  則更好
  因爲當神祕的面紗被揭開
  人們不再思考這些幾何問題的根源了
  除了考試與訓練之外 不再在乎這些問題了
  通過以這種方式揭開面紗
  歐幾里得 抹殺了遊戲的一切快樂
  它使得人們如此懶於思考
  以至於一千多年之後人們才確切地發現非歐幾何之可爲

# 真理

- x -
  我相信某些東西是對的
  我相信它們果真如此
  我對這種信仰有無比的熱情
  但是同時
  我也相信這些東西有錯的可能
  這些東西可能是假的

- k -
  這看來並不矛盾
  甚至沒有矛盾的可能
  因爲這並非一個確切的判斷或命題

- x -
  沒錯
  說 '我相信'
  就代表了這些真理是就我而言的
  說 '可能' 就更模糊了判斷
  使得矛盾成爲不可能了
  如果說
  我相信這隻矛可能是世界上最鋒利的矛 它能戳破一切的盾
  我相信這隻盾可能是世界上最堅固的頓 它能抵擋一切的矛
  而不是說
  這隻矛是世界上最鋒利的矛 它能戳破一切的盾
  這隻盾是世界上最堅固的頓 它能抵擋一切的矛
  那矛盾就讓它矛盾去吧
  只要試試到底誰厲害
  我就馬上改信新的事實
  不相信事實不是愚蠢的嗎

- k -
  形成理論 但是隨時依照新的經驗來更改理論

- x -
  沒錯
  我甚至不能否定這種態度
  因爲 當有新的經驗時
  其只所以 稱爲 '新的經驗'
  可能就意味着
  我要利用這些新的經驗來重寫我的理論了

# 辯證法的濫用

- x -
  對話與辯證法只能用來真誠地討論未知的問題
  而不能就某個問題來自問自答

# topological structure of mapping space

# 代數結構 與 空間之間的同倫等價

- x -
  首先我總結 AT1 中可能出現代數結構

  (1)
  空間本身作爲高維代數結構
  特點是乘法是沿着公共邊界的 粘合
  高維的元素就是低維元素之間的關係

  (2)
  mapping-space such as
  loop-space and path-space
  作爲代數結構

  (3)
  空間之間的變換作爲代數結構
  空間本身爲元素
  空間之間的同倫等價就是元素之間的相等

  問題是
  如果依我之前所說
  空間之間的同倫等價之定義
  是爲了使得 相互等價的空間有 相互同構的 同倫羣
  那麼別的代數結構
  比如 mapping-class-group 與 同調羣
  如何呢

- k -
  (1) 是否可以劃歸爲 (2)
  如果有此種劃歸
  那麼 (1) 中高維代數的特點如何表現在 (2) 中呢

- x -
  可是說 如果做了這樣的劃歸
  那麼 (1) 中多種乘法的特點就被消除了
  假設我有 (: f (-> X A)) 與 (: g (-> Y A))
  我知道 X f bdry 與 Y f bdry 有重合的部分
  考慮這個重合的部分如何把 X Y 粘合成 Z
  我們就得到了 (: j (-> Z A))
  loop-space 中的乘法就是如此獲得的

- k -
  可否說
  (2) 中處理元素乘法的方式
  是把 (1) 中的某些乘法模式 固定下來了
  並給以了命名

- x -
  如若如此
  (1) 中所描述的空間之元素高維狂野乘法就是一切的基礎
  而空間的等價正是 (1) 中 元素關係的體現
  注意
  在 (1) 中
  高維元素就是低維元素的關係

- k -
  如果如此
  也可以說是
  (1) 給出了空間的 groupoid 結構
  而 (2) 給出了描述這個 groupoid 的性質的語言
  那麼
  爲什麼我們需要用 (2) 來定義 同倫羣
  而不能直接在 (1) 所描述的 groupoid 中研究同倫羣呢

- x -
  利用 (2) 我們描述的是
  groupoid 的 各階 sub-group
  whitehead 的定理就是說
  各階 sub-group 之等價 等同於 groupoid 本身的等價

- k -
  沒錯

- x -
  我們現在想要明瞭的是
  目前定義空間同倫等價的方式是否 '正確'
  這個定義是否是 infi-groupoid 之間的同構?

- k -
  whitehead 的定理是說
  (a)
  空間的等價 => 各階同倫羣等價
  就算是這一點我們都沒有搞清楚
  我們之前的形式討論只是說明了
  空間的等價 => 一階同倫羣等價
  對於高階的情況 我們的語言還沒有確定下來
  (b)
  各階同倫羣等價 => 空間的等價
  就這個方向而言
  我們更是要檢驗當前的
  空間之同倫等價 的定義的 '正確' 性

- x -
  mapping-space 想要有代數結構
  就必須如同 loop-space 一樣特殊
  如果如此
  假設 現在對空間同倫等價的定義是 '正確' 的
  我們現在的語言已經足夠描述 whitehead 的定理了

- k -
  但是還不足以明確什麼是一般的 mapping-space 之間的同倫等價

- x -
  沒錯

- k -
  那我們先看看 infi-groupoid 之間的自然的等價應該如何定義

- x -
  就是推廣一下 group 的表示 之間的同構的定義 不是嗎?

- k -
  一個連續函數就是一個同態

- x -
  但是 目前對空間同倫等價的定義看似要比 infi-groupoid 之間的同構要弱
  我們要看看這是否是真的弱
  還是 檢驗 infi-groupoid 之間同構的一個弱化了的準則
  以便檢查起來更方便

- k -
  首先要知道 (1) 是
  combinatorial infi-groupoid theory
  正如 combinatorial group theory
  我們通過 group 的 generator 和 relation 來研究 group
  在 combinatorial infi-groupoid theory 中
  我們有高階的 relation

- x -
  我們之間所證明的
  (~~ A B) => (~~ (-> I A) (-> I B))
  只不過是說 在 (-> ... ...) 中
  等價可以替換等價
  但是不同 arrow 形式的空間 不能等價
  比如 (~~ A (-> I B))
  但是我發現 可以把 (~~ A (-> I B)) 定義爲
  A 和 (-> I B) 的所有同倫羣等價
  這樣不同 arrow 形式的空間也能等價了
  也就是說
  定義的序列是
  (a)
  同一個函數空間中元素的等價
  (: [f, g] (-> X A))
  (: (~ f g) (-> (* X I) A))
  特殊地 我們得到了空間之間元素的等價
  (: [f, g] A)
  (: (~ f g) (-> I A))
  (b)
  空間之間的等價
  (: (~~ A B) (and (-> (* A I) A) (-> (* B I) B)))
  (c)
  空間之間的等價 <=> 所有同倫羣的等價
  (d)
  利用 所有同倫羣的等價 來把空間之間的等價關係從 simple-space
  擴展到 任意 mapping-space

- k -
  這種定義序列是可行的
  但是這是 '正確的' 嗎

- x -
  如果我們能證明
  對 simple-space 或這 mapping-space 的同倫羣的定義
  與古典定義相重合
  那麼這就是正確的

- k -
  回到 combinatorial infi-groupoid theory
  我們首先要證明 空間之間的同倫等價 是 infi-groupoid 之間的同構

- x -
  首先我們考慮 combinatorial group theory 如何是
  特殊的 combinatorial infi-groupoid theory
  注意
  infi-groupoid theory 可能能夠被劃歸到 infi-group theory
  因爲每個 complex 都可以化爲只有一個點的 complex

- k -
  infi-groupoid 中乘法之狂野
  是否也能通過這種劃歸來簡化呢

- x -
  我們先複習 combinatorial group theory

# combinatorial group theory

- x -
  我們要考慮 algebraic-topology 中的概念
  如何能夠體現在 combinatorial group theory 中
  然後再推廣回 groupoid
  algebraic-topology 中的概念有
  - 同倫等價
  - 同倫羣
  - 同調羣
  - 映射類羣
  等等

# cutting-and-regluing

用空間 B F 製作 bundle-space 時
cutting-and-regluing 用
(-> (*  B' fiber) fiber)
中的元素完成
其中 B' 是 B 的低一維子空間
沿着 B' 要能夠切開 B

# lens-space

what lens-space are equal to?

different ways to construct lens-space
- dehn-surgery

# hopf fibration

## note

The dependent product of sphere-1 and a circle in sphere-2
is a torus fibered as a circle of sphere-1 linked together
as hopf links.

| base-space (S2) | total-space (S3) |
|-----------------+------------------|
| a point         | a circle         |
| two points      | two hopf links   |
| a path          | a hopf band      |
| a circle        | a torus (*)      |
| half S2         | a solid torus    |

A hopf band is a surface whose boundary are hopf links,
in hopf fibration, there are full of such surfaces.

The torus with (*) in the table
might be useful for the construction of hopf-fibration of S3
by Dehn surgery.

Can I construct the solid torus first by half S2?

I must be able to express the facts above in my language.

## mobius

```scheme
(define I
  (type space
    i0 i1 (-1 <>)
    i01 (0 i0 i1)))

(define sphere-1
  (type (-> space)
    b1 (-> (-1 <>))
    loop (-> (0 b1 b1))))

(: (* sphere-1 I) (* space space))

(: (* b1 i0)      (-1 (* sphere-1 I)))
(: (* b1 i1)      (-1 (* sphere-1 I)))

(: (* b1 i01)     (* b1 (0 i0 i1)))
(=>               (0 (* b1 i0) (* b1 i1)))

(: (* loop i0)    (* (0 b1 b1) i0))
(=>               (0 (* b1 i0) (* b1 i0)))

(: (* loop i1)    (* (0 b1 b1) i1))
(=>               (0 (* b1 i1) (* b1 i1)))

(: (* loop i01)   (+ (* loop (0 i0 i1)) (* (0 b1 b1) i01)))
(=>               (+ (1 (* loop i0) (* loop i1) rev)
                     (1 (* b1 i01) (* b1 i01) rev))
                  (1 (* loop i0) (* b1 i01)
                     (* loop i1) rev (* b1 i01) rev))

(define twist
  (lambda (-> I I)
    (with (-> (-1 I) (-1 I))
      (-> i0 i1)
      (-> i1 i0))
    (with (-> (0 i0 i1) (0 i1 i0))
      (-> (1 i01) (1 i01 rev)))))

(define mobius
  (type fiber-space
    (& sphere-1 I
       [twist (& b1 i01)])))

(: (& b1 i0)      (-1 mobius))
(: (& b1 i1)      (-1 mobius))

(: (& b1 i01)     (& b1 (0 i0 i1)))
(=>               (0 (& b1 i0) (& b1 i1)))

(: (& loop i0)    (& (0 b1 b1) i0))
(=>               (0 (& b1 i0) (& b1 i0)))

(: (& loop i1)    (& (0 b1 b1) i1))
(=>               (0 (& b1 i1) (& b1 i1)))

(: (& loop i01)   (+ (& loop (0 i0 i1)) (& (0 b1 b1) i01)))
(=>               (+ (1 (& loop i0) (& loop i1) rev)
                     (1 (& b1 i01) (& b1 i01) rev))
                  (1 (& loop i0) (& b1 i01)
                     (& loop i1) rev (& b1 i01) rev))
```

## hopf-fibration

```scheme
(define sphere-1
  (type (-> space)
    b1 (-> (-1 <>))
    loop (-> (0 b1 b1))))

(define sphere-2
  (type (-> space)
    b2 (-> (-1 <>))
    surf (-> (1 b2 refl))))

(: (* sphere-2 sphere-1) (* space space))

(: (* b2 b1)           (-1 (* sphere-2 sphere-1)))

(: (* b2 loop)         (* b2 (0 b1 b1)))
(=>                    (0 (* b2 b1) (* b2 b1)))

(: (* surf b1)         (* (1 b2 refl) b1))
(=>                    (1 (* b2 b1) refl))

(: (* surf loop)       (+ (* surf (0 b1 b1))
                          (* (1 b2 refl) loop)))
(=>                    (+ (2 (* surf b1) (* surf b1))
                          (2 (1 (* b2 loop)) refl))
                       (2 (* surf b1) (* surf b1)
                          (1 (* b2 loop)) refl))
```

# >< 同倫羣中乘法的定義

- x -
  還是需要利用 n-disk 來定義乘法
  ```scheme
  (define I
    (type space
      (: )))

  (define n-disk
    (type space
      (: )))
  ```

# >< 空間的邊界

- x -
  如何定義空間的邊界

- k -
  對於 cylinder 這種有 embedding 的空間來說
  很容易看出其邊界
  但是對於複雜的空間比如
  ```scheme
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
  ```
  定義就不簡單了

- x -
  但是我們可以用 brentano 的處理方式

# 截斷空間的階級

- x -
  截斷空間的階級所證明的等價 或者說 '羣的同構'
  其實就是空間的基本羣的同構
  但是這並非是類型爲 ((><><>< ~~ 1) (-> I A) (-> I B)) 的證明
  這種證明基本羣同構的方式能否被推廣到一般情形
  即 推廣到簡單空間和函數空間之間基本羣之同構
  並 推廣到高階同倫羣之同構的證明

# syntax problem and new syntax

- x -
  說之所以需要 weaken the definition of infi-groupoid
  是因爲 syntax 沒有足夠的能力描述多樣的乘法
  而使用 new syntax 之後
  看似豐富的表達能力是否是本質的
  即問 表達能力是否過於豐富了
  比如
  考慮同調論中單一的乘法
  這種從豐富到單一的簡化 爲什麼是有效的?

# refl

- x -
  if 'refl' can be used to define dim-down map,
  can it be used to define dim-up map?

- x -
  可以想象 refl 所生成的空間不在原空間內

- x -
  認爲路的寬度不是零而是無窮小何如

- k -
  我想在 at1 中維度的變化主要體現在 refl 這個操作上

- x -
  我們先來總結一些現象吧
  (1)
  首先 (x refl) 可以被理解爲單位元 [也就是說可以隨時消去和引入]
  並且 (x refl boundary boundary) = 0
  因爲 (x refl boundary) = (+ x x rev)
  (2)
  其次 計算 cylinder 的 mapping-class-group 時
  所給出的扭轉
  在 at1 中看起來是把一部分面積給擠壓掉了
  可能需要用無窮小量來理解這種映射

# relative homotopy group

- x -
  爲什麼人們會考慮 relative homotopy 這個概念?

- k -
  可能是定義 homotopy group 自然要用到這個概念
  因此這個概念就被泛化了

- x -
  也就是說 'relative' 可能是有關 空間的正規化的
  ```scheme
  (-> (> X A) (> Y B))
  ```

# 兩種觀點

- x -
  (1)
  生成子和關係
  (2)
  空間的變換
  即 functions of type (-> X X)

- k -
  但是在我們的語言中
  生成子和關係就是空間

- x -
  這樣兩種觀點就融合在一個語言中了
  並且
  類型爲 (-> X X) 的函數的可逆性可能要涉及到邏輯式編程
  考慮一般的 (2) 的時候 我們得到的是 n-cat 而不是 n-groupoid

  可能 想要瞭解 由 (2) 定義的羣的性質
  就還是要把它劃歸到 (1)
  即 找到生成子 並發現生成子之間的關係

# 考慮有限羣和置換羣之間的關係

- x -
  有限羣可以被作爲子羣嵌入在高階置換羣中
  羣 作爲 對稱 總是 對某個結構化的集合的元素的置換
  但是 對於我們所定義的簡單空間而言
  空間的自我映射所形成的羣
  還要考慮 空間的元素的 refl 等等

# mapping-class-group

- x -
  G -- (underlying-space G)
  (auto G) -- (mapping-class-group (underlying-space G))

  第一個對應關係 幾乎是平凡的
  可能只是兩種看待空間的方式 這兩種方式中 等價關係的定義不同

  我們需要一些論證 才能說明第二個對應關係
  因爲映射被降維了

# note

## critiques

### a critique of eckmann-hilton argument

- about interchange law.

- x -
  所謂 eckmann-hilton argument
  與高階乘法的 '交換性'
  應該被視爲一個語言學現象

  首先
  古典理論中對高階乘法的交換性的證明
  用到了連續的 homotopy
  而只是在形式化的處理方式中才需要用到 eckmann-hilton argument

  eckmann-hilton argument 說
  利用如下的條件
  (A x B) o (C x D) = (A o C) * (B o D)
  [即 多種粘合次序能粘合出同一個幾何體]
  就能證明高階乘法的交換性

  但是 這個條件本質上是交換性的另一種表述方式
  考慮一階元素的兩種相乘方式 [>< 此處需要圖示]
  (p * q) 與 ((p rev) * (q rev))
  此時 eckmann-hilton 的條件就變成了
  ((a * b) rev) * ((c * d) rev) =
  ((a rev) * (c rev)) * ((b rev) * (d rev))
  即
  (b rev) * (a rev) * (d rev) * (c rev) =
  (a rev) * (c rev) * (b rev) * (d rev)
  另 c = d = 1 得
  b * a = a * b

  其實 正確的理解方式是
  兩種相乘方式之所以相等 (A x B) = (A o B)
  是因爲它只是就 a 與 b 的兩個不同的公共邊界來相乘
  所得到的將是對同一個幾何體的兩種邊界不同的表述
  這種不同的表示本質上代表相同的幾何體
  因爲兩種相乘方式是 '同位的'
  [比如 (p * p) 不等於 (p * (p rev)) 是因爲 用於相乘的公共邊界不是同位的]
  以這一階幾何體爲邊界的更高階幾何體 將表示這這一階幾何體之間的關係
  這些關係可以重載於這兩種不同的邊界表示方式之上

- k -
  可以看出
  數學語言之缺陷在於
  它總是被侷限在語法上
  而沒有考慮語法與語義之間的關係

### a critique of fibration in hott

- x -
  hott 對乘法的處理方式與 AT 的直覺相左
  並且與對 fiber-space 的對稱處理相衝突

  對稱的處理方式在於
  同一個幾何體的不同邊界表示 本質上還要被認爲是同一個幾何體
  比如 (m : ((p1 * p2) = q)) 與 (m : (p2 = ((p1 rev) * q)))
  而 hott 中處理它們的方式 講給幾何體加上多餘的信息
  [考慮 一個邊在面中漸進到另一個邊]
  這種多餘的信息 將使兩者不能視爲本質等同

  所得到的 tp 將使得
  一個 m 在同一個 fiber 中有兩種不同的像
  同一個 m 沿着不同的推進方式 將被映爲同一個 fiber 中的兩個面
  [在只考慮 globe 的情形下 是體現不出來的]

- x -
  [bridges 與 logic programming]
  在已有的形式處理中 不好的一點是 totel-space 是 fiber 的不交並
  因爲其不交 所以當要把一個 path lift 到 totel-space 中時
  f : ((x : A) -> x P)
  ~ : ((p : (x = y)) -> (x f = y f))
  而後面的這個 (x f = y f) 是不可能的
  即 兩個不同空間中的點沒有路

  已有的形式處理方式
  可以在 '不交並' 的前提下
  巧妙地給出這種 path 的定義

  而我想用 bridge 把 fiber 連起來
  這樣就不能說是 '不交並' 了

  可逆性可以用 logic language 來處理
  一個 path 引出兩個 fiber 之間的 等價
  可以試着使用一個 relation 來處理這個等價
  比如
  p(x, y) 用來 unify x, y 的同時 還能返回一個邊
  m(p1, p2, p3) 做 unify 的同時 也能返回一個面

## mimicing

### intro

- x -
  when developing n-groupoid theory,
  we can mimic the development of group theory.
  then what is the development of group theory?

- k -
  先看表面的現象
  羣論有一個乘法
  而其公理中有
  (1) 封閉性
  (2) 單位元與可逆性
  (3) 結合性

- x -
  (1) 封閉性
  我們考慮的是 生成子 加 關係 所定義的羣
  同樣 生成子 加 關係 也可以定義高維的代數結構
  此時封閉性是自然的
  (2) 單位元與可逆性
  單位元也是自然的
  可逆性需要考慮相乘的位置
  首先可以用 '相乘的位置'
  來解釋一般的羣論中的乘法與逆元
  爲不同位置的相乘
  然後再推廣到高維
  (3) 結合性
  相乘的結果最終
  與各種可能的相乘的方式無關

- k -
  羣論本身可以用來描述空間的對稱性
  高維的代數可以用來描述什麼呢?

- x -
  group 描述對稱性
  groupoid 可以看成是描述對稱性 但是加上了類型信息 和 類型檢查

- [關於正規性]
  從一般的高維乘法看來
  羣論之所以可以被如此簡化
  是因爲線段是規則的圖形
  正如 cube 和 simplex 是 規則的圖形一樣

- higher dim elements can be used
  to record the proofs of equality
  between elements of one dim lower.

- x -
  the computation of product of higher algebra,
  is enabled by two kinds of rules,
  (1) same-position-self-gluing
  (2) one-dim higher elements as relations

### generators and relations

- x -
  adding types to the generators,
  we get a combinatorial 1-groupoid theory.

### object group [subgroup structure] of 1-groupoid

- x -
  about tree
  ><><><

### 正規子羣與羣同態

- x -
  在 combinatorial group theory 中
  通過增加 relation 可以得到已有的 g.r.group 的正規子羣
  正規子羣 與 羣同態 之間 有一一對應關係
  對於 combinatorial infi-groupoid theory
  我們可以形成類似的理論嗎?

- k -
  這種現象稱爲 第一同態定理
  groupoid theory 並沒有這種現象
  把 groupoid theory 劃歸爲 group theory
  就能觀察到這個定理的缺失

### fundamental theorem of finitely generated abelian groups

- x -
  fundamental theorem of finitely generated abelian groups
  is just like the fundamental theorem of arithmetic.

## phenomena

### higher homotopy groups are always commutative

- x -
  by the definition of homotopy group I showed in AT1,
  can I prove higher homotopy groups are always commutative?

### 高維代數結構中的乘法

對於高維的乘法 (n ...)
既然乘法是 '可交換的'
爲什麼我們還需要相乘的序
因爲 序 給出指明乘合體中位置的方式

### interchange law

2 homotopy group (Cech 1932) -> two group structure ->
interchange law -> one group structure

2 homotopy groupoid -> two groupoid structure ->
interchange law -> more then one groupoid structure (more non abelian)

# intro

the simple idea is to study algebraic structures
by their generators and relations.

I will show how to design and implement a language
to formalize and machinalize
a little part of algebraic topology.

I call this prototype 'at1',
which is an abbreviation of 'algebraic-topology-1'.

[advice for reader]
If you know how to implement an interpreter,
try to imagine how you would implement this language
by directed graph processing.

# group

in combinatorial group theory,
generators is described by a list of generators,
and relations are described by equivalent relations
between two expressions formed by generators.

draw each generator as a edge.

draw relation as a face
whose boundary is attached to the circle
induced by the equivalent relation.

we get a space whose fundamental group is the group.

# groupoid

in combinatorial groupoid theory,
we still can use generators and relations to study the algebraic structure.

while, generators must be expressed by a graph,
[instead of a list of elements, in the case of group]
which includes the informations about the type of elements.

relations -- faces.

the groupoid is the fundamental groupoid of the space.

if a graph has no faces, thus no relations,
its fundamental groupoid is free generated groupoid of the graph.

examples
free groupoid
graph without faces
><><><

# higher algebraics structure

if the space have higher level elements,
it can generate a graded algebraic structure,
in which compositions are typed by boundary of elements.

we need a language to express
how to compose higher level elements together.

空間給出的高維代數結構
包含了空間的所有拓撲信息
[至少是同倫等價下的拓撲信息]
代數拓撲的方法就是去找這個高維代數結構的子結構
這些子結構可能容易計算一些
因此就有 '實用的' 分類空間[否定空間相等]的工具了
但是其實 這些子結構永遠都沒法包含原高維代數的所有信息

# 高階代數的表示論之語言的特點

1. 需要設計新的語法來描述階元的乘法
   '乘號' 本身應該被高階生成元的邊界結構化

   - 可以用語言學來論述 '不存在良好的高維幾何[代數]語言'
     比如 在 CL 中消去 lambda
     所謂 '消去' 只是轉變了編碼方式
     而不能從本質上簡化語言

   - 幾何體的分類問題可以簡化描述的複雜度

2. 相乘的條件是有公共邊界
   相乘後公共邊界被消除

   - thus 'boundary as type'
     which determines when and how
     two elements can be composed together.

3. 高一階元素是低一階元素之間的關係[等式]

4. 必須能描述一個元素的邊界的所有位置
   同一個元素就相同的位置自乘則相消

   - 描述粘合方式的語言必定是線性的
     線性的描述方式自然給出指明粘合體中所有位置的方法

# ><

- x -
  seifert–van kampen theorem
  太平凡了在 AT1 中
  這就是平凡的一句話
  高維情形也是如此

  在一個 groupoid 的表示中
  找一個支撐樹
  把它收縮成一點
  就得到了 fundamental group 的表示

  高維的情形可能是這樣的
  (1) 把空間化爲一種有特殊性質的標準空間
  (2) 在這個空間中尋找代數結構作爲不變量

# ><

- x -
  如何檢查一個組合是否爲 Sn?
  [因而可以成爲某個高階生成子的邊界]

# note 關於高階代數 與 knot 所引發的難題

之所以說 'not finitely describable'
可能是因爲 R3 是無界的

但是
即便考慮的是 knot complement in disk-3
並且考慮的是 cell-complex 而不是 simplicial complexes
描述三維空間的方式 與直接考慮 knot 相比 也是不經濟的

當考慮 羣 G 的 (underlying-space G) 的時候
這個 underlying-space 通常是沒法嵌入 Rn 的
但是 underlying-space 就是羣表示
而我們能夠用這個羣表示來研究空間的性質

但是對於 高階情形如何呢?
高階的空間 是什麼代數結構的 underlying-space 呢?
在研究 combinatorial group theory 的時候
代數方面的理論的重要意義是 它能超越人的直覺
所以這裏我們也要先發展一個高階的代數理論

連續映射就是代數結構之間的同態
但是這裏沒有代數結構的同態基本定理

對於 groupoid 就已經沒有同態基本定理了
但是 groupoid 可以劃歸到 group
那麼
我們是否能把 我們所假象的高階代數
劃歸到更簡單的有同態定理的高階代數呢
[是否有聯通性的時候就有 groupoid 的同態基本定理了?]

通過找一個節點的支撐樹
就能把 groupoid 轉化爲 group
那麼
高階的代數結構是否也能以類似的方式轉化到
更具有正規性的高階代數結構呢

同態基本定理說
假設映射是滿射 (-> A B)
被映射成單位元的元素稱爲 K [kernel]
那麼 (= B (/ A K)) (= A (* B K))
[其中 '=' '/' '*' 都是待定的]

如果考慮高階代數結構
這裏單位元應該被推廣成 [x refl]
把 [x refl] 換成 kernel 中的 階數高於 x 的元素
就是一個填充過程

除了 kernel 以外
滿射中的 多對一 部分 也要處理
[就是 covering-space of groupoid]
但是在 group 理論中並沒有這種現象

在 group 理論中
如果有局部上的重複
就一定有全局上的降階
但是在 groupoid 理論中
並沒有這種現象

groupoid 到 group
是通過統一邊的邊界完成的
邊的邊界是兩點
統一之後就是一點

二階圖形中 面的邊界構成了一個網 網有支撐樹 支撐樹過所有的點 支撐樹可以縮到一點
三階圖形中 體的邊界構成了一個蜂窩 蜂窩有支撐牆 支撐牆過所有的邊 支撐牆可以收縮到一點
但是這是錯誤的 torus 是沒有支撐牆的
[圓柱也沒有支撐牆 但是圓柱不是體的邊界 [不是有向閉曲面]]
[因此 尋找支撐樹的算法 不能用來尋找支撐牆]

因此 二階[高階]同倫羣並不能捕捉二階[高階]代數的所有性質
也就是說
邊的邊界可以統一到點
但是面的邊界不可以統一到點

那麼面的邊界是否可以統一到 一個別的[不要求其總是可收縮到點的]圖形呢?
比如一個圓 [S1]

所謂統一面邊界
是說
所有的面的邊界將構成很多圓的粘合
而我們是否能夠把這些很多圓統一到一個圓
統一的方式還是收縮

可能是可行的
因爲如果有兩個面的邊界圓
以非退化的方式相粘合了
那麼這兩個圓就可以化爲一個圓

><><><
接下來就是要找三維空間的例子
[當然是限制與 cell-complex]
看看對這些三維空間可否實行這種簡化
再看看簡化而得的空間是什麼樣的

><><><
knot 所形成的 三維空間 很難用我的語言給以描述
因爲就這種空間而言 我的語言所給出的額外信息太多了

# note 代數系統的分類

- x -
  高階 -- 低階
  需要完整的類型系統 -- 需要有限的類型系統 -- 不需要類型系統

- x -
  torus 上的需要完整的類型系統的二階代數結構
  不能轉化爲 不需要類型系統的二階代數結構 [即 二階同倫羣]

# note about free group

沒有二維以上元素的空間對應於自由羣
沒有三維以上元素的空間是否也能對應與自由的二階代數結構?

# note covering-space of surface

- x -
  對於一維空間的 covering-space
  在 covering-space 中的一條線 不用回到自身
  它在 base-space 中的像可能就已經回到自身了

  對於二維空間的 covering-space
  也是如此
  在 covering-space 中的一條線 不用回到自身
  它在 base-space 中的像可能就已經回到自身了

  但是二維空間中的 face 可能沒有回到自身
  [邊界沒有被映射到重複的 edge 上]
  但是它在 base-space 中的像 可能已經回到自身了

  對於一維空間有
  covering-space 的 fundamental-group 是
  base-space 的 fundamental-group 的 subgroup
  這是因爲在 covering-space 中回到自身的 path
  在 base-space 中可能回到自身很多次了

  但是這個現象如何推廣到二階呢?

- k -
  我們還可以發現
  對於一個 group G, which is defined by representation
  G 是 (underlying-space G) 的 fundamental-group
  注意
  (underlying-space G) 只有一個 base-point
  但是一個 space 不必只有一個 point
  我們也能求它的 fundamental-group
  covering-space 就是如此

- x -
  這才只是二維的 covering-space
  高維情形又是怎樣呢?

------

- x -
  一維空間的 covering-space
  其實就是一個帶有類型的代數結構的同態
  只有當有一種代數結構的去類型化[一種簡化]時
  這種同態才對應於子羣[子羣關係就是特殊的同態[即嵌入]]
  [如何說這是一種 galois 理論呢?]

  二維空間額 covering-space
  可否有類似的理論呢?

- x -
  注意
  G acting on (covering-space (underlying-space G))
  二階[高階]是否也可以這樣?

  注意
  上面的 acting 是 acting on all points
  可能二階的是 acting on all edges

  或者
  我們應該放棄這種 acting 的可能?

# note normalization of underlying-space

- x -
  underlying-space = g.r.representation of group
  thus
  normalization of underlying-space =
  normalization of g.r.representation of group

- k -
  but what is the initial groupoid to be normalized?
  ><><><

- x -
  the checking condition is more strong than that of cell-complex.
  ><><><

# note cayley-graph

cayley-graph [with faces] is the contractible.
G act on (cayley-graph G)
there is (: covering (-> (cayley-graph G) (underlying-space G)))

# about developing theories

- x -
  爲了發展 combinatorial theory of higher algebraic structure
  我們學習 combinatorial group theory
  學習他人對這個理論的展示方式
  學習這個理論的歷史
  學習如何發展一個理論

- x -
  看這個理論的所謂 '發展'
  感覺是一些愚蠢的人在以愚蠢的方式處理愚蠢的問題

# 什麼是代數拓撲

- x -
  如果說給拓撲空間一個代數結構就算是代數拓撲了
  那麼我們的高維代數結構已經算是一個完整的拓撲不變量了

- k -
  我想 重要的是
  (1) 能夠用可讀的語法構造各種拓撲空間
  (2) 能夠以相對更簡單的方式判斷代數結構的等價

- x -
  看來這兩個任務都是具有高度構造性的
  我們可以利用例子來檢驗我們的理論的良好性

# normalization

normalization = simplify with certain direction.

# 計算同倫羣

計算同倫羣的時候
(1) 先找生成子
(2) 再找生成子之間的關係
[證明 von kampen 定理的時候也是如此]

# normal form of higher algebraic structure

- x -
  可能很難找到 不帶有類型的高階代數的 類似 group 的 normal form
  但是可能能夠找到帶有類型的高階代數的 類似 groupoid 的 normal form

  但是就二維空間的分類而言 groupoid 的 normal form 是什麼?

- k -
  也就是說
  雖然能夠通過消去類型來簡化 groupoid 到 group
  從而分類二維流行
  但是我們想想別的簡化方式
  使得這種簡化方式能夠推廣到高維

# manifold 的條件

manifold 的條件
可以以代數的[組合的]方式表述
但是可能沒有判別算法

# 用 cell 來構造 complex

- x -
  可能 用 cell 來構造 complex
  才能很容易地賦 complex 以幾何

------

- x -
  又過着並非如此

# note

我們可以先在我們的語言中處理二階 complex 的經典定理
有關
- 流形的條件
- 單類型化
- 流形的生成子和關係的正規化
- covering-space 有關的定理
然後在試着處理三階 complex

# von kampen theory

- x -
  可以說這個定理是平凡的
  因爲
  ><><><

  也可以說它是不平凡的
  比如下面的例子
  ><><><

  但是這個定理是很有限的
  因爲它不能用來計算 circle 的 fundamental-group
  這在於
  ><><><

- k -
  如果說 groupoid 可以被接受爲代數不變量
  那麼 高階的代數結構也可以被接受
  但是 groupoid 與 group 的區別在於什麼呢?
  二者之間如何轉化?
  那些典型的空間的 fundamental-groupoid 都是什麼呢?

# postnikov system

postnikov, mikhail m. (1951).
"[determination of the homology groups of a space by means of the homotopy invariants]".
doklady akademii nauk sssr. 76: 359–362.

# knot

考慮 knot 的補空間的時候
可以想象成是 空間的所有 singularity 都擁擠到一個 circle 上了

# manifold-checker

- x -
  檢查二階流形的時候
  其一階骨架可能是任意的有向圖
  檢查三階流形的時候
  其二階骨架可能是任意的有限生成羣

- k -
  也可能不是任意的
  但是即使不是任意的
  其限制也非常少

# 爲什麼把幾何限制於流形

- x -
  爲什麼把幾何限制於流形
  而不是就一般的 complex 定義幾何

- k -
  可能是因爲測地線不唯一

- x -
  如果具有唯一性的測地線
  可以解釋經典力學中的現象
  那麼
  不唯一的測地線
  不正是能夠解釋量子力學中的現象嗎

# shelling

shell 這個術語的意思取自其 '剝落外殼' 的意思
在 2-dim manifold complex 的 normalization 中

one removesany simplex,
then tries to remove a sequence of free simplexes
until only one simplex is left.

用的是與之相反的意思 即 union
在 complex 的 manifold 條件下
通過 union 來把許多 polygon 化爲一個 polygon

所謂 shelling 之失敗 導致我們不能獲得 normalization
是不成立的

# homeomorphism vs. homotopy equivalent

如何在我們的語言中區分這兩種等價關係

# homotopy theory 的解釋

如果想要獲得高維的代數
就必須要考慮 homotopy
同時 homotopy 可能有 homomorphism 解釋
[在 complex 的 category 中]

# fundamental-polyhedron

the first step of normalization
gives us fundamental-polyhedron.

# geometrization conjecture

既然二階的時候
流形的正規化代表了對幾何的分類
那麼三階的時候
對幾何的分類也代表了代數結構的正規化

# 利用視覺來想象三維空間

- x -
  當使用 polyhedron schema 構造了一個三維空間的時候
  可以想象自己在其中運動
  同時也可以想象光在其中運動
  可以想象在這個空間中的視覺體驗
  而視覺所體驗到的就是這個空間中的幾何
  因爲光所走的是空間的 '測地線'

  同一道光可以沿着不同的路徑到達一點
  因此就會看見重影

  還可以想象聲音
  還可以想象熱量在空間中的擴散
  等等

- k -
  有趣的是
  儘管我們永遠不可能[或者說很難]
  在實際生活中經驗到這種空間
  [除非是在視頻[或者虛擬現實]遊戲中]
  但是我們能夠想象這種空間
  [否則我們也設計不出來這樣的視頻遊戲了]

- x -
  link and knot 与 fundamental-polyhedron 之间的关系
  可以通过把 link and knot 镶嵌在 fundamental-polyhedron 的面上来体会

  注意
  这些 link 与 knot
  是 fundamental-polyhedron 所形成的空间的真正边界

  其实
  没有必要把这些 边界理解为嵌入在面上
  可以还是把它们理解为边
  因为在面上嵌入边 可以理解为对 fundamental-polyhedron 的细分
  只要还是把这些边理解为边界就可以了

- k -
  镶嵌的条件是什么?

- x -
  我想
  条件就像 带有边界的 fundamental-polygon

  也许 只所以 knot 与 link 如此重要
  就是因为 fundamental-polyhedron 的所有二维边界都可以同伦成一维的线

  但是为什么要用 knot 来得到 hyperbolic-geometry?
  爲什麼不直接適用 fundamental-polyhedron?

- k -
  如果從激勵我們的工作的角度講
  我們可以說 這是因爲人們還不瞭解二階的代數

- x -
  我還體會到人們不僅不沒有合適的語言來表達這些具體數學
  人們還沒有合適的語言來表達他們在實踐中所積累的形而上的想法[方法]


------

- x -
  一個 fundamental-polyhedron 就決定了一個幾何
  但是帶有邊界的 [由 not knot 確定的] 流形
  其幾何可以不一樣
  因爲一個 knot 能夠對應多種 fundamental-polyhedron 嗎?

- k -
  但是 第一個例子裏考慮 平面除去一點的時候
  其 fundamental-domain 是確定的
  但是其 幾何不同

- x -
  其幾何真的不同嗎?
  不是都是 歐幾里得幾何嗎?
  只是 covering-space 不一樣而已吧
  對於找個 fundamental-domain 不論如何 cover
  都不能得到 simple-connected 的 universal-covering-space

------

- x -
  根據 Kneser 的分解定理 [prime 3-manifolds]
  connected-sum 可以用來正規化三維流形

  可定向的 n 流形在 connected-sum 下是
  交換的 以 Sn 爲單位元的 semigroup

  當用 connected-sum 來正規化二維流形的時候
  所對應的代數操作是什麼?

  分解定理是如何證明的?
  如此一來二階的代數結構的 'prime'
  類比於 有限單羣 嗎?
  類比於 groupoid 又是什麼分解呢?

  就三維流形而言
  分解的方式是找
  不能收縮的 S2
  [即 不 bound D3 的 S2]
  [即 essentially embedded S2]
  然後沿着找個 S2 切開

  但是別的可定向二維流形好像也可以用來做切割
  比如用 torus 的话 就能把 knot 切割出来了

  我能想象在 fundamental-polyhedron 中
  这些可以在边界[线]所構成的網絡上找

# not-knot

not-knot 是否被認爲是沒有邊界的?
因爲 三維有邊界的流形的邊界 應該是二維流形?
[對於 同倫理論 來說 退化的邊界也可以]

# 實心的 torus

實心的 torus = S3 - S1

# thurston 利用羣作用來給出幾何的定義

說穩定子是 ><><><
就一定有黎曼度量

# torus surgery 於代數處理

- x -
  可以先考慮在邊界上做文章
  還可以考慮可以用 dehn surgery 生成的簡單空間
  比如 lens-space

- k -
  但是
  如果有代數處理的話
  knot 的 link 所帶有的信息
  如何被體現於其中?

- x -
  thurston 如何從 knot 和 link 獲得三維幾何?
  我們可否模仿他的方法?

# thurston 給出了 (knot-and-link -> polyhedral-schema)

- x -
  但是
  爲什麼除去邊可以等價於除去點?
  ```scheme
  因爲在扭動之後
  在局部 knot 的部分
  就成了 (* X I) 中的
  形如 (: (* x0 i01) (* (-1 X) (0 i0 i1))) 的邊
  它們對應於點
  ```

- 一個複數 決定一個雙曲空間中的四面體

- x -
  可以看到 雙曲幾何的各種模型非常重要

  如果 四面體可以有如此良好的數值表達
  那麼多面體所形成的代數是否也能獲得類似的表達呢?

- 還有這些複數於 dehn surgery 之間的關係

- 以 hyperbolic-geometry 的 volume 爲拓撲不變量

# covering-space

- x -
  covering-space 被某些人當作羣
  並且 一個 polyhedron 按照其對稱性
  而平鋪空間
  被看作是 羣的生成子在生成羣

- k -
  看來這些關於各種空間的描述是非常重要
  非常緊急的

- x -
  但是我們必須先處理好二維的情況

# research program

- x -
  what is the core of our research program?

- k -
  高維代數 與 語言設計
  因爲當想要處理和描述高維代數的時候
  機器的幫助是必要的
  因此設計程序語言就是必要的

- x -
  在真正寫論文之前
  我還有很多東西需要看
  比如 dehn 和 whitehead 的論文
  還有 對 fiber-space 和 mapping-class-group 的良好理解

# limitation of language

as an art or a science been developed by people along the history,
new phenomena been observed,
then, new languages must be invented
to merely let people be able to talk about these new phenomena.

a new concept, such as combinatorial group theory and category theory,
been an indication of certain math structures,
meanwhile can also be an indication of a researching program.

a new researching program while seemingly only helping people
understanding the phenomena in a better viewpoint,
will inevitably masking certain other viewpoints.

when a new language is invented,
certain features of the phenomena are made speakable,
but we must do not forget the limitation of any languages,
as a single language, it will inevitably made certain other features
of the phenomena hard to be expressed.

when I am designing my new language here,
I hope to capture some phenomena we observed
when we think about continuum,
and I am motivated by realizing that
those phenomena are hard to be expressed
by any languages which have been invented by people.

but I am so aware of the limitation of any languages,
so, you will see me criticizing myself along the way,
I wish my readers do so too.

# orbifold

# fundamental-domain might not be polyhedron

以帶有洞的圓 與 其分形平鋪 爲例子

# oneness

do not view them as two fighting,
but view them as one changing.

summer is not fighting with winter,
the relation between sun and earth is changing.

when play weiqi with a friend,
do not go against him to win,
view it as one game playing and enjoy it.

when running a interpreter of a language,
the system is one, and the system is changing.

to allow the state of soul changing from one to another,
to allow the state of body changing from one to another.

# group action

- group acting on covering-space -> quotient-space as surface

- groupoid acting on covering-space -> quotient-space as the same surface
  but quotient-space must be expressed with type system.

- this type system is also what needed by 2-algebra.
  2-algebra acting on the path-space of the covering-space.

- we care about path-space,
  because it is about fundamental-group.

- but when we say
  "covering-space solves the word problem
  of the fundamental-group of the underlying-space"
  what do we mean?
  should we already use 2-algebra to express this?

- classically, we mean the following :

  each path of the underlying-space
  can be lifted to many paths in the covering-space.

  and if we fix a base point in the covering-space,
  each path of the underlying-space
  can be lifted to one path in the covering-space.

  a path returns to its initial point in the underlying-space,
  after lifted to the covering-space,
  will not returns to its initial point in the covering-space,
  if the path is not contractible to a point in the underlying-space.

  a path returns to its initial point in the underlying-space,
  after lifted to the covering-space,
  will returns to its initial point in the covering-space,
  if the path is contractible to a point in the underlying-space.

  thus,
  to see if a loop in the fundamental-group is equal to identity,
  we lift it to the covering-space,
  and see if it returns to its initial point.
  i.e. if its end point is equal to its initial point.

  thus,
  different elements in the fundamental-group,
  correspond to different points in the covering-space.
  and
  production of fundamental-group
  induces a production of points in the covering-space,
  i.e. gives the points in the covering-space a group structure.

  for example,
  elements of the fundamental-group of torus,
  correspond to integer points in the 2-dim plane.
  and
  production of the fundamental-group of torus,
  corresponds to vector addition.

  thus,
  we can say,
  different elements of the underlying-space are different,
  but sometimes it is hard to see they are different.
  after lifting them to the covering-space,
  it is easy to see they are different.

- different points of the covering-space
  might be the viewed as equal point in the quotient-space [underlying-space].
  but it is the relation between points of the covering-space
  and paths of the underlying-space, that is important.

- x -
  what is the meaning of our 2-algebra then?

# covering-space and normalization

- x -
  we can build the covering-space of a surface,
  after we normalize it.

  can we just build the covering-space,
  without normalizing it?

- k -
  根據古典定義
  covering-space 是 fiber-space 的特殊情況
  如何在我們的語言中形式化這些定義呢?

# group action

- x -
  只有當 covering-space 有 ><><>< 的性質時
  underlying-space 才能作用在上面

- "covering transformations"

- x -
  underlying-space 的邊
  (1) 可以作用於 covering-space 的點
  (2) 也可以作用於 covering-space 的邊

  underlying-space 的面
  (1) 可以作用於 covering-space 的邊
  (2) 也可以作用於 covering-space 的面

  我想 (1) 這種作用纔是 underlying-space
  對 covering-space 的作用

  整個的 quotient-space 要用
  covering-space 模去
  underlying-space 以 (1) 的方式對 covering-space 的作用
  來定義

- k -
  但是 quotient-space 應該說是一個 onto-map
  而 acting 只是理解找個 onto-map 的一種方式

# 1 2 3

1 平凡
2 简单
3 困难
这是在 manifold 中出现的情况
同时这也是在算数的级数中出现的情况
[需要借助素数才能进行分类]

# normalization of surface

## 目標

我們現在的目標是
用我們的形式化語言來描述 normalization of surface

## 回顧當前的語法和語義

首先我們來回顧一下當前的語法和語義

## 形成形式化的表達

- x -
  函數可以被理解爲證明
  當強調 函數 時
  想的是其可用於轉寫
  當強調 證明 時
  想的是其可用於記錄

  我们现在来讨论如何形成形式化的表达

  在形成形式化的表达的时候
  用于表达 normalization 过程的
  是一个函数

  而这个函数的参数是空间本身
  即 two-dimensional manifold
  这是 complex 的子类型
  [难点 1] 我们可能需要子类型的概念了

  其输出的是一个 normal two-dimensional manifold
  还有一个证明 证明记录了 两个 manifold 之间的等价

- k -
  我们可以把 [难点 1] 分解
  我们可以说 输入给 two-dimensional-manifold-normalizer 的
  不单单是一个 complex
  还有一个 找个 complex 是 two-dimensional-manifold 的证明
  找个证明本身所包含的信息也是处理过程中可以使用的

- x -
  假设这后一个命题是
  [:x is-two-dimensional-manifold]
  这个命题是否有归纳的定义呢?

  我想这就涉及到可证明理论了

  但是
  我们是否要把这个命题归纳定义为类型呢?
  我们可否写一个谓词来检验其性质呢?

- k -
  归纳的定义可能是不可能的
  而检验 是简单的
  检验的过程本身是否就是生成证明的过程呢?
  谓词可否被视为一个 证明生成器?

- x -
  设想 一个数是偶数 这个命题 [这个类型]
  它显然是有归纳的定义的
  ```scheme
  (define is-even
    (type (-> nat type)
      (: zero-is-even [zero is-even])
      (: even-add-two-is-even (-> [:n is-even] [:n two add is-even]))))
  ```
  此时
  想要证明 six is-even 是很繁琐的
  ```scheme
  (lambda [six is-even]
    [zero-is-even
     even-add-two-is-even
     even-add-two-is-even
     even-add-two-is-even])
  ```

- k -
  此时是否应该使用谓词呢?
  或者应该写一个证明生成子

  证明生成子 在检验一个数是否 is-even 的同时
  还生成一个证明?

  如果如此
  那么不就也是允许谓词了吗?
  只是谓词都必须以证明生成子这种很强的形式出现

- x -
  先不管这些
  我们从简单的开始
  谓词比证明生成子简单
  谓词可能很容易被改进为一个证明生成子

  假设我们允许使用谓词
  那么 应该如何实现 manifold-checker?

## manifold-checker

- x -
  先看这个函数的类型
  (-> complex bool)

- k -
  我们关心一个 complex 是否是 manifold
  因为 我们用的等价关系是 同胚 而不是 同伦
  但是 我们目前所使用的证明空间等价的方式
  是 同伦
  我们需要明白我们所使用的等价关系

- x -
  在一个形式化的语言中 应该如何区分同伦与同胚?
  我想第一个问题应该是 我们是否应该考虑同胚而不考虑同伦
  毕竟 就这个二维空间的分类定理而言
  对同伦进行叙述 也是有效的

## 動態

如何描述 normalization 的動態效果呢?
一系列的同倫等價可否被演示成動態效果?

## ><

```scheme
(define two-dimensional-manifold-normalizer
  (lambda (-> [complex %:x :x is-two-dimensional-manifold]
              (complex %:y :y is-two-dimensional-manifold :y is-normalized))
    ...))

side-cancellation
unify-vertex
cross-cap-normalization
handle-normalization
handle-to-cross-cap
```

# fiber-space

- x -
  fiber-space 的定義
  E -> B
  可以被轉化爲
  B -> (F -> F)

  把變換的信息記錄在點中
  是切開再粘合

  把變換的信息記錄在邊中
  是 ><><>< [是 切開再粘合 的對偶]

# 联系于古典理论

如果想要给 at1 以古典的拓扑学的理论基础
就需要给这些空间以实现
即把它们嵌入高维欧氏空间
然后证明同伦等价对于二者来说是一致的

# 空间之间的同伦等价 与 高维代数结构

- x -
  空间之间的同伦等价 对于 高维代数结构 而言
  由什么意义?
  既然它已经远离代数结构的同构了

- k -
  也许这在于
  n 阶代数结构 只需要 n+1 阶代数结构作为其证明

- x -
  那么 如何理解 二阶图形的三阶同伦群是非平凡的?

- k -
  首先需要证明的是 S3 可以以本质地嵌入 S2
  这在我们的形式语言中也并不是不可能的

- x -
  但是 如果说
  只有找到 S3 的一个等价的 fiber-space 表示
  然后我们才能表达 S3 到 S2 的本质嵌入

  既然这个嵌入 依赖于等价的图形的 不同表示方式
  那不是有悖于基本等价关系吗?

# 關於細分

- x -
  細分顯然形成同倫
  並且 其實所形成的是同胚
  但是在形式語言中
  爲了證明一個細分是同倫
  需要補充很多信息
  這是否說明 在形式語言中定義同倫的方式有問題呢?
  或者只是說明我們 要在給出細分的時候 自動生成同倫
  哪種看待問題的方式是正確的?

  - 形式語言中 函數 (: f (-> A B))
    可以被看作是 A 細分 B 的條件是什麼?

- k -
  前一種看待問題的方式要求我們尋找更好的空間同倫的定義
  更好的定義可能根本是不存在的
  並且要知道
  前一種方式並不能包含所有的同倫等價
  在 whitehead 所發展的 simple homotopy theory 中
  就可以明白這一點

# decomposition

different decompositions of a function
are different decompositions of an element in the mapping-space,
just like an element in a [simple] space
can have different decompositions.

# manifold 的條件 與 normalization

如果只有在 manifold 的條件下才能做 normalization
那麼對這些條件的否定 就將阻礙 normalization
我們需要在具體的例子中觀察這種阻礙

# 連續性 與 鄰域

- x -
  對於 complex 中的點 我們可以給出 neighborhood 的概念
  是否可以用 這種 neighborhood 的概念定義連續性?
  是否需要 subdivision 的幫助?
  [subdivision 意味着 潛在的點集拓撲]

  就邊界而定義的連續性 與 就 neighborhood 定義的連續性
  之間有什麼關係?
  二者是否等價 或者 其中一個比另一個強?

  [猜想] 這兩個定義是等價的

- k -
  這意味着我們不應該過分排斥古典的拓撲公理
  而應該把它們融入到我們的語言中來

- x -
  當考慮這些問題的時候
  我發現我幾乎會排斥所有不能融入到我們語言中來的概念

- k -
  這就是波利亞所說的獵人的直覺嗎?

- x -
  說 complex 作爲 manifold
  確實有無窮多個點
  但是其中的點是潛在無窮多的
  neighborhood 剛好給我們了一種方法
  能夠讓我們描述 manifold 中的很多潛在的點

  同時
  除了 neighborhood 這種方法之外可能還有別的方法
  比如取中點等等

  如此看來
  complex 就像是 manifold 的離散座標系一樣

- k -
  但是這種座標系是 拓撲的 而不是幾何的

- x -
  但是也許有了拓撲座標系之後
  我們距離給 complex 附以幾何就更進了一步

------

- x -
  用這種 neighborhood 概念來敘述連續性時
  我們不必說對於某一點的任意 neighborhood 如何如何
  而只用說 對於某一點的 哪個 neighborhood 如何如何

- k -
  甚至從代數角度看 取 neighborhood 也是有意義的
  因爲 對於 polygon 而言
  我們將給這個 manifold 增加兩條邊
  在計數 n-gon 的 manifold 個數時 可能可以用到這個操作

------

- x -
  點集 topology 中
  對連續函數的定義方式將決定
  如何定義空間之間的等價
  [等價是用連續雙射證明的[或者說 定義的]]

  所研究的 topology 性質
  必須在所定義的等價關係下不變

  當[在點集 topology 中]用 neighborhood 來定義連續函數時
  利用 neighborhood 可以 '生成' 很多
  顯然在這個等價關係下不變的性質

  也就是說
  在定義了一種等價關係之後
  我們可以先觀察 用來定義本身能夠生成那些在等價關係下不變的性質
  [這想到一個性質 再證明這個性質在等價關係下不變 剛好相反]

# polygon -> circle

- x -
  當考慮 polygon 定義 manifold 時
  增加邊的個數 使得 polygon 變成了 circle
  此時所獲得的代數和幾何是什麼樣的?

  首先可以看出我們確實能有一個代數結構

- k -
  一個有無窮多虧格的曲面?
  這個曲面是否有古典意義上的拓撲結構呢?
  如何描述這個曲面中的點呢?
  用離散的座標系?

- x -
  我覺得
  離散的座標系 幾乎於實數的進位制表示相同了
  在進位制下 實數集本身也可以想成是潛在無窮的集合

- k -
  在潛在無窮下 之所以要給出實在無窮
  是爲了要給出 以極限定義的無理數

- x -
  不
  應該說是
  是爲了要 一次 給出 所有 以極限定義的無理數
  我認爲這是沒有必要的

# manifold 區別於一般的 complex

manifold 區別於一般的 complex 在於
space looks [from a point] the same in all directions.

manifold 的條件是如何保證這種直觀效果的?

# different kinds of complex

- x -
  simplicial-complex 與 polyhedron-complex 是等價的
  simplicial-complex 是特殊的 polyhedron-complex
  polyhedron-complex 可以被細分爲 simplicial-complex

  simplicial-complex 的古典定義要求 可以用點來確定面[或更高階單形]
  因此 不同的面不能有相同的邊界
  這種限制的效果是 [以三維爲例子]
  避免了對面和邊的命名 只要實用點就可以了
  但是需要充分細分才能保證這個性質
  因此與 使用命名 的方法相比
  這種 不用命名 的方法 所需要的細分可能更細

  cell-complex 與上面二者之間的區別在於
  定義 sphere 的時候
  不用引入邊
  而可以直接將 2-cell 的 boundary circle attach 到點上

  在考慮 homotopy 中的維數變化時
  cell-complex 可能是必要的
  但是這也引發了很多困難

  如何處理這些困難 我還不確定
  [可能需要好好讀 j-h-c-whitehead]

- k -
  如果用極簡的 cell-complex 來構造 S3 與 S2
  那麼 S3 到 S2 的 hopf-map 就是難以想象的
  可能這種構造下 根本沒法表達 hopf-map

- x -
  根據 hauptvermutung
  我們可以用 common-subdivision 來定義低維流形之間的同胚
  所以
  也許我們根本沒有必要用現在在語言中所使用的同倫等價

- k -
  但是要知道 simple homotopy theory 與 homotopy theory 不同
  我們必須明白二者的差異

# 關於 universal-covering-space

- x -
  sphere 的 universal-covering-space 是 sphere 本身
  而 sphere 還帶有一個二階的洞
  [二階同倫羣不是平凡的]

  - 爲什麼我們只要求 universal-covering-space 的一階同倫羣是平凡的
    而不要求其他?

  我們只考慮了 可定向的閉曲面
  而沒有考慮 不可定向的閉曲面 和 sphere

- k -
  我想一個 universal-covering-space
  的各階 homotopy group 都應該是平凡的

- x -
  universal-covering-space 到底如何定義?
  在 universal-covering-space 中
  underlying-space 中的每一點是不是都以同樣的方式被 cover?

- k -
  covering-space 的條件是
  每一點的被 lift 之後
  其鄰域要相互同胚
  正是這個條件使得我們不能用不帶有二階洞的曲面
  來 cover sphere

- x -
  我們可不可以忽略這個要求
  即 使用 branching-cover
  以消除二階洞
  這會影響 cover 的什麼性質嗎?
  爲什麼大家都不考慮 branching-cover?

- k -
  我們可以看一下 cover 是如何被使用的
  然後希望這種 cover 不會影響 cover 的使用

  還可以看看黎曼幾何中 branching-cover 是如何使用的

  既然我們的目的是消除各個階的洞

# branching-cover and riemann-surface

## intro

- x -
  我發現我們需要 branching-cover
  才有可能將 universal-covering-space 的概念推廣到高維

- k -
  branching-cover 在 riemann-surface 理論中是如何使用的?

## neighborhood and continuity

在形式語言中
我們可以用 neighborhood 這個構造
來判斷 branching 點 或 branching 線

## note

- x -
  當用 polygonal schema 構造曲面時
  這種構造就是曲面基本羣的展示[presentation]

  如此由曲面引出的羣的元素個數是無窮多的

  當獲得拓撲圖形的代數不變量之後
  我們還要發展理論以處理所得到的代數結構

  - 與其說是用代數來研究拓撲學
    不如說 只是利用了代數的語言而已
    沒有這種語言
    就很難精確表達拓撲學中的現象

  此處代數結構與曲面之間的聯繫在於
  group 的 cayley-graph 就是 曲面的 covering-space

  羣中的元素是 surface 中的 path
  path 可以看成是 一階的 未經交換化的 chain
  也可以看成是 I 嵌入在 surface 中

  [嘗試類比]
  二階代數中的元素是 3-manifold 中的 cloth
  cloth 可以看成是 二階的 未經交換化的 chain
  也可以看成是 (* I I) [或者 2-disk] 嵌入在 3-manifold 中

  cayley-graph 與 covering-space
  是否也能在二階代數中得到類比?
  就二階代數結構 是否可以構造 'cayley-graph'

  首先 3-manifold 的 universal-covering-space 是可以定義的
  [可以嗎?]

  對於 group 而言
  兩個元素是否相等
  只要看其在 universal-covering-space 中的 '邊界' 是否相等就可以了
  此時的邊界是 path 的起點和終點

  同樣
  對於高階代數而言
  兩個元素是否相等
  只要看其在 universal-covering-space 中的 '邊界' 是否相等就可以了
  此時的邊界是 disk 的邊界 circle
  [universal-covering-space 中 disk 的邊界 circle 不是 knot]

  考慮一般拓撲學的話
  可以說 X [二階空間] 的 universal-covering-space 就是 X 的 path-space
  但是這裏所看到的 X [三階空間] 的 universal-covering-space
  好像是 X 的 disk-space

  [問題]
  path-space 與 disk-space 這兩個空間是等價的嗎?

- k -
  二者的關係是什麼?
  二者都是函數空間
  前者是 (-> I X) 後者是 (-> (* I I) X)
  因此後者也是 (-> I (-> I X))
  這兩個空間顯然不是等價的

  說 path-space 就是 universal-covering-space
  是不是只是對曲面而言的?

- x -
  曲面中的 path 對應與 universal-covering-space 中的兩個點
  曲面中的 loop 是特殊的 path
  因此 它也對應於 universal-covering-space 中的兩個點
  當這兩個點重合的時候
  這個 loop 就可以在曲面中被收縮到一點

  類似的
  3-manifold 中的 cloth 對應與 universal-covering-space 中的一個 circle
  曲面中的 sphere 是特殊的 cloth
  因此 它也對應於 universal-covering-space 中的一個 circle
  當這個 circle 有重合的邊時
  這個 sphere 就是特殊的
  - 儘管 這個 circle 所界的 disk 不能有自交
    但是這個 circle 作爲其邊界卻可以有重合的邊
  當這個 circle 的所有邊退化並重合於一點時
  這個 sphere 就可以在 3-manifold 中被收縮到一點

  如上的是我的猜想

  - 這個猜想是錯誤的
    考慮 (* S1 I) 的 universal-covering-space
    我所想象的這個 covering-space 是不是 universal?
    如果是的話
    原來空間中不能收縮到一點的 sphere
    在 covering-space 中還是 sphere
    並且還是不能收縮到一點

- k -
  我們要發展理論與技術來證明這個猜想

- x -
  我可以提出一個問題
  首先我們知道
  想要構造曲面的 universal-covering-space
  人們先對曲面進行 normalization

  [問題 1]
  可否對 3-manifold 進行 normalization?

  [問題 2]
  可否在不進行 normalization 的條件下
  構造 [曲面 和 3-manifold 的] universal-covering-space?

  [問題 3]
  simplicial-complex 所提供的視角
  是不是不利於構造 universal-covering-space?

------

- x -
  group 的 universal-covering-space
  groupoid 的 universal-covering-space
  高階代數 的 universal-covering-space
