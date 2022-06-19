---
title: Homotopy Type Theory
subtitle: Univalent Foundations of Mathematics
author: The Univalent Foundations Program
---

# Before Notes

My research problem is to design a formalized foundation of mathematics,
to manipulate theorems and proofs like polynomial equations.

The Aim of my study of HoTT is to learn about
how to extend Martin Löf's basic type theory in general.

# introduction

## type theory

- [根據 bishop] 要定義一個集合
  首先 要說明如何構造這個集合中的元素
  其次 要說明如何判斷這個集合中的兩個元素相等
- lambda calculus
- 則式演算
- 棧 與 函數複合
- 函數 以 類型 爲元數據

## homotopy theory

- 首先
  考慮空間之間的一些連續映射
  f : (X -> Y)
  g : (X -> Y)
  可以被看成是 空間 Y 被 X 參數化
  同樣的 X 當 f 和 g 分別跑遍 定義域 時
  在 Y 中 所得到的像不同
  而 映射的像 通常就是 Y 中幾何體的表示
  這樣 f 和 g 就可以看成是對 Y 中兩個不同幾何體的表示了

- 空間的連續映射 之間的 同倫[homotopy]
  就是 連續映射 h : (X [0, 1] -> Y)
  滿足
  x 0 h = x f
  x 1 h = x g
  h 稱作是 x 到 y 的連續形變[continuous deformation]

- 其次
  考慮 同倫等價[homotopy equivalent] 的概念
  兩個空間 X Y
  若有
  a : (X -> Y)
  b : (Y -> X)
  滿足
  a b 和 X id 之間有 homotopy
  b a 和 Y id 之間有 homotopy
  稱 X Y 是 同倫等價 的
  即 兩個空間相互參數化之後
  得到的兩個參數映射之兩種複合
  可以變形到兩個空間的恆同函數

- 同倫等價的空間 具有相同的 代數不變量[algebraic invariant]
  比如 同調[homology]
  和 基本羣[fundamental group]
  等價關係給出了分類方式
  稱此分類爲 同倫類型(同倫類)(同倫型)(homotopy type)

## homotopy type theory

- 我們需要使用 type 這個術語
  然而已經有了術語 homotopy type
  古典意義下 它指 同倫等價的空間的等價類
  但是
  構造主義 不用 等價類 這個技術
  而是使用 等價類的編碼
  我們的 type
  其實是對 同倫等價的空間所形成的等價類 的編碼
  可以用 等價類的代表元素[此處是代表空間] 來理解這類編碼
  所以當說 a 是 [homotopy] type A 的元素的時候
  所說的是 a 同倫等價類的 代表空間 A 中的點
  而不是 等價類 A 之中的一個代表空間 a

- 前面說過
  首先 需要考慮兩個空間之間的各個連續映射
  其次 需要利用 連續映射 和 連續形變 來定義空間的 同倫等價
  type 就可以被看成是 對 同倫等價 的空間的等價類的代表空間 的編碼
  a : A 是說 a 是代表空間 A 中的點
  那麼
  反過來
  連續映射 和 連續形變 的意義是什麼呢
  首先 連續映射 的概念也變了
  f : A -> B 是模掉 連續形變 之後的 連續映射
  即 對連續映射的同倫等價類的編碼
  而 連續形變 就內蘊在 這種編碼中了
  可以發現
  連續形變所帶來的幾何體的差異
  已經被我們的編碼完全抽象掉了

- type = space [with homotopy equivalent] = higher groupoid

- type constructor = space constructor [respect homotopy equivalent]

- 只考慮同倫性質
  而不考慮拓撲性質
  比如 不考慮 開集 與 點列的極線點

- 比如
  每個類型 A
  都有 判斷這個類型的兩個元素 a b 相等 的方式
  這樣一個對相等的證明 p : a == b 就是 A 中的一條 path
  或者記爲 p : a =A= b
  或者記爲 p : a b A id
  而 這裏的 A id 就是 A 中所有 path 所組成的空間

## univalence axiom

- 對於 所有[小]空間的空間 U 這個集合 [常稱爲 universe]
  其元素 A B 爲[小]空間
  我們指定其等詞爲 A B 之間存在雙射
  這就是 univalence axiom

- p : A B U id 即 在 U 中 A B 之間有路 p
  即 p : A =U= B
  定義爲
  A B 之間存在雙射

- 其合理性可考慮如下
  觀察古典的 同倫等價[homotopy equivalent] 這個概念
  在類型論中的變化
  兩個空間 X Y 同倫等價 當爲判定二者相等之等詞
  即有
  a : (X -> Y)
  b : (Y -> X)
  因爲 a b 只是對 連續函數的編碼
  所以 此時 在編碼意義上 同倫等價的條件就成了
  a b 和 X id 相等
  b a 和 Y id 相等
  X Y 同倫等價的條件 就被轉化爲了雙射存在

- 在編程的時候
  ua [denotes univalence axiom]
  可以把 A B 之間的雙射 bi : A B exist-bijection?
  轉化成 兩個空間 A B 相等的證明 f ua : A B U id
  而 transport 可以把這樣一個證明轉化爲
  相等元素在 任何語境下的代換
  f ua context transport : A context -> B context

- 引入此公理的動機
  是爲了在形式體系的規範下
  證明 homotopy theory 中的定理時
  能夠 更簡練 更接近非形式數學

## 記 bishop 如是說

- 要定義一個集合
  首先 要說明如何構造這個集合中的元素
  其次 要說明如何判斷這個集合中的兩個元素相等

- 看來 hott 就是 bishop 之誡 的實踐
  univalence axiom 只不過是
  對於某個特殊的集合 強調 bishop 之誡 而已

- 那麼
  在設計程序語言的時候
  我們先考慮如何把 bishop bishop 之誡
  實現爲語言的核心性狀
  也就是說
  每個類型都要配以基本等詞
  而 type 這個類型的等詞就是[小]類型之間的雙射
  注意
  保持等詞的類型之間的操作纔稱函數

- 要把 quotient type 做爲核心性狀
  可以通過改變 一個類型的基本等詞
  來構造新的類型

## 記 hott 計算模型的缺失

- hott 之缺失計算模型
  就像是 bishop 的 constructive analysis 之缺失計算模型

- univalence axiom 捕捉了能夠在有限步驟內能夠完成的所有計算
  但是 某些計算並沒有高效的算法實現
  正如 bishop 聲明自己的 constructive analysis
  考慮的是所有有限步驟內能夠完成的計算的一般理論
  而暫時忽略效率問題

- 我想
  可以像在 constructivism 中論證 中值定理 缺乏構造性 一般
  我也可以構建一個計算模型
  來用反證法論證
  univalence axiom
  還有 bishop 的實數
  都 缺乏有效可計算性
  我可以證明
  如果 univalence axiom 和 constructive analysis
  具有可以被估計的時間複雜度
  那麼
  關於時間複雜的的很多疑難問題 就都將被解決了

## higher inductive type

這裏指的是如下對幾何體的定義，
類似 CW complexes 中幾何體的生成法。

```
datatype 1-sphere {
  base: 1-sphere

  loop: base == base

  loop: Equal(1-sphere, base, base)
}

datatype 2-sphere {
  base: 2-sphere

  2-loop: (base == base) == (base == base)

  2-loop: Equal(
    Type, // This might be wrong.
    Equal(2-sphere, base, base),
    Equal(2-sphere, base, base),
  )
}

datatype 2-torus {
  base: 2-torus

  warp: (base == base)
  weft: (base == base)

  2-loop: (weft warp == warp weft)

  warp: Equal(2-torus, base, base)
  weft: Equal(2-torus, base, base)

  2-loop: Equal(
    2-torus,
    // Suppose we have a syntax for composing path.
    path { weft, warp },
    path { warp, weft },
  )
}
```

## 記 算數的分級

注意
natural 就是以 inductive type 的方式定義的

```
natural
  base: natural
  succ: natural -> natural
```

而且
由 natural 擴展而來的 integer 與 1-sphere 等價
所以說 算數的分級 也許可以在 這種遞歸定義中考慮
也就是說 這種遞歸定義 給出有趣的代數結構
也許 可以用以給出新的 '算數' 從而用以研究 算數的分級
而 他們給出的幾何直覺 可以幫助我們理解 算數的分級

## sets in univalent foundations

野心是 代替 集合論 做爲當前數學基礎的地位

但是
想要恢復集合論 就要用到 完全離散的空間
加上空間 A 的離散性的證明之後
a : A 就可以被解釋爲 a 屬於集合 A 了

## informal type theory

語言設計上 想要貼近傳統的數學語言

## constructivity

proof relevance
according to which
mathematical statements, and even their proofs
become first-class mathematical objects

# 1 type theory

## type theory versus set theory

- 一個集合必須有等詞
  聲明兩個元素相等時 必須給出證明
  對兩個元素相等 可以有很多證明
  就像是兩個點之間可以有多條路
  對兩元素相等的證明 就是對路的構造
- 等詞的分類
  propositionally equal
  judgmental equality [definitional equality]
  後者其實是說 有基本等詞
  這種基本等詞應該消除
  所有的等詞之間的關係都是相對的
  基本等詞的產生
  是因爲有基本數據結構 在 agda 中 即 tree of symbol
- definitional equality
  爲了解決全局命名所帶來的
  不同名字所構造的結構之間的相等
  全局命名只是在給所構造出來的結構貼標籤做引用而已

## function types

- 在我的類型系統中 只有一種類型
  即 則式
  (... -> ...)
  這種語法更方便函數複合
  但是
  在古典的類型系統中
  卻可以把 各種邏輯連詞做爲類型構造子引入
  其實 在蟬語中
  則式 也可以被類型構造子引入
  只不過 所有的語法都是圍繞 則式 優化的
  所不容易發現這一點
  就像是
  面向對象的語言 用 associate list 來實現對象時
  所有的 語法都是圍繞 關於 associate list 的操作 而優化的
  所以 看不出 associate list 了 一樣
- 可以看出 definitional equality
  就是 agda 所選取的相當任意的一個基本等詞
  爲所有 term 所共用
- function type
  - formation
    (... -> ...)
    (A -> B)
  - introduction [constructors]
    []
    [(A -> B) ...]
    or
    pattern matching
  - elimination [eliminators]
    apply
    (A (A -> B) -> B)
  - computation [reduction]
    a [(A -> B) ...] apply
  - uniqueness principle [expansion]
    f
    [f]

## 記 如何定義集合

- 我之前說
  定義一個新集合的時候
  需要指定如何判定其中二元素相等
  而引入類型的方式有二
  一是
  先構造然後再命名
  二是
  給出名字之後做遞歸定義
  遞歸定義 就是指明這個數據類型的 構造子[指定其接口函數]
  那麼
  這兩種方式之所以能用以引入新的集合
  就是因爲它們滿足定義集合的兩個條件
  如何滿足呢
  說 則式 是特殊的類型構造子 又如何如此呢
- 先構造然後再命名
  就涉及到類型構造子[返回類型的函數]
  既然這個函數代表一族類型
  那麼它也要給出一族等詞
  比如 有函數的類型爲 (set set -> set)
  它所構造的集合的等詞
  就一定是用它的兩個參數集合的等詞構造的
- 遞歸定義又如何呢
  我可以讓觀察具體一點
  遞歸定義 給出了構造屬於這個類型的元素的方式
  [或者說 規定了這個類型的元素的表達式的語法]
  也就是給出一個遞歸定義的謂詞
  來判斷 一般表達式是否表達這個類型的元素
  那麼
  等詞也一樣是這樣一種遞歸函數
- 我覺得最重要等詞相關的概念被忽略了
  或者說被以隱含方式處理了
  設想一下如果要給語言加入 商類型 這種新的類型構造子的話 將如何
  根本沒法良好定義這個新的引入類型的方式
  因爲每次定義類定的時候 [比如 使用歸納定義的時候]
  給出集合的等詞的方式太平凡了
  就是 使用基本的數據結構的等詞而已
  對於 商類型 來說 這是不充分的
  比如
  quotient (? set -> set)
  如果這樣聲明類型發現根本就沒法定義 quotient 的函數體
  ```cicada
  set
  ({(term -> bool) #predicate}
   {(term #term1 term #term2
     {:term1 :predicate apply true?}
     {:term2 :predicate apply true?} -> bool) #equality}
   -> :predicate :equality)
  ```
  如若如此
  quotient (predicate equality quotient-function -> predicate equality)
  看起來也不是很正確
  因爲
  在現有的語言中 實現 set 的方式都不是如此
  而且
  {} 成了一個對真假的判斷
  這正是基本原理所反對的
  如果 用兩個 term 空間的函數來定義 set
  那麼 類型構造子[返回類型的函數]
  就成了返回函數的函數
  這正是我的基本原理所反對的
- 我發現 要求給出等詞 可能太強了
  比如 在 lambda term 的空間中
  考慮 由 reduction 所生成的等價關係所義的等詞
  有了等詞之後
  相當於是給出了一個判別任何連個元素是否相等的算法
  也就是自動生成證明的算法
  所生成的證明 其實就是把計算過程記錄下來
  這個計算過程代表了如何從一點走到另一點
  因此就是一條路
- 定義等詞的不應該是一個返回布爾值的謂詞
  而應該是 一個對返回類型的函數的遞歸定義
  每個這種遞歸定義都能生成一個判別函數
  來判別 term 是否滿足這個遞歸定義
  滿足這個遞歸定義的 term 就是對相等的證明
  也就是說
  我們並沒有給出 對兩個[某類]元素是否相等的判別
  我們給出的是 對一個證明 是否是 對相等的證明的判別
  這樣要求就弱多了
  [所有的謂詞 都以這種方式處理]
  cicada:equal? (cicada cicada -> proposition)
- 但是 此時 cicada:equal? 又是一個類型構造子了
  [因爲它是一個返回類型的函數]
  如果要求所有的集合都帶有等詞
  我們又需要給出這個高階集合的等詞
  那將是

  ```cicada
  cicada:equal2?
  ({cicada #cicada1 #cicada2}
   :cicada1 :cicada2 cicada:equal?
   :cicada1 :cicada2 cicada:equal? -> proposition)

  cicada:equal3?
  ({cicada #cicada1 #cicada2}
   {:cicada1 :cicada2 cicada:equal? #cicada11 #cicada12}
   :cicada11 :cicada12 cicada:equal2?
   :cicada11 :cicada12 cicada:equal2? -> proposition)
  ```

  這是沒完沒了的
  也就是說
  如果想要把等詞處理爲 path 的集合
  而又要求 對每個集合都要給以等詞
  那麼對等詞的定義將是沒完沒了的

- 也就是說 連個基本原理之間發生衝突了
  其一是
  應該把所有的謂詞都實現爲返回類型[命題][集合]的函數
  而不是返回真假值的函數
  其二是
  定義每個集合的時候都要給以等詞
  其矛盾在於
  等詞是謂詞
  如果把等詞實現爲返回集合的函數
  那麼又要定義新的等詞了
- 如何調和這兩個基本原理之間的矛盾
  只有一種方法
  那就是 在定義了第一個層次的等詞之後
  其他層次的等詞 應該做爲一個潛在無窮的集合
  而被自動生成
  也就是說要給出生成這個潛在無窮集合的方式
  也許有不同的方式呢
  對於 lambda term 的空間來說 確實如此
  但是 定義高階路徑的時候 有多種方式
  也許我們每次給出等詞時都要給出這個潛在無窮等詞列
  但是 通常只有第一項是非平凡的
  所以 當之給出這個無窮列的前幾項[比如 第一項]時
  就假設其其他項是由前幾項 以默認方式生成的
- 也就是說
  這兩個看似矛盾的基本原則合在一起
  使得我們在定義集合時
  要聲明的東西更多
  所聲明的信息 甚至可以是潛在無窮多
- 注意
  還有一種解決矛盾的方式
  就是發現基本原則之一是錯誤的
  在 bishop 的基本原則下
  等詞是唯一特殊的謂詞
  其他的謂詞都不必如此
  正是這種特殊性導致了衝突
  我想
  可以通過消除這種特殊性來化解衝突
  可以稍稍改變一下 bishop 的原則
  定義一個集合時
  需要指明構造這個集合的元素的方式
  還需要至少給出一個基本謂詞
  給出基本謂詞的方式是
  給出一個涉及所定義的集合的返回類型[集合]的函數
  這樣
  關於函數的定義也要調整
  函數[證明]是一個能夠在有限步驟內完成的操作
  函數所保持的可以不是等詞
  而是那個集合的基本謂詞
- 注意
  典型的難以定義等詞的集合就是 函數的集合
  p : f1 f2 (A -> A) id
  p 是一個證明
  它證明了 兩個以 (A -> A) 爲類型的函數 f1 與 f2 相等
  其實在 typed lambda-calculus 裏是可以有函數的等詞的
- 這種默認生成
  可以說是 對等詞的繼承
  hott 中所有的地方都使用了這種默認的對等詞的繼承
  只有一個地方沒有使用就是 ua 的地方
  不知道這樣的說法對不對
  > <><><
- 如果是繼承
  那麼 就是對接口的繼承
  那麼 就是子類型的概念了
  如果是不要求等詞
  那麼 有什麼數據類型是沒有等詞的呢
  數值分析邪 概率論邪
  > <><><

## universes and families

- cumulative
  sub-type [sub-set]
- families of types [dependent types]
  B (A -> U)
  or
  B (A -> set)

## dependent function types

- formation
  (... -> ...)
  (A #a -> :a B)
- introduction [constructors]
  []
  [(A #a -> :a B) ...]
  or
  dependent pattern matching
- elimination [eliminators]
  apply
  (A (A #a -> :a B) -> :a B)
- computation [reduction]
  a [(A #a -> :a B) ...] apply
- uniqueness principle [expansion]
  f
  [f]

## >< product types

- product in stack
  - formation
    (... -> ...)
    (-> A B)
  - introduction [constructors]
    f (-> A)
    g (-> B)
    f g (-> A B)
    or
    function composition
  - elimination [eliminators]
    f (-> A B)
    f drop (-> A)
    f swap drop (-> B)
  - computation [reduction]
  - uniqueness principle [expansion]
- 爲了使得依賴性可以被表達
  product type 是用 lambda abstraction 定義的
  這是因爲 沒有多值函數 也沒有返回多值的函數
  參數之間的依賴性 和 返回值之間的依賴性
  就必須用 curry 來處理
- product in memory
  - formation
  - introduction
  - elimination
  - computation
  - uniqueness

## dependent pair types

- in stack
- in memory

## coproduct types

## the type of booleans

## the natural numbers

## pattern matching and recursion

## propositions as types

## 記 商空間

- 假設 集合都配以等詞爲基本接口性質
  那麼 做商空間的方式就是
  以一個更強的 等詞代替 原有等詞
- 然而 對於原集合
  定義與其上的變換分兩種
  保持等詞者稱函數
  不保持等詞者稱操作
  當做 商空間 的時候
  所有函數的性質都改變了
  所以需要重新定義接口函數
  或者重新證明接口函數保持新的等詞
- 如果 對等詞可以有如此繼承
  那麼 對別的接口也可以
  商空間 和 子類型 還有 類型類
  說的都是這種對接口函數的繼承與修改
  注意 商空間修改等詞之後 集合的元素就不同了
  所以 商空間與子空間是很不同的

## identity types

- 怎麼可能對任何型都有一致的方式引入等詞呢
  如果這樣的話 根本就沒有 商類型可言了
  這確實做到了 對於每個類型 都有一個等詞
  但是放起了對這個等詞的操作

# 2 homotopy type theory

# 3 sets and logic

# 4 equivalences

# 5 induction

# 6 higher inductive types

# 7 homotopy n-types

# 8 homotopy theory

# 9 category theory

# 10 set theory

# 11 real numbers
