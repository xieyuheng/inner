---
title: A Language for Equivalence
newline: preserve
---

# [todo-list]

- syntax for covering space
- 總結 chain 之間的等價
  chain 之間的等價關係在 continuity-check 中用到
- 總結化學類比
- 爲空間之間的等價設計語言
  能夠描述 normalization theory
- 說明高階代數結構可以經過忘記信息而得到 homology group

# [note]

## 定向

- x -
  在語義上定向可以被理解爲
  在沿着某個公共的 edge glue 兩個 polygon 的時候
  不光要鏈接這兩個 edge
  同時還要鏈接這兩個 edge 的端點
  這樣 如果 edge 的端點是相同的點
  就有兩種 glue 方式了
  定向可以區別這兩種

## 是否需要在語義上加上對定向信息的編碼

- x -
  如果有這些信息
  並且在 chain 複合的時候沒有消去這些信息
  這些信息就會累積起來
  這可能是不好的性質
  所以我們需要重新審視這個問題

## 定義 性質 對象 等價

- x -
  定義所描述的是一般屬性
  這些屬性可以用來判斷某些構造物是否符合這個定義

  構造給出一般的對象
  構造給出存在之物

  存在並不是對象的性質
  而只能被構造證明

  用屬性來定義
  因構造而存在

  我發現這是我所遵循的理解方式
  但是它是否正確?

  在這種理解方式下
  我們不能定義某一個對象
  只能定義某一種特殊性質
  並且構造一個對象
  然後再證明這個對象是滿足這個性質的唯一對象

  這種證明中就需要用到對象之間的等價關係
  如何定義等價關係呢?
  把它定義成就兩個對象而做的判斷?

  是否某些時候
  因爲我們不能寫出某個程序來做自動地判斷
  所以需要手動地證明某兩個對象之間的等價關係?

  比如在證明函數之間的相等時
  比如證明代數結構中元素的相等時 [有限生成羣的字問題]
  此時就需要手動證明等價性
  但是也可以寫一些程序來做幫助
  而進行半自動的證明

  比如在我們的高階代數中
  chain 之間的等價
  機器所能做的可能只是消去其中出現的相鄰互逆元
  但是想要自動尋找到所需要的高階元素來把 chain 相互轉寫
  是不可能的

## 語義

- x -
  在語義所引導的設計中
  - 所谓语义引导
    很多不同的描述 obj 的語法
    所描述出的 obj 之等價很容易判斷
  定向的位置何在?
  等式的位置何在?

- k -
  語義可能要求當兩個元素相 glue 時
  二者的定向信息不會被消除
  也就是說
  兩個等式沒有被融合
  只是標記出了代入位置

- x -
  我想我們可以定義 高階 groupoid 中元素的基本等價關係的檢查

  我們可以把閉合的鏈定義爲 兩個邊界相等的鏈
  我們更需要的是閉合的鏈之間的等價關係
  我想移項就應該在這裏使用

  閉合的鏈之間的 glue 可以定義爲
  等式兩邊的 chain 分別在相同的邊界位置 glue

  glue 之後很可能有能夠消除的項

  closed-chain 之間的等價 還要考慮同倫形變
  此時可能真的是同倫而不是同胚了

## 定向

- x -
  ``` scheme
  ;; 當考慮一個一階元素自乘形成 chain 時
  (type space
    (0 (: p0 (0 <>)))
    (1 (: a0 (= p0 p0))))
  (+ a0 a0) ;; 只有兩種方式

  ;; 當考慮一個二階元素是
  (type space
    (0 (: p0 (0 <>)))
    (1 (: a0 (= p0 p0)))
    (2 (: b0 (= (+ a0 a0) (- a0)))))
  (+ b0 b0) ;; 有幾種方式?
  ;; 用等式來理解定向的話
  ;; 應該只有兩種本質不同的方式
  ```

# combinatorial topology

## intro

- topology, or at least combinatorial topology,
  give raise to algebraic topology,
  the aim of which is to find
  algebraic structures as invariants of topology space.

  my argument is that combinatorial topology it self
  can be viewed as an higher algebraic structure,
  finding different algebraic invariants
  of a combinatorially defined topology space
  is to simplify the higher algebraic structure in different ways.

- 組合羣論中說羣是由生成子和關係展示的
  其實這裏所說的關係都是特殊的關係 即 等式
  在我的語言中 我強調這一點
  並且給等式的方向以解釋

- my aim is to provide a language in which
  all the phenomena of algebraic topology can be expressed and explained.

- [並非劃歸 圖形是必要的]
  在觀察這些從圖形得到的代數結構時
  我們不能脫離對這些圖形的想象本身
  因爲不藉助這些圖形來處理
  等式的兩種恆等變形之間的等價
  是難以想象的

- [recover geometry]
  am i to reduce topology to algebra?
  by no means, since topology, as and reduction of geometry,
  relies on geometry;
  so will my higher algebraic structure relies on geometry.

- [語言問題]
  我們已經有數學語言來描述拓撲和幾何空間了
  我們使用地很好
  我們能夠利用這些數學語言來交流我們對這些東西的想象
  但是當我們需要把這些東西描述給機器的時候
  我們已有的語言就有問題了
  此時我們需要明確的語法來讓機器明白我們的意思
  同時我們還需要明確的語義來編碼這些東西

  此時我們就需要設計一個程序語言

- [侷限性]
  但是每個程序語言都有侷限性
  當我用一種特定的語法來描述這些東西時
  某些東西變得容易表達而某些東西變得不容易表達
  當我用某種特定的編碼方式來實現這些東西時
  某些東西變得容易計算而某些東西變得難以計算

  程序語言設計者的工作就是要
  設計語言來向機器描述這些東西
  並且探索這些他所設計的語言的侷限性的邊界

## subdivision -- equivalence between spaces

- when one combinatorial space is a subdivision of another,
  the proof, by two functions, of homotopy equivalent between the two spaces
  are very special.

  viewed in one direction [by one of the functions],
  it seems like describing the subdivision,

  while viewed in the other direction [by the other function],
  it seems like describing the potential subdivision implied by [x refl].

- x -
  按照我們的計劃
  我們應該用 subdivision 來理解代數結構生成子的變化
  因爲 normalization 就是生成子的變化
  這可以理解爲代數結構的座標系的轉換
  注意 我們所考慮的代數結構中的元素一定要是閉合的

## combinatorial topology

- x -
  我們需要更經濟的語言來描述空間之間因爲公共細分而形成的同胚等價
  而不能用兩個函數所給出的同倫等價

  - 一個函數就足以建立空間之間的細分關係

## 更經濟的等價判別

- 可能是謂詞 [like 'even?']
  而不是命題 [like '<even>']

## 编码问题

- x -
  在考虑粘合 polyhedron 的時候
  可能就已經出現 編碼問題了
  因爲此時 邊和面太多了

## 語言的侷限性 與 實驗的重點

- x -
  由於語法會隨着維數的增加而變得非常複雜
  所以我們應該把實驗的重點限制在低維拓撲中
  我們主要考慮三階流形的二階代數結構

## 語言的有效性

- 語言的有效性 還需要檢驗
  1. 無窮空間
  2. 三階空間
     非自由二階代數結構
     和自由三階代數結構
  3. 要能夠表達二階曲面的分類定理
  4. 要能夠表達 product-space 的 boundary-rule

- 語言的有效性是對比而得的
  需要對比別的編碼和描述圖形的方式

## 一階的例子 與 covering-space

- x -
  如果想給出非平凡的一階圖形的例子
  那就一定要考慮無窮圖
  此時無窮個點的生成子就類似於邊
  無窮個邊的生成子就類似於面 [但是可能是不可逆的]
  等等

- k -
  我想對這些東西的表達很重要
  在考慮這些之前去實現語言
  就是 '過早優化' 了

- x -
  還要考慮到 我們之前說過
  covering-space 的編碼 能夠解決代數結構中元素的等價問題

## >< indexed-space

- When defining a space, parts of it can be indexed
  by parts of another space.
  Such a named indexing of parts is called an index,
  which is also a subspace of the larger space.

- Note that,
  if the indexing is to be viewed as a map,
  it would be a level-up map.

- rule for indexed space [? cellular]

- 'I' indexed by a space,
  is the suspension of the space.

- While 'I' is 1-cell,
  how about 2-cell 3-cell and n-cell?
  how about continuum other then n-cell?
  note that, n-cell indexed by a space 'A',
  is specified by (-> A [n-cell boundary])

## >< fiber-space

- fiber-space = dependent product-space?

- To view product-space as special fiber-space,
  for which a gluing pattern is given.

- After a construction of a fiber-bundle,
  we can proof the total-space is equal to another space,
  and by doing so, we get a level-down map from the space to the base-space.

- A level-down map can only be achieved by means of fiber-bundle.

- x -
  高於維數階的同倫羣
  也許只能被理解爲 fiber-space
  而不能被理解爲 chain group

## >< lifting-problem

- A lifting-problem is expressed for a [?] on a fiber-space.

- [lifting-problem and cross-section-problem]
  cross-section-problem can be viewed as
  lifting a subspace [instead of function] of the base-space,
  or just lifting the base-space itself [i.e. global cross-section].

## >< homotopy theory

- x -
  homotopy group 被定義爲帶有代數結構的 mapping-space
  但是 對我們來說
  我們可以直接說它是 combinatorial group theory

- k -
  也許 mapping-space 是必要的
  考慮高維的情形就知道了

- x -
  不想用 mapping-space
  是因爲與 combinatorial theory 相比
  這種空間的生成元和關係不明顯

- k -
  我們可以考慮 在低維的情形
  mapping-space 的代數結構如何獲得生成元與關係
  然後再推廣到高維

# 代數結構

## 之前的錯誤在於沒有考慮定向 [現在知道定向就是等式的方向]

- 一個 disk 或 ball 的邊界多種表示方式
  當圖形作爲代數結構中的元素而自乘時
  之前的想法是
  只要有公共公共邊界就可以相乘
  其中某種相乘 被認爲是 same-position-self-gluing
  這種相乘將消除一對元素

  現在知道必須把這些
  作爲 disk 或 ball 的邊界的 circle 或 sphere
  所形成的 close chain
  理解爲等式
  等式是有固定方向的
  這個方向對應於 disk 或 ball 的定向
  多種表示方式和相乘方式 = 等式的恆等變換
  允許 左右兩邊的移項 這種恆等變換
  但是 當改變等式的方向的時候 必須要給出明顯的記錄
  因此 same-position-self-gluing 不能理解爲簡單的自乘了
  而應該被理解爲取反向之後的相乘
  想要相消 必須是 (+ A -A) 而不能是 (+ A A)
  這樣 有關相消的信息就被明顯地表達出來了

## 代數結構

- [generator and chain]
  chain 是代數中的元素
  即 生成子的複合
  這裏所考慮的 chain 不是交換的

- [boundary operator]
  組成 chain 的元素可能屬各個層次
  層次之間有 boundary 算子
  邊界爲空的 chain 是閉的

- [adding generator as adding equation to free algebraic structure]
  disk 與 ball 是 chain 所形成的等式

- [generator 的複合 就是 等式的融合]
  disk 與 ball 在 glue 下形成 二階或者三階 chain
  這種 glue 記錄了 一階或二階等式之間的融合 [或者形成等式組]

- [語法難點]
  假設我們的語法是線性的
  - 一階代數中
    | 非閉合元素 | line segment | 有唯一的表達方式 |
    | 閉合元素   | circle       | 有很多表達方式   |
    | 等式       | circle       | 有很多表達方式   |
  - 二階代數中
    | 非閉合元素 | sphere with holes [disk = sphere with one hole] |
    | 閉合元素   | sphere                                          |
    | 等式       | sphere                                          |

- [等式可以用來 rewrite chain]
  用一個等式去變換一個 chain
  利用等式來做 rewriting
  這種 rewriting 可以看成是函數的同倫
  因爲 空間 A 中的 chain 可以看成是某個空間 X 嵌入 A
  (: f (-> X A))

  如何區分 同倫變換 與 同痕變換 與 外在同痕變換

  如果這樣想
  我們就是在用高階代數中的元素[chain]來取代函數
  高階代數中的元素的性質 就反映了 空間 X 的性質
  二者之間應該有嚴格的對應

  有了函數之間的同倫等價
  那麼空間之間的同倫等價如何呢?
  [即 hauptvermutung 問題]

- [同倫時維數變化的代數解釋]
  每個元素本身都蘊含着一個平凡的等式
  那就是 其與自身相等

## 高階代數結構

- x -
  不用過多地考慮古典的 hauptvermutung
  其實我們現在已經有了不錯的理解函數之間同倫的方式
  重要的是找到在我們的語言中解釋空間之間的等價的方式
  [其定義可能有別於 Hurewicz 的 homotopy-equivalence]
  我們要找在我們的語言中自然的等價關係
  然後看看這個等價關係在更古典的語言中的解釋

  函數空間 (-> A B) 就是 B 的具有特殊性質的 chain 的空間
  對於函數空間之間的等價
  我們也是不知道該如何定義的
  我們目前知道的只是
  元素之間的等價 和 函數之間的等價
  這二者都可以說是一階的等價

- k -
  我想 對於空間之間的等價關係而言
  公共細分所定義的等價關係就已經夠了
  細分是有代數解釋的 [考慮 group 被細分爲 group]
  並且 把類型當作數據的時候
  細分也可以用函數來實現 [考慮二維流形的分類定理]

- x -
  如果這樣說的話 我們其實是在用 hauptvermutung
  但是我們不知道古典意義上的 hauptvermutung 的意義是什麼
  我們不知道 爲什麼 hauptvermutung 是需要證明的
  低維時這個定理是如何被證明的?
  高維是這個定理爲什麼會被否定?
  四維時爲什麼還沒有結果?
  當證明 hauptvermutung 的時候 我們證明的是什麼?
  其構造性如何?
  - 既然在高維這個定理可以被否定
    那麼就是說 有另外一種定義空間之間的等價的方式
    並且在高維的時候
    這種定義 比 公共細分所做的定義 帶有的信息要多

- k -
  但是我們也要明白
  如果採用這種方式的話
  我們就離開了同倫
  而回到了對同胚研究
  維數之間不能有變化了

- x -
  首先 跨越維數的映射
  在我們的語言中本來就是不自然的
  其次 從等式的角度看 維數的變化也是可以解釋的
  因爲對等式的平凡的恆等變形也算是恆等變形

- k -
  我們還需要考慮如何定義函數空間之間的等價關係

- x -
  如果我能找到一個高階代數結構之間的等價關係
  使得它介於同倫與古典的同胚之間
  我就能證明高階同倫羣是不重要的了
  因爲同倫羣所帶有的信息
  就是同倫等價所需要的所有信息

- k -
  首先我們已經有代數結構的細分了
  如果想要定義更強的等價
  就要允許更多的對代數結構的變換
  還有什麼可以允許的呢?

- x -
  可能這種想法太刻意了
  不如想想能夠如何模仿 homology group
  來給我們的高階代數結構做去類型化與交換化
  我們可以試着找出同調羣無法區分的空間
  然後以新的方式弱化代數結構
  使得如此獲得的代數結構之間的同構問題可解
  並且能夠區分那些同調羣無法區分的空間

- k -
  如果我們的理論與語言有效的話
  我們應該能自己構造出很多這種空間
  不用 knot 而用 polyhedron

## 同倫類

- x -
  mobius-band 與 cylinder 有相同的同倫類型
  但是 它們的二階代數結構不同

  mobius-band 的非平凡二階元素只有一個
  cylinder 的非平凡二階元素於整數對應

- k -
  但是 我們如何定義代數結構之間的等價來形成空間的代數不變量呢?

- x -
  對於上面兩個例子
  首先我們發現 相乘方式是固定的
  這就類似於古典的代數結構
  我們需要更多的例子

## local-to-global

- x -
  一個 local-to-global theorem 可以給我們靈活的計算方式
  是否可以對高階代數結構形成 local-to-global theorem?

## 非交換的代數結構

- x -
  whitehead 已經知道的那些空間的非交換的高階代數結構是什麼?
  [cross-module]

## subdivision

- subdivision 有代數解釋
  從代數結構總結出代數不變量的時候
  必須保證所定義的不變量在 subdivision 下不變
  homology group 就是典型的例子

## 二階代數的交換性

- x -
  ``` scheme
  (type space
    (0 (: a0 (0 <>)))
    (1 (: b0 b1 (= a0 a0))))

  (: (+ b0 b1) (= a0 a0))
  (: (+ b1 b0) (= a0 a0))


  (type space
    (: a0 a1 <>)
    (: b0 (= a0 a1))
    (: c0 c1 (= b0 b0)))

  (: (+ c0 c1) (= b0 b0))
  (: (+ c1 c0) (= b0 b0))

  (: [c0 (=< b0)] (= (+ (- b0) b0) (+)))
  (: [c1 (<= b0)] (= (+) (+ (- b0) b0)))

  (: (+ c0 (=< b0) c1 (<= b0))
     (= (+ (- b0) b0) (+ (- b0) b0)))

  (:? (= (+ c0 c1) (+ c1 c0)))


  (type space
    (: a0 <>)
    (: c0 c1 (= [a0 refl] [a0 refl])))

  (: (+ c0 c1) [a0 refl])
  (: (+ c1 c0) [a0 refl])

  (:? (= (+ c0 c1) (+ c1 c0)))

  (type space
    (: a0 a1 <>)
    (: b0 b1 b2 b3 (= a0 a1))
    (: c0 (= b0 b1))
    (: c1 (= b1 b2))
    (: c2 (= b2 b3))
    (: c3 (= b3 b0)))

  (: (+ c0 c1) (= b0 b2))
  (: (+ c2 c3) (= b2 b0))
  (: (+ (+ c0 c1) (+ c2 c3)) (= b0 b0))

  (: (+ c3 c0) (= b3 b1))
  (: (+ c1 c2) (= b1 b3))
  (: (+ (+ c3 c0) (+ c1 c2)) (= b3 b3))
  ```

- k -
  關於 chain 的等價
  我們看來得到了一個悖論
  按照古典的說法 我們必須有交換性
  但是我們卻沒法證明這一點

  我想
  既然你說 對 covering-space 的編碼能解決高階代數的字問題
  那麼就解決一下試試
  看看二者的編碼是否相等

# encoding a space by its generators

- a space is encoded by a list of its generators.

  ``` scheme
  (define cylinder
    (type space
      (: a0 a1 <>)
      (: b0 (= a0 a1))
      (: b1 (= a1 a1))
      (: b2 (= a0 a0))
      (: c0 (= (+ b0 b1) (+ b2 b0)))))

  (define mobius-band
    (type space
      (: a0 a1 <>)
      (: b0 (= a0 a1))
      (: b1 (= a1 a0))
      (: b2 (= a0 a1))
      (: c0 (= (+ b0 b1) (+ b2 (- b0))))))
  ```

- a generator has a dimension.
  a generator of n-dimension is called a n-generator.

- a n-generator records an equation between two
  (n-1)-chain formed by gluing (n-1)-generators.

- an equation has a direction.
  we inverse a generator
  to record the reversing of the direction of an equation.

  ``` scheme
  (: b0 (= a0 a1))
  (: (- b0) (= a1 a0))
  ```

# chains

- the boundary of a n-chain is an equation of (n-1)-chain.

  chain 本身詳細記錄獲得最終 equation 的等價變形過程
  chain 中有一列元素 [生成子 或者 嵌套的 chain]

  爲了 相鄰的兩個元素 類型相符 而複合
  chain 中的每個元素還要輔以移項信息

  - x -
    語義[實現]強調的是 chain 作爲數據
    而語法強調的是 chain 作爲等式變換之記錄

# 一維自由代數結構 / 1 dimensional free algebraic structure

- 首先我們有很多點
  點之間有很多被標記以方向的邊 稱爲有向邊
  我們可以認爲這樣一個圖是一個代數結構 稱爲 free-groupoid
  有向邊爲生成子
  生成子相乘可以得到這個代數中的一般元素

  代數的元素之間有等式
  比如平凡的等式 (= a a)

  沿着一條有向邊走 過去又馬上走回來 就等同於留在原地
  這樣我們就得到了一個看似非平凡的等式 (= (+ a (- a)) (+))
  我們可以說這是一種消去
  當 (+ a (- a)) 出現時它們可以被消除

  但是我們發現 (= a a) 與 (= (+ a (- a)) (+)) 其實是同一個等式
  因爲經過移項 二者之間可以相互轉化
  這就是等式的恆等變形

  或者我們可以說
  對於代數中的每個元素
  我們都能形成平凡的等式
  ``` scheme
  (: (refl a) (= a a))
  (: (refl (+ a b)) (= (+ a b) (+ a b)))
  ```

  我們還可以利用移項來對這些等式進行恆等變形
  即 基本恆等變形
  ``` scheme
  (: (refl a) (= a a))
  (: (+ (refl a) (=> a)) (= (+ a (- a)) (+)))
  (: (+ (refl a) (=< a)) (= (+ (- a) a) (+)))
  (: (+ (refl a) (>= a)) (= (+) (+ a (- a))))
  (: (+ (refl a) (<= a)) (= (+) (+ (- a) a)))
  ```

  一般的規則如下
  ``` scheme
  (: =< (-> [<::> = <::>] [(- <:) <::> = :>]))
  (: => (-> [<::> = <::>] [<::> (- :>) = <:]))
  (: <= (-> [<::> = <::>] [:> = (- <:) <::>]))
  (: >= (-> [<::> = <::>] [<: = <::> (- :>)]))

  (: =< (-> (= (+ <: :>)
               (+ <: :>))
            (= (+ (- <:) <: :>)
               (+ :>))))

  (: => (-> (= (+ <: :>)
               (+ <: :>))
            (= (+ <: :> (- :>))
               (+ <:))))

  (: <= (-> (= (+ <: :>)
               (+ <: :>))
            (= (+ :>)
               (+ (- <:)) <: :>)))

  (: >= (-> (= (+ <: :>)
               (+ <: :>))
            (= (+ <:)
               (+ <: :> (- :>)))))
  ```

  我們可以說這些是同一個元素
  只不過我們用來表達這些元素的語言是線性的
  所以對同一個元素 產生了不同的表達方式
  當把形式語言實現爲程序語言的時候
  我們可以寫一些檢查器
  來幫助我們判斷 某些式子是否是同一個東西的不同表達方式

  等式可以用來轉寫代數結構中的元素
  平凡的等式 和 平凡的等式經過移項變形而得到的等式
  所形成的轉寫都是平凡的
  這些轉寫不用記錄 直接讓機器處理就好

  - 也就是說我們可以自由的消去相鄰的互逆的元素
    但是這是不應該被允許的
    因爲 消去互逆的項與否 整個 chain 的邊界就改變了
    而 chain 要能被看作是 對邊界之成爲當前這樣的邊界 的忠實記錄
    邊界就是等式組 chain 是對等式組的變換的記錄

  - 但是如果這些平凡的轉寫不能讓機器自動處理
    那麼表達移項的語法可能就非常複雜了

  - 之所以這樣說是因爲
    把移項理解爲了 先在等式兩邊同時乘以某個元素
    然後再消去相鄰的項
    (a b = c)
    (a b -b = c -b)
    (a = c -b)
    只要拋棄這種觀點就行了

  - 核心的想法是
    chain 的語法是對等式組變換的忠實記錄
    即 所有關於變化的信息 都要被明顯的語法表達出來

  說這個代數結構是 free-groupoid
  說其 'free' 就在於除了這些利用 'refl' 生成的平凡等式之外
  沒有別的等式了

# 一維非自由代數結構  / 1 dimensional non-free algebraic structure

- 添加一些別的等式 free-groupoid 就變成了 groupoid
  首先我們發現 (= (+ b0 b1) (+ b1 b0))
  是不能由平凡等式經過基本恆等變形來得到的
  我們可以把這個等式添加到我們的代數結構中來
  ``` scheme
  (: c0 (= (+ b0 b1) (+ b1 b0)))
  ```

  添加的時候給以了名字
  每次想要使用這個等式的時候
  我們就用這個名字做記錄

  我們還可以再添加一個
  ``` scheme
  (: c1 (= (+ b0 b1) (+ b1 b0)))
  ```
  雖然二者作爲等式是相同的
  但是在用於轉寫時
  二者的性質可能不同
  比如 二者消耗的能量可能不是一個數量級的
  或者 二者消耗的能源類型不同
  或者 二者消耗的時間不是一個數量級的

  比如我們還可以引入
  ``` scheme
  (: c (= b0 b0))
  ```
  雖然它的作用效果是平凡的
  但是它與平凡的等式 (refl b0) 是不同的

  我們假設每個這樣的引入
  所引入的都是與之前等式不同的新等式

  我們可以想象 (+ b0 b1) 是某個分子結構
  我們發現了一種物質 c0 利用這個物質 和某種處理方式
  我們能把 (+ b0 b1) 變成 (+ b1 b0)
  之後我們又發現了 另一種類似的物質 c1 也有類似的效果
  等等

  這樣等式對元素所做的變化 就能被實體化
  動詞的名詞化 就是爲了記錄
  爲了能夠把動詞所代表的變化當作名詞一樣來處理
  反向的等式就是反物質
  等等

  這些等式也可以用來轉寫一階代數結構中的元素
  這些等式也可以進行移項這種恆等變形
  如果只在乎把 (+ b0 b1) 變爲 (+ b1 b0)
  那麼使用 c0 和 c1 那個都無所謂

  並且我們可以把兩個等式融合來得到新的等式
  我們要設計語法來記錄等式的融合
  並且 融合兩個等式之後再作用 與 相繼的作用兩個等式 必須是等價的

# 語義 [三種等價變換]

- x -
  改变之前的观点
  主要把空間視爲數據結構而非類型

  也許我們能更好地處理 product-space 和 fiber-space
  這些構造新空間的方式

  這樣我們主要考慮的就是如何設計這些數據結構
  還有圖形之間的各種等價關係
  這樣我們就可以用語義來引導我們的設計

- k -
  可能只有考慮了數據結構如何設計之後
  才能考慮等價關係

- 數據結構

  - space
    生成子的列表 和 生成子之間的邊界關係
    實現 space
    - 0 維 儲存點的名字
    - 1 維 邊和邊的類型 (: b (= a0 a1))
    - 2 維 面和面的邊界圓 (: c (= (+ ...) (+ ...)))
    - 3 維 體和體的邊界球面 (: d (= (+ ...) (+ ...)))
    其中 (+ ...) 都是 chain
    因此空間之間的等價涉及到 chain 之間的等價

    - subdivision 也用到 chain 之間的等價?

# 等價

- chain = 對 equation 之變形的忠實記錄
  an equation is the boundary of a chain
  a chain is the record of the formation of an equation
  (+ ...)
  (: (+ ...) (= ... ...))

  用等式進行 rewrite = chain 之間的同倫
  對 chain 的 rewrite =
  (+ ...) (apply (+ ...))
  - 一個 n+1 階 chain 是一個 n 階等式
    這個等式本身也可以給以移項變換
  - 等式可能是 (refl ...) 經過移項得到的
    這種等式稱作是非本質的 它們所給出的變換也稱作是非本質的

  space 之間的等價 = subdivision
  (~~ A B)
  (~> A B) (subdivision A B)

- 函數 被 chain 代替
  同倫 被 rewrite 代替

- x -
  有兩種理解 (+ ...) 的方式
  (1) 先生成一個 list 然後複合它們
  (2) 直接在棧中構造複合的數據
  我們用 (1) 這種理解方式

- x -
  chain 有類型
  當用 高階的 chain 來轉寫 chain 的時候
  chain 的類型如何變化?
  chain 的類型就是 chain 的 boundary
  當進行 rewrite 的時候 boundary 不變

- x -
  唯一可以被機器處理的等價就是相鄰的相逆項的消除

# >< 等式中缺少信息

- 等式 (= a b) 中缺少 a b 的边界信息
  对于 (= a (+)) 这种等式 尤其如此
  只有加上边界信息 才能描述形成 chain 的条件

  否则 如果允许等式 (= a []) 与 (= [] c)
  被融合爲 (= a c)
  就可能形成不联通的 chain

- x -
  因此 我們應該重新考慮 chain 之等價的定義

------

- x -
  chain 上的消去是特殊的轉寫
  只要 apply 就行了

  邊界中的消去需要記錄

  但是邊界就是兩個 chain

- k -
  what is type of type?

- x -
  always empty

- k -
  then what is the information needed?

# equivalent relation between chains

- the equivalent relation between chains
  are defined by three kinds of transformations.
  chains that can be transformed to each other by such transformations
  are defined as equivalent.

  this equivalent relation will be implemented
  by predicate (chain-equivalent ...).

  1. rewriting

     more precisely
     rewriting a n-chain by a (n+1)-chain.

     rewriting can be applied to any part in the n-chain,
     as long as the type of the (n+1)-chain
     matches the part of the n-chain.

     such rewriting will not change the type of the n-chain.

     trivial (n+1)-chain are those given by of reflection n-chain.
     if in a n-chain there occurs a n-generator
     glued to its inverse by the same position,
     then this part of the n-chain can be eliminated
     by applying the reflection of the n-generator.

  2. shifting

     more precisely
     shifting an element from one side of the equation
     recorded by a n-chain,
     to the other side of the equation.

     shifting changes the equation recorded by the n-chain,
     thus changes the boundary of the n-chain.

  3. trivial adding or subtracting

     more precisely
     adding or subtracting a trivial n-chain
     given by reflection of (n-1)-chain,
     to any position in the n-chain where it fits.

     such transformation changes the equation recorded by the n-chain,
     by adding or subtracting the same (n-1)-chain occurs
     in both side of the equation recorded by the n-chain,
     or by eliminating (n-1)-chain followed by its inverse
     that occurs on one side of the equation recorded by the n-chain.

# equation

- 在檢查函數的連續性時
  需要判斷 boundary 的等價
  因此 boundary 即 equation 之間的等價關係纔是重要的?
  而 chain 之間的等價不重要?

- chain 本身就是對 equation 的等價變形的記錄

# 類比化學語言

## intro

- 我們可以把上面的類比加以系統發展
  用以理解高階代數中的現象

  物質
  物質之間的轉化
  轉化是利用高階的物質完成的
  反物質
  等等

## 對 chain 的等價變換

- chain 是一個複合物
  其中的每個元素 都能用來促成某種化學變化
  chain 之爲複合物就是化學變化的複合

  1. 用高階元做轉寫
     這是聲明的等價關係

  2. 移項
     這代表了化學反映可以沿着各個方向進行
     只要改變催化物的角度就行了

  3. 等式兩邊同時添加相同的元素
     催化物不影響這些元素
     反映前後這些元素都存在

## 化學類比

- x -
  上面對 chain 之間的等價 進行了化學語言的類比
  其他有待類比的概念 :
  空間
  映射
  空間的細分

- k -
  空間中的所有生成子都是物質
  一階物質能夠催化零階物質的變化
  二階物質能夠催化一階物質的變化 等等

- x -
  空間的細分
  就是這個空間所代表的物質的進一步分解

  利用空間之間的細分來說明空間之間的等價
  就是說 這兩類物質之間的關係是相同的

  說空間 B 可以被看成是空間 A 的細分
  這是用一個函數類描述的
  (: f (subdivision (-> A B)))

  也就是說
  空間之間的函數和函數的性質
  是用來描述空間之間的關係的

  空間之間的關係
  不在乎空間之中物質的名字
  而在乎它們之間的反映關係

## 化學類比 continuity-check

- x -
  此時尤其能夠看出 所謂 continuity-check
  其實是數學結構之間的同態

  使用化學類比的優點是
  可以不考慮維數增加帶來的想象困難

  維數增加所帶來的語法困難
  可以解釋爲

  比如
  三階物質的使用方式很多
  是因爲 二階物質的複合方式很多

  開始的時候物質是不能用於化學反應的點

## 化學類比 product-space 與 fiber-space

- x -
  product-space
  物質組 (* A B)
  其中的物質爲 (* a b)
  a 作用時 b 必須出現 (* ... b)
  b 作用時 a 必須出現 (* a ...)
  [並且 b 的作用方向是相反的]
  因此如果想要 (* a b) 作用
  那麼反應物就一定是 (* (lhs a) b) 與 (* a (rhs b)) 的複合物
  生成物是 (* a (lhs b)) 與 (* (rhs a) b) 的複合物
  等等

  fiber-space is dependent product-space
  類似物質組 (* A B)
  但是物質之間的反映關係被一個函數 (: f (-> B B)) 扭曲了

# 二階空間

- 我們發現
  1. 等式有逆元素
  2. 兩個等式如果有相同的元素
     就可以利用元素代入來將兩個等式融合爲一個
     這種融合就像是代數結構中的乘法

  這樣我們就有了一個二階的代數結構 稱其爲 2-complex
  我們可以把一階時候的術語統一一下
  稱之前的 free-groupoid 爲 1-complex
  這樣 1-complex 之 'free' 就在於 其中沒有非平凡二階元素

  就像一階情形一樣
  首先我們要找到 基本恆等變形
  這些恆等變形 當然就是 (refl c0) 之類的平凡等式 經過移項獲得的
  我們需要設計語法來描述這些信息

# 選取例子以驗證語言的有效性

## intro

- 邊數很少的 polygon
  面數很少的 polyhedron
  的所有可能
  然後以它們爲基礎看各種算法作用於它們的效果

- 用正多面體來形成簡單的例子
  對稱性越強的圖形
  編碼所用的信息就越少

- 那麼這首先將引出一個計數問題
  通過計數我們可以估計問題的複雜性
  首先我們要確定
  有多少由 n-gon 或 n-hedron 構造 manifold 的方式

## >< how to enumerate n-gon and n-hedron?

- ><

## 2-complex formed by gluing the edges of one n-gon together

### 0-gon

``` scheme
(type space (note S2)
  (: a0 <>)
  (: c0 (= [a0 refl] [a0 refl])))
```

### 2-gon

``` scheme
(type space
  (: a0 a1 <>)
  (: b0 b1 (= a0 a1))
  (: c0 (= b0 b1)))

(note glue b1 to b0)

(type space (note S2)
  (: a0 a1 <>)
  (: b0 (= a0 a1))
  (: c0 (= b0 b0)))

(note glue a1 to a0)

(type space (note pseudo-manifold)
  (: a0 <>)
  (: b0 (= a0 a0))
  (: c0 (= b0 b0)))

(note cut b0 to b0 and b1)

(type space
  (: a0 <>)
  (: b0 b1 (= a0 a0))
  (: c0 (= b0 b1)))

(note glue b1 to (- b0))

(type space (note projective space)
  (: a0 <>)
  (: b0 (= a0 a0))
  (: c0 (= b0 (- b0))))
```

### cut 和 glue

- x -
  施行上面這個 cut 和 glue 操作的條件是什麼?
  雖然這些操作是不同胚的
  但是也應該把它們加到語言中來

- k -
  在窮舉 n-gon 所形成的 complex 的時候
  cut 與 glue 這兩個操作可以形成一個 complex 之間的有向圖

### the neighborhood function

``` scheme
(define S2
  (type space
    (: a0 a1 <>)
    (: b0 (= a0 a1))
    (: c0 (= b0 b0))))

(topological-equivalent
 (neighborhood S2/a0)
 (type neighborhood-space
   (: b0 <>)
   (: c0 (= b0 b0))))

(define PS2
  (type space
    (: a0 <>)
    (: b0 (= a0 a0))
    (: c0 (= b0 b0))))

(topological-equivalent
 (neighborhood PS2/a0)
 (type neighborhood-space
   (: b0 <>)
   (: (- b0) <>)
   (: c0 (= b0 b0))
   (: (+ c0 (=< b0) (>= b0)) (= (- b0) (- b0)))))
```

### >< 4-gon

## 3-complex formed by gluing the faces of one n-hedron together

# 二維空間的例子

## mobius-band

``` scheme
(define mobius-band
  (type space
    (: a0 a1 <>)
    (: b0 (= a0 a1))
    (: b1 (= a1 a0))
    (: b2 (= a0 a1))
    (: c0 (= (+ b0 b1) (+ b2 (- b0))))))

(: c0                   (= (+ b0 b1) (+ b2 (- b0))))
(: (+ c0 (=< b2))       (= (+ (- b2) b0 b1) (- b0)))
(: (- c0)               (= (+ b2 (- b0)) (+ b0 b1)))
(: (+ (- c0) (<= b2))   (= (- b0) (+ (- b2) b0 b1)))

(: (+ c0 (=< b2)
      (- c0) (<= b2))
   (= (+ (- b2) b0 b1) (+ (- b2) b0 b1)))

(: (refl c0)
   (= c0
      c0))

(: [(refl c0) (=> c0)]
   (= (+ c0 (=< b2)
         (- c0) (<= b2))
      (+)))

(chain-equivalent
 [(+ b0 b1) (apply c0)]
 (+ b2 (- b0)))
```

## cylinder and dehn-twist

``` scheme
(define cylinder
  (type space
    (0 (: a0 a1 <>))
    (1 (: b0 (= a0 a1))
       (: b1 (= a1 a1))
       (: b2 (= a0 a0)))
    (2 (: c0 (= (+ b0 b1) (+ b2 b0))))))

(in cylinder
  (: c0                   (= (+ b0 b1) (+ b2 b0)))
  (: (+ c0 (=< b2))       (= (+ (- b2) b0 b1) b0))
  (: (+ c0 (>= b1))       (= b0 (+ b2 b0 (- b1))))
  (: (- c0)               (= (+ b2 b0) (+ b0 b1)))
  (: (+ (- c0) (<= b2))   (= b0 (+ (- b2) b0 b1)))
  (: (+ c0 (=< b2)
        (- c0) (<= b2))
     (= (+ (- b2) b0 b1)  (+ (- b2) b0 b1)))
  (: (+ c0 (=< b2)
        c0 (>= b1))
     (= (+ (- b2) b0 b1) (+ b2 b0 (- b1)))))

(define dehn-twist
  (map
    (0 (% (-> (0 cylinder) (0 cylinder))
          (-> a0 a0)
          (-> a1 a1)))
    (1 (% (-> (= a1 a1) (= a1 a1))
          (-> b1 b1))
       (% (-> (= a0 a0) (= a0 a0))
          (-> b2 b2))
       (% (-> (= a0 a1) (= a0 a1))
          (-> b0 (+ b0 b1))))
    (2 (% (-> (= (+ b0 b1) (+ b2 b0)) (= (+ b0 b1 b1) (+ b2 b0 b1)))
          (-> c0 (+ c0))))))

(: [c0 dehn-twist]
   [(= (+ b0 b1) (+ b2 b0)) dehn-twist]
   (= (+ b0 dehn-twist b1 dehn-twist) (+ b2 dehn-twist b0 dehn-twist))
   (= (+ b0 b1 b1) (+ b2 b0 b1))
   == [c0 boundary]
   (= (+ b0 b1) (+ b2 b0)))
```

## sphere

``` scheme
(define sphere
  (type space
    (: a0 a1 <>)
    (: b0 (= a0 a1))
    (: c0 (= b0 b0))))

(define sphere
  (type space
    (: a0 a1 a2 <>)
    (: b0 (= a0 a1))
    (: b1 (= a1 a2))
    (: c0 (= (+ b0 b1) (+ b0 b1)))))
```

## solid-tetrahedron

- the following description will break
  the beautiful symmetry of solid-tetrahedron,
  only by this way,
  can we express each symmetry by a homeomorphism
  from the solid-tetrahedron to itself.

``` scheme
(define solid-tetrahedron
  (type space
    (: a0 a1 a2 a3 <>)
    (: b01 (= a0 a1))
    (: b02 (= a0 a2))
    (: b03 (= a0 a3))
    (: b12 (= a1 a2))
    (: b13 (= a1 a3))
    (: b23 (= a2 a3))
    (: c012 (= (+ b01 b12) b02))
    (: c123 (= (+ b12 b23) b13))
    (: c013 (= (+ b01 b13) b03))
    (: c023 (= (+ b02 b23) b03))
    (: d1234
       (= (+ (- c012) (=< b01)
             c123 (>= b23)
             (<= (- b01))
             (=> (- b23)))
          (+ (- c013)
             c023 swap))
       (note
         (= (+ (- c012) (=< b01)
               (:> (= (+ (- b01) b02) b12))
               c123 (>= b23)
               (:> (+ (= (+ (- b01) b02) b12)
                      (= b12 (+ b13 (- b23))))
                   (= (+ (- b01) b02) (+ b13 (- b23))))
               (<= (- b01))
               (=> (- b23))
               (:> (= (+ b02 b23) (+ b01 b13))))
            (+ (- c013)
               (:> (= b03 (+ b01 b13)))
               c023 swap
               (:> (+ (= (+ b02 b23) b03)
                      (= b03 (+ b01 b13)))
                   (= (+ b02 b23) (+ b01 b13)))))))))

(define solid-tetrahedron
  (type space
    (: a0 a1 a2 a3 <>)
    (: b01 (= a0 a1))
    (: b02 (= a0 a2))
    (: b03 (= a0 a3))
    (: b12 (= a1 a2))
    (: b13 (= a1 a3))
    (: b23 (= a2 a3))
    (: c012 (= (+ b01 b12) b02))
    (: c123 (= (+ b12 b23) b13))
    (: c013 (= (+ b01 b13) b03))
    (: c023 (= (+ b02 b23) b03))
    (: d1234
       (= (+ (- c012) (=< b01)
             c123 (>= b23)
             (<= (- b01))
             (=> (- b23)))
          (+ (- c013)
             c023 swap)))))
```

# map, product-space, equivalent between maps

## map and continuity-check

- For level-same maps, the rule for continuity-check is simple.
  Suppose we have map (: f (-> A B)),
  and 'p' is a n-dim part of 'A'.

  continuity-check is simply
  ``` scheme
  (= [p f boundary] [p boundary f])
  ;; or
  (: [p f] [p boundary f])
  ```

  i.e. how the boundary of 'p' is mapped to 'B' by 'f',
  will constrain how 'p' can be mapped to 'B' by 'f'.

- Since a map has many levels.

- 兩個空間 A B 之間的映射可以被理解爲
  B 中的一組 chain
  其中每個 chain 被 A 中的生成子命名
  並且生成子的邊界給 B 中的這一組 chain 的選取加以了限制

- 也可以被理解 aristotle 對連續體的解釋

- 映射的複合
  就是對 chain 的雙重限制

- 映射是一个 limited chain
  映射之間的同倫是 等式對 limited chain 的轉寫

- x -
  某個函數可以看成是
  限制對 chain 所施行的代數操作
  這類似於羣的同態定理

  考慮羣同態 (: f (-> G1 G2))
  (~~ (/ G1 (ker f)) (img f))

  考慮連續函數 (: f (-> A B))
  它被理解爲 B 中的 chain
  但是 A 的形式限制了 chain 的性質
  並且 (ker f) 是 A 中那些被放棄不用的 對 chain 的限制
  [有很多的限制方式 但是我放棄使用其中一些]

## product-space

``` scheme
(note there can be many ways by which we can compose new spaces,
      each of such way must shows
      1. what are the parts of the space?
      2. what are the boundarys of the parts?)

;; the rule of product-space
(: (* a b) (= (* a boundary b) (* a b boundary)))

(define I
  (type space
    (0 (: i0 i1 <>))
    (1 (: i01 (= i0 i1)))))

(in (* I I)
  (: (* i01 i0) (= (* i0 i0) (* i1 i0)))
  (: (* i1 i01) (= (* i1 i0) (* i1 i1)))
  (: (* i01 i1) (= (* i0 i1) (* i1 i1)))
  (: (* i0 i01) (= (* i0 i0) (* i0 i1)))
  (: (* i01 i01) (= (+ (* i01 i0) (* i1 i01))
                    (+ (* i01 i1) (* i0 i01)))))

(note A function of type (-> (* I I) X)
      can be defined as follow)

(define f
  (map
    (0 (% (-> (* (0 I) (0 I)) (0 X))
          (-> (* i0 i0) ...)
          (-> (* i0 i1) ...)
          (-> (* i1 i0) ...)
          (-> (* i1 i1) ...)))
    (1 (% (-> (* (0 i0 i1) I %:i)
              (0 (* i0 :i) <>
                 (* i1 :i) <>))
          (-> (* (1 i01) i0) ...)
          (-> (* (1 i01) i1) ...))
       (% (-> (* I %:i (0 i0 i1))
              (0 (* :i i0) <>
                 (* :i i1) <>))
          (-> (* i0 (1 i01)) ...)
          (-> (* i1 (1 i01)) ...)))
    (2 (% (-> (* (0 i0 i1) %:p0
                 (0 i0 i1) %:p1)
              (1 (* (1 :p0) i0) <>
                 (* i1 (1 :p1)) <>
                 (* (1 :p0) i1) <> rev
                 (* i0 (1 :p1)) <> rev))
          (-> (* (1 i01) (1 i01)) ...)))))

(define f
  (map
    (0 (% (-> (* I I) X)
          (-> (* i0 i0) ...)
          (-> (* i0 i1) ...)
          (-> (* i1 i0) ...)
          (-> (* i1 i1) ...)))
    (1 (% (-> (* (= i0 i1) I %:i)
              (= (+ (* i0 :i) <>)
                 (+ (* i1 :i) <>)))
          (-> (* i01 i0) ...)
          (-> (* i01 i1) ...))
       (% (-> (* I %:i (= i0 i1))
              (= (+ (* :i i0) <>)
                 (+ (* :i i1) <>)))
          (-> (* i0 i01) ...)
          (-> (* i1 i01) ...)))
    (2 (% (-> (* (= i0 i1) %:p0
                 (= i0 i1) %:p1)
              (= (+ (* :p0 i0) <>
                    (* i1 :p1) <>)
                 (+ (* :p0 i1) <>
                    (* i0 :p1) <>)))
          (-> (* i01 i01) ...)))))
```

## [note] product-space

- x -
  1. 元素和元素的乘積 記以 (* a b)
  2. 乘積的邊界公式爲 (+ (* [a boundary] b) (* a [b boundary]))
  3. [a boundary] 是一個等式 而 b 是一個 chain

  因此我們需要 等式 與 chain 相 product
  但是如果簡單地把 (* ...) 分配給等式的兩邊
  所得到的等式就不滿足等式的條件了

- k -
  所以邊界公式是錯誤的
  正確的格式應該是 (: (* a b) (= (+ ...) (+ ...)))
  但是我們不能直接用 [a boundary]
  而應該有函數[或語法]來取 [a boundary] 的等式的左右兩邊

- x -
  ``` scheme
  (: a (= (lhs a) (rhs a)))
  (: b (= (lhs b) (rhs b)))
  (: (* a b)
     (= (+ (* (lhs a) b) (* a (rhs b)))
        (+ (* a (lhs b)) (* (rhs a) b))))
  ;; thus
  (: (* i01 i01)
     (= (+ (* (lhs i01) i01) (* i01 (rhs i01)))
        (+ (* i01 (lhs i01)) (* (rhs i01) i01)))
     (= (+ (* i0 i01) (* i01 i1))
        (+ (* i01 i0) (* i1 i01))))
  ```

  但是
  與其說這是一個一般的邊界公式
  不如說這是專門描述 (* i01 i01) 的邊界的

- k -
  ``` scheme
  (: (* (* i01 i01) i01)
     (= (+ (* (lhs (* i01 i01)) i01) (* (* i01 i01) (rhs i01)))
        (+ (* (* i01 i01) (lhs i01)) (* (rhs (* i01 i01)) i01)))
     (= (+ (* (+ (* i0 i01) (* i01 i1)) i01) (* (* i01 i01) i1))
        (+ (* (* i01 i01) i0) (* (+ (* i01 i0) (* i1 i01)) i01)))
     (= (+ (* i0 i01 i01) (* i01 i1 i01) (* i01 i01 i1))
        (+ (* i01 i01 i0) (* i01 i0 i01) (* i1 i01 i01))))

  (: (* i01 (* i01 i01))
     (= (+ (* (lhs i01) (* i01 i01)) (* i01 (rhs (* i01 i01))))
        (+ (* i01 (lhs (* i01 i01))) (* (rhs i01) (* i01 i01))))
     (= (+ (* i0 (* i01 i01)) (* i01 (+ (* i01 i0) (* i1 i01))))
        (+ (* i01 (+ (* i0 i01) (* i01 i1))) (* i1 (* i01 i01))))
     (= (+ (* i0 i01 i01) (* i01 i01 i0) (* i01 i1 i01))
        (+ (* i01 i0 i01) (* i01 i01 i1) (* i1 i01 i01)))
     ;; different from
     (= (+ (* i0 i01 i01) (* i01 i1 i01) (* i01 i01 i1))
        (+ (* i01 i01 i0) (* i01 i0 i01) (* i1 i01 i01))))
  ;; thus (* ...) is not associative
  ```

- x -
  我們需要仔細對比 (* (* I I) I) 與 (* I (* I I))
  因爲上面的二階 (+ ...) 中沒有二階元素相乘細節

  也許這種語法是不對的
  也許我的想法錯了

  但是
  想象一下這樣一個語言
  它可以用來描述拓撲空間
  可以把這些空間當成數據來處理
  可以把這些空間當成類型而取其中的元素
  等等
  我不可能放棄這樣一個語言

- k -
  但是沒有 (+ ...) 的細節是不行的
  對於一階元素可以 但是對於二階元素不行
  想要得到一個一般的乘積元素的邊界規則
  我們就要生成 (+ ...) 的細節

## extension-problem

- An extension-problem is expressed for a partial-map on a subspace.

- To solve an extension-problem
  is to extend a partial map to a total-map step by step,
  while maintain the continuity of the map.

- A partial-map is a map defined on subspace of a space.

- The subspace relation between spaces is encoded by parts.

- Suppose 'A' is a subspace of 'X'
  the following is to extend a partial-map 'g'
  to a total-map 'f'
  ``` scheme
  (let ([g (map partial
             (0 (% (-> A Y)
                   ...))
             ...)])
    (define f
      (map
        (extend-from g)
        (0 (% (-> X Y)
              ...))
        ...)))
  ```

## equality

- With product-space and extension-problem,
  we can define equality between two functions
  as the extension-problem for certain kind of product-space.

- Suppose (: [f0, f1] (-> A B)),
  to proof (~ f0 f1),
  we need to extend a partial-map of type (-> (* A I) B)
  ``` scheme
  (let ([f0 (map partial
                 (0 (% (-> A B)
                       ...))
                 ...)]
        [f1 (map partial
                 (0 (% (-> A B)
                       ...))
                 ...)])
    (map
      (extend-from
        (note 這裏對所 extend 的 partial 的表達是不完全的)
        (map
          (% (-> (* A I) B)
             (-> (* :a i0) [:a f0])
             (-> (* :a i1) [:a f1]))))
      ...))
  ```

- With the equality between functions,
  we can define the equality between space.

- Suppose 'A' and 'B' are two spaces,
  to proof (~~ A B),
  is to find (: f (-> A B)) and (: g (-> B A))
  and to proof (~ [f g] [A id]) and (~ [g f] [B id])
  ``` scheme
  (map (extend-from
         (map (% (-> (* A (0 I)) A)
                 (-> (* :a i0) [:a f g])
                 (-> (* :a i1) :a))))
       (0 (% (-> (* A I) A)
             ...))
       ...)

  (map (extend-from
         (map (% (-> (* B (0 I)) B)
                 (-> (* :b i0) [:b g f])
                 (-> (* :b i1) :b))))
       (0 (% (-> (* B I) B)
             ...))
       ...)
  ```

# subdivision

## sphere and bool suspend

``` scheme
(define sphere-1
  (type space
    (: b <>)
    (: loop (= b b))))

(define bool
  (type space
    (: #f #t <>)))

(define bool-suspend
  (type space
    (: n s <>)
    (: m (-> bool (= n s)))))

(: (+ #f m) (= n s))
(: (+ #t m) (= n s))

(map subdivision
     (0 (% (-> (0 sphere-1) (0 bool-suspend))
           (-> b n)))
     (1 (% (-> (= b b) (= n n))
           (-> loop (+ #f m (- #t m))))))

(define sphere-2
  (type space
    (: b2 <>)
    (: surf (= [b2 refl] [b2 refl]))))

(define bool-suspend-suspend
  (type space
    (: n2 s2 <>)
    (: m2 (-> bool-suspend (= n2 s2)))))

(: (+ n m2) (= n2 s2))
(: (+ s m2) (= n2 s2))
(: (+ #f m m2) (= (+ n m2) (+ s m2)))
(: (+ #t m m2) (= (+ n m2) (+ s m2)))

(map subdivision
     (0 (% (-> (0 sphere-2) (0 bool-suspend-suspend))
           (-> b2 n2)))
     (1 (% (-> (= (+ b2 refl) (+ b2 refl))
               (= (+ n2 refl) (+ n2 refl)))
           (-> surf
               (+ (+ #f m m2 (<= (+ n m2))
                     (- #t m m2) (=< (+ n m2))
                     (+ n2 refl refl) (<= (+ n2 refl)))
                  (>= (+ n2 refl)))))))
```

## ><

``` scheme
(define sphere-1
  (type space
    (0
     (: b <>))
    (1
     (: loop (= b b)))
    (2
     (: [loop refl] (= loop loop)))))

(define sphere-2
  (type space
    (0
     (: b2 <>))
    (1
     (: [b2 refl] (= b2 b2)))
    (2
     (: surf (= [b2 refl] [b2 refl])))
    (3
     (: [surf refl] (= surf surf)))))

(define f
  (map
    (0 (% (-> (0 sphere-2) (0 sphere-1))
          (-> b2 b)))
    (2 (% (-> (= [b2 refl] [b2 refl]) (= [b refl] [b refl]))
          (-> surf (+ loop refl
                      loop refl))))))

(: [loop refl]             (= (+ loop) (+ loop)))
(: [loop refl (<= loop)]   (= (+) (+ (- loop) loop)))
(: [loop refl (=< loop)]   (= (+ (- loop) loop) (+)))


(: c               (= (+ loop) (+ loop)))
(: [c (<= loop)]   (= (+) (+ (- loop) loop)))
(: [c (=< loop)]   (= (+ (- loop) loop) (+)))

(: [surf f]
   (= [b2 refl f] [b2 refl f])
   (= [b2 f refl] [b2 f refl])
   (= [b refl] [b refl]))


(define sphere-3
  (type space
    (0
     (: b3 <>))
    (1
     (: [b3 refl] (= b3 b3)))
    (2
     (: [b3 refl refl] (= [b3 refl] [b3 refl])))
    (3
     (: cell (= [b3 refl refl] [b3 refl refl])))))

(define non-continuous-map
  (map
    (0 (% (-> sphere-3 sphere-2)
          (-> b3 b2)))
    (3 (% ...
          (-> cell (+ surf refl))))))

(: [cell f]
   (= [b3 refl refl f] [b3 refl refl f])
   ==>
   (= [b3 f refl refl] [b3 f refl refl])
   (= [b2 refl refl] [b2 refl refl])
   =/=
   [surf refl boundary]
   ==>
   (= surf surf))

(define non-essential-map
  (map
    (0 (% (-> sphere-3 sphere-2)
          (-> b3 b2)))
    (3 (% ...
          (-> cell (+ surf refl (<= surf)
                      (- surf refl) (=< surf)
                      (><><>< b2 refl refl refl)
                      (note (add-to-both-sides-of-the-equation-to-the-left
                             b2 refl refl))))))))

(note
  (: [surf refl (<= surf)]     (= (+) (+ (- surf) surf)))
  (: [(- surf refl) (=< surf)] (= (+ (- surf) surf) (+))))

(note non-essential
      because???
      (equivalent
       (+ (><><>< b2 refl refl refl))
       (+ surf refl (<= surf)
          (- surf refl) (=< surf)
          (apply surf refl refl (<= ...))
          (><><>< b2 refl refl refl))))

(define essential-map
  (map
    (0 (% (-> sphere-3 sphere-2)
          (-> b3 b2)))
    (3 (% ...
          (-> cell (+ surf refl (<= surf)
                      surf refl (=< surf)
                      (><><>< b2 refl refl refl)))))))

(note how to define essentialness?
      the definition must explain both dehn-twist
      and the essentialness of the essential-map above

      if we define essentialness
      to allow the above map be viewed as essential
      then not only (-> sphere-3 sphere-2)
      there will also be (-> sphere-2 sphere-1)

      if it is something at all
      it is not hopf-map)
```

# different ways to glue a cube

``` jojo
(define 3-torus
  (type space
    (: a0 <>)
    (: b0 b1 b2 (= a0 a0))
    (: c0 (= (+ b0 b1) (+ b1 b0)))
    (: c1 (= (+ b0 b2) (+ b2 b0)))
    (: c2 (= (+ b1 b2) (+ b2 b1)))
    (: d0 (= (+ c0 (=< b1)  (:> (= (+ (- b1) b0 b1)
                                   (+ b0)))
                c1 (>= b2)  (:> (= (+ b0)
                                   (+ b2 b0 (- b2))))
                glue        (:> (= (+ (- b1) b0 b1)
                                   (+ b2 b0 (- b2))))
                (<= (- b1)) (:> (= (+ b0 b1)
                                   (+ b1 b2 b0 (- b2))))
                (=> (- b2)) (:> (= (+ b0 b1 b2)
                                   (+ b1 b2 b0)))
                (=> b0)     (:> (= (+ b0 b1 b2 (- b0))
                                   (+ b1 b2)))
                c2 glue     (:> (= (+ b0 b1 b2 (- b0))
                                   (+ b2 b1)))
                (>= (- b0)) (:> (= (+ b0 b1 b2)
                                   (+ b2 b1 b0))))
             (+ c2 (=> b1)  (:> (= (+ b1 b2 (- b1))
                                   (+ b2)))
                c1 (<= b0)  (:> (= (+ b2)
                                   (+ (- b0) b2 b0)))
                glue        (:> (= (+ b1 b2 (- b1))
                                   (+ (- b0) b2 b0)))
                (>= (- b1)) (:> (= (+ b1 b2)
                                   (+ (- b0) b2 b0 b1)))
                (=< (- b0)) (:> (= (+ b0 b1 b2)
                                   (+ b2 b0 b1)))
                (=< b2)     (:> (= (+ (- b2) b0 b1 b2)
                                   (+ b0 b1)))
                c0 glue     (:> (= (+ (- b2) b0 b1 b2)
                                   (+ b1 b0)))
                (<= (- b2)) (:> (= (+ b0 b1 b2)
                                   (+ b2 b1 b0))))
             (+ c2          (:> (= (+ b1 b2)
                                   (+ b2 b1)))
                (=< b2)     (:> (= (+ (- b2) b1 b2)
                                   (+ b1)))
                c0          (:> (= (+ b0 b1)
                                   (+ b1 b0)))
                (<= b0)     (:> (= (+ b1)
                                   (+ (- b0) b1 b0)))
                glue        (:> (= (+ (- b2) b1 b2)
                                   (+ (- b0) b1 b0)))
                (<= (- b2)) (:> (= (+ b1 b2)
                                   (+ b2 (- b0) b1 b0)))
                (=> b0)     (:> (= (+ b1 b2 (- b0))
                                   (+ b2 (- b0) b1)))
                (=> b1)     (:> (= (+ b1 b2 (- b0) (- b1))
                                   (+ b2 (- b0))))
                c1          (:> (= (+ b0 b2)
                                   (+ b2 b0)))
                (<= b0)     (:> (= (+ b2)
                                   (+ (- b0) b2 b0)))
                (=> b0)     (:> (= (+ b2 (- b0))
                                   (+ (- b0) b2)))
                glue        (:> (= (+ b1 b2 (- b0) (- b1))
                                   (+ (- b0) b2)))
                (>= (- b1)) (:> (= (+ b1 b2 (- b0))
                                   (+ (- b0) b2 b1)))
                (>= (- b0)) (:> (= (+ b1 b2)
                                   (+ (- b0) b2 b1 b0)))
                (=< (- b0)) (:> (= (+ b0 b1 b2)
                                   (+ b2 b1 b0))))))))
(note
  without (:> ...))

(define 3-torus
  (type space
    (: a0 <>)
    (: b0 b1 b2 (= a0 a0))
    (: c0 (= (+ b0 b1) (+ b1 b0)))
    (: c1 (= (+ b0 b2) (+ b2 b0)))
    (: c2 (= (+ b1 b2) (+ b2 b1)))
    (: d0 (= (+ c0 (=< b1)
                c1 (>= b2)
                glue
                (<= (- b1))
                (=> (- b2))
                (=> b0)
                c2 glue
                (>= (- b0)))

             (+ c2 (=> b1)
                c1 (<= b0)
                glue
                (>= (- b1))
                (=< (- b0))
                (=< b2)
                c0 glue
                (<= (- b2)))
             ;; or
             (+ c2
                (=< b2)
                c0
                (<= b0)
                glue
                (<= (- b2))
                (=> b0)
                (=> b1)
                c1
                (<= b0)
                (=> b0)
                glue
                (>= (- b1))
                (>= (- b0))
                (=< (- b0)))))))


(note [1] =/= [2]
      [2] === [3]

      [1] (+ c0 (=< b1)
             c1 (>= b2)
             glue (<= (- b1)) (=> (- b2)) (=> b0)
             c2
             glue (>= (- b0)))

      [2] (+ c2 (=> b1)
             c1 (<= b0)
             glue (>= (- b1)) (=< (- b0)) (=< b2)
             c0
             glue (<= (- b2)))

      [3] (+ c2 (=< b2)
             c0 (<= b0)
             glue (<= (- b2)) (=> b0) (=> b1)
             c1 (<= b0) (=> b0)
             glue (>= (- b1)) (>= (- b0)) (=< (- b0))))
```

- x -
  用移項來描述 glue 其語法可能太不經濟了

- k -
  我們之前想的是利用這種底層的語言
  能夠更容易地定義 [chain 之間的]等價關係
  但是現在發現可能並非如此

- x -
  首先我們假設後兩個 chain 是等價的
  然後找找語法來揭示其等價

- k -
  語義能夠解決等價問題嗎?
  二者在語義上爲什麼是等價的?
  爲什麼前二者又是不等價的?

------

- x -
  R3 就是 T3 的 universal-covering-space
  而且 R3 的 2-skeleton 就是 T3 的 2-skeleton 的 universal-covering-space
  儘管這個 universal-covering-space 中有二階洞
  但是這個 covering 好像是足以判斷 (2 T3)
  這個二階代數中的 2-chain 之間的等價關係了

# covering-space

- x -
  用 covering-space 來解釋二階鏈之間的等價關係?
  其 2-skeleton 的 covering-space 是什麼?

  先試試 covering-space of circle
  這是我能想到的最簡單的例子了

  ``` scheme
  (define S1
    (type space
      (0 (: a0 <>))
      (1 (: b0 (= a0 a0)))))

  (in S1
    (: a0 S1)
    (: b0
       (= a0 a0))
    (: (+ b0 b0 b0)
       (= a0 a0))
    (: (+ b0 b0 (- b0))
       (= a0 a0)))


  (define R1
    (type covering-space
      (0 (: a0 <>)
         (: #b0 (-> <> <>)))
      (1 (: b0 (-> (: :a <>)
                   (= :a [:a #b0]))))))

  (in R1
    (: a0 R1)
    (: [a0 #b0] R1)
    (: [a0 #b0 #b0 (- #b0)] R1)
    (: [a0 #b0 #b0 b0] (= [a0 #b0 #b0] [a0 #b0 #b0 #b0])))


  (define R1-covers-S1
    (map
      (0 (% (-> R1 S1)
            (-> a0 a0)
            (-> [:a #b0] a0)))
      (1 (% (-> (= :a [:a #b0]) (= a0 a0))
            (-> [:a b0] b0)))))
  ```

- k -
  第二簡單的是 covering-space of torus

  ``` scheme
  (define T2
    (type space
      (0 (: a0 <>))
      (1 (: b0 b1 (= a0 a0)))
      (2 (: c0 (= (+ b0 b1) (+ b1 b0))))))

  (define R2
    (type space
      (0 (: a0 <>)
         (: #b0 #b1 (-> <> <>)))
      (1 (: b0 (-> (: :a <>)
                   (= :a [:a #b0])))
         (: b1 (-> (: :a <>)
                   (= :a [:a #b1]))))
      (2 (: c0 (-> (: :a <>)
                   (= (+ :a b0 :a #b0 b1)
                      (+ :a b1 :a #b1 b0)))))))
  ```

- x -

  ``` scheme
  (define 2-skeleton-of-3-torus
    (type space
      (: a0 <>)
      (: b0 b1 b2 (= a0 a0))
      (: c0 (= (+ b0 b1) (+ b1 b0)))
      (: c1 (= (+ b0 b2) (+ b2 b0)))
      (: c2 (= (+ b1 b2) (+ b2 b1)))))

  (type space
    (0 (: a0 <>))
    (1 (: b0 b1 b2 (= a0 a0)))
    (2 (: c0 (= (+ b0 b1) (+ b1 b0)))
       (: c1 (= (+ b0 b2) (+ b2 b0)))
       (: c2 (= (+ b1 b2) (+ b2 b1)))))

  (type covering-space
    (0 (: a0 <>)
       (: b0 b1 b2 (-> <> <>)))
    (1 (: c0 (-> (+ b0 b1) (+ b1 b0)))
       (: c1 (-> (+ b0 b2) (+ b2 b0)))
       (: c2 (-> (+ b1 b2) (+ b2 b1)))))
  ```

# 三維空間之間的映射

# neighborhood 與 幾何 [recover geometry]

- x -
  如果想要引入 neighborhood 的概念
  那麼是否也要同時引入 幾何 呢?
  如果要的話
  那麼所有的線都應該理解爲空間中的直線[測地線]
  而所有的面都應該理解爲空間中的什麼?

- k -
  使用 neighborhood 的意義在於有限覆蓋定理
  但是我們所定義 neighborhood 的方式
  目前只是爲了把它聯繫於 用邊界所定義的連續性

- x -
  我想我們必須引入幾何了

- k -
  我想你之所以這樣說
  是因爲當考慮到有限覆蓋的時候
  就像是給了圖形以座標系

------

- 假設使用降維的 neighborhood
  在取點的 neighborhood 的時候
  這一點在邊中的所有出現都將稱爲新的點

  具體的規則與語法是什麼?

# >< 忘記高階代數結構中的某些信息就得到同調理論

## 引

- 如何解釋 S2 的三階同倫羣爲非平凡羣?
  一個非平凡羣的 abelianization 可以是平凡羣嗎?

- 只能解釋爲 兩個平凡等式 可能不等價

## 規則

- 說 homology group 是 高階代數的 abelianization
  首先要取 高階代數的 close chain

- 經過 abelianization 之後
  邊界爲 0 的 k 階生成子是 k 階同調羣的元素
  k+1 階生成子給出 k 階同調羣中元素之間的等式

  - 只取生成子是不夠的
    因爲不閉的生成子 可能複合爲閉的元素

- 高階代數中的 chain 經過 abelianization
  不能給出 homology theory 中的所有 chain

- 也可以在簡化的時候不打散所有的碎片
  保持其中部分相連

## 例子

``` scheme
(: abelianization (-> space homology-space))

(note homology-space reuse syntax such as (: ...) and (+ ...))

(define S2
  (type space
    (: a0 a1 <>)
    (: b0 (= a0 a1))
    (: b1 (= a1 a0))
    (: c0 (= (+ b0 b1) (+ b0 b1)))))

(homological-equivalent
 (abelianization S2)
 (type homology-space
   (: a0 a1 <>)
   (: b0 (+ a0 (- a1)))
   (: b1 (+ a1 (- a0)))
   (: c0 (+))))

(note S2 :: Z 0 Z)

(define P2
  (type space
    (: a0 a1 <>)
    (: b0 (= a0 a1))
    (: b1 (= a1 a0))
    (: c0 (= (+ b0 b1) (+ (- b1) (- b0))))))

(homological-equivalent
 (abelianization P2)
 (type homology-space
   (: a0 a1 <>)
   (: b0 (+ a0 (- a1)))
   (: b1 (+ a1 (- a0)))
   (: c0 (+ b0 b1 b1 b0))))

(note P2 :: Z Z/2 0)

(define T2
  (type space
    (: a0 <>)
    (: b0 b1 (= a0 a0))
    (: c0 (= (+ b0 b1) (+ b1 b0)))))

(homological-equivalent
 (abelianization T2)
 (type homology-space
   (: a0 <>)
   (: b0 b1 (+))
   (: c0 (+))))

(note T2 :: Z Z+Z Z)

(define K2
  (type space
    (: a0 <>)
    (: b0 b1 (= a0 a0))
    (: c0 (= (+ b0 b1) (+ b1 (- b0))))))

(homological-equivalent
 (abelianization K2)
 (type homology-space
   (: a0 <>)
   (: b0 b1 (+))
   (: c0 (+ b0 b0))))

(note K2 :: Z Z+Z/2 0)
```

# 獲得拓撲不變量

## 引

- 取閉合的鏈
  是否就能簡化代數結構而得到能力更強的拓撲不變量呢?

  我想問題應該劃歸爲
  當限制 代數中的元素爲 閉合的鏈時
  可否得到對這些元素的更高效的編碼?
  能否把這個代數結構化簡到正規形式?

  曲面的分類定理 可否被表達爲 對這個代數結構的化簡?
  一維的時候 我們可以把這個代數結構化簡成基本羣
  [我們要形成等價的定理 但是表達方式已經不一樣了]
  二維的時候 二階同倫羣是不夠的
  只考慮零虧格的曲面嵌入三維空間是不夠的
  還需要考慮高虧格的曲面嵌入三維空間
  三維空間中可能有高虧格的洞

## 基本羣的另一種表示方式

- 我們可以說兩個圈相加
  而不必說兩個圈相加而得到一個圈

## 簡化代數結構的方法

- 同倫的閉合鏈產生與對代數結構的過度細分
  找出同倫的鏈然後融合細分中的部分
  就能化簡代數結構

# >< 覆蓋空間 與 元素的等價問題

- x -
  fundamental-group of surface =
  group of covering-transformation of universal-covering-space of surface
  這個等價如何推廣到高維代數?

- k -
  首先我們看到推廣的可能的時候
  並不是從這個角度考慮的
  這在於 path 的邊界 形成一個點到點的[可逆]映射
  而 disk 的邊界 不能形成點到點的映射
  它所形成的是 很多邊之間的[可逆]關係

  我們想的其實還不是 點到點的可逆變換
  而是 給定 起點的時候 path 與終點對應 [降維]

- x -
  groupoid 也可以 acting on covering-space
  只不過 covering-transformation 要加上類型

  branched-covering 使得 path lifting 不唯一

  帶有 一階洞 的 covering
  不同的 loop 經過 lifting
  可能給出 covering-space 中的同一個終點
  [因爲 covering-space 中可能有不可收縮的 loop]

  也就是說
  一個使得點變多
  一個使得點變少
  只有 universal-covering-space 的點是與 path 一一對應的

# >< 用沒有一階洞的一階無窮複形覆蓋一般一階複形

## covering-space 與 universal-covering-space

- 對於 2-man 觀察沒有 normalization 的條件下 獲得 universal-covering-space
  對於 3-man 嘗試獲得 universal-covering-space

# >< 用沒有二階洞的二階無窮複形覆蓋一般二階複形

- 此時有必要使用 branched-covering
  我們可以試試找一個 branched-covering of torus
  看看這個 branched-covering 能否解決 torus 的字問題

# >< 曲面分類定理的形式化

## normalization

- normalization 減少元素的數量
  這與遞歸計數剛好相反
