---
title: A textbook of topology
subtitle: Lehrbuchder topologie
author: Herbert Seifert
year: 1934
newline: preserve
---

# 1 例子 / illustrative material

## 1 拓撲學的基本問題 / the principal problem of topology

- 拓撲學的研究對象是幾何圖形
  所研究的幾何圖形的性質 是其在拓撲映射下不變的性質

  所謂 拓撲映射
  就是 連續的一一映射 並且要求其逆映射也是連續的

  所謂 幾何圖形
  是三維或者更高維空間之中的點集

  - x -
    在描述點集時
    我們可以侷限於如下的方法
    幾何圖形 被分爲一片一片的
    對其中的每一片
    我們給以其參數表示
    片與片之間的關係需要被加以限定
    限定方式可能多種多樣
    因此具體如何限定
    先暫且不談

- 連續函數只需要定義於幾何圖形之中的點
  而不需要定義於幾何圖形所在的整個空間中的所有點

- 在拓撲映射下不變的性質
  稱爲拓撲性質

- 兩個圖形
  如果可以被某個拓撲映射變換爲另一個
  就稱這兩個圖形同胚

- 如果在直覺上
  一個圖形經過扭曲和形變可以變爲另一個
  那麼二者就是同胚的

- 想要發現很多很多同胚的圖形很容易
  然是想要證明兩個圖形不同胚則很難

- 正如全等的圖形在歐氏幾何中被認爲是沒有本質上的區別的
  同胚的圖形在拓撲學中也被認爲是沒有本質上的區別的

  兩個全等的圖形總是可以通過一個整個空間的疊合變換而被重疊
  但是 可能沒有整個空間的同胚變換來重疊兩個同胚的圖形

  比如
  在三維空間中
  結 與 圈 同胚
  不經扭曲的閉合的帶 與 莫比烏斯帶 同胚
  但是這些同胚都不能擴展到整個空間到自身的拓撲映射

  然而
  當把這些圖形嵌入在四維空間中時
  這些同胚卻又可以擴展到整個空間到自身的拓撲映射了

  如此更可以看出 同胚 這個等價關係
  是就圖形的點而言的
  而不是就圖形所在的空間而言的

- 因此我們必須區分兩種拓撲性質
  一種是內蘊於圖形本身的拓撲性質
  另一種拓撲性質 是關於圖形如何被放置於外在的空間之中的
  後一種拓撲性質 在外在空間的拓撲映射下不變

  在觀察圖形與外在空間的關係時
  所得到的一些差異
  在考慮內蘊的拓撲時
  可能就不復存在了

- 這裏所區分的 內蘊性質與外在性質
  在微分幾何中同樣也會出現
  三維空間中曲面的內蘊度量性質由其第一基本形式決定 [first fundamental form]
  而外在的度量性質由其第二基本形式決定 [second fundamental form]

- 拓撲學的基本問題是
  判斷兩個圖形是否同胚
  並且分類列舉所有不同胚的圖形

  儘管人們發展出了很多理論來處理歐氏空間的任意子空間
  但是我們將限制我們討論的對象
  以避免集合論所帶來的困難

  我們首先限制我們的研究對象於
  布勞威爾[Brouwer] 所引入的 復形[complex]
  然後進一步侷限於 流形[manifold]

- 復形之有別於任意點集所構成的圖形
  在於它們可以被 三角化 [或者說單形化]

  與其說對一個已有的空間進行三角化
  也可以說是利用單形來構造復形
  這後一種構造是不依賴於外在空間而內蘊的

  - x -
    因此 作者在這裏考慮的是 简单復形[simplicial complex]

- 可三角化 這個性質
  將爲我們排除點集拓撲中的很多病態情形

  儘管復形的拓撲學被戲稱爲 '橡皮糖拓撲學'
  但是其實經過這樣的限制之後
  我們恰恰可以把精力集中於具有更強幾何意義的研究對象

- 在嘗試研究拓撲學的基本問題的過程中
  人們找到了很多關於復形的拓撲不變量
  利用這些不變量 我們可以區分不同胚的復形

  其中最重要的兩個不變量
  是復形的同調羣與基本羣
  這將是我們研究的核心

- 然而我們現在卻要試試
  在不利用這兩個不變量的情形下
  來研究研究拓撲學的基本問題

  - x -
    因此我們目前的主要工具就是直覺與想象

  首先我們問
  不同胚的閉曲面都由哪些?

## 2 閉曲面 / closed surfaces

- 我們可以通過將多邊形的邊粘合在一起來構造曲面
  這個多邊形稱爲 基本多邊形[fundamental polygon]
  由此而構造的曲面的所有內蘊拓撲性質
  爲多變形的粘合方式所完全確定

  然而此時
  空間的度量是不確定的
  因此曲面的面積是不確定的
  曲面嵌入外部空間的方式也是不確定的
  曲面的拓撲學並不關心這些性質

- ><

- 我們還不確定如上列出的閉曲面代表了所有的閉曲面
  之後我們將利用正規化算法來證明
  所有的閉曲面[二維閉流形] 都可以被化爲如上所列出的一種 [38 小節]

  其次
  我們還需要證明如上生成的閉曲面相互不同胚
  這需要先明白同調羣和同倫羣之類的代數不變量才行
  [39 小節]

## 3 同痕 同倫 同調 / isotopy, homotopy, homology

- 證明流形之不同胚 就是分類流形
  我們的方式是形成不變量
  而形成不變量的方式是
  去分類低維圖形到這些流形之中的映射

- 首先我們處理的是閉曲面
  首先考慮的映射是 曲線到曲面的映射

- ><

## 4 高維流形 / higher dimensional manifolds

# 2 简单複形 / simplicial complexes

## 5 鄰域空間 / neighborhood spaces

## 6 映射 / mappings

## 7 歐氏空間中的點集 / point sets in euclidean spaces

## 8 等價空間 / identification spaces

## 9 n 階單形 / n-simplexes

- m 維仿射空間中的 n 維幾何單形
  定義爲 線性獨立的 n 的點的仿射組合所能描述的子空間
  確定了這 n 個點 就確定了 幾何單形
  並且 n 個點的順序不重要

- 拓撲意義上的單形 被理解爲
  嵌入任意拓撲空間中的 歐氏空間中的幾何單形

- 當指定單形的頂點列表之順序時
  就給出了單形的定向[orientation]
  頂點的偶置換[even permutation] 給出相同的定向

  這中定義可以解釋爲
  單形頂點的置換確定一個單形到自身的線性映射
  這個線性映射的行列式是正或負的 取決於置換是偶或奇

  進一步可以解釋爲

  | 一維定向 | 由兩點確定 | 行進的方向 |
  | 二維定向 | 由三點確定 | 旋轉的方向 |
  | 三維定向 | 由四點確定 | 螺旋的方向 |

## 10 简单複形 / simplicial complexes

- 简单複形是由單形構造的鄰域空間
  在之後我們只討論這種空間
  2 3 4 5 7 8 11 章討論一般的複形
  6 9 10 章 討論流形[特殊的複形]

- 說簡單複形是由單形構造的鄰域空間
  也可以說 簡單複形是能夠被細分爲單形的鄰域空間
  這樣的細分定義如下 :

  設一個單形的列表
  其基數爲 有限 或 可數無窮
  並且這個列表就 取單形的面 而言封閉

  - (k1) 每一點至少在一個單形中
  - (k2) 每一點只在有限個個單形中
  - (k3) 兩個單形的關係只有三種
    1. 不交
    2. 一個是另一個的面
    3. 二者相交於一個[且只能是一個]公共的面
  - (k4)
    - 如何簡化這個條件?
      這與就點取鄰域有關 它可以排除某些無窮複形
      這些無窮複形中點的組合鄰域是點自身

- [關於術語]
  複形[complex] 是能夠被細分爲簡單複形的空間
  簡單複形[simplicial-complex] 是複形與某個具體的細分

  (k2) 和 (k4) 與下面的條件等價
  (k4') 任意一點都有鄰域 這個鄰域只與有限個單形相交

## 11 简单複形的概形 / the schema of a simplicial complex

- 一個簡單複形中的兩個單形的頂點不能完全等同
  因爲 (k3) 對兩個單形相交的情形有很強的限制

  根據這一性質
  在簡單複形中
  我們可以用 一個單形的頂點的列表 來表示這個單形 而沒有歧義
  從而整個複形就也可以用一些頂點的列表的列表來表示了

  這就是簡單複形的概形[schema]

  另外一種 概形[incidence matrices] 將在之後定義

- 反過來我們觀察 頂點的列表的列表
  我們稱頂點的列表爲單形 [也就是說術語要重載了]
  所以也可以說是觀察 單形的列表
  只要滿足簡單的條件 它們就能被看成是 簡單複形的概形
  - (sch1) 單形的列表就取子列表而言封閉
    取子列表 就是 取單形的面
  - (sch2) 單形中的頂點不重複
  - (sch3) 每個頂點只出現在有限個單形中

- 概形可以幫助我們編碼簡單複形
  我們也可以利用概形的抽象性來推廣簡單複形的概念

  純粹的組合拓撲學直接以概形爲研究對象
  並且用一些組合變換來定義概形之間的等價關係

## 12 有限 純 均一 複形 / finite, pure, homogeneous complexes

## 13 正規細分 / normal subdivision

## 14 複形的例子 / examples of complexes

# 3 同調羣 / homology groups

## 引 / intro

- 這一章是純粹組合學的
  也就是說 我們不使用 鄰域 和 [用鄰域定義的]連續性 這些概念
  也就是說 我們可以把簡單複形看成是純粹形式化的概形

  這一章中我們定義簡單複形的同調羣
  下一章中我們證明
  一個空間的任意兩個單形劃分 所給出的同調羣 是同構的

## 15 鏈 / chains

- n 維簡單複形中的 k 階鏈[k-chain]
  是 k 單形 的形式線性和 其係數爲整數
  整數的正負可以被解釋爲定向[orientation]
  整數的大小可以被解釋爲重複出現的次數[multiplicity]

  這樣形成的代數結構是一個 abel 羣
  其生成子爲 k 階單形
  且每個生成子都是無限階的
  (m * U = 0) -> (U = 0) or (m = 0)

## 16 邊界 閉鏈 / boundary, closed chains

- 一個帶有定向的 k 階單形
  可以誘導出其個階面的定向
  利用這裏誘導出來的定向
  我們可以定義 k 階單形 的邊界爲 其 (k-1) 階面的和
  並且每個面的係數爲這個誘導出來的定向

- '取邊界' 是 abel 羣之間的線性算子[羣同態]

- 邊界爲零的鏈稱 閉鏈[closed-chain]
  閉鏈形成子羣

- 可以簡單地證明 k 階鏈 的邊界 總是 閉的 k-1 階鏈
  或者說 邊界算子的平方是零
  或者說 邊界鍊 是 閉鍊 的子羣

## 17 同調鏈 / homologous chains

- 每個邊界鏈都是閉鏈
  但是一個閉鏈不一定是邊界鏈

  考慮 k 階閉鏈 所形成的 abel 羣
  其中爲 k+1 階鏈的邊界者
  也就是之前我們所說的邊界鏈
  稱爲零同調的[null homologous]
  記爲 (U ~ 0)

  兩個閉的或非閉的鏈稱爲同調的[homologous]
  如果它們的差是零同調的
  記爲 (U ~ V) := (U - V ~ 0)

- 鏈之間的同調[homology]關係 是一個等價關係
  同調的閉鏈 屬於同一個同調類[homology class]
  m 與 U 不等於 0 的時候 也可能有 (m * U ~ 0)

## 18 同調羣 / homology groups

- 羣論的語言中
  同調類是 閉鏈模零同調鏈 的剩餘類[residue class]
  剩餘類 形成商羣
  這個商羣就稱爲同調羣

- 當考慮某個具體的單形細分時
  取這樣的商羣看似多此一舉
  看似 不如直接考慮所有的鏈所形成的代數結構
  但是 之後我們將要證明 這個商羣是不依賴與劃分的
  即 同調羣是拓撲不變量

- It is important that
  the k-th homology group is not only an abstract group
  determined by its Betti number and torsion coefficients,
  but also that it is realizable by a homology basis.

## 19 計算同倫羣的簡單例子 / computation of the homology groups in simple cases

## 20 homologies with division

## 21 computation of homology groups from the incidence matrices

## 22 block chains

## 23 chains mod 2, connectivity numbers, euler's formula

## 24 pseudomanifolds and orientability

# 4 simplicial approximations

## 25 singular simplexes

## 26 singular chains

## 27 singular homology groups

## 28 the approximation theorem, invariance of simplicial homology groups

## 29 prisms in euclidean spaces

## 30 proof of the approximation theorem

## 31 deformation and simplicial approximation of mappings

# 5 local properties

## 32 homology groups of a complex at a point

## 33 invariance of dimension

## 34 invariance of the purity of a complex

## 35 invariance of boundary

## 36 invariance of pseudomanifolds and of orientability

# 6 surface topology

## intro

- The principal problem of topology,
  the homeomorphism problem,
  can be solved in dimension 2,
  using methods which cannot be generalized to higher dimensions.

  For this reason we shall develop surface topology
  independently of our previous results,
  which were valid for arbitrary dimension.

  We shall start with polygons,
  instead of with a simplicial complex,
  and we shall construct closed polyhedral surfaces
  by identifying sides of polygons.

## 37 closed surfaces

- [manifold-checker]
  We first devote our attention only to systems of polygons
  for which the total number of sides is even
  and each side is paired with exactly one otherside
  by the topological mapping.

- [topological feeling [neighborhood space]]
  In the system of polygons,
  points which map into one another
  are to be considered equivalent
  in the sense that
  points in a neighborhood space are equivalent.

- [combinatorial natural of a system of polygons]
  We can then recognize the following classes of equivalent points
  in a system of polygons :
  - an inner point of a polygon is equivalent only to itself;
  - an inner point of a polygonal side
    is equivalent to exactly one other point;
  - a vertex may be equivalent to one, several, or even no other points.

- [connectedness condition]

- system of polygons -> closed surfaces

- In the treatment which follows,
  our interest shall be not in the particular system of polygons
  but, rather, in the surface which it determines.

  We now set ourselves the task of discovering
  when two systems of polygons determine the same surface,
  that is, finding when the polyhedral surfaces
  produced by the identification of equivalent points
  are different polygonal decompositions of the same surface.

  [we solve this by normalization]

- [orientation]
  We now orient the polygonal sides.
  That is, we choose one of the two boundary points of each side
  as initial point
  and the other as endpoint of the side.

  - x -
    such orientation is nothing but maintain the information
    of different positions in the boundary of a n-disk.

  - k -
    but a polygon only has two orientations.

    while so much information are maintained,
    are they really the information about orientation?

- The orientation of the polygon
  will determine a sense of traversal of the boundary,
  that is, a cyclic ordering of the sides.

  [is this the same for polyhedron?]

## 38 transformation to normal form

### step 1: one fundamental polygon

### step 2: side cancellation

### step 3: transformation to a polyhedral surface having a single vertex

### step 4: cross-cap normalization

### step 5: handle normalization

### step 6: transformation of the handles into cross-caps

## 39 types of normal form: the principal theorem

## 40 surfaces with boundary

## 41 homology groups of surfaces

# 7 the fundamental group

## 42 the fundamental group

## 43 examples

## 44 the edge path group of a simplicial complex

## 45 the edge path group of a surface complex

## 46 generators and relations

## 47 edge complexes and closed surfaces

## 48 the fundamental and homology groups

## 49 free deformation of closed paths

## 50 fundamental group and deformation of mappings

## 51 the fundamental group at a point

## 52 the fundamental group of a composite complex

# 8 覆蓋複形 / covering complexes

## intro

- fundamental-group of surface
  對應於 group of covering-transformation of universal-covering-space of surface
  其他的 unbranched-covering 對應與 fundamental-group 的 subgroup
  這樣找到了所有的 subgroup 就能找到所有的 unbranched-covering

  - x -
    但是找到了所有的 unbranched-covering 又有什麼用呢?
    這裏的敘述 好像顯得 topology 空間的基本羣 這個代數不變量
    可以用來解決有關 covering 的問題
    但是其實 covering 的問題本身就沒什麼用

    相反
    max dehn 的觀點是
    這裏我們是在用 covering
    來解決 空間的基本羣 這類特殊的羣 之中 兩個元素的相等問題
    並且獲得一種代數語言來描述 某些拓撲顯現

    我覺得 dehn 的觀點更爲可取

## 53 無分支的覆蓋複形 / unbranched covering complexes

- (: g (-> E B)) is unbranched-covering :

  [條件 1] g 是滿射

  [條件 2] [nonbranching condition]
  g is locally topological
  ``` scheme
  (-> [(: :b B) (: :e E) (= [:e g] :b)]
      [g is (homeomorphic (neighborhood :e) (neighborhood :b))])
  ```

  [條件 3] [nonbounding condition]
  如果 E 中的一點 x 被映到 b 的鄰域中的一點
  那麼 x 一定在 b 的某個逆像 e 的鄰域中
  ``` scheme
  (-> [(: :x E) (: :b B) (: [:x g] (neighborhood :b))]
      [(: :e E) (= [:b g] :e) (: :x (neighborhood :e))])
  ```

  - x -
    說 [條件 3] 是爲了 path lifting property
    我覺得也可以直接以 path lifting property 作爲公理
    [這樣將更弱]
    或者說 neighborhood lifting property?

    我們可以總爲
    [條件 2] 說 neighborhood mapped to neighborhood topologically.
    [條件 3] 說 path can be lifted to path respecting the covering map.

- ><
  這裏的例子可以用來實驗 normalization 算法

## 54 基本路徑 與 覆蓋路徑 / base path and covering path

- path lifting property
  ``` scheme
  (: g (-> E B))
  (-> [(: :w (0 :a :b)) (: [:a, :b] B) (: :a~ E) (= [:a~ g] :a)]
      [(: :w~ (0 :a~ :b~)) (: :b~ E) (= [:w~ g] :w)])
  ;; uniqueness of the output path :w~ is naturally not required
  ```

- x -
  這裏敘述並根據上面的定義證明了
  path lifting property 與
  homotopy lifting property
  我想 [條件 2] 並不會影響這裏的證明
  也就是說 沒有必要要求 unbranch

  branch 會導致 lifting 不唯一
  但是這可能並不影響我們對 covering 的使用

  對 unbranch 的要求可能出現在下一節
  尋找 fundamental-group 的 subgroup 與 covering-space 的對應關係的時候

- k -
  我們還應該看一下這裏對 neighborhood 的使用方式
  能否被容納到我們的形式語言中

- x -
  我發現 neighborhood 的概念非常重要
  我們需要仔細把它形式化

## 55 覆蓋 與 基本羣的子羣 / coverings and subgroups of the fundamental group

- induce
  ``` scheme
  (: g (-> E B))
  ;; induce
  (: [g homotopy-induce] (-> [E fundamental-group] [B fundamental-group]))
  ;; [g homotopy-induce] is group homomorphism
  ```

- homotopy lifting property 保證了這個 [g homotopy-induce] 是單的
  即 兩條不同倫的 path 不會被映射爲兩條同倫的 path
  因爲是 單的
  所以 [E fundamental-group] 可以被看成是 [B fundamental-group] 的 subgroups

  注意
  只有選定了 B 中的一點 還有這一點在 E 中 lifted 的一點時
  才能有上面的 induce

- 有了 subgroup 之後
  我們可以把 [B fundamental-group] 分解爲陪集的並
  然後給這些分解以拓撲解釋
  [B fundamental-group] 中的元素可以看成是選定了某個點作爲起點和終點之後
  所有 loop 的集合
  loop 之間的等價 取 同倫等價

  [E fundamental-group] 所對應的 [B fundamental-group] 的子羣
  是那些被 lift 成 E 中閉曲線 [即 loop] 的 B 中的 loop
  這個子羣的陪集是那些 被 lift 成 E 中有相同終點的 path 的 B 中的 loop

- ><
  注意
  covering 所對應的是 a class of conjugate subgroup
  而不是一個 subgroup

- x -
  既然這裏的敘述都是對 subgroup 而言的
  那麼 高階代數 的 sub-代數是什麼呢?

  就羣的展示而言
  重新選取生成子 [關係的變化可以 reduce 出來]
  就算是一個子羣了

  對於高階代數的展示
  是否也有類似的理論?
  重新選取生成子 就得到子代數?

## 56 通用覆蓋 / universal coverings

- 任意 complex 都有 universal-covering-space
  其構造
  可以看成是把 path-space 化爲 simple-space
  化爲 simple-space 可以看成是找一個編碼的過程
  化爲 simple-space 本身還不算是一個編碼
  要使得同倫的 loop 有相同的編碼
  才算是找到了編碼

- 那麼對應於 subgroup 的中間 covering-space 應該如何構造呢?

## 57 正規覆蓋 / regular coverings

## 58 the monodromy group

# 9 三維流形 / 3-dimensional manifolds

## 59 一般準則 / general principles

- complex 成爲 manifold 的條件是 homogeneous

  A 3-dimensional closed manifold,
  is a 3-dimensional connected finite homogeneous complex.

## 60 representation by a polyhedron

- [full polyhedron]
  full (solid) polyhedron is defined as the following,
  a closed 3-ball (or a topological image of a closed 3-ball)
  whose boundary been divided into polygons
  so that the following conditions are satisfied :
  1. each polygon is at least a 2-gon.
  2. each point of boundary belongs to at least one polygon.
  3. two polygons are either disjoint
     or have certain common edges or vertices.

  for example :
  - solid dodecahedron.
  - a closed 3-ball
    whose boundary sphere has been decomposed into two hemispheres
    by a great circle also becomes a full polyhedron
    when one subdivides the great circle by two or more vertices.

- x -
  使用三角形
  也許可以大大簡化描述二階代數時 所需語言的語法之複雜度

  當考慮 polyhedron 所構造的三維流形時
  manifold-checker 很簡單
  就是 euler number 爲 0

  現在要問的是
  這個 manifold-checker 對 manifold 的 2-skeleton
  有什麼限制
  [我們已經知道找個 2-skeleton 不能是任意的有限生成羣了]

  知道這些限制之後
  我們就可以試着模仿二階的情形來設計 normalization 算法

  同時也可以試着模仿 Dehn 的算法
  來設計相似的算法
  以判斷二階代數中任意兩個元素是否相等

## 61 homology groups

## 62 the fundamental group

## 63 the heegaard diagram

## 64 3-dimensional manifolds with boundary

## 65 construction of 3-dimensional manifolds out of knots

# 10 n-dimensional manifolds

## 66 star complexes

## 67 cell complexes

## 68 manifolds

## 69 the poincare duality theorem

## 70 intersection numbers of cell chains

## 71 dual bases

## 72 cellular approximations

## 73 intersection numbers of singular chains

## 74 invariance of intersection numbers

## 75 examples

## 76 orientability and two-sidedness

## 77 linking numbers

# 11 continuous mappings

## 78 the degree of a mappings

## 79 a trace formula

## 80 a fixed point formula

## 81 applications

# 12 auxiliary theorems from the theory of groups

## 82 generators and relations

## 83 homomorphic mappings and factor groups

## 84 abelianization of groups

## 85 free and direct products

## 86 abelian groups

## 87 the normal form of integer matrices
