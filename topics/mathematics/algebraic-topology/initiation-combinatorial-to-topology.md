---
title: 组合拓扑入门 / initiation combinatorial to topology
authors: [Maurice Frechet, Kt Fan]
years: [1946, 1967]
note: Translated from French, by Howard W. Eves
newline: preserve
---

# 拓扑一般性 / topological generalities

## 1 定性的 [非定量的] 几何性质 / Qualitative Geometric Properties

## 2 地图着色 / Coloring Geographical Maps

- x -
  染色問題在三維中是平凡的
  即 沒有染色數的上限
  因爲對三維空間的分割可能形成非常複雜的相鄰狀態
  或者說在三維空間中 有很靈活的方式來形成多種相鄰狀態

- k -
  這種因爲維數增加 而使得理論不能類比的情形
  還真是令人擔心呢

- x -
  這些都是事實
  我們只要描述它就行了

  這裏需要問的是
  這種複雜的相鄰情況
  是否會使得我們很難形成三階代數

- k -
  knot 所帶來的複雜性 就有些類似

- x -
  沒錯
  並且這裏還要注意染色問題是就流形而言的
  對於一階流形 只要兩個顏色就夠了
  當不限於流形的時候 染色問題也是平凡的

- k -
  人們在考慮一階圖形的時候
  不把自己限制於流形 而是考慮有向圖
  但是在二階的時候 一般的 complex 就過於複雜了
  因而人們把自己限制於流形

- x -
  我想限制與流形還有一個原因
  那就是要給 complex 以幾何
  但是其實不限制與流形可能也可以給其以幾何

  在之後討論 complex 到流形的限制時
  我們也應該從一階流形開始

## 3 邻域问题 / The Problem of Neighboring Regions

## 4 拓扑, 橡皮几何 / Topology, India-Rubber Geometry

- India-Rubber,
  which we are able to distort as we wish
  in a continuous fashion without tearing.

- x -
  這種想法可以給拓撲不變量一個直觀的解釋

## 5 同胚 / Homeomorphism

- x -
  可逆的連續雙射 homeomorphism
  可以用來捕捉之前的
  "to distort as we wish in a continuous fashion without tearing."
  這在於 空間 A 的各种性質 都是利用連續映射来描述的
  比如 f : X -> A 或者 g : A -> Y
  因此當有 homeomorphism 的時候
  這些 描述依然有效
  並且 所描述的性質之間的關係依然被保持

- k -
  可以發現
  我們是先發現了很多有趣的不變量
  之後在慢慢得到這個等價關係[同胚]的正確定義的
  但是在講述的時候
  我們會先定義等價關係
  然後再定義這些不變量
  其實對定價關係的定義正是爲了使得這些不變量成爲不變量

- x -
  用德里達的話來說
  某個具體的 等價關係 並不是研究的中心
  不變量並不附屬於等價關係
  而是正相反
  等價關係起源於所發現的不變量

  當想要描述的不變量不適用於某種等價關係了
  就要修改這個等價關係的定義
  使得這些不變量重新變爲不變量
  [比如 homeomorphism 到 ambient-isotopy]

------

- x -
  在定義連續映射的時候
  作者強調了這裏描述的映射是點到點的
  而不是寫 polar reciprocation 那種點到線的
  但是後者也可以是連續映射
  當把空間看作是線組成的時[而不是點組成的]
  topology 公理可能就是必要的了
  也就是說 topology 公理可能不能被理解爲分析算數化的產物
  而應該被理解爲有效的推廣

- k -
  但是我們所使用的 uncurry 原則
  則是說
  所有的這種 level-up 映射都應該被 uncurry 爲 level-same 映射

- x -
  很難想象這種處理方式能夠描述 polar reciprocation
  一種離散的 polar reciprocation?

- k -
  我想這是因爲 polar reciprocation 並非純粹的 topology 變換

- x -
  什麼是純粹的 topology 變換?

- k -
  首先
  當以組合的方式來描述空間時
  我們能夠定義很多空間之間的變換
  這樣
  我們就可以把純 '粹連 topology 變換'
  定義爲 與我們能夠定義的變換同倫的變換

- x -
  何以見得 polar reciprocation 不能與這種變換同倫呢?

- k -
  我們可以嘗試找 polar reciprocation 的同倫
  正如我們找 dehn twist 的同倫一樣
  映射可要被降階

- x -
  我想我們需要先找到空間的合適劃分
  然後看看這個描述與點的變換限制於這些劃分的時候的樣子
  這個問題應該先放一放
  因爲我們對 polar reciprocation 與圓錐曲線的理解還不夠
  [注意 圓錐曲線可以被推廣到一般空間所形成的 cone]

  ><><><
  一般描述於點的連續變換的組合化
  以及 polar reciprocation 的組合化

------

- x -
  這裏還指出了
  "to distort as we wish in a continuous fashion without tearing."
  必須用 ambient-isotopy 這個概念來捕捉
  而不能用 homeomorphism 來捕捉
  因爲 homeomorphism 還可以是 cut and re-glue

  同時這裏還指出了 一個線段和一個三角形之間可以有 homeomorphism
  但是這些都是集合論語言的問題
  當我們以組合的方式處理拓撲學時
  這些病態的現象並不會出現

## 6 拓扑, 连续几何 / Topology, Continuous Geometry

- It is important to note that
  a topological property of a set
  is not necessarily a topological invariant,

  in which case the property is said to be relative.
  In other words, two homeomorphic sets Ε and F
  can have certain different topological properties,
  as when it is a matter not of properties of Ε and F alone,
  but of properties concerning Ε and F and the spaces which contain them.

- x -
  也就是說 relative topological properties 起源於拓撲不變量的細化
  而不是 直接起源與 空間與其子空間
  畢竟 子空間 與 空間 之間的關係
  並不直接涉及到 relative 這個詞

## 7 初等几何, 摄影几何 与 拓扑学 之对比 / Comparison of Elementary Geometry, Projective Geometry, and Topology

- x -
  這裏跟隨開篇對 poincare 的引用

- "projected from a center onto a figure."
  understood as "make a cone and make a section."

  from the point of view of projective geometry,
  one does not distinguish the three types of conies.

- x -
  攝影變換下的等價
  可以利用 cone 來理解
  注意
  使用 cone 的時候
  就是增加了一個維度

  類似的
  uncurry 也是通過增加一個維度
  來理解變換之間的等價

- k -
  但是
  當用變換之間的同倫
  來定義空間之間的同倫等價時
  情況就有些複雜了
  但是也許還是可以通過增加維度來理解

- x -
  比如 mapping cylinder
  看來我們需要好好總結一下 mapping cylinder

  ><><><
  總結 mapping cylinder
  總結 空間之間同倫等價之判別的簡化

## note about erlangen program

- to form a geometry theory
  1. first, the fundamental element of the geometry (point, line,circle, etc.);
  2. next, the manifold or space of these elements
     plane of points, ordinary space of points,
     spherical surface of points, plane of lines,
     pencil of circles, etc.;
  3. and finally, the group of transformations
     to which the manifold of fundamental elements is to be subjected.

## 8 相对的拓扑性质 / Relative Topological Properties

- x -
  這裏所說的是 ambient-isotopy
  所說的相對性是指 sub-space 相對於 ambient-space
  [而不是相對同倫羣中的相對性]

## 9 点集拓扑 与 组合拓扑 / Set Topology and Combinatorial Topology

- Since the figures studied in set topology are extremely general sets,
  it is natural that the results obtained in this way
  are frequently far from intuitive
  and even sometimes in contradiction with intuition.

- [bib]

  - [28]
    R. L. Wilder,
    "Point sets in three and higher dimensions
    and their investigation by means of a unified analysis situs,"
    Bull. Amer.Math. Soc, 38 (1932), 649-692.

- Our book, as indicated by its title,
  is directed principally to combinatorial topology,
  accordingly, to the topology of complexes.

  Since complexes are generalizations of surfaces,
  we shall limit ourselves, in the other two chapters of this book,
  to the topology of surfaces.

  - In connection with the topology of surfaces,
    one can also consult: H.Seifert and W. Threlfall,
    "a textbook of topology",
    chapter six, surface topology.

## 10 拓扑学的发展 / The Development of Topology

- Poincare's combinatorial topology vs. Cantor's set topology

# 曲面的拓扑概念 / topological notions about surfaces

## 11 Descartes'Theorem

- A polyhedron is a system of a finite number of polygons
  which are situated in a mutual relationship
  such that the following four conditions are verified:

  1. each pair of polygons of the system
     have no common interior points.

  2. for each side of a polygon,
     there exist two and only two polygons having this side in common.

     the two polygons can be the same one,
     thus, two coinciding sides can belong to the same polygon.

  3. each pair of polygons of the system
     can be joined by a sequence of polygons of the system
     in the sense that each of these
     has a side in common with the following one.

     i.e.
     there exist joining sequence of faces, for every pair of two faces.
     thus connectivity is ensured in a non-degenerate way.

  4. the polygons about any vertex
     can be placed in a cyclic order
     so that each consecutive pair
     have a common side passing through the vertex.

     thus exclude some kind of singularity.

  According to this definition,
  when we speak of a polyhedron,
  we always mean the surface of the polyhedron.

- A polyhedron is said to be simple
  if one can continuously deform it into the surface of a sphere.

## 12 An Application of Descartes'Theorem

- all regular polyhedra
  the tetra-hedron
  the octa-hedron
  the icosa-hedron
  the cube
  the dodeca-hedron

## 13 Characteristic of a Surface

- euler characteristic

## 14 Unilateral Surfaces
## 15 Orientability and Nonorientability

- orientability can be defined by
  1. a oriented circle travelling on the surface.
  2. a subdivision of the surface
     and observing the orientation of the faces limited on the boundaries.

  the 2nd definition is easier to be applied,
  and we can check that heptahedron is nonorientable.

## 16 Topological Polygons

## note the thread

1. define cell-complex
2. as example, construct the normal form
3. manifold-checker
4. manifold-normalizer

## 17 construction of closed orientable surfaces from polygons by identifying their sides

## 18 construction of closed nonorientable surfaces from polygons by identifying their sides

- mobius band = cross cap

  the curve of penetration on the cross cap
  corresponds to the mediancurve on the M6bius band.

- 2 mobius band = klein bottle

- thus
  2 cross cap = klein bottle

## 19 topological definition of a closed surface

- x -
  this chapter is golden,
  I will analysis of the heuristic of the author :

  In the last two sections,
  we have constructed some closed surfaces from polygons by identifying their sides.

  This shows us the possibility, by operating in the reverse order,
  of decomposing any one of these surfaces into one or more polygons
  satisfying certain conditions.

  - "decomposing any one of these surfaces"
    means that surfaces is going to be defined this way.

  These surfaces are so varied from the point of view of their metrical form
  that their very generality allows us in the following
  to limit the consideration of surfaces
  to those that can be cut up into polygons
  subjected to certain conditions that we are going to make precise.

  - refrain.

  It is quite natural to choose these conditions
  so as to generalize the conditions
  imposed on the polyhedra defined before.

  We have given a definition of the word polyhedron
  that gives us a more general interpretation
  than is meant by this word in elementary geometry.

  - not a generalization of the definition,
    but a generalization of the interpretation.

- x -
  consider a surface defined in set topology theory,
  one can divide it in many different ways to different polyhedra.

  we replace "the surface to be divided"
  by an equivalent relation between polyhedra,
  to be pure combinatorial.

# 闭合曲面的拓扑分类 / topological classification of closed surfaces

## 20 the principal problem in the topology of surfaces

- the principal problem in the topology of closed surfaces
  is the search for the topological invariants of each closed surface
  so that we can tell if two arbitrarily given closed surfaces
  are or are not homeomorphic.

- we shall see in this chapter that
  the characteristic and the property of being orientable or not
  are two topological invariants that suffice to characterize each class.
  In other words,
  two closed surfaces are homeomorphic
  if and only if they have the same characteristic
  and they are both orientable or both nonorientable.

## 21 planar polygonal schema and symbolic representation of a polyhedron

- x -
  這一章講 如何利用 polygon 構造 polyhedron
  polyhedron 的條件將被轉述於構造中

  polygon 被想象爲在某個 plane 上

  planar polygonal schema ->
  symbolic representation of a polyhedron

  same symbolic representation implies homeomorphic.
  but
  the same surface can give rise to different forms of polyhedra,
  the same polyhedron can give rise to different forms of symbolic representations.

## note 'glue to each other' vs. 'glue to a common skeleton'

- x -
  "there are exactly two equivalent sides in the plane
  corresponding to each edge ofthe polyhedron."

  關於這個條件

  如果是在構造一個空間
  這個條件就體現爲檢查每個邊出現在面中的次數
  最多只能出現兩次

  如果是在一個已經構造好的 complex 中
  利用 (2 ...) 找 polyhedron
  這個條件就體現在 被使用於 glue 的邊
  在 glue 之後將被消除

- x -
  作者的理解方式是 glue polygon
  而我們所採用的是類似 cell-complex 的逐階構造方式

  作者說 glue 的時候單單標出名稱是不夠的
  還要標出方向

  這一點在我們的構造方式中如何體現?
  在構造三階幾何體的時候又如何?

- k -
  後一種想法的特點在於
  當想要描述 2-cell 的 glue 情況時
  已經有 1-cell 作爲骨架來幫助描述了
  爲了在構造中引入一個 2-cell
  只需要找一個 circle 到 1-cell 骨架的 immersion
  [immersion 即一個一般映射]

  這個 immersion 中非單值的地方 就包含着 glue 的信息
  [非單值的地方 就是邊界的部分名稱重複的地方]

  給 immersion 時所使用的 circle 以定向
  就能看出 兩種 glue 同一個邊的方式
  就對應與 兩種走過 1-cell 骨架中同一邊的方式

- x -
  但是 sphere 到 2-skeleton 的 immersion 卻沒有類似的現象
  因爲 sphere 沒有像 circle 一樣的簡單定向

- k -
  這正是我們之前說的
  (1 ...) 與 (2 ...) 這兩個語法的區別

- x -
  我們需要更多的例子
  [用 lens-space]

## ><><>< position in boundary

- x -
  上面說到了 (1 ...) 與 (2 ...) 的語法差異
  (1 ...) 是微分語法
  (2 ...) 是積分語法
  前者 可以描述 穿過一個 loop 的兩種方式
  而後者如果只是利用邊界信息的話
  就沒法描述出 穿過 loop 的兩種方式

- k -
  我們之前說過
  對於 (2 ...) 而言 積分語法與微分語法都要有
  而我們只有積分語法

  這兩種描述方式對於 (1 ...) 也同樣應該適用
  我們也應該給 (1 ...) 加上積分語法
  這樣各階元素之間的語法差異就消失了

## ><><>< lens-space as example

- ><

## 22 elementary operations

- Given two polyhedra
  [two symbolic representations of two polyhedra],
  what are the conditions that must be satisfied
  in order that the two closed surfaces determined by them be homeomorphic?

- solve this by a set of operations, on symbolic representation,
  that change a polyhedron to a polyhedron which is homeomorphic to it.

  [any function that establishes homotopic equivalence between spaces can do this.]

- 4 elementary operations [on planar polygonal schema] :
  | 1-dim-subdivision | add point on edge                                  |
  | 1-dim-union       | to view two edges as one                           |
  | 2-dim-union       | to view two polygons as one                        |
  | 2-dim-subdivision | dividing one polygon in two by means of a diagonal |
  when "to view two as one",
  the two must not already been one.

- the closed surfaces
  determined by two elementarily associated polyhedra
  are homeomorphic.

  to prove this condition of homeomorphic is sufficient is easy,
  we can also prove it is necessary in section 27.

## 23 use of normal forms of polyhedra

- By the preceding definition,
  polyhedra can be placed in pairwise disjoint classes
  in such a way that two polyhedra belong to the same class
  if and only if they are elementarily associated.

- x -
  normal form 的作用就像我們把一個分數約分爲最簡一樣
  有了把一般元素化爲正規元素的算法
  並且兩個正規元素等價與否可以有簡單的判斷
  那麼我們就能判斷任意兩個元素是否等價了

- k -
  但是與兩個分數之間相等的判斷又不同
  因爲當給出兩個分數的時候
  與其把二者都化爲正規形式 然後再比較
  我們顯然有更簡單的算法
  當給出 (a / b) (c / d) 的時候
  我們可以判斷 ((a * d) = (c * b))
  如果知道了 c > a 且 d > b
  我們也可以判斷 (c / a) 與 (d / b) 都是整數
  並且 ((c / a) = (d / b))

- Among all the polyhedra of a class,
  we are going to try to choose the simplest one possible,
  and call it the normal form.

  It is clear that the normal form of a class
  will depend on the character of simplicity adopted.

  it is natural,
  from a certain point of view,
  to consider as the simplest
  those for which the numbers of faces, edges, and vertices
  are as small as possible.

## 24 Reduction to Normal Form: I

- We shall accomplish the reduction of a planar polygonal schema
  to its normal form in six stages,
  exclusively using a finite number of elementary operations in each stage.

  first three stages, reduce the number of faces, edges, and points.
  last three stages, make the schema as regular as possible.

  - [reference]
    In the reduction to normal form,
    we follow the method given by Seifert and Threlfall [22], pages 135-139.
    For another method of reduction, see J. W.Alexander [1].

    - [1]
      J. W. Alexander,
      "Normal forms for one- and two-sided surfaces,"
      Amah of Math., 76(1915), 158-161.

    - [22]
      H. Seifert and W. Threlfall,
      "Lehrbuch der Topologie." [textbook of topology]
      Leipzig:Teubner, 1934.

- first stage :

  to use 2-dim-union,
  to get a schema that only have one polygon,
  which has even number of sides,
  which are pairwise equivalent.
  [for example, do this for tetrahedron]

  (a+ a-) and (a+ a+) are normal forms.

  in the following,
  we suppose the polygon has at least four sides.

- second stage :

  (~~~~ a+ a- ~~~~) -> (~~~~ ~~~~)

  which is specified by the author as :
  2-dim-subdivision
  1-dim-union
  2-dim-union

  by repeating this procedure a finite number of times,
  we will finally arrive at one of the following two possible cases:
  either where we will have a polygon of two sides,
  or where we will have a polygon having at least four sides
  and not containing two adjacent sides of the form (a+ a-).
  In the first case, we have already arrived at the normal form (4) or (9).
  We have to pursue the reduction only for the second case.
  We will suppose, then, in the following,
  that the considered schema is composed of a single polygon
  having at least four sides and that this polygon
  does not have two adjacent sides of the form (a+ a-).

- third stage :

  the aim is to reduce the number of vertices to one.

  - x -
    作者所描述的確實是一個遞歸算法的各個細節
    只是用的不是形式語言罷了
    可以發現
    這是一個遞減字典序的遞歸操作

    如果想要在我們的語言中執行這個操作
    看似就要執行 2-dim-subdivision 與 2-dim-union
    而這必須要引入新的點
    但是
    其實這些操作都可以被壓縮到邊界

## 25 Reduction to Normal Form: II

- x -
  此時 groupoid 已經被化爲了 group

- fourth stage :

  non-adjacent equivalent sides with like signs ->
  adjacent equivalent sides with like signs

- fifth stage :

  - x -
    這一步 在說
    (~~~~ c+ ~~~~ c- ~~~~) ->
    (~~~~ c+ ~~~~ d+ ~~~~ c- ~~~~ d- ~~~~)
    的時候 好像由漏洞

- sixth stage :

  當有 cross-cap 的時候
  一個 handle 可以化爲 兩個 cross-cap

## note bib about the proof

- The proof,
  in Sections 24 and 25, of the classification theorem
  is based upon the proof given by H. R. Brahana,
  "Systemsof circuits on two-dimensional manifolds".

## note 形式化的難點

- x -
  我已經看到了一個形式化的難點了
  可能我們目前的語言的抽象能力並不夠強

  這在於
  在我們的語言中 一個空間是一個類型
  而這裏的算法要求我們能夠把一個空間當作一個數據來處理

- k -
  我們可以想想看這整個算法的類型是什麼

- x -
  首先輸入的是一個空間
  輸出的是一個被簡化了的空間
  同時還有一系列的證明
  證明這個簡化過程中所給出的一系列空間是相互等價的

  我知道利用 (type ...) 這個語法關鍵詞
  我們能夠構造匿名的類型
  利用 (lambda ...) 這個語法關鍵詞
  我們能夠構造匿名的證明

- k -
  我想這裏我們可能要
  跨過 groupoid 與 cat 的邊界
  跨過可逆與不可逆的邊界
  跨過 intrinsic-type 與 extrinsic-type 的邊界

## note 組合拓撲學的目的

- x -
  我們要先確定學科的目的
  然後才能判斷我們所採用的方式的優劣

  組合拓撲學的目的是
  給空間分類

  類比別的 已經發展完備的 簡單數學理論 比如分數的理論

  (1)
  給出一種語言使得我們能夠描述拓撲空間
  ><><>< 語言其實是手段 其優劣應該由其目的判斷

  (2)
  ><><><

  引入代數
  引入各種構造新空間的方法也是手段 而不是目的

## 26 Characteristic and Orientability

- A polyhedron is said to be orientable
  if we can determine a cyclic sense of direction
  for the perimeter of each polygon of its planar polygonal schema
  so that, in the corresponding symbolic representation,
  each letter representing two equivalent sides
  appears twice with opposite signs.

  In particular,
  for a polyhedron having only one face to be orientable,
  it is necessary and sufficient that
  each letter representing two equivalent sides appear twice
  with opposite signs in every possible symbolic representation of this polyhedron.

- x -
  利用 euler characteristic 與 可定向性 這兩個簡單的不變量
  可以證明上面所獲得的 normal form 是相互不等價的

- In order that two polyhedra be elementarily associated,
  it is necessary and sufficient that
  they have the same characteristic
  and be simultaneously orientable or simultaneously nonorientable.

- can it be, then, that two polyhedra obtained from the same closed surface
  have distinct characteristics
  or be such that one is orientable and the other not?

  we accept this fact, that it can not be,
  without proving it.

## 27 The Principal Theorem of the Topology of Closed Surfaces

## 28 Application to the Geometric Theory of Functions

## 29 Genus and Connection Number of Closed Orientable Surfaces

- We shall call the maximum number of pairwise disjoint closed Jordan curves
  that can be drawn on a closed orientable surface
  without dividing the surface, the genus of the surface.

- If we do not insist that the closed Jordan curves be disjoint,
  We call the maximum number of closed Jordan curves (disjoint or not)
  that can be drawn on a closed orientable surface
  without dividing the surface, the connection number of the surface.

- Thus,
  for closed orientable surfaces,
  the genus is equal to exactly half the connection number.
