---
title: classical topology and combinatorial group theory
author: john stillwell
date: 1980, 1992
---

# info

- topology is ostensibly the study of arbitrary continuous functions.
  in reality, however, we can comprehend and manipulate
  only functions which relate finite "chunks" of space
  in a simple combinatorial manner,
  and topology originally developed on this basis.

- x -
  雖然作者在開頭這麼說了
  但是作者在之後的敘述中 還是遵循了拓撲空間那種 算數化的語言
  如下的用詞就是一些徵兆 :
  - neighbourhood
  - local compactness

# 0 introduction and foundations

## 0.1 The Fundamental Concepts and Problems ofTopology

### 0.1.1 The Homeomorphism Problem

- One may consider a geometric figure to be an arbitrary point set,
  and in fact the homeomorphism problem
  was first stated in this form, by Hurwitz 1897.
  However, this degree of generality
  makes the problem completely intractable,
  for reasons which belong more to set theory than geometry,
  namely the impossibility of describing or enumerating all point sets.

- x -
  也就是說這種處理問題的方式不具有構造性
  沒法在計算機上用數據結構來實現這個模型
  更沒法用算法來實現就這個模型而敘述的函數

- For the moment
  we wish to claim that all "natural" geometric figures
  are either simplicial complexes or homeomorphic to them,
  which is just as good for topological purposes.

- knot complement in R3 is not finitely describable in terms of simplexes.

### 0.1.2 Continuous Functions, Open and Closed Sets

### 0.1.3 Examples of Continuous Maps

### 0.1.4 Identification Spaces

- x -
  again, glue is handled by quotient-space by the author.
  while in AT1, I handled it by common boundary.

### 0.1.5 The H-ball and the ^-sphere

### 0.1.6 Manifolds

- x -
  這里給出了一個例子來說明
  ``` scheme
  (type space
    (: base (-1 <>))
    (: p (0 base base))
    (: c (1 p p p)))
  ```
  並非是 manifold
  也就是說 group 的 generators 與 relations
  所給出的 underlying-space 並非是 manifold

### 0.1.7 Bounded Manifolds

- x -
  既然可以把 manifold 推廣到 bounded manifold
  那麼也沒有什麼理由不把一般的 group 的 underlying-space
  包含到考慮範圍之內
  而所得到的 underlying-space 的差異
  都可以通過 group 的性質來描述

  之前問
  what are the underlying-space and covering-space
  of low order finite groups?
  現在知道這些就是不是 manifold 的空間了

  之前關於有限生成羣的分類的問題
  二維曲面的分類很簡單 因爲這些曲面是 很特殊的羣的 underlying-space
  而一般的羣的分類很難 對應的 underlying-space 的分類也將很難

  而且之所以我們的討論要限制於
  那些 underlying-space 是 manifold 的羣
  可能就是因爲這種羣的 underlying-space 可以有簡單的嵌入
  從而讓人能夠直觀

  而對一般的羣的 underlying-space 的研究
  其實都是用代數方法來完成的

- In dimension 2
  we can distinguish manifolds from bounded manifolds
  by the fundamental group (4.2.1 and 5.3.3).

### 0.1.8 Embedding Problems

- next to the homeomorphism problem,
  the most important type of topological problem is that of
  distinguishing different embeddings of one figure in another.

- x -
  這裏的 homeomorphism problem 代數化之後就是空間之間的同倫等價
  即 (~~ A B)
  而後面的的 embedding 問題就是特殊的函數之間的等價
  即 (~ f g)
  其特殊性在於
  證明等價時所用的 (-> (* A I) B) 不能有自交

### 0.1.9 Homotopy and Isotopy

- The group properties depend crucially on the fact that
  the curve is not required to be simple at any stage,
  and in fact the deformation may create more singularities
  than were present at the beginning.

  Only then can one introduce a product of closed curves,
  and cancel a closed curve by its inverse.

- x -
  同倫羣的定義依賴於各階球面的自交的映射

- isotopy seems to be a more natural notion of deformation,
  but it is not algebraically tractable.
  In the case of simple curves on a 2-manifold
  the situation is saved by a theorem of Baer 1928 (6.2.5)
  which says that simple curves are isotopic
  if and only if they are homotopic.

- note the difference between

  - isotopy
    the "knotted part" can be shrunk to nothing
    without acquiring asingularity at any stage.

  - ambient-isotopy
    knot theory

- x -
  在 AT1 中
  應該如何定義上面兩個概念
  如果要求 證明等價時所用的 (-> (* A I) B) 不能有自交
  是否就有了 ambient-isotopy?

  有可能不行
  因爲 ambient-isotopy 是用 (-> (* B I) B) 定義的

## 0.2 Simplicial Complexes

### 0.2.1 Definition and Basic Properties

- simplicial complex as schema.
  It is a consequence of the triangulation and Hauptvermutung that
  all homeomorphism questions for 2- and 3-manifolds
  reduce to combinatorial questions about schemata.

### 0.2.2 Orientation

- in general,
  we interpret the ordered (n + 1)-tuple (P0, ..., Pn+1)
  as an orientation of the n-simplex.

  orientations are equivalent
  if they differ by an even permutation of the vertices,
  so there are in fact two possible orientations,
  - (+ (P0, ..., Pn+1)) which is just (P0, ..., Pn+1)
  - (— (P0, ..., Pn+1)) obtained by
    an odd number of exchanges of vertices.

- an orientation of an n-complex
  is an assignment of orientations to its simplexes.
  The orientation is coherent
  if n-simplexes which share an (n-1)-dimensional face
  induce opposite orientations in that face.

### 0.2.3 Realization in Euclidean Space

- any n-complex can be embedded in R^(2n+1)

### 0.2.4 Cell Complexes

- in the last resort,
  one can always view cells and the dividing cells inside them
  as unions of simplexes in a simplicial decomposition.
  The point of considering cell complexes at all
  is to minimize the number of cells,
  which usually helps to shorten computations.

- x -
  作者有使用 simplicial complex 的傾向

### 0.2.5 Triangulation and Hauptvermutung

- The Hauptvermutung (main conjecture) of Steinitz 1908
  states that homeomorphic manifolds are combinatorially homeomorphic.

## 0.3 The Jordan Curve Theorem

### 0.3.1 Connectedness and Separation

- The statement, as a theorem,
  that every simple closed curve in R2
  separates it into two regions (Jordan 1887)
  was important in the history of topology
  as the first moment when an "obvious" fact
  was seen to require proof.

### 0.3.2 The Polygonal Jordan Curve Theorem

### 0.3.3 0-graphs

### 0.3.4 Arcs Across a Polygon

### 0.3.5 The Jordan Separation Theorem

### 0.3.6 Arcs in a Polygon

### 0.3.7 No Simple Arc Separates R2

### 0.3.8 The Jordan Curve Theorem

### 0.3.9 The Jordan-Schoenflies Theorem

- there are topologically distinct embeddings of S2 in R3.

## 0.4 Algorithms

### 0.4.1 Algorithmic Problems

### 0.4.2 Recursively Enumerable Sets

### 0.4.3 The Diagonal Argument

## 0.5 Combinatorial Group Theory

### 0.5.1 The Fundamental Group

### 0.5.2 Generators, Words, and Relations

### 0.5.3 Group Presentations

### 0.5.4 Coset Decomposition, Normal Subgroups

- a set of elements v < G
  generates a normal subgroup N of G
  ``` scheme
  (-> [(: :g G) (: :v V)]
      [(: [:g :v :g rev] N)
       (: [:g :v rev :g rev] N)])
  ```

- generating a subgroup by inverse and product,
  generating a normal subgroup also by conjugation.

- x -
  how about the fact that
  the fundamental group of a covering space of (underlying-space G)
  is a subgroup of G?

### 0.5.5 Quotient Groups and Homomorphisms

- we can get quotient group of a group representation
  by adding new relations.
  [where new relations generates a normal subgroup.]

- isomorphism = (kernel = id)

### 0.5.6 Dyck's Theorem (Dyck 1882)
### 0.5.7 The Word Problem and Cayley Diagrams

- solution of the word problem for G
  is equivalent to the construction of Cayley diagram of G.

- x -
  如何等同呢?
  用有向圖實現這個 [有可能是無限的] cayley diagram 嗎?
  path 到 vertex 的 hash function 嗎?

### 0.5.8 Tietze Transformations

- Tietze's Theorem.
  Any two finite presentations of a group G
  are convertible into each other
  by a finite sequence of Tietze transformations.

### 0.5.9 Coset Enumeration

- x -
  這裏作者給出了不計算法複雜性的傻瓜算法

# 1 complex analysis and surface topology

## 1.1 Riemann Surfaces

### 1.1.1 Introduction

- A complex function is a map from sphere onto sphere.
  [a covering map.]

- The general purpose of Riemann surfaces in function theory
  is to provide domains on which
  all algebraic functions become single-valued.

### 1.1.2 Branched Coverings of the 2-sphere

- torus can cover sphere.

### 1.1.3 Connectivity and Genus

- In general
  the connectivity of a surface
  can be measured
  by the maximu mnumber of disjoint closed curves
  which can be drawn on the surface without separating it.
  [This number is called the genus of the surface.]

- mobius
  clifford
  klein

### 1.1.4 Branched Coverings of Higher Dimension

- for 3-dim covering-space

- james w. alexander

## 1.2 Nonorientable Surfaces

### 1.2.1 The Mobius Band

- Klein imagines a small oriented circle (the indicatrix)
  placed on the surface,
  then transported round an arbitrary closed curve.

### 1.2.2 The Projective Plane

### 1.2.3 The Klein Bottle

### 1.2.4 Dyck's Classification of Nonorientable Surfaces

- crosscap + handle = 3 crosscaps.

- special underlying-space

  |-------------------------|------------------|
  | group                   | underlying-space |
  |-------------------------|------------------|
  | general                 | cell-complex     |
  | orientable manifold     | ...              |
  | non-orientable manifold | ...              |
  | manifold with boundary  | ...              |
  |-------------------------|------------------|

## 1.3 The Classification Theorem for Surfaces

### 1.3.1 Combinatorial Definition of a Surface

- Dehn and Heegaard (1907)
  define a closed surface to be
  a finite 2-dimensional simplicial complex.

### 1.3.2 Schemata

- build surfaces from polygons instead of only using triangles.

### 1.3.3 Reduction to a Single Polygon with a Single Vertex

- amalgamate polygons to one polygon.
  reduce the number of vertices of the polygon.

### 1.3.9 Bounded Surfaces

- x -
  Dehn's paper about 1-holed sphere, 2-holed sphere, and so on,
  is based on the normalization of surface.

## 1.4 Covering Surfaces

### 1.4.1 The Universal Covering Surface

- covering without branch points
  covering with branch points

### 1.4.2 The Universal Cover of an Orientable Surface of Genus > 1

### 1.4.3 Fuchsian Groups

### 1.4.4 The 2-sheeted Cover of a Nonorientable Surface

# 2 graphs and free groups

## 2.1 Realization of Free Groups by Graphs

### 2.1.1 intro

- to vividly illustrate the dual view of
  a group as fundamental group of a space
  and automorphism group of a covering-space.

### 2.1.2 Graphs, Paths, and Trees
### 2.1.3 The Cay ley Diagram of a Free Group
### 2.1.4 Solution of the Word Problem for Free Groups
### 2.1.5 Spanning Trees
### 2.1.7 Generators for the Fundamental Group
### 2.1.8 Freeness of the Generators
### 2.1.9 The Tree as the Universal Covering Graph of the Bouquet of Circles

## 2.2 Realization of Subgroups

### 2.2.1 Covering Graphs

- covering-map can be adequately represented
  by labelling and orienting the edges in covering-space.

### 2.2.2 The Subgroup Property

- x -
  帶有類型的代數結構之間的同態
  誘導出去掉類型的代數結構之間的單同態

### 2.2.3 Realization of an Arbitrary Subgroup of a Free Group

### 2.2.4 The Nielsen-Schreier Theorem

- Every subgroup of a free group is free.

### 2.2.6 Schreier Transversals

### 2.2.7 Normal Subgroups and Cayley Diagrams

- (: covering (-> X (underlying-space G)))
  if (fundamental-group X) is normal subgroup of G,
  then (= X (cayley-graph (/ G (fundamental-group X))))

# 3 foundations for the fundamental group

## 3.1 The Fundamental Group

### 3.1.1 introductionthe fundamental group

- such a definition
  makes the computation of generators and relations routine,
  but it is open to the objection that
  the group is not obviously a topological invariant.

  since the topologists of the time
  pinned their hopes on the hauptvermutung,
  they could be satisfied with a proof that
  the fundamental group was invariant under combinatorial homeomorphisms,
  which was supplied by tietze 1908.

- the only group which has to be derived from first principles
  is that of the circle;
  all the other fundamental groups we need
  are then obtained by combining the simple technique of
  deformation retraction
  with the seifert-van kampen theorem.

## 3.2 The Fundamental Group of the Circle

### 3.2.2 Tightening a Path

- x -
  證明某些生成子能過生成整個函數空間
  要證明函數空間中的任意元素都能化爲生成子所生成的正規形式

- k -
  但是這與我們理解基本羣的方式並不相同
  計算基本羣的方式也不同
  我們計算同倫羣的方式是
  把一個 groupoid 化爲 group

- x -
  groupoid 化爲 group 的過程
  可以看作是收縮那些沒有自交的 path
  最終得到的全是 circle

  二維圖形也可以如此
  最終得到的卻不全是 sphere
  而是什麼?

- k -
  我們必須找一些例子來觀察

- x -
  在這個正規化的過程中
  我們也可以先處理點
  即 用一個支撐樹
  把所有的點收縮成一個點

- k -
  這樣我們就會得到一個高階 group 了

### 3.2.3 Brouwer Degree

### 3.2.4 Invariance of the Brouwer Degree under Homotopy

## 3.3 Deformation Retracts

## 3.4 The Seifert-Van Kampen Theorem

## 3.5 Direct Products

# 4 fundamental groups of complexes

## 4.1 Poincare's Method for Computing Presentations

## 4.2 Examples

## 4.3 Surface Complexes and Subgroup Theorems

# 5 homology theory and abelianization

## 5.1 Homology Theory

## 5.2 The Structure Theorem for Finitely Generated Abelian Groups

## 5.3 Abelianization

# 6 curves on surfaces

## 6.1 Dehn's Algorithm

### 6.1.1 Introduction

- The fundamental problem
  in the topological classification of curves on surfaces
  is to decide whether a given closed curve contracts to a point.
  We shall call this the contractibility problem.

- The problem was solved,
  at least from a geometric point of view,
  with the introduction of the universal covering surface in the 1880s.

  If a curve p on the surface F
  is lifted to a curve p' in the universal covering surface F',
  then p contracts to a point on F if and only if p' is closed in F'.

  - x -
    note that,
    the covering-space of a closed surface
    can only be [hyperbolic] plane or sphere.
    plane and sphere both are simple-connected
    [with trivial fundamental-group].

  It is admittedly not very convenient
  because of the very dense packing of polygons required
  even for genus 2 (see Figure 111).

- Dehn observed that
  the labelled net of polygons on the universal covering surface
  was in fact the Cayley diagram of the fundamental-group of F,
  and that the contractibility problem was therefore
  the same as the word problem for the fundamental-group of F.

  thus,
  Dehn's algorithm solves the problem.
  His first proof depended on the metric in the hyperbolic plane (Dehn 1912a),
  but he then saw how the algorithm could be justified
  by purely topological properties of the net (Dehn 1912b).

  - Dehn, M. (a):
    Uber unendliche diskontinuierliche Gruppen,
    Math. Ann. 71 (1912), 116-144.
    Dehn's algorithm for solution of the word problem for surface groups,
    proved using hyperbolic geometry.

  - Dehn, H. (b):
    Transformation der Kurven auf zweiseitigen Flachen,
    Math. Ann. 72 (1912), 413-421.
    Topological proof of Dehn's algorithm.

### 6.1.2 Some Special Cases
### note 二階元素指導 一階元素之間等價的證明

- x -
  否則代入方式可能是不明確的

### 6.1.3 The Subpath Property

- Then if p is any closed path in the net,
  p contains
  1. either a spur
  2. or a subpath consisting
     of more than half the edges
     in the boundary of a polygon in succession.

### 6.1.4 Dehn's Algorithm

## 6.2 Simple Curves on Surfaces

## 6.3 Simplification of Simple Curves by Homeomorphisms

### 6.3.1 Introduction

### 6.3.2 Twist Homeomorphisms

### 6.3.3 Curves Which Meet at a Single Point and Cross

### 6.3.4 Removal of Intersections

### 6.3.5 Taking a Curve off the Handles

### 6.3.6 Mapping onto a Canonical Curve

## 6.4 The Mapping Class Group of the Torus

### 6.4.1 Introduction

### 6.4.2 Canonical Curve Pairs on the Torus

### 6.4.3 Classification of Canonical Curve Pairs

### 6.4.4 Generating Homeomorphisms for Transformations of Canonical Curves

### 6.4.5 Homeomorphisms Are Determined up to Isotopy by the Transformation of the Canonical Curves

### 6.4.6 The Mapping Class Group

### 6.4.7 Automorphisms of H1(F) when F Is Orientable of Genus > 1

# 7 knots and braids

## 7.1 Dehn and Schreier's Analysis of the Torus Knot Groups

## 7.2 Cyclic Coverings

## 7.3 Braids

# 8 three-dimensional manifolds

## 8.1 Open Problems in Three-Dimensional Topology

### 8.1.2 Methods of Constructing 3-manifolds

- [simplicial decomposition]
  A 3-manifold is a union of solid tetrahedra
  with disjoint interiors,
  with at most two tetrahedra meeting at each face
  (exactly two for a closed manifold),
  and finitely many at each edge and vertex.

  Furthermore, the neighbourhood surface of each vertex
  must be a 2-sphere,
  so that each point in the complex
  has a neighbourhood homeomorphic to the 3-ball
  (see also 8.2).

  If the complex is finite,
  then all the above properties are decidable,
  so we have an algorithm for deciding
  whether a finite 3-complex is a manifold
  (and of course, for distinguishing
  between closed and bounded manifolds).

  As in dimension 2,
  we can amalgamate simplexes
  until we have a single polyhedron,
  homeomorphic to a ball,
  with faces identified in pairs.
  This cell decomposition method of constructing 3-manifolds
  is therefore completely general
  and could serve as an alternate definition
  provided we include the statement that
  the neighbourhood surface of each vertex is a sphere.

- the remaining three methods of construction
  we shall mention yield only the orientable 3-manifolds.

- [heegaard splitting]
  decomposes the manifold into two handlebodies.
  two handlebodies can produce infinitely many different manifolds
  via different homeomorphisms between their boundary surfaces.

  by starting with a simplicial decomposition.
  It is fairly easy to show that
  every finite orientable 3-manifold has a Heegaard splitting (8.3.1).

- [dehn surgery]
  One removes some solid tori from an S3
  and sews them back differently.
  To obtain all orientable 3-manifolds
  the tori have to be knotted or linked in most cases.

  In 8.4 we shall follow Lickorish 1962 in
  deriving the surgery construction from a Heegaard splitting.

- [branched coverings of S3]
  the 3-dimensional analogue of Riemann surfaces.
  Like surgery, it is generally based on knots or links,
  and can in fact be derived from the surgery construction
  (Lickorish 1973, see 8.5).

- Each of the above methods
  yields a finite description of the 3-manifold
  which can be effectively translated into a simplicial decomposition.

  and different forms of description are intertranslatable.
  [which is often useful for showing that two manifolds are the same.]

  The problem is that
  the same manifold has infinitely many descriptions,
  so we cannot always be sure
  whether different descriptions
  actually represent different manifolds.

- thus we have algorithm
  for effectively enumerating all finite 3-manifolds
  (with repetitions).

### 8.1.3 The Homeomorphism Problem

- Since we do not yet know how to recognize the simplest 3-manifold S3
  (see 8.1.4)
  it must be admitted that
  the homeomorphism problem is far frombeing solved.
  The principal obstacles seem to be
  1. absence of plausible normal forms,
  2. lack of bounds on the length of constructions
     which convert one desscription of a manifold to another.
     - to know when to stop looking for a description
       among the infinitely many descriptions equivalent to it.
       thus, bounding the length of the search.
       thus, we can solve the problem
       even when we do not have a normal form.

- The homeomorphism problem has been solved
  for the subcase of lensspaces (Reidemeister 1935).
  We shall see in 8.3
  that the lens spaces are the 3-manifolds of Heegaard genus 1.

### 8.1.4 Recognizing the 3-sphere

### 8.1.5 An Unshellable Triangulation of the 3-ball

### 8.1.6 The Poincare Conjecture

### 8.1.7 The Word Problem for 3-manifold Groups

- The Word Problem of fundamental group of 3-manifold,
  might be solvable,
  because many groups can not be fundamental group of 3-manifold.
  [for example, (Z x Z x Z x Z) (Stallings 1962)]

- to solve the word problem in fundamental group of M,
  we only need to construct the universal covering space of M.
  The trouble is that we do not know
  what the potential covering spaces might be in general.

- x -
  what is the relations between
  universal covering space
  and geometric of space.

### 8.1.8 Above Dimension 3

- It is not yet known whether all 4-manifolds can be triangulated.

- any finitely presented group can be realized as fundamental group of a 4-manifold.

- for dimensions >= 5,
  The Hauptvermutung is false (Milnor 1961).

## 8.2 Polyhedral Schemata

### 8.2.1 manifolds and pseudomanifolds

- Poincare 1895 introduces the construction of 3-manifolds
  by identifying faces of simply-connected polyhedra :
  (1) a finite set of polyhedra (topological 3-balls) called cells, with disjoint interiors,
  (2) faces of cells identified in pairs, with vertices corresponding to vertices,
  (3) resulting in a connected complex.

- However,
  Poincare observes that
  these conditions do not guarantee that
  the outcome will be a manifold.
  [thus, the concept of pseudomanifolds.]

  to get manifold,
  we need :
  (4) The neighbourhood surface of each vertex is a 2-sphere.

### 8.2.2 The Euler Characteristic of a Pseudomanifold

- 3-manifolds have Euler characteristic 0.
  [but pseudomanifold do not.]

### 8.2.3 An Example

- TODO

### 8.2.4 Remarks

- Polyhedral schemata
  have never been used in a systematic way
  for the construction of 3-manifolds,
  even though some interesting manifolds
  originally arose in this way
  (for example lens spaces, cf. 4.2.8.
   See also Threlfall and Seifert 1930, 1932
   and Weber and Seifert 1933
   for manifolds obtainedfrom the Platonic solids).

  Apparently polyhedral schemata
  do not admit anything like the reductions applicable to polygon schemata,
  but it is not clear that anyone has worked very hard on the problem.

  Only recently, Thurston 1977 has found
  polyhedral forms of many 3-manifolds
  which can be used to tesselate hyperbolic 3-space,
  yielding a theory like the classical theory of 2-manifolds.

## 8.3 Heegaard Splittings

### 8.3.1 Existence

- x -
  when getting heegaard-splitting from a polyhedral-schema,
  it seems more complex then polyhedral-schema.

  but we can also get heegaard-splitting from other picture of 3-manifold.

  we have :
  (polyhedral-schema -> heegaard-splitting)

### 8.3.2 Heegaard Diagrams

- A manifold which splits into handlebodies H1 H2
  is determined up to homeomorphism
  by the map (: h (-> [H1 boundary] [H2 boundary]))
  [which is determined by the images of the canonical meridians.]

  when a set of n disjoint simple closed curves
  on a handlebody of genus n is a Heegaard diagram?

### note

- x -
  Heegaard Diagram 可以給 3-manifold 一個非常簡單的表示
  但是 我們缺很難判斷兩個 表示是否等價

  how about
  (heegaard-splitting -> polyhedral-schema)?

### 8.3.3 Reading a Presentation of the Fundamental Group

### 8.3.4 Lens Spaces

- x -
  do not use the simple algorithm of
  (polyhedral-schema -> heegaard-splitting)
  to get the heegaard-splitting,

  but to use the core dissection
  to get a heegaard-splitting of genus 1.
  (lens-space -> heegaard-splitting of genus 1)

  after add (0, 1) lens space,
  all heegaard-splitting of genus can be pictured as lens space.

### 8.3.5 Alexander's Proof that the (5, 1) Lens Space and the(5, 2) Lens Space Are not Homeomorphic

### 8.3.6 Heegaard Diagrams of Bounded 3-manifolds

### 8.3.7 Fundamental Groups of Bounded 3-manifolds

### 8.3.8 Heegaard Diagrams of Knot and Link Complements

## 8.4 Surgery

## 8.5 Branched Coverings

# 9 unsolvable problems

## 9.1 Computation

## 9.2 HNN Extensions

## 9.3 Unsolvable Problems in Group Theory

## 9.4 The Homeomorphism Problem
