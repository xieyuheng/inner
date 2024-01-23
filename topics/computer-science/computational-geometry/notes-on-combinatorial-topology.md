* (2009) (jean gallier) notes on convex sets, polytopes, polyhedra, combinatorial topology, voronoi diagrams and delaunay triangulations

*** 1 Introduction

***** 1.1 Motivations and Goals

      - Chapter 6 on combinatorial topology is hardly original.
        However, most texts covering this material
        are either old fashion or too advanced.
        Yet, this material is used extensively in meshing and geometric modeling.
        We tried to give a rather intuitive yet rigorous exposition.
        We decided to introduce the terminology combinatorial manifold,
        a notion usually referred to as triangulated manifold.

      - A recurring theme in these notes
        is the process of "conification" (algebraically, "homogenization"),
        that is, forming a cone from some geometric object.

        Indeed, "conification" turns an object into a set of lines,
        and since lines play the role of points in projective geometry,
        "conification" ("homogenization")
        is the way to "projectivize" geometric affine objects.

        Then, these (affine) objects
        appear as "conic sections" of cones by hyperplanes,
        just the way the classical conics (ellipse, hyperbola, parabola)
        appear as conic sections.

      - [bib]

        - [43]
          Gunter Ziegler.
          Lectures on Polytopes.
          GTM No. 152. Springer Verlag, first edition,
          1997

        - [38]
          Rekha R. Thomas.
          Lectures in Geometric Combinatorics.
          STML, Vol. 33. AMS, first edition,
          2006

*** 2 Basic Properties of Convex Sets

***** 2.1 Convex Sets

***** 2.2 Carathéodory’s Theorem

***** 2.3 Vertices, Extremal Points and Krein and Milman’s Theorem

***** 2.4 Radon’s and Helly’s Theorems and Centerpoints

*** 3 Separation and Supporting Hyperplanes

***** 3.1 Separation Theorems and Farkas Lemma

***** 3.2 Supporting Hyperplanes and Minkowski’s Proposition

***** 3.3 Polarity and Duality

*** 4 Polyhedra and Polytopes

***** 4.1 Polyhedra, H-Polytopes and V-Polytopes

***** 4.2 The Equivalence of H-Polytopes and V-Polytopes

***** 4.3 The Equivalence of H-Polyhedra and V-Polyhedra

***** 4.4 Fourier-Motzkin Elimination and Cones

*** 5 Projective Spaces and Polyhedra, Polar Duality

***** 5.1 Projective Spaces

***** 5.2 Projective Polyhedra

***** 5.3 Tangent Spaces of Hypersurfaces

***** 5.4 Quadrics (Affine, Projective) and Polar Duality

*** 6 Basics of Combinatorial Topology

***** note 幾何與拓撲

      - x -
        在這本書的處理方式下
        幾何與拓撲是沒有分離的
        當考慮的是嵌入在歐氏空間中的圖形的時候
        [当强调 complex 有一种几何实现的时候]
        沒有考慮過還能給這些圖形以別樣的幾何

***** note 細節

      - x -
        可以發現這本書中對理論的處理非常細緻
        但是我覺得例子還是不夠

      - k -
        也許你可以仔細讀這本書
        然後自己補充例子
        也可以形成自己的理解方式與描述方式

***** 6.1 Simplicial and Polyhedral Complexes

      - Recall that
        a simplex is just the convex hull of a finite number of affinely independent points.
        We also need to define faces, the boundary, and the interior of a simplex.

      - x -
        在一般的處理代數拓撲的方式中
        把 simplex 定義成這樣看似是沒有必要的
        但是
        可能只有這種定義才能讓人把 simplicial complex 展示出來

      - k -
        但是重點是不要把這些圖形化出來不是嗎
        是要在不畫出來它們的條件下研究它們的性質

      - x -
        在這種構造方式中
        不能假設各個 simplex 的內點是不交的

      - k -
        但是這些以代數方式[組合方式]定義的圖形[尤其是流形]
        都可以被嵌入在高維的歐式空間中 [geometric realization]
        因此可以假定內點不交
        因此這種定義方式是具有一般性的

      - x -
        但是如此嵌入在高維空間中的圖形
        並不能被人所直觀
        如果說算數化的構造方式是爲了展示給人的化
        這種構造就也沒有什麼用處

      - A polytope is the geometric realization of some simplicial complex.

***** 6.2 Combinatorial and Topological Manifolds

      - A combinatorial d-manifold is any space, X,
        homeomorphic to the geometric realization, |K| ⊆ En,
        of some pure (simplicial or polyhedral) complex, K,
        of dimension d, whose faces are all nonsingular.

      - x -
        因此有關 manifold 的終點就在於 singularity 的定義
        但是當考慮 homotopy 的時候
        singularity 可以被化解掉

      - k -
        可能我們需要把這種化解 singularity 的想法構建到我們的語言中來

      - x -
        可能不光是要化解 singularity
        我們還要能夠在不同維度之間自由的變換
        利用 refl
        我們要能夠把點視爲線
        把線視爲面 等等

*** 7 Shellings and the Euler-Poincaré Formula

***** 7.1 Shellings

      - the aim is to proof Euler-Poincaré Formula inductively.
        but also can be viewed as finding the normal form.

***** 7.2 The Euler-Poincaré Formula for Polytopes

***** 7.3 Dehn-Sommerville Equations for Simplicial Polytopes

***** 7.4 The Upper Bound Theorem

*** 8 Dirichlet–Voronoi Diagrams

***** 8.1 Dirichlet–Voronoi Diagrams

***** 8.2 Triangulations

***** 8.3 Delaunay Triangulations

***** 8.4 Delaunay Triangulations and Convex Hulls

***** 8.5 Stereographic Projection and the Space of Spheres

***** 8.6 Stereographic Projection and Delaunay Polytopes

***** 8.7 Applications
