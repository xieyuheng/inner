---
title: the history of combinatorial group theory: a case study in the history of ideas
authors: [bruce chandler, wilhelm magnus]
year: 1982
---

# info

- Nearly the entire body of research in the field
  is due to mathematicians who either are still alive
  or who were the teachers or senior colleagues of living mathematicians.
  This makes it possible to supplement the written tradition
  with oral information which is particularly valuable
  when dealing with questions of motivation
  for a particular investigation or of the transfer of ideas.

- x -
  it seems the tradition of writing hides motivations,
  while the dialog reveals motivations.

  it might also be the authors who fail to convey their motivations
  are not good at their art.

# Part I The Beginning of Combinatorial Group Theory

## 1 Introduction to Part I

- Part I of our book covers the period from 1882 to 1918,
  the end of the first World War.

## 2 The Foundation: Dyck's Group-Theoretical Studies

- in Dyck's paper :
  For the further development of the present group theoretical problems
  the analytic (combinatorial) formulation has to replace every geometric description.
  However, the primary geometric interpretation has produced certain viewpoints
  and it is the purpose of the present paper
  to develop both their geometric version and their analytic content.

- "a geometric concretization (Versinnlichung) of the group G."
  like the theory of fuchsian groups.

  tessellation of the hyperbolic disk

- Burnside's textbooks.

- De Siguier used Cantor's set theory
  and coined the term 'semigroup'.

- What we called Proposition 1
  [there exist free group for a set of generators]
  is then what Dehn called the solution of the word problem for free groups.

- x -
  當 [像 Dehn 一樣]
  用 模型的實現 與 算法 來理解這些理論時
  某些堂皇的命題就是顯然的事實了

  作者區分了 The existence proof 與 The combinatorial proof
  後者是 構造主義 的

## 3 The Origin: The Theory of Discontinuous Groups

- poincare and klein

- discontinuous action
  discontinuous group acting on space

- the early theory of discontinuous groups did not stimulate
  the development of the theory of group presentations,
  because the discontinuous groups are not primarily given by presentations.
  They are defined either by a set of generating elements
  which describe circle-preserving conformal self-mappings of the complex plane [geometric]
  or as a set of 2x2 matrices, subject to arithmetic conditions [arithmetic].

## 4 Motivation: The Fundamental Groups of Topological Spaces

- poincare -> tietze -> dehn

- [bib]

  - Birman, J. S.
    Poincare's conjecture and the homeotopy group of a closed, orientable 2-manifold
    1973

  - heinrich tietze
    on the topological invariants of multidimensional manifolds
    1908

- Tietze's results are motivated by an observation of a specific difficulty
  arising when working with groups which are merely givenby a presentation.

- Dehn recognized that the difficulties of combinatorial group theory
  start at a much lower level than that of the isomorphism problem as stated by Tietze.

- [bib]

  - Greendlinger, M, 1960a,
    Dehn's algorithm for the word problem,
    Comm. Pure and Appl.Math.

  - Greendlinger, M., 1960b,
    Dehn's algorithm for the conjugacy and word problems, with applications,
    Comm. Pure and Appl. Math.

## 5 The Graphical Representation of Groups

- cayley diagram -> burnside [finite order] -> coxeter [finite order]

- The point to be made here is that
  this is such an extremely simple definition
  when phrased in topological terms.
  Any possible translation into algebraic terms
  appears to be clumsy and opaque.
  Of course, one may say that
  all that matters about a definition is its precision.
  But this was certainly not Dehn's opinion.
  In a public talk for a nonmathematical academic audience,
  Dehn [1928] explained that the continued growth of mathematics
  requires the continued emergence of new ideas which reduce its complexity.
  He also expressed the hope that
  topology would renew its power through such an injection of new ideas.
  The reader should note the year when this was said.
  Within the following 10 years,
  there appeared monographs on topology by
  Alexandroff and Hopf,
  by Lefshetz, and by Seifert and Threlfall,
  documenting an upsurge in topological research which has not yet abated.

  - Dehn, M, 1928,
    Uber die geistige Eigenart des Mathematikers,
    Frankfurter Universitatsreden No. 27. 25 pp.,
    Universitatsdruckerei Wemer und Winter, Frankfurt am Main.
    Preface and I.5,

## 6 Precursors of Later Developments

### note

- x -
  這章節值得仔細分析
  既然 羣論 可以用於描述諸多現象
  那麼 推廣而得到的 高階代數 是否也能描述這些現象的推廣呢?

- k -
  如果可以的話
  就可以說 羣論 到 高階代數 的推廣是一個成功的推廣了

### A. Arithmetically Defined Linear Groups in Higher Dimensions

- siegel

### B. Arithmetically Defined Linear Groups in Two Dimensions

- magnus

### C. Geometric Constructions. Fuchsian Groups

### D. Braid Groups and Mapping Class Groups

- hurwitz
  in a fiber space, the fundamental group of the base space
  acts as a group of self-mappings on the fiber.

### E. Differential Equations, Linear Groups,and Lie Groups

### F. Finite Groups

## 7 Summary

- Group theory started as the theory of groups of transformations,
  i.e., of one-to-one and onto self-mappings of a mathematical object.
  If the object is a finite set, the group appears as a permutation group.
  [Camille Jordan on "substitutions and algebraic equations"]

- Infinite transformation groups
  became a topic of extensive research at about the same time.
  But this development was initiated not by a fundamental work but by a manifesto.
  [Felix Klein at Erlangen]
  [Sophus Lie]
  a program, according to which
  geometry should be considered from the point of view
  of transformation groups which act on certain spaces.

  for to get the generators of the group of Klein as in combinatorial group theory,
  the concept of generators has to be modified
  in the sense of Lie's "infinitesimal substitutions".

- the tiling designs or tessellations
  of the euclidean or non-euclidean plane
  with a polygon as the basic tile,
  The generators and their inverses
  simply correspond to the edges of the basic polygonal tile,
  and the defining relations correspond to its vertices.

  this is the dual of the cayley-graph,
  so we have three graphs now,
  (1) underlying-space [classifying-space]
  (2) covering-space [cayley-graph]
  (3) acting-space [dual-cayley-graph]

- finite group vs infinite group
  for finite group, we have :
  | finite group            | number            |
  | finite simple group     | prime number      |
  | direct product of group | product of number |
  but for infinite group, we do not have such.

- The object on which a group of transformations acts
  has disappeared completely
  if we define a group through a presentation by generators and relations.

  while Poincare's fundamental group
  makes group generators and relations worth study.

- the theory of representations of groups
  as linear transformations of finite-dimensional vector spaces

  [Frobenius Burnside, and L. E. Dickson]
  [linear groups over finite fields]

- combinatorial group theory emphasis on algorithms.
  "to find a method of deciding in a finite number of steps"

- combinatorial group theory
  geometric -> geometric interpretation
  algebraic -> classify and decomposition

## 8 Modes of Communication. Growth and Distribution of Research in Group Theory

## 9 Biographical Notes

## 10 Notes on Terminology and Definitions

- It is part of the purpose of every historical essay
  to make it easier to go back to the sources for those readers
  who may be interested in doing so.

## 11 Sources

# Part II The Emergence of Combinatorial Group Theory as an Independent Field

## 1 Introduction to Part II

- This part of our book deals mainly with the developments during the period
  from 1918 to 1945, that is, from the end of World War I
  to the end of World War II.

- the mapping class group of a surface
  is the group of automorphisms
  of the fundamental group of the surface.

  - x -
    how about mapping-class-group with fixed boundary?

## 2 Free Groups and Their Automorphisms

- Nielsen
  Hopf
  McCool

- [bib]

  - McCool, J., 1974,
    A presentation of the automorphism group of a free group of finite rank

  - McCool, J. 1975a,
    On Nielsen's presentation of the automorphism group of a free group

  - McCool, J., 1975b,
    Some finitely presented subgroups of the automorphism group of a freegroup

## 3 The Reidemeister-Schreier Method

## 4 Free Products and Free Products with Amalgamations

## 5 One-Relator Groups

## 6 Metabelian Groups and Related Topics

## 7 Commutator Calculus and the Lower Central Series

## 8 Varieties of Groups

## 9 Topological Properties of Groups and Group Extensions

- this may be described as
  a method of associating a topological cell complex
  with the group and deriving results about the group from this association.

- it was shown that
  the association of cell complexes and groups
  leads to a purely algebraicde finition of homology and cohomology groups
  belonging to any given group G which are invariants of G.

## 10 Notes on Special Groups

## 11 Postscript: The Impact of Mathematical Logic

## 12 Modes of Communication

## 13 Geographical Distribution of Research and Effects of Migration

## 14 Organization of Knowledge
