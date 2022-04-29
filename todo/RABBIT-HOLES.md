# design

- design 的词源
- design 是关于选择的 笔记
- https://xyheme.gitee.io/inner/note/computer-science/architecture/architecture-with-ashi-krishnan.md.html

# foundations

- [vv] How I became interested in foundations of mathematics
- [vv] Lecture about Univalent Foundations at the Institut Henri Poincaré
- [Roland Backhouse] Do-it-yourself Type Theory
  - how to understand `prop`
- [frege] Begriffsschrift

# type theory

- [martin-löf] Constructive Mathematics and Computer Programming
  - can we use `<:` and `:` for all judgements?
- [martin-löf] Intuitionistic Type Theory
- [alexander-grothendieck]
- [henk-barendregt] Lambda Calculus With Types
  - recursive type
  - intersection type
- [de-bruijn] automath

# inductive type

- [zhaohui-luo] Computation and Reasoning
- [peter Dybjer] Inductive Families

# category theory

# topos theory

- we are interested in topos theory
  because we want to know
  is topos theory the algebraic semantics
  behind the use of partial function?
- use partial function to specify algebraic structure
  - essentially algebraic theories
  - C-system
  - "Aspects of topoi", by Peter Freyd, 1972.
  - what is the main idea of topos theory?
    capture membership relation?
  - what is the relation between topos and partial function?
  - this might be very useful to
    - use algebraic structure in language without dependent type
    - capture composition of pi type in the dependent type theory

# categorical semantics

- https://ncatlab.org/nlab/show/categorical+model+of+dependent+types
- inversion principle of inference rule is special case of adjoint functor
- is it make sences to introduce (- A) and { A } to factor A -> B ?
- some questions for luo
  - what is categorical semantics of inductive type ? -- F-algebra, and initial algebra
  - what is categorical semantics of dependent type ? -- also adjoint functor

# a theory of object

# vladimir voevodsky

- more about C System

# bebop

# psychology

- ideas and cognitive dissonance
  - ideas that is good to survive
  - ideas that lower cognitive dissonance

# logic

- [note] we study a logic by its
  - proof theory
  - and categorical semantics
- modal logic
- linear logic
- relevance logic
- framework for substructural logics

# proof theory

- [schroeder-heister]
  - standard formulation of elimination rule for implication
    v.s. another elimination rules to follow the same pattern,
    that is, the pattern exhibited by the rules of falsehood, disjunction, and existence elimination,
    has been considered by Schroeder-Heister.
- [dag-prawitz] proof theory and natural deduction

# algebraic data type

- maybe before implementing inductive type
  we should study algebraic data type first

# jojo

- in jojo cut is the only inference rule that requires two premises
  - inference rule with one premise can be viewed as equation?
- Monoidal category, should we understand element in language as object instead of arrow,
  and use the bifunctor of Monoidal category to model composition? (instead of composition of arrow.)

# combinatory logic

- [haskell-curry] combinatory logic

# report

- report on wiki product

# actor model

- armstrong_thesis_2003.pdf
- 神經元 as actor
  - 神經網絡 閥值 no timeout
    the state of an actor encode informations
    the whole network encode informations

# prover

- martin lof
  - formalize girard-s-paradox
- coq
- discovering-modern-set-theory
- find some theory to proof
  - number theory
    heyting arithmetic
  - plane geometry
  - must be able to use type class
- predicate logic as prover
  - ACL2
  - the little prover -- little ACL
- heyting algebra

# chiso

- classical logic
- cat
  - lambek

# physics

- 经典力学
  - 爲了理解無窮小分析在物理學中的應用
  - 兩組視頻
    walter lewin
    leonard susskind
  - 兩本書
    micheal spivak -- physics-for-mathematicians--1--mechanics
    structure-and-interpretation-of-classical-mechanics

# bishop

- 如何明確地敘述 bishop 的計劃?
- 對算法複雜度的形式化描述與機械化推導
  就是向 bishop 的計劃 更買進了一步

# cover space and galois theory

# formal concept analysis and galois connection

# fiber-space

- 完成對 fiber-space 的理解 並寫好 at1 的綱領
- 意譯 euler 以學教學法 與代數基礎
- 讀古典 以批判分析的算數化

# math reading list

- classical
  - newton
  - euler
- at
  - thurston/three-dimensional-geometry-and-topology--volume-1.djvu
  - dehn/papers-on-group-theory-and-topology--max-dehn.djvu
  - sze-tsen-hu/homotopy-theory.djvu
  - sze-tsen-hu/elements-of-general-topology.djvu
  - cell-complex/the-topology-of-cw-complexes--albert-lundell.djvu
  - hatcher/AT.pdf
  - norman-steenrod/how-to-write-mathematics--norman-steenrod.djvu
  - norman-steenrod/the-topology-of-fibre-bundles.djvu
  - algebraic-topology/simplicial-homotopy-theory.pdf
- ag
  - algebraic-geometry-a-problem-solving-approach.pdf
- dc
  - a-comprehensive-introduction-to-differential-geometry
- cs
  - feynman/lectures-on-computation.pdf

# database

- cmu database course
