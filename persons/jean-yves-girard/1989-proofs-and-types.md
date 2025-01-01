---
title: Proofs and Types
author: Jean-Yves Girard
Translaters: [Paul Taylor, Yves Lafont]
year: 1989
---

# 学习动机

系统地学习 Girard 理解逻辑学的方式。

# 1 Sense, Denotation and Semantics

## 1.1 Sense and denotation in logic
### 1.1.1 The algebraic tradition
### 1.1.2 The syntactic tradition
## 1.2 The two semantic traditions
### 1.2.1 Tarski
### 1.2.2 Heyting

# 2 Natural Deduction

## 2.1 The calculus
### 2.1.1 The rules
## 2.2 Computational significance
### 2.2.1 Interpretation of the rules
### 2.2.2 Identification of deductions

# 3 The Curry-Howard Isomorphism

## 3.1 Lambda Calculus
### 3.1.1 Types
### 3.1.2 Terms
## 3.2 Denotational significance
## 3.3 Operational significance
## 3.4 Conversion
## 3.5 Description of the isomorphism
## 3.6 Relevance of the isomorphism

# 4 The Normalisation Theorem

## 4.1 The Church-Rosser property
## 4.2 The weak normalisation theorem
## 4.3 Proof of the weak normalisation theorem
### 4.3.1 Degree and substitution
### 4.3.2 Degree and conversion
### 4.3.3 Conversion of maximal degree
### 4.3.4 Proof of the theorem
## 4.4 The strong normalisation theorem

# 5 Sequent Calculus

## 5.1 The calculus
### 5.1.1 Sequents
### 5.1.2 Structural rules
### 5.1.3 The intuitionistic case
### 5.1.4 The “identity” group
### 5.1.5 Logical rules
## 5.2 Some properties of the system without cut
### 5.2.1 The last rule
### 5.2.2 Subformula property
### 5.2.3 Asymmetrical interpretation
## 5.3 Sequent Calculus and Natural Deduction
## 5.4 Properties of the translation

# 6 Strong Normalisation Theorem

## 6.1 Reducibility
## 6.2 Properties of reducibility
### 6.2.1 Atomic types
### 6.2.2 Product type
### 6.2.3 Arrow type
## 6.3 Reducibility theorem
### 6.3.1 Pairing
### 6.3.2 Abstraction
### 6.3.3 The theorem

# 7 Gödel’s system T

## 7.1 The calculus
### 7.1.1 Types
### 7.1.2 Terms
### 7.1.3 Intended meaning
### 7.1.4 Conversions
## 7.2 Normalisation theorem
## 7.3 Expressive power: examples
### 7.3.1 Booleans
### 7.3.2 Integers
## 7.4 Expressive power: results
## 7.4.1 Canonical forms
## 7.4.2 Representable functions

# 8 Coherence Spaces

## 8.1 General ideas
## 8.2 Coherence Spaces
### 8.2.1 The web of a coherence space
### 8.2.2 Interpretation
## 8.3 Stable functions
### 8.3.1 Stable functions on a flat space
### 8.3.2 Parallel Or
## 8.4 Direct product of two coherence spaces
## 8.5 The Function-Space
### 8.5.1 The trace of a stable function
### 8.5.2 Representation of the function space
### 8.5.3 The Berry order
### 8.5.4 Partial functions

# 9 Denotational Semantics of T

## 9.1 Simple typed calculus
## 9.1.1 Types
## 9.1.2 Terms
## 9.2 Properties of the interpretation
## 9.3 Gödel’s system
### 9.3.1 Booleans
### 9.3.2 Integers
### 9.3.3 Infinity and fixed point

# 10 Sums in Natural Deduction

## 10.1 Defects of the system
## 10.2 Standard conversions
## 10.3 The need for extra conversions
### 10.3.1 Subformula Property
### 10.3.2 Extension to the full fragment
## 10.4 Commuting conversions
## 10.5 Properties of conversion
## 10.6 The associated functional calculus
### 10.6.1 Empty type (corresponding to ⊥)
### 10.6.2 Sum type (corresponding to ∨)
### 10.6.3 Additional conversions

# 11 System F

## 11.1 The calculus
## 11.2 Comments
## 11.3 Representation of simple types
### 11.3.1 Booleans
### 11.3.2 Product of types
### 11.3.3 Empty type
### 11.3.4 Sum type
### 11.3.5 Existential type
## 11.4 Representation of a free structure
### 11.4.1 Free structure
### 11.4.2 Representation of the constructors
### 11.4.3 Induction
## 11.5 Representation of inductive types
### 11.5.1 Integers
### 11.5.2 Lists
### 11.5.3 Binary trees
### 11.5.4 Trees of branching type U
## 11.6 The Curry-Howard Isomorphism

# 12 Coherence Semantics of the Sum

## 12.1 Direct sum
## 12.2 Lifted sum
### 12.2.1 dI-domains
## 12.3 Linearity
### 12.3.1 Characterisation in terms of preservation
### 12.3.2 Linear implication
## 12.4 Linearisation
## 12.5 Linearised sum
## 12.6 Tensor product and units

# 13 Cut Elimination (Hauptsatz)

## 13.1 The key cases
## 13.2 The principal lemma
## 13.3 The Hauptsatz
## 13.4 Resolution

# 14 Strong Normalisation for F

## 14.1 Idea of the proof
### 14.1.1 Reducibility candidates
### 14.1.2 Remarks
### 14.1.3 Definitions
## 14.2 Reducibility with parameters
### 14.2.1 Substitution
### 14.2.2 Universal abstraction
### 14.2.3 Universal application
## 14.3 Reducibility theorem

# 15 Representation Theorem

## 15.1 Representable functions
### 15.1.1 Numerals
### 15.1.2 Total recursive functions
### 15.1.3 Provably total functions
## 15.2 Proofs into programs
### 15.2.1 Formulation of HA2
### 15.2.2 Translation of HA2 into F
### 15.2.3 Representation of provably total functions
### 15.2.4 Proof without undefined objects

# A Semantics of System F

## A.1 Terms of universal type
### A.1.1 Finite approximation
### A.1.2 Saturated domains
### A.1.3 Uniformity
## A.2 Rigid Embeddings
### A.2.1 Functoriality of arrow
## A.3 Interpretation of Types
### A.3.1 Tokens for universal types
### A.3.2 Linear notation for tokens
### A.3.3 The three simplest types
## A.4 Interpretation of terms
### A.4.1 Variable coherence spaces
### A.4.2 Coherence of tokens
### A.4.3 Interpretation of F
## A.5 Examples
### A.5.1 Of course
### A.5.2 Natural Numbers
### A.5.3 Linear numerals
## A.6 Total domains

# B What is Linear Logic?

## B.1 Classical logic is not constructive
## B.2 Linear Sequent Calculus
## B.3 Proof nets
## B.4 Cut elimination
## B.5 Proof nets and natural deduction
