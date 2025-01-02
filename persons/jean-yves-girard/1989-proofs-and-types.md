---
title: Proofs and Types
author: Jean-Yves Girard
Translaters: [Paul Taylor, Yves Lafont]
year: 1989
---

# 学习动机

[2025-01-01] 系统地学习 Girard 理解逻辑学的方式。

# 1 Sense, Denotation and Semantics

## 1.1 Sense and denotation in logic

> A first attempt is to say that we have an _equality_

```
27 × 37 = 999
```

> This equality makes sense in the mainstream of mathematics by saying
> that the two sides denote the same integer and that × is a function
> in the Cantorian sense of a graph.

> This is the denotational aspect, which is undoubtedly correct, but
> it misses the essential point:

> There is a finite _computation_ process which shows that the
> denotations are equal.

Paul Lockhart 在 2017-arithmetic 中虽然讲的是 arithmetic，
也就是 compute to normal form，但是所强调的却是 "denotational aspect"。

> Concretely we ask a question, 27 × 37, and get an answer, 999.  The
> two expressions have different senses and we must do something (make
> a proof or a calculation, or at least look in an encyclopedia) to
> show that these two senses have the same denotation.

> Whereas denotation was modelled at a very early stage, sense has
> been pushed towards subjectivism, with the result that the present
> mathematical treatment of sense is more or less reduced to syntactic
> manipulation. This is not a priori in the essence of the subject,
> and we can expect in the next decades to find a treatment of
> computation that would combine the advantages of denotational
> semantics (mathematical clarity) with those of syntax (finite
> dynamics).

为什么说 syntax 和它的 finite dynamics 没有 mathematical clarity？
denotational semantics 如果能被实现出来，就是解释器。
也许大多数 denotational semantics 都没法被实现出来。

> So, one of the most fundamental distinctions in logic is that made
> by Frege: given a sentence A, there are two ways of seeing it:

> - as a sequence of instructions, which determine its _sense_, for
>   example A ∨ B means “A or B”, etc..

> - as the ideal result found by these operations: this is its
>   _denotation_.

sense 是对人而言的语义，
而 denotation 是数学意义上的语义。
如果这部分数学理论被实现出来了，
就是对计算机而言而语义，
就是解释器用来解释语法的 evaluation 函数。

> Two sentences which have the same sense have the same denotation,
> that is obvious; but two sentences with the same denotation rarely
> have the same sense.  For example, take a complicated mathematical
> equivalence A ⇔ B. The two sentences have the same denotation (they
> are true at the same time) but surely not the same sense, otherwise
> what is the point of showing the equivalence?

denotation 的 domain 中尽管定义了等价关系，
但是人们还是能在这个等价关系之外区分出来等价类中的元素。

> This example allows us to introduce some associations of ideas:

> - sense, syntax, proofs;
> - denotation, truth, semantics, algebraic operations.

> That is the fundamental dichotomy in logic.

为什么 sense 是和 syntax 一组的？
也许因为 syntax 是最底层的 equivalence，
当两个东西的 syntax 相等时，
任何人的 sense 都会认为它们是相等的。

### 1.1.1 The algebraic tradition

> This tradition (begun by Boole well before the time of Frege) is
> based on a radical application of Ockham’s razor: we quite simply
> discard the sense, and consider only the denotation. The
> justification of this mutilation of logic is its operational side:
> it works!

> In fact, considering logic from the point of view of denotation,
> i.e. the result of operations, we discover a slightly peculiar kind
> of algebra, but one which allows us to investigate operations
> unfamiliar to more traditional algebra. In particular, it is
> possible to avoid the limitation to -- shall we say -- equational
> varieties, and consider general definable structures. Thus Model
> Theory rejuvenates the ideas and methods of algebra in an often
> fruitful way.

denotation 在于 the result of operations，
就是说在 domain 中定义的函数与运算。

avoid the limitation to equational varieties，
应该是指传统代数几何中的 variety 概念。
只有满足很传统的运算律才能发展代数几何，
逻辑所带来的 denotation domain 中的函数与运算，
经常不具备这些运算律。

### 1.1.2 The syntactic tradition

> On the other hand, it is impossible to say “forget completely the
> denotation and concentrate on the sense”, for the simple reason
> that the sense contains the denotation, at least implicitly. So it
> is not a matter of symmetry. In fact there is hardly any unified
> syntactic point of view, because we have never been able to give an
> operational meaning to this mysterious sense. The only tangible
> reality about sense is the way it is written, the formalism; but the
> formalism remains an unaccommodating object of study, without true
> structure, a piece of soft camembert.

我觉得未必如此。
从线性的 postfix notation 的 word sequence，到 lisp 的 tree，
再到 proof nets 和 interaction nets 的 graph，
这一路看来，语法的本质应该是容易被人们轻易掌握的拓扑结构。

> Does this mean that the purely syntactic approach has nothing
> worthwhile to say? Surely not, and the famous theorem of Gentzen of
> 1934 shows that logic possesses some profound symmetries at the
> syntactical level (expressed by cut-elimination). However these
> symmetries are blurred by the imperfections of syntax. To put it in
> another way, they are not symmetries of syntax, but of sense.

这样我基本可以把 sense 理解为，
直觉主义所说的人类朴素的拓扑直觉了。

> So, summing up our opinion about this tradition, it is always in
> search of its fundamental concepts, which is to say, an operational
> distinction between sense and syntax. Or to put these things more
> concretely, it aims to find deep geometrical invariants of syntax:
> therein is to be found the sense.

"it aims to find deep geometrical invariants of syntax"，
像不像是在说 to find deep topological invariants？
我觉得二者是等价的，
对于 tree 和 graph 来说是如此，
对于书写和具体的语法设计而言甚至还需要几何结构。

下面说 computer science 从 algebraic 学派手上拯救了证明论，哈哈哈。

> The disaster was averted because of computer science -- that great
> manipulator of syntax -- which posed it some very important
> theoretical problems.

"that great manipulator of syntax"
也在于 bits 之间的等价也是最基本的等价关系，
并且人们在用计算机编程时所考虑的 pointer 等概念，
也是一直在诉诸于拓扑直觉。

> We are led, then, to a revision of proof theory, from the
> fundamental theorem of Herbrand which dates back to 1930. This
> revision sheds a new light on those areas which one had thought were
> fixed forever, and where routine had prevailed for a long time.

> In the exchange between the syntactic logical tradition and computer
> science one can wait for new languages and new machines on the
> computational side.  But on the logical side (which is that of the
> principal author of this book) one can at last hope to draw on the
> conceptual basis which has always been so cruelly ignored.

有一种以发展为己任的感觉，相比 CS 领域的快速发展，不能落下风。

## 1.2 The two semantic traditions

### 1.2.1 Tarski

> This tradition is distinguished by an extreme platitude: the
> connector “∨” is translated by “or”, and so on.

哈哈哈。

这里有一个对 Tarski 的 Model Theory 的很好的简短总结。

> 1. For atomic sentences, we assume that
>    the denotation is known; for example:
>    - 3 + 2 = 5 has the denotation t.
>    - 3 + 3 = 5 has the denotation f .

> 2. The denotations of the expressions A ∧ B, A ∨ B, A => B and ¬A
>    are obtained by means of a truth table.

> 3. The denotation of ∀ξ.A is t iff
>    for every a in the domain of interpretation, A[a/ξ] is t.
>    Likewise ∃ξ.A is t iff A[a/ξ] is t for some a.

> Once again, this definition is ludicrous from the point of view of
> logic, but entirely adequate for its purpose. The development of
> Model Theory shows this.

### 1.2.2 Heyting

> Heyting’s idea is less well known, but it is difficult to imagine a
> greater disparity between the brilliance of the original idea and
> the mediocrity of subsequent developments. The aim is extremely
> ambitious: to model not the denotation, but the proofs.

> Instead of asking the question “when is a sentence A true?”, we
> ask “what is a proof of A?”.  ... We take the view that what we
> write as a proof is merely a description of something which is
> already a process in itself.

然后描述了 Heyting 对逻辑的理解，其实就是构造主义对逻辑的理解。

这里给出了一个非常贴近程序语言的，
对构造主义逻辑的解释，
这样没有看似 tautology 的废话了。
我就不摘抄了，因为这些已经太熟悉了。

> Undeniably, Heyting semantics is very original: it does not
> interpret the logical operations by themselves, but by abstract
> constructions. Now we can see that these constructions are nothing
> but typed (i.e. modular) programs. But the experts in the area have
> seen in this something very different: a functional approach to
> mathematics. In other words, the semantics of proofs would express
> the very essence of mathematics.

这种与 Tarski 的逻辑学的对比，
是非常好的介绍 "proposition as type" 的方式。
相当于是一种复习，然后对比差异才能让观众体会到后者厉害的地方。

> The tradition of Heyting is original, but fundamentally has the same
> problems. ... If we wish to explain A by the act of proving A, we
> come up against the fact that the definition of a proof uses
> quantifiers twice (for => and ∀). Moreover in the => case, one cannot
> say that the domain of definition of f is particularly well
> understood!

> Since the => and ∀ cases were problematic (from this absurd
> foundational point of view), it has been proposed to add to clauses
> 4 and 6 the codicil “together with a proof that f has this
> property”.

以 4 为例：

> 4. A proof of A => B is a function f,
>    which maps each proof p of A to a proof f (p) of B.

这其实就是说需要实现一个函数的类型检查器，
而类型检查器就是证明检查器，
所以也可以理解为逻辑的意义最终来自于类型检查器之实现。

# 2 Natural Deduction

> As we have said, the syntactic point of view shows up some profound
> symmetries of Logic. Gentzen’s sequent calculus does this in a
> particularly satisfying manner. Unfortunately, the computational
> significance is somewhat obscured by syntactic complications that,
> although certainly immaterial, have never really been overcome.
> That is why we present Prawitz’ natural deduction before we deal
> with sequent calculus.

我怀疑，以类 forth 的 stack-based language 为基础，
外加 Prowl 的 costack 的想法，
可能就能给出 sequent calculus 的忠实的 term language。

- https://github.com/UberPyro/Prowl-Ideas-Pile/issues/3

> Natural deduction is a slightly paradoxical system: it is limited to
> the intuitionistic case (in the classical case it has no
> particularly good properties) but it is only satisfactory for the
> (∧, =>, ∀) fragment of the language: we shall defer consideration
> of ∨ and ∃ until chapter 10. Yet disjunction and existence are the
> two most typically intuitionistic connectors!

为什么说 ∨ and ∃ 没有处理好？
我感觉目前的语言已经把 ∨ and ∃ 处理的挺好了。

> The basic idea of natural deduction is an asymmetry: a proof is a
> vaguely tree-like structure (this view is more a graphical illusion
> than a mathematical reality, but it is a pleasant illusion) with one
> or more hypotheses (possibly none) but a single conclusion. The deep
> symmetry of the calculus is shown by the introduction and
> elimination rules which match each other exactly. Observe,
> incidentally, that with a tree-like structure, one can always decide
> uniquely what was the last rule used, which is something we could
> not say if there were several conclusions.

proof 就是程序语言中的 expression，所以这里说的也是 expression。
说 tree-like structure 是 illusion，就在于在语法层面是 tree，
但是解释的时候局部变量会形成图。

这里对 tree-like expression 的批评，
和 2009-propagation-networks 中对 expression 的批判类似。

## 2.1 The calculus

natural deduction 模仿人类用自然语言证明数学定理的过程，
在引入 term language 之前，先要理解这一点。

### 2.1.1 The rules

## 2.2 Computational significance

> We shall re-examine the natural deduction system in the light of
> Heyting semantics; we shall suppose fixed the interpretation of
> atomic formulae and also the range of the quantifiers. A formula A
> will be seen as the set of its possible deductions; instead of
> saying “δ proves A”, we shall say “δ ∈ A”.

"A formula A will be seen as the set of its possible deductions"
看似是循环定义，因为 deduction 的定义依赖 A 的定义，
因为只有引入了 term language 才能消除这种表面上的循环定义，
让这句话看起来更自然一些。

### 2.2.1 Interpretation of the rules

用构造主义的方式去解释 rules，就引入了 term language。

### 2.2.2 Identification of deductions

描述 proofs 之间的等价关系。
如果使用了 term language，这里的描述就会简单很多。

# 3 The Curry-Howard Isomorphism

TODO

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
