---
title: Proofs and Types
author: Jean-Yves Girard
Translaters: [Paul Taylor, Yves Lafont]
year: 1989
---

# 学习动机

[2025-01-01] 系统地学习 Girard 理解逻辑学的方式。

[2025-01-02] 想要理解 linear logic 的起源，
说是起源于 coherence semantics of the sum，
但是这是一种 denotational semantics，
我完全看不懂为什么要定义 coherence spaces。
可能需要先读 Soctt 关于 denotational semantics 的论文，
然后才能看懂。

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

- 之所以要避免用 term language 来定义，
  可能是因为在 sequent calculus 和 linear logic 的情形下，
  人们还不知道 term language 是什么。

### 2.2.1 Interpretation of the rules

用构造主义的方式去解释 rules，就引入了 term language。

### 2.2.2 Identification of deductions

描述 proofs 之间的等价关系。
如果使用了 term language，这里的描述就会简单很多。

# 3 The Curry-Howard Isomorphism

> We have seen that Heyting’s ideas perform very well in the
> framework of natural deduction. We shall exploit this remark by
> establishing a formal system of typed terms for discussing the
> functional objects which lie behind the proofs.

term language 就是 "typed terms for discussing the functional objects
which lie behind the proofs"。

- 注意，从解释器的角度看，term 是 expression，
  而 functional object 是 value。

## 3.1 Lambda Calculus

Church 风格的 simple typed lambda calculus。

### 3.1.1 Types

> When we think of proofs in the spirit of Heyting,
> formulae become types.

### 3.1.2 Terms

> Proofs become terms; more precisely,
> a proof of A (as a formula)
> becomes a term of type A (as a type).

## 3.2 Denotational significance

> Types represent the kind of object under discussion.

为了定义 object 的集合，需要定义 object 之间的等价关系。
就用关于 term 的 beta 和 eta 两种类型的 rewrite rule 来定义。

当我们在实现类型检查器的时候，
通过 NbE 来计算 object 之间的等价，
其实我们实在定义 object 的集合。

## 3.3 Operational significance

> In general, terms will represent programs. The purpose of a program
> is to calculate (or at least put in a convenient form) its
> denotation. The type of a program is seen as a specification,
> i.e. what the program (abstractly) does. A priori it is a commentary
> of the form “this program calculates the sum of two integers”.

> At a more general level, abstracting away from any peculiar
> syntactic choice, one should see a type as an instruction for
> plugging things together.

在考虑 inet 和 propagator 的类型系统时这一点尤其明显。

> This idea of arbitrary pluggings seems mathematisable, but to
> attempt this would lead us too far astray.

Girard 的 mathematisable 是什么意思？

> This way of seeing variables and values as dual aspects of the same
> plugging phenomenon, allows us to view the execution of an algorithm
> as a symmetrical input/output process. The true operational
> interpretation of the schemes is still in an embryonic state (see
> appendix B).

appendix B 是关于 linear logic 的。
从古典的逻辑，获得了 linear logic，
linear logic 是什么程序语言的类型系统，
大家还不知道。

应该就是 inet，
但是 inet 中只有 principal port 能反应，
这一点在 linear logic 中如何体现？

## 3.4 Conversion

## 3.5 Description of the isomorphism

## 3.6 Relevance of the isomorphism

这种 proof 化简和 functional object
作为 rewrite system 之间的 isomorphism，
给出了一种方法论，让我们在发明了新的 logic 时，
寻找其对应的计算模型；
在发明了新的计算模型时，
寻找其对应的 logic 来作为类型系统。

linear logic 到 proof nets 就是使用这个方法论的例子。

> Basically, the two sides of the isomorphism are undoubtedly the the
> same object, accidentally represented in two different ways. It
> seems, in the light of recent work, that the “proof” aspect is
> less tied to contingent intuitions, and is the way in which one
> should study algorithms. The functional aspect is more eloquent,
> more immediate, and should be kept to a heuristic rôle.

在发现了 proof 和 term 之间的等价之后，
Girard 依然认为 proof 更本质。
可能是因为 linear logic 的 term 没有找到。

# 4 The Normalisation Theorem

## 4.1 The Church-Rosser property

> This property states the uniqueness of the normal form,
> independently of its existence.

## 4.2 The weak normalisation theorem

> This result states the existence of a normal form
> -- which is necessarily unique --
> for every term.

> Its immediate corollary is the decidability
> of denotational equality.

> There is perhaps a small difficulty hidden in calculating the normal
> forms, since the reduction is not a deterministic algorithm. That
> is, for fixed t, many conversions (but only a finite number) are
> possible on the subterms of t. So the theorem states the possibility
> of finding the normal form by appropriate conversions, but does not
> exclude the possibility of bad reductions, which do not lead to a
> normal form. That is why one speaks of weak normalisation.

解释术语 weak 的意义。

> Having said that, it is possible to find the normal form by
> enumerating all the reductions in one step, all the reductions in
> two steps, and so on until a normal form is found. This inelegant
> procedure is justified by the fact that there are only finitely many
> reductions of length n starting from a fixed term t.

对于 lambda calculus 而言，
这样的并行计算可能会产生分支和重复。
但是，对于 inet 而言，reduction 与顺序无关。
并且我们正是要利用 inet 来做并行计算。

> The strong normalisation theorem will simplify the situation by
> guaranteeing that all normalisation strategies are good, in the
> sense they all lead to the normal form. Obviously, some are more
> efficient than others, in terms of the number of steps, but if one
> ignores this (essential) aspect, one always gets to the result!

inet 的 normalisation 甚至更 strong，
因为每个 strategy 都同样 efficient。

## 4.3 Proof of the weak normalisation theorem

TODO

### 4.3.1 Degree and substitution
### 4.3.2 Degree and conversion
### 4.3.3 Conversion of maximal degree
### 4.3.4 Proof of the theorem

## 4.4 The strong normalisation theorem

> There are several methods to prove that every term (of the typed
> λ-calculus) is strongly normalisable:

> - internalisation: this consists of a tortuous translation of the
>   calculus into itself in such a way as to prove strong
>   normalisation by means of weak normalisation. Gandy was the first
>   to use this technique [Gandy].

> - reducibility: we introduce a property of “hereditary
>   calculability” which allows us to manipulate complex
>   combinatorial information. This is the method we shall follow,
>   since it is the only one which generalises to very complicated
>   situations. This method will be the subject of chapter 6.

# 5 Sequent Calculus

> The sequent calculus, due to Gentzen, is the prettiest illustration
> of the symmetries of Logic. It presents numerous analogies with
> natural deduction, without being limited to the intuitionistic case.

> This calculus is generally ignored by computer scientists . Yet it
> underlies essential ideas: for example, PROLOG is an implementation
> of a fragment of sequent calculus, and the “tableaux” used in
> automatic theorem-proving are just a special case of this
> calculus. In other words, it is used unwittingly by many people, but
> mixed with control features, i.e. programming devices. What makes
> everything work is the sequent calculus with its deep symmetries,
> and not particular tricks.  So it is difficult to consider, say, the
> theory of PROLOG without knowing thoroughly the subtleties of
> sequent calculus.

注释中提到了 Gallier 没有忽视 sequent calculus：

- [Gallier] J. Gallier, Logic for Computer Science, Harper and Row (1986).

这应该是一本不错的教科书。

> From an algorithmic viewpoint, the sequent calculus has no
> Curry-Howard isomorphism, because of the multitude of ways of
> writing the same proof. This prevents us from using it as a typed
> λ-calculus, although we glimpse some deep structure of this kind,
> probably linked with parallelism. But it requires a new approach to
> the syntax, for example natural deductions with several conclusions.

其实 new approach to the syntax 很简单，
实现 inet 的时候自然就会想到。

这里可以看出为什么在介绍 proof 和 term 之间的对应时，
Girard 认为 proof 更本质，因为他还不知道如何描述 term。

Girard 关于 sequent calculus
没有 Curry-Howard isomorphism 的观点，
可能影响了很多人。

注意，inet 对应于 untyped lambda calculus，
想要真的以 linear logic 为类型系统，
可能需要看类似 propagator 的计算模型，
和 untyped lambda calculus 一样，
在获得 proof 和 object 的对应之后，
inet 所给出的 rewrite system，
只是用来化简 proof 的 rewrite system。

Curry-Howard isomorphism 并非无意义的简单等价，
而是连接起来了两个本来可以独立研究的领域。

## 5.1 The calculus

### 5.1.1 Sequents

### 5.1.2 Structural rules

### 5.1.3 The intuitionistic case

> The intuitionistic restriction is in fact a modification to the
> management of the formulae -- the particular place distinguished by
> the symbol |- is a place where contraction is forbidden -- and from
> that, numerous properties follow. On the other hand, this choice is
> made at the expense of the left/right symmetry. A better result is
> without doubt obtained by forbidding contraction (and weakening)
> altogether, which allows the symmetry to reappear.

这里应该是 linear logic 这个 idea 的起源。
即，意识到能带来很多好处的直觉主义逻辑本质上来自于哪里。

### 5.1.4 The “identity” group

这里解释为什么 identity axiom 和 cut rule 是对称的。
可能了解了 cut elimination 的证明之后会对这一点有更好的理解。

```
------ identity axiom
A |- A

X |- A
     A |- Y
----------- cut
X    |-   Y
```

> The deep content of the two results is the same;
> they only differ in their syntactic dressing.

### 5.1.5 Logical rules

## 5.2 Some properties of the system without cut

TODO

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

> The fundamental idea of denotational semantics is to interpret
> reduction (a dynamic notion) by equality (a static notion). To put
> it in another way, we model the invariants of the calculi.

> We have in mind rather to take literally the naı̈ve interpretation
> -- that an object of type U → V is a function from U to V --
> and see if we can give a reasonable meaning to the word “function”.
> In this way of looking at things, we try to avoid being obsessed by
> completeness, but instead look for simple geometrical ideas.

我理解上面的 abstraction，
但是我完全不理解这一章的内容。

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

> Of course, our example shows that cut elimination in sequent
> calculus does not satisfy the Church-Rosser property: it even
> diverges in the worst way! There are two options to eliminate this
> pathology:
>
> - making the calculus asymmetric: this leads to intuitionistic logic;
>
> - forbidding structural rules, except the exchange which is harmless:
>   this leads to linear logic.

由于没有 term language，所举的例子不是很明显。

## B.2 Linear Sequent Calculus

用两种风格介绍 linear logic 的 inference rule。

- 对称的：sequent 有 antecedent 和 succedent
- 不对称的：sequent 只有 succedent

虽然我还不理解如何解释 linear logic 的规则，
但是一个事实是可以将 intuitionistic logic 翻译成 linear logic：

> ... in such a way that an intuitionistic formula is valid iff its
> translation is provable in Linear Sequent Calculus.

## B.3 Proof nets

> Here, we shall concentrate on the so-called multiplicative fragment
> of linear logic.

> From an algorithmic viewpoint, this fragment is very unexpressive,
> but this restriction is necessary if we want to tackle problems
> progressively. Furthermore, multiplicative connectors and rules can
> be generalised to make a genuine programming language.

也就是能够定义 datatype，
然后不用 if else，
而是用 pattern matching 来形成分支。
类似 haskell 与 prolog。

在 graph 语法中，link 与 cut 完全对称：

- link 用于引入 atom；
- cut 用于连接两个带有互补类型的 wire。

奇怪的是 ⊗ 和 ⅋ 的差异在于，
被连接的两个 wire 是否在同一个联通分支上。

- ⊗ 只能连接两个联通分支；
- ⅋ 只能连接一个联通分支。

在已有的规则和例子中，
我看不出来为什么要有联通分支的限制。
如果看之后的 inet，也是只有当 constructor 和 duplicator
分别与自己相连的时候才会消除 node 获得 wire 直连。
而这种消除 node 获得 wire 直连的现象，
现在发生在 ⊗ 和 ⅋ 的反应上。

TODO long trip condition 是什么？ see [Gir87]

另外一个疑问是，在这里把 sequent 解释为 graph 之后，
已经完全看不出来 succedent 是 disjunction 了。

## B.4 Cut elimination

> Proofs nets provide a very nice framework
> for describing cut elimination.

> Proposition: Any proof net reduces to a (unique) cut free one.

> To prove the proposition, it is enough to see that the rewrite rules
> defines a terminating and confluent relation, and a normal form is
> necessarily cut free, unless it contains circle, which is impossible
> in a proof net.

> Termination is obvious (the size decreases at each step) and
> confluence comes from the fact that conversions are purely local,
> the only possible conflicts being: reducing two wires to one wire.

这也是实现时必须用 wire 是 half-edge 而不是 edge。

所谓的 "purely local" 就是可以用过 shared-memory + multithread
来实现真正的并行计算，而不同的 thread 在处理不同的计算步骤时不会相互影响。

> This turbo cut elimination mechanism is the basic idea
> for generalising proof nets to non-multiplicative connectives
> (geometry of interaction).

听起来 geometry of interaction 就是为了这一点而发展出来的。

## B.5 Proof nets and natural deduction

> It is fair to say that proof nets are the natural deductions of
> linear logic, but with two notable differences:

> - Thanks to linearity, there is no need for parcels of hypotheses.

> - Thanks to linear negation, there is no need for discharge or for
>   elimination rules.

这后一点，体现在 inet 这种程序语言上，
就是 construction rule
与 elimination rule
没有本质的分别。

TODO 这里对于 natural deduction 和 proof nets 之间的对应，
没有给出 term syntax。我们可以尝试把 term syntax 都给出来。

> So linear logic is not just another exotic logic:
> it gives new insight into basic notions
> which had seemed to be fixed forever.
