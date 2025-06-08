---
title: fundamental concepts in programming languages
author: christopher strachey
year: 1967
---

# My Motive

[2025-06-08] 据说下面这些概念都是从这篇论文来的：

- R-value and L-value
- ad hoc polymorphism
- parametric polymorphism
- referential transparency

所以要读一下。

# Foreword

Foreword 的作者是 Peter Mosses 写于 2000。

> Strachey’s paper starts with some philosophical remarks about the
> need to focus on semantic issues in the design of programming
> languages, and to “recognise and isolate the central concepts --
> things analogous to the concepts of continuity and convergence in
> analysis”.

这为程序语言理论的 Edinburgh 学派奠定了基础：

- [The History of Standard ML: Ideas, Principles, Culture](https://www.youtube.com/watch?v=NVEgyJCTee4&ab_channel=ICFPVideo)

类比 continuity 和 convergence 之类的概念，
就是把布尔巴基结构主义的研究方法带到程序语言理论中来。

> Strachey then proceeds to give a clear and incisive exposition of
> many of his insights into programming language design and formal
> semantics, covering the following main topics:
>
> - assignment commands, L- and R-values;
> - expression evaluation and environments;
> - commands and sequencing;
> - modes of parameter-passing and variable binding;
> - functions and routines as data items;
> - types and (parametric) polymorphism; and
> - compound data structures and pointers.

> He also indicates how to model some of these concepts
> using λ-expressions.

> The style of semantics proposed by Strachey in this paper was
> further developed (and put on a firm mathematical foundation) in his
> collaboration with Dana Scott, which started in 1969; initially
> referred to as ‘mathematical semantics’, or simply as
> ‘Scott-Strachey semantics’, the framework has since become known as
> denotational semantics.

> Thus Strachey’s paper has undoubtedly a high degree of historical
> interest and relevance. His practical experiences with programming
> throughout the early days of computing (he had an excellent
> reputation as a master programmer) together with his deep
> involvement in programming-language design, had made him
> exceptionally well-qualified to address the fundamental concepts of
> programming languages.

> One potential difficulty with reading Strachey’s paper is that most
> of the illustrations of programs are given in a lesser-known
> language called CPL, which had been developed by Strachey and his
> colleagues during the mid-1960’s.

可以尝试把 CPL 的例子改成 lisp 语法。

> Another possible source of confusion is that the semantic functions
> L and R for expressions, introduced in Sect. 3.3.2, do not
> explicitly take any environment arguments. However, Strachey clearly
> explains that “we speak of evaluating an expression in an
> environment (or sometimes relative to an environment) which provides
> the values of component [identifiers]” (Sect. 3.2.2), and moreover,
> the representation of functions in Sect. 3.5.2 makes explicit
> reference to such an environment.

也许是在实现时把 environment 当作全局变量了，
但是如果实现为全局变量，就没法在递归调用中更改 environment 了。

# Abstract

> This paper forms the substance of a course of lectures given at the
> International Summer School in Computer Programming at Copenhagen in
> August, 1967. The lectures were originally given from notes and the
> paper was written after the course was finished.

> In spite of this, and only partly because of the shortage of time,
> the paper still retains many of the shortcomings of a lecture
> course. The chief of these are an uncertainty of aim -- it is never
> quite clear what sort of audience there will be for such
> lectures—and an associated switching from formal to informal modes
> of presentation which may well be less acceptable in print than it
> is natural in the lecture room.

带有 informal mode 是好事情。

> There are numerous references throughout the course to CPL
> [1–3]. This is a programming language which has been under
> development since 1962 at Cambridge and London and Oxford. It has
> served as a vehicle for research into both programming languages and
> the design of compilers.

> The lack of a precise formulation for CPL should not cause much
> difficulty in this course, as we are primarily concerned with the
> ideas and concepts involved rather than with their precise
> representation in a programming language.

# 1 Preliminaries

## 1.1 Introduction

> Any discussion on the foundations of computing runs into severe
> problems right at the start. The difficulty is that although we all
> use words such as ‘name’, ‘value’, ‘program’, ‘expression’ or
> ‘command’ which we think we understand, it often turns out on closer
> investigation that in point of fact we all mean different things by
> these words, so that communication is at best precarious.

研究程序语言的语义的动机，
首先是想要把自己的想法在与同事的交流之中表达楚。
让人想起 Peirce 的 "how to make our ideas clear"。

> These misunderstandings arise in at least two ways. The first is
> straightforwardly incorrect or muddled thinking. An investigation of
> the meanings of these basic terms is undoubtedly an exercise in
> mathematical logic and neither to the taste nor within the field of
> competence of many people who work on programming languages.  As a
> result the practice and development of programming languages has
> outrun our ability to fit them into a secure mathematical framework
> so that they have to be described in ad hoc ways. Because these
> start from various points they often use conflicting and sometimes
> also inconsistent interpretations of the same basic terms.

程序语言领域缺乏良好的数学框架做基础。

> A second and more subtle reason for misunderstandings is the
> existence of profound differences in philosophical outlook between
> mathematicians.

## 1.2 Philosophical considerations

> The important philosophical difference is between those
> mathematicians who will not allow the existence of an object until
> they have a construction rule for it, and those who admit the
> existence of a wider range of objects including some for which there
> are no construction rules. (The precise definition of these terms is
> of no importance here as the difference is really one of
> psychological approach and survives any minor tinkering.)

即直觉主义数学或者说构造主义数学，与形式主义数学的区别。
注意，由于数学对象是在想象中的，
所以这里说的是 "a construction rule for it"，
而不是真的 construction。

> This may not seem to be a very large difference, but it does lead to
> a completely different outlook and approach to the methods of
> attacking the problems of programming languages.

构造对形式的差异如何影响人们解决程序语言中的问题？
在程序语言领域除了构造主义好像不能有别的主义存在，
毕竟当面对机器的时候人们是不得不构造的。

> The advantages of rigour lie, not surprisingly, almost wholly with
> those who require construction rules. Owing to the care they take
> not to introduce undefined terms, the better examples of the work of
> this school are models of exact mathematical reasoning.

"models of exact mathematical reasoning"
指证明论中的各种 logic system。

> Unfortunately, but also not surprisingly, their emphasis on
> construction rules leads them to an intense concern for the way in
> which things are written -- i.e., for their representation,
> generally as strings of symbols on paper -- and this in turn seems
> to lead to a preoccupation with the problems of syntax.

研究构造主义数学和证明论的人，
需要构造逻辑系统，这就需要设计语法。
但是人们经常陷在语法相关的问题中，而忽略了语义。

> This is probably an unfair criticism, for, as will become clear
> later, I am not only temperamentally a Platonist and prone to
> talking about abstracts if I think they throw light on a discussion,
> but I also regard syntactical problems as essentially irrelevant to
> programming languages at their present stage of development. In a
> rough and ready sort of way it seems to me fair to think of the
> semantics as being what we want to say and the syntax as how we have
> to say it.

lisp 可以缓解这一点，
即减少人们在处理语法上所花费的时间。

> In these terms the urgent task in programming languages is to
> explore the field of semantic possibilities.

semantic possibilities 在我看来，
很大一部分包括探索各种计算模型。

即便是在现在，对计算模型的探索还在初级阶段，
很多模型都没有被实现为能够让人满意地编程的语言。
比如：

- interaction nets
- petri nets
- pi calculus
- propagator
- ...

> When we have discovered the main outlines and the principal peaks we
> can set about devising a suitably neat and satisfactory notation for
> them, and this is the moment for syntactic questions.

> But first we must try to get a better understanding of the processes
> of computing and their description in programming languages.
> In computing we have what I believe to be a new field of mathematics
> which is at least as important as that opened up by the discovery
> (or should it be invention?) of calculus.

> We are still intellectually at the stage that calculus was at when
> it was called the ‘Method of Fluxions’ and everyone was arguing
> about how big a differential was.

> We need to develop our insight into computing processes and to
> recognise and isolate the central concepts -- things analogous to
> the concepts of continuity and convergence in analysis.

如果人们对于程序语言的理解，
正如作者所言还在如此初期，
那么发展的潜力就还很大。
对比无穷小分析的发展，感觉确实如此。

> To do this we must become familiar with them and give them names
> even before we are really satisfied that we have described them
> precisely. If we attempt to formalise our ideas before we have
> really sorted out the important concepts the result, though possibly
> rigorous, is of very little value -- indeed it may well do more harm
> than good by making it harder to discover the really important
> concepts. Our motto should be ‘No axiomatisation without insight’.

这里可以看出，发展理论的过程和软件开发的过程非常类似。
比如这里说的很多建议类似 Sendi Metz 和 Kent Beck 关于软件开发的讨论。
‘No axiomatisation without insight’ 对应的就是「不要过早抽象」。

> However, it is equally important to avoid the opposite of perpetual
> vagueness. My own view is that the best way to do this in a rapidly
> developing field such as computing, is to be extremely careful in
> our choice of terms for new concepts. If we use words such as
> ‘name’, ‘address’, ‘value’ or ‘set’ which already have meanings with
> complicated associations and overtones either in ordinary usage or
> in mathematics, we run into the danger that these associations or
> overtones may influence us unconsciously to misuse our new terms --
> either in context or meaning.

这是认知语言学的观点。

> For this reason I think we should try to give a new concept a
> neutral name at any rate to start with. The number of new concepts
> required may ultimately be quite large, but most of these will be
> constructs which can be defined with considerable precision in terms
> of a much smaller number of more basic ones. This intermediate form
> of definition should always be made as precise as possible although
> the rigorous description of the basic concepts in terms of more
> elementary ideas may not yet be available. Who when defining the
> eigenvalues of a matrix is concerned with tracing the definition
> back to Peano’s axioms?

> Not very much of this will show up in the rest of this course. The
> reason for this is partly that it is easier, with the aid of
> hindsight, to preach than to practice what you preach. In part,
> however, the reason is that my aim is not to give an historical
> account of how we reached the present position but to try to convey
> what the position is. For this reason I have often preferred a
> somewhat informal approach even when mere formality would in fact
> have been easy.

early formality 类似于 early abstraction。

# 2 Basic concepts

## 2.1 Assignment commands

> One of the characteristic features of computers is that they have a
> store into which it is possible to put information and from which it
> can subsequently be recovered. Furthermore the act of inserting an
> item into the store erases whatever was in that particular area of
> the store before -- in other words the process is one of
> overwriting. This leads to the assignment command which is a
> prominent feature of most programming languages.

> The simplest forms of assignments such as

```
x := 3
x := y + 1
x := x + 1
```

```scheme
(set! x 3)
(set! x (add1 y))
(set! x (add1 x))
```

> The following assignment commands show this danger.

```
            i := a > b j, k
         A[i] := A[a > b j, k]
A[a > b j, k] := A[i]
   a > b j, k := i
```

```scheme
(set! i (if (> a b) j k))
(vector-set! A i (vector-ref A (if (> a b) j k)))
(vector-set! A (if (> a b) j k) (vector-ref A i))
(set! (if (> a b) j k) i) ;; not valid in standard scheme
(if (> a b) (set! j i) (set! j k))
```

就赋值命令而言，等号左边的表达式和等号右边的表达式意义不同。

就 variable 而言，确实要区分 L-value 和 R-value；
但是就 vector（或 list）而言，
这样的区分只是为了把 `vector-set!` 和 `vector-ref` 这两个函数，
强行捏在一起，使用一个语法元素来表达 `:=`。

> Roughly speaking an expression on the left stands for an ‘address’
> and one on the right for a ‘value’ which will be stored there.

> We shall therefore accept this view and say that there are two
> values associated with an expression or identifier. In order to
> avoid the overtones which go with the word ‘address’ we shall give
> these two values the neutral names: L-value for the address-like
> object appropriate on the left of an assignment, and R-value for the
> contents-like object appropriate for the right.

发明 L-value 和 R-value 这对术语是为了实践前文提到的认知语言学。

## 2.2 L-values and R-values

> An L-value represents an area of the store of the computer. We call
> this a location rather than an address in order to avoid confusion
> with the normal store-addressing mechanism of the computer.

> The two essential features of a location are that it has a content
> -- i.e. an associated R-value -- and that it is in general possible
> to change this content by a suitable updating operation.

## 2.3 Definitions

TODO

## 2.4 Names
## 2.5 Numerals
## 2.6 Conceptual model

# 3 Conceptual constructs

## 3.1 Expressions and commands

## 3.2 Expressions and evaluation

### 3.2.1 Values
### 3.2.2 Environments
### 3.2.3 Applicative structure
### 3.2.4 Evaluation
### 3.2.5 Conditional expressions

## 3.3 Commands and sequencing

### 3.3.1 Variables
### 3.3.2 The abstract store
### 3.3.3 Commands

## 3.4 Definition of functions and routines

### 3.4.1 Functional abstractions
### 3.4.2 Parameter calling modes
### 3.4.3 Modes of free variables
### 3.4.4 Own variables
### 3.4.5 Functions and routines
### 3.4.6 Constants and variables
### 3.4.7 Fixed and free
### 3.4.8 Segmentation

## 3.5 Functions and routines as data items

### 3.5.1 First and second class objects
### 3.5.2 Representation of functions

## 3.6 Types and polymorphism

### 3.6.1 Types
### 3.6.2 Manifest and latent
### 3.6.3 Dynamic type determination
### 3.6.4 Polymorphism
### 3.6.5 Types of functions

## 3.7 Compound data structures

### 3.7.1 List processing
### 3.7.2 Nodes and elements
### 3.7.3 Assignments
### 3.7.4 Implementation
### 3.7.5 Programming example
### 3.7.6 Pointers
### 3.7.7 Other forms of structure

# 4 Miscellaneous topics

## 4.1 Load-Update Pairs
## 4.2 Macrogenerators
## 4.3 Formal semantics
