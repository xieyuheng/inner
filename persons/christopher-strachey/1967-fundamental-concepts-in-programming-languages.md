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
> generally as strings of symbols on paper—and this in turn seems to
> lead to a preoccupation with the problems of syntax.

TODO

# 2 Basic concepts

## 2.1 Assignment commands
