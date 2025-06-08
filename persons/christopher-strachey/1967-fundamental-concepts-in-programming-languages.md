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

TODO

# 1 Preliminaries

TODO

# 1.1 Introduction

TODO
