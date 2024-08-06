---
title: Building Problem Solvers
authors: [Kenneth D. Forbus, Johan de Kleer]
year: 1993
---

# 学习动机

动机 A：

- 介绍 TMS 的奠基论文我看不懂，也许这个教科书级别的介绍可以看懂。
- 之所以想学 TMS 是因为 propagator 中用到了 TMS 为 cell 的 content。

动机 B：

- 也许这里介绍的 Problem Solvers 的实现，
  可以与 Polya 的 "How to Solve Problem" 中介绍的技术联系起来。

# 1 Preface

## 1.1 The role of this book

> Assuming familiarity with Common Lisp allows us to explore more
> advanced topics.

> We focus on techniques for building reasoning systems, instead of
> attempting to cover every variety of Al programming.  For example,
> we do not explore the design of natural language systems, vision
> programs, learning systems, or neural networks.

> We tie principles of problem-solving techniques to practice. We
> discuss the underlying principles of each problem-solving technique,
> then demonstrate the ideas through programs that clearly exhibit
> their essence. Finally, how these ideas work is illustrated through
> several examples, often substantial, to provide a deeper
> understanding.

这种教学方式很棒，可以开一个 `problem-solvers` 的 git repo 来跟着学了。

# 2 Introduction

> What makes an AI program different from other programs? Two key
> differences are explicit representation of knowledge and increased
> modularity. The ability to use explicit representations of knowledge
> is perhaps the best hallmark of Al programs. Traditional programs,
> to be sure, have knowledge embedded in their procedures and data
> structures. But AI programs include structures that can be
> interpreted declaratively, that is, independently of any single
> usage, both by the program and the programmer.

在后 ChatGPT 时代，很难想象有人能对 AI program 做出这样的定义。
在现在这个时代，应该如何定义 AI program 呢？
可能是用到神经网络吧。

## 2.1 Comparing AI reasoning systems to conventional programs

讽刺的是，现在用来实现神经网络的代码，
更像是这里想要批判的代码的样子
-- "a large slab of procedural code"。

这里看似在对比代码的属性，
但是其实可以理解为是在描述，
那个年代 AI 应用的实现模式。

这一点从下面的断言中可见一斑：

> A program that stores the absolute minimum amount of information
> required to get a particular job done will always perform faster
> than one that must consult explicit knowledge and record the
> rationales for its decisions.

## 2.2 Phases of AI programming

这里对 Phases 的描述还是蛮不错的，
甚至区分了 Kent Beck 说过的探索阶段和收获阶段：

- 探索阶段需要 speed of coding 和 program simplicity。
- 收获阶段需要 robustness 和 clean interfaces。

## 2.3 Outline of the book

> The bulk of this book focuses on exploiting truth maintenance
> systems in building reasoning systems. The literature on truth
> maintenance systems has often been turgid and confusing.

这正是我读这本书的原因。

> We impose a classification scheme on such systems, in hopes of
> bringing order. Roughly, we break truth maintenance systems into
> justification-based, logic-based, and assumption-based systems.
> These basic ideas are developed in Chapter 6, with the next eight
> chapters devoted to exploring their implications, including how to
> interface truth maintenance systems to various inference engines,
> and how to use them effectively as part of a larger problem-solving
> system.

从晦涩的论文，到具体的代码，
并且还分类组织好了，
并且还考虑到了如何应用。

# 3 Classical Problem Solving

> In the early days of AI, it was often hoped that a small set of
> grand principles could be found that provided the basis for
> understanding the nature of intelligence, much as Newton's laws
> provided a basis for understanding the interactions of force,
> matter, and motion. One of the first principles proposed was
> search. Why search? Cognitive science starts with the assumption
> that human intelligence is a computational process. A natural
> question to ask is, what kind of computation is it? Intelligence
> seems utterly unlike simple algorithms, such as sort routines or
> accounting systems. Such algorithms perform a single task extremely
> well, but cannot deal with situations where what to do next isn't
> clear. Intelligence seems to require the ability to try something
> out, look at how well it did, and try something else until you get
> something that works. That is search.

也许，人类的认知有两大类：

- search 代表理性；
- pattern recognition 代表直觉。

> While few today hold that search is the single key idea underlying
> intelligence, most would agree that search has a central role to
> play in building Al programs.  Here we examine how to implement a
> classical model of problem solving, the problem space model[5], in a
> clean and modular fashion.

但是感觉 ChatGPT 之类的大模型，
表现出了理性，但是没有搜索，
而是一个超大规模的 pattern recognition。

# 4 Pattern-Directed Inference Systems
# 5 Extending Pattern-Directed Inference Systems
# 6 Introduction to Truth Maintenance Systems
# 7 Justification-Based Truth Maintenance Systems
