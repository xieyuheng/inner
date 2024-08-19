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

趣闻：

- 本书的第二作者 Johan de Kleer 是 Sussman 的学生。

# 总结

这本书有一个特点是，在主要 ideas 之外，
还掺杂了很多不必要的对各种 lisp programming 中的 tradeoff 的讨论。

其实，既然所有的 solver 都是以 Simon 的 problem space model 为基础的，
那么就可以以 problem space model 为核心 idea 来组织全书。

用不需要讨论那么多 tradeoff 的程序语言来介绍这些概念，
应该能成为很好的学习材料。

也许正如 Kent Beck 所说的，所有的 commit 分为两类：

- 一类是改变代码行为的 behavior change；

- 一类是不改变代码行为，
  而只优化代码结构的 structural change，
  即 refactoring。

一个 commit 应该只包含一种类型的 change，而不应该混合。
讲课教书也是如此，主要是介绍解决方案的书，
和介绍 refactoring 的书应该分开，
或者如果目标是想要在一本书中同时讲授方案与 refactoring，
至少要做到将它们分开在不同的章节中。

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

## 3.1 The problem space model

problem space model 来自 Simon 的书：

- [5] Newell, A., and Simon, H., Human Problem Solving, Prentice Hall, 1972.

> The problem space model of problems and problem solving makes
> similar use of the ideas of location, shape, and distance that are
> so useful in physical space.

## 3.2 CPS design

CPS 就是本章标题 classical problem solving 的缩写，
也是所实现的最基础的 problem space model 的框架的名字。

- 可惜 CPS 这个缩写与 continuation passing style 的缩写重名了。

我决定在我的代码库中完全不用缩写，
用 `classical-solver/`
作为这部分代码的文件夹名就可以了。

> Conceptually, CPS consists of two parts: an interface for
> user-supplied problem spaces, and a search engine.

`State` 的相关的接口函数：

1. Goal detection:
   Ascertain whether a given state
   satisfies the goal criterion.

2. State identity:
   Detect when two descriptions of states
   refer to the same state.

3. State display:
   Produce a human-readable description
   of a given state.

Operators 的接口：

1. Identify what operators are available.

2. Determine whether a given operator is
   applicable to a particular state.

3. Given a state and an operator applicable to it,
   ascertain all the ways the operator
   can be instantiated on that state.

   - 注意，这里一个 operator 作用于 state 上，
     可能给出多种新的 state，而不是一个。
     也可以理解为一个用户给出的是一个模糊的 operator，
     系统有机会将它翻译成多个精确的 operators。

4. Figure out what new state results
   from applying an instantiated operator to a state.

这里列出了四点，但是在简单的实现中，
可能只需要一个 `State` 到 `List(State)` 的函数就可以了。

搜索的策略：

- breadth-first search = FIFO queue

- depth-first search = LIFO queue

- best-first search = sorting the queue according to
  the minimum estimated distance to the goal

- beam search = best-first search + limit on the size of the queue

> What should elements of the queue be? Clearly the state to be
> explored must be part of it. For some problems, that would be
> enough—we may only care about the solution to an equation, for
> example, not how it was derived. For other problems the path taken
> to the goal is of paramount importance: a chess program needs the
> path the search took in order to move towards a win. Consequently,
> when a search succeeds we stipulate that the search engine must
> return the path it found between initial and goal states. The path
> is a list of alternating states and operator instances.

## 3.3 CPS implementation issues

关于代码结构：

- 解释为什么不用 CLOS，用 common-lisp 的 defstruct。
  这个选择也许是正确的，因为 CLOS 的语法很啰嗦。

## 3.4 The CPS implementation

common-lisp 的类型系统太乱了，
我用一个想象中的 lisp 来记录这里的实现。

```scheme
(define-class problem ((state type) (operator type))
  :name string
  :goal-recognizer (-> state boolean)
  :operator-applier (-> operator state (list (pair operator state)))
  :operators (list operator)
  :state-equal? (-> state state boolean))

(define boston
  (make (problem boston-state boston-operator)
    :name "boston"
    :goal-recognizer (lambda (the-state) ...)
    :operator-applier (lambda (the-operator the-state) ...)
    :operators (make-list ...)
    :state-equal? (lambda (left-state right-state) ...)))
```

在书中 :operator-applier 这个 API 的设计不是很好，
operator 可能是一个 pattern of operators，
首先要找到在给定 state 下适用的具体 operator 列表，
然后给出具体 operator 到 state 的 map。
这其实是说 :operators 是依赖 state 的。
所以正确的设计是：

```scheme
:operator-applier (-> operator state state)
:operators (-> state (list operator))
```

并且这两个函数其实还可以复合成：

```scheme
(-> state (list (tuple operator state)))
```

但是我们需要返回 solution-path，
而 operator 就在 solution-path 中，
所以明显地出现在 API 中也是合理的。

并且命名也有问题，
class 的名字不应该叫 problem，而应该叫 classical-solver，
这个 solver 的状态代表当前搜索（solving）的状态，
state 的名字应该直接用 problem，
goal 应该叫 solution。

```scheme
(define-class classical-solver ((problem type) (operator type))
  :name string
  :problem-is-solution? (-> problem boolean)
  :problem-equal? (-> problem problem boolean)
  :valid-operators (-> problem (list operator))
  :operator-apply (-> operator problem problem)
  :problem-expand (-> problem (list (pair operator problem))))

(define boston
  (make (classical-solver boston-problem boston-operator)
    :name "boston"
    :problem-is-solution? (lambda (the-problem) ...)
    :problem-equal? (lambda (left-problem right-problem) ...)
    :valid-operators (lambda (the-problem) ...)
    :operator-apply (lambda (the-operator the-problem) ...)
    :problem-expand (lambda (the-problem) ...)))
```

关于代码结构：

- 作者还在这里说，为什么不依赖尾递归，而是用循环语法。
  其实就是反对 scheme 而倾向于 common-lisp 的偏见。

- 还讲到了，如果需要的话，在循环中使用多个 return 是好的风格。

这里的 API 还有一个严重的问题是，搜索到一个结果就结束了，
并且放弃了目前 queue 中搜索到的其他未完成的 path，
这浪费了很多计算，并且没法处理想要多个结果的情况。
正确的设计是，solver 本身可以像一个 generator 一样，
返回 next solution。

TODO 第 32 页。

# 4 Pattern-Directed Inference Systems
# 5 Extending Pattern-Directed Inference Systems
# 6 Introduction to Truth Maintenance Systems
# 7 Justification-Based Truth Maintenance Systems
