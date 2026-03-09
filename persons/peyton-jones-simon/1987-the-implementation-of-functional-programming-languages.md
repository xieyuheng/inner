---
title: the implementation of functional programming languages
authors: [peyton jones simon, philip wadler, peter hancock, david turner]
year: 1987
---

# My Motive

[2026-02-28] 在实现 x-lisp 的编译器时，
我需要学习如何实现 pattern matching 的编译与类型检查。

[2026-03-04] 单独实现这里所描述的语言，
也是一个不错的练习，可以叫做 miranda-lisp。

相比之下 x-lisp 更像是 typescript，
而不像 miranda 和 haskell。

# 1 Introduction

## 1.2 Part I: Compiling High-level Functional Languages

介绍如何用 lazy graph reduction 实现类似 miranda 的语言。
把 miranda 编译到 enriched lambda calculus，
再编译到 lambda calculus。

然后用 lazy graph reduction 实现 lambda calculus。

## 1.3 Part II: Graph Reduction

关于 graph reduction 的知识也许可以用到 interaction nets 上。

## 1.4 Part II1: Advanced Graph Reduction

介绍如何用 G-machine 来实现 graph reduction。

# 2 The Lambda Calculus

## 2.5 The Denotational Semantics of the Lambda Calculus

### 2.5.1 The Eval Function

在学习类型系统与计算模型时，
我总是感到缺少一个基础，知识之树缺少一个稳固的根基。

从 Milner 还有同时代的学者对 denotational semantics 的态度来看，
denotational semantics 就是这个基础，也就是说以基于集合论的数学为基础。

但是基于集合论的数学基础，其自身就有很多问题。
为了避免这些问题，可以重新回到「可计算性」这个概念。
就是定义 bishop 集合的时候，要求其元素之判断，
以及元素之间的等价关系的判断，都有高效的算法。
也就是说「数学的基础是高效的算法」。

否则 denotational semantics 很容易被看成是，
用更复杂的东西来给更简单的东西以语义。
domain 作为集合可能不具有上面提到的「可计算性」，
甚至常常像实数的集合一样，不具有「可构造性」。

> We will take the existence and soundness of domain theory and
> denotational semantics for granted, and the framework they provide
> will prove to be quite useful. They are rich and beautiful areas of
> computer science, and Stoy [1981] is a good starting-point for
> further reading.

尽管有这些缺点，但是 denotational semantics 还是有用的，
其最重要的用处在于理解对 syntax object
以 operational 的方式所定义的等价关系，
比如 lambda calculus 的 conversion rules。
又比如用 infinite tree 来理解递归定义，
不同的递归定义可能展开成相同的 infinite tree。

这种意义上，也可以说 denotational semantics 是想把
operational equivalent class 定义为函数，
并且以函数值域中的元素的逆像为 equivalent class。

最好是能够给出 graph theory based denotational semantics，
因为 digraph 的表达能力足够强，可以表达递归，
并且任何用 c 语言写的代码，object 之间的 pointer 关系，就构成 digraph。
- 比如 2012-the-garbage-collection-handbook 中，实现 GC 的时候，
  这种 digraph 特别明显。

也就是说，一个程序语言的 graph theory based denotational semantics，
将会非常接近于这个程序语言的真实实现。
或者说，实现一个程序语言的过程，
就是给出这个语言的 graph theory based denotational semantics 的过程。

- [From Dynamic to Static Semantics, Quantitatively, Thomas Seiller](https://ar5iv.labs.arxiv.org/html/1604.05047)
- [A graph model for imperative computation, Guy McCusker](https://ar5iv.labs.arxiv.org/html/0910.5399)

# 4 Structured Types and the Semantics of Pattern-matching

## 4.2 Translating Miranda into the Enriched Lambda Calculus

支持 pattern-matching 的方式：

- 首先支持 pattern lambda abstraction（case-lambda），
  其 application 可能返回 FAIL。
  - 也许这个名字，可以具体化为 #pattern-match-fail。
  - 注意，FAIL 与所有 pattern matching 失败后所得的 ERROR 不同。

- 用 FAIL 这个特殊值，和类似 next 的 lazy operator（类似 or）
  把多个 application 组合起来。

- FAIL x = FAIL 可以用来处理多个参数的 application。

- 处理用 pattern 定义的函数时，用 eta-convention
  把函数转化为 pattern lambda abstraction 的 application。

## 4.3 The Semantics of Pattem-matching Lambda Abstractions

介绍如何写支持 case-lambda 的解释器。

由于有了 case-lambda 的概念，
所以写解释器的时候递归很顺利，
而不需要专门实现 pattem-matching 相关的函数。

## 4.4 Introducing case-expressions

这里说的 case-expression 就是 `(match)`（不允许 nest），
与之相比，application of case-lambda 的用法是灵活的，是可以自由组合的。

对于新 lisp 语言的实现而言，可以直接实现 `(match)`。

将 list lookup 优化为 map lookup，
或者将连续的 if else 优化为 jump table。

# 5 Efficient Compilation of Pattern-matching

这里设计 pattern-matching 编译算法的过程非常值得学习。

TODO 5.2.4 The Empty Rule

# 6 Transforming the Enriched Lambda Calculus

这章显然只有通过添加新的 constant 才能完成转换。
这践行了 curry 想要用组合子表达一切的思路。

注意，这里没有消除 lambda 表达式本身，
而组合子甚至可以消除 lambda 表达式。

这里的转化给人的印象是，在纯函数范式内是如此，
任何语法都可以被转化成组合子。

TODO 研究如何把 dependent type system
中的语法构造转化为组合子。
这可能就是 curry 未完成的事业。

### 6.2.8 Dependency Analysis

分析一个 letrec 所定义的变量之间的依赖关系，
然后做排序和分组，分成 let 和 小的 letrec。

分析的方式是先构造 dependency graph，
然后用 graph 的方法来分析。

directed graph 的 strongly connected component
就是给 letrec 分组的依据。

以 strongly connected component 为节点，
又形成 directed graph（coalesced graph），
这个 graph 是 acyclic 的，对这个 graph 做拓扑排序可以了。

## 6.3 Transforming case-expressions

`(match)` 会被编译为 `(case-list)`：

```scheme
(case-list list
  (let ()
    ...)
  (let ((head (li-head list))
        (tail (li-tail list)))
    ...))
```

这样 `case-list` 的类型就类似：

```scheme
(polymorphic (A B)
  (-> (list-t A) B B
      B))
```

注意 `case-list` 对其后两个参数而言是 lazy 的，
在 strict 语言中，需要用语法关键词的方式来实现 `case-list`，
或者用：

```scheme
(case-list list
  (lambda ()
    (let ()
      ...))
  (lambda ()
    (let ((head (li-head list))
          (tail (li-tail list)))
      ...)))

(polymorphic (A B)
  (-> (list-t A) (-> B) (-> B)
      B))
```

但是这是不合理的，因为多了一层 lambda 会降低效率。

注意，`case-list` 与 "the little typer" 中的 `match-list` 不同，
后者的类型更复杂：

```scheme
(match-list list
  ...
  (lambda (head tail) ...))

((list-matcher
  ...
  (lambda (head tail) ...))
 list)
```

这样做的缺点是，match 函数（比如 `match-list`）
不带有 variants 的名字，而是依赖顺序，
这样当 sum type 的 variants 很多时就不方便。
但是就编译器中间语言而言，也许是合理的。

此时，如果能支持 keyword argument，就可以解决这个问题。

```scheme
(match-list list
  :nil ...
  :li (lambda (head tail) ...))
```

给每个 data constructor 生成一个 match 函数，
然后再组合返回的 FAIL，也可以做到不依赖顺序：

```scheme
(match-sequence
 (match-nil ... list)
 (match-li (lambda (head tail) ...) list))

((matcher-sequence
  (match-nil ...)
  (match-li (lambda (head tail) ...)))
 list)
```

但是这种组合法的效率比一个 `match-list` 低，
后者相当于 `(match)`，前者相当于 `(case-lambda)`。

# 7 List Comprehensions

# 8 Polymorphic Type-checking

类型推导的重点是给每个表达式一个类型变量，
然后根据表达式的结构，形成类型变量之间的方程。

这种类型检查器的写法，只需要一个 infer 函数，
infer 函数在递归遍历一个表达式的时候，
有机会给每个表达式赋以类型变量。

TODO 而 dependent type 的 bidirectional 类型检查器，
同时使用 check 和 infer 两个函数。
这是为什么？

当类型不独立于值时，
也就是当类型依赖于值时，
也就是当类型中包含表达式时，
为什么没法通过解方程类做类型推导了？

给出一些无法利用解方程做类型推导的表达式的例子。

## 8.3 Type Inference

> In general, by examining the context of an expression, we may be
> able to deduce an expression for the form of the type of an object
> which can fit into that context. By examining the expression itself,
> we may be able to deduce the form of the types which that expression
> can take on. So we have two type expressions that will usually
> contain variables, the first giving the form of the type required by
> the context (deduced from the 'outside'), and the second giving the
> form of type which the object can take (deduced from the 'inside').
> For the whole expression to be well typed, these two type
> expressions must match, in the sense that by substituting for the
> schematic variables of the type expressions, they can be brought to
> the same form.

这里不是从列方程，然后求解的方式来理解类型推导的，
而是理解为算两次 type 然后做比较：
- 一次来自 "the context of an expression"；
- 一次来自 "the expression itself"。

从 "the expression itself" 计算 type
就是 type inference 的过程，也就是 `infer` 函数。

从 "the context of an expression" 计算 type
的过程一般不会通过函数明显表达出来，
而是写在 `infer` 函数的每个 case 中。

在 bidirectional type checking 中，
`check` 函数表达了一部分 "the context of an expression"，但是也不是全部，
全部的信息还是在 `infer` 和 `check` 函数的每个 expression 的 case 中。

## 8.4 The Intermediate Language

TODO 这里说如果不先对 letrec 做 6.2.8 所提到的 dependency analysis，
可能就没法完成类型检查，并且指向引用 [Mycroft 1984]。
我需要知道为什么会这样。

这里说对 pattern-matching 的类型检查，
要先转化为类似 `case-list` 的形式，
再使用这里的类型检查器，
这大大简化了类型检查器的实现！

> The type-checking algorithm can still be developed when
> pattern-matching is present in the language. Indeed for practical
> reasons it is better to type-check while the program is still close
> to the form in which it was entered, in order that error messages
> can refer to program text that the programmer can recognize.

尽管如此，知道 pattern-matching 能被转化为等价的组合子，
也能让我们确信类型检查器的正确性，
即，在直接实现 pattern-matching 的类型检查时，
确保其与组合子方案的类型检查效果一致。

## 8.5 How to Find Types

> Presumably, when we construct an expression E in a program, we
> reason to ourselves that it is well typed. As a product of this
> reasoning, we are in a position to say what the type is of any
> subexpression E' of E. We can, as it were, label each subexpression
> with the type which we think it has. When we enter that expression
> into the text of our program, that 'labelling' has been lost. It is
> the job of the type-checker to reason out the type structure of the
> expression once again, and to recover the labelling.

就是给每个 sub-expression 都加上 `(the)`。

比如，在 "the little typer" 中，
expression 被 elaborate 到 core expression 的过程，
就是要给每个 expression 都加上 `(the)`。

从「函数即证明」的角度看，给每个 sub-expression 加上 `(the)`，
就是补全 proof tree 的所有细节的过程。

这里介绍的类型推导的方式，
也是给每个 sub-expression 一个类型变量，
然后解方程。

这一章是对 Milner 类型系统的非常好的介绍。
对于为什么要特殊处理 `let`，
以及因该如何处理 `letrec`，
和在 lambda 中的 local `let`，
都有非常好的说明。

# 9 A Type-checker

## 9.3 Success and Failure

关于报错，这里推荐了 philip wadler 的论文：

- 1985-how-to-replace-failure-by-a-list-of-successes.pdf

## 9.4 Solving Equations

### 9.4.1 Substitutions

用函数来表示 substitution，
并且讨论 substitution 的相关概念：

- identity
- composition
- idempotent
  - 如果允许 not "fully worked out" 的 substitution，
    就可以用 idempotent 来定义 "fully worked out" 的 substitution。
    注意，这也排除了不能通过 occurrence check 的情况。
- fixpoint
  - 一个 substitution 的 fixpoint 就是 "deep walked" value。

## 9.5 Keeping Track of Types

介绍 "type scheme"，
也就是我所说的 polymorphic type 或者 nu type。
