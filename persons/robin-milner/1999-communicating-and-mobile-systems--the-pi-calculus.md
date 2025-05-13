---
title: communicating and mobile systems
subtitle: the pi calculus
author: robin milner
year: 1999
---

# My Motive

[2025-05-09] 为了看懂 philip wadler 的 2014-propositions-as-sessions，
需要先了解一下 pi calculus。

# My Summary

[2025-05-14] 总结一下需要记住的 ideas。

数学在于想象力，
所以首先要记住的 automata 的 black box 模型，
对一个现象要有多种观察的角度，想象才能丰富。
所以还要记住 automata 的 transition graph
这第二种观察 automata 的角度。

transition graph 来自 labeled transition system，
类似 rewrite system 但是每个 rewrite step 带有 label，
label 可以理解为 event 或者输入，
因此这种 system 所建模的是 interactive system。

还要记住 process expression 所形成的 process calculus，
这比直接描述 automata 所形成的 graph 的表达能力要强很多，
用 expression 来构造 automata，其特点就是适于 composition，
可以用递归了描述非有限的 automata。

每个 process 对应一个 automata，
process 递归调用自己的时候带上不同的参数，
就代表 automata 的不同状态，
这和 erlang 类似。

给 process expression 加上 concurrent composition，
就把 labeled transition system 重新转化为了
unlabeled rewrite system。

给 concurrent composition 中所匹配的数据带上 channel，
只有这样才能有有效的实现。

让 channel 成为 first class value，就能描述 mobility 了。

还有一些关于理论证明的 ideas，
比如 simulation 和 congruent 下次再总结。
simulation 可以联系到一般的二元关系，
congruent 可以追溯到其几何起源。

# 1 Introduction

# 2 Behaviour of Automata

## 2.3 The language of an automaton

通过在一个代数结构中解方程，
来找到一个 automaton 所对应的 regular language。

本书的后面进一步以这个代数结构为基础，
发展出了 process calculus，也就是一种 rewrite system。
这种发展我还是第一次简单。

## 2.5 Black boxes, or reactive systems

这里的 black box 模型可以用来理解下面将要介绍的，
用 simulation 来定义的等价关系。

black box 隐藏了 automata 的状态，
black box 上的按钮对应于 automata 的 event。

# 3 Sequential Processes and Bisimulation

## 3.2 Strong simulation

用 automata 所能接受的语言，可以定义 automata 之间的等价关系。
这里想要定义新的等价关系，来区分更多的 automata。
想要区分的重点，恰好和 petri net 中的两种 or 类似，
即 deterministic or 和 non-deterministic or。

可以用 black box 模型来理解这里的 simulation，
如果以任意一个状态为起点，
一个 black box 的所有操作（所有按钮序列），
都可以被另一个 black box 模仿而不卡住，
就说后一个 black box 就可以 simulate 前一个。

注意，这里要求对应的按钮相同，
但是 black box 会隐藏 automata 的状态。

为了用数学语言定义上面的等价关系，
作者引入了两个 automata state 之间的，
保持 transition 的二元关系。

## 3.4 Sequential process expressions

这一节应该是对语言设计者而言最重要的，
也是对读 2014-propositions-as-sessions 而言最重要的。

> In the work that follows, especially when we introduce concurrency,
> it will help to represent each state of a system by a _process
> expression_, which carries information about both the behaviour and
> the structure of the system.

可见计算机科学中的很多研究，
是通过设计新的形式语言来完成的。

注意，这里的 process 表达式的语法，
就是来自前面解方程来找 automata 所对应的正则表达式时，
相应的代数结构中的表达式！

```c
process A(a, b) = choice {
  a.A(a, b)
  b.B(a, a)
}

process B(c, d) = c.d.0
```

```scheme
(define (A a b)
  (choice
   [a (A a b)]
   [b (B a a)]))

(define (B c d)
  (choice
   [c d 0]))
```

这样看来，process calculus 的表达式，
也可以用来描述 automata。

## 3.5 Boolean buffer

```c
process Buff2 = choice {
  in(0).Buff2(0)
  in(1).Buff2(1)
}

process Buff2(i) = choice {
  out(i).Buff2
  in(0).Buff2(0, i)
  in(1).Buff2(1, i)
}

process Buff2(i, j) = choice {
  out(j).Buff2(i)
}
```

```scheme
(define Buff2
  (choice
   [(in 0) (Buff2 0)]
   [(in 1) (Buff2 1)]))

(define (Buff2 i)
  (choice
   [(out i) Buff2]
   [(in 0) (Buff2 0 i)]
   [(in 1) (Buff2 1 i)]))

(define (Buff2 i j)
  (choice
   [(out j) (Buff2 i)]))
```

一个 process 表达式的前缀可以不只是一个 symbol，
还可以带参数，这样 substitution 的情况就复杂多了。

## 3.6 Scheduler

```c
process Scheduler = Sched(1, {})
process Sched(i, X) = {
  if (in(i, X)) {
    option_sum(X, (j) => finish(j).Sched(i, remove(X, j)))
  } else {
    option_sum(X, (j) => choice {
      finish(j).Sched(i, remove(X, j))
      start(i).Sched(mod(add1(i), n), add(X, i))
    })
  }
}
```

```scheme
(define Scheduler (Sched 1 {}))

(define (Sched i X)
  (if (in i X)
    (choice-sum X (lambda (j) (finish j) (Sched i (remove X j))))
    (choice-sum X (lambda (j)
                     (choice
                      [(finish j) (Sched i (remove X j))]
                      [(start i) (Sched (mod (add1 i) n) (add X i))])))))
```

## 3.7 Counter

```c
process Count = Count(0)
process Count(n) = if (equal(n, 0)) choice {
  inc.Count(1)
  [zero].Count(0)
} else choice {
  inc.Count(add1(n))
  [dec].Count(sub1(n))
}
```

```scheme
(define Count (Count 0))
(define (Count n)
  (if (equal n 0)
    (choice
     [inc (Count 1)]
     [(op zero) (Count 0)])
    (choice
     [inc (Count (add1 n))]
     [(op dec) (Count (sub1 n))])))
```

# 4 Concurrent Processes and Reaction

## 4.2 Observations and reactions

这里有解释 Petri nets 是如何作为 automata 的推广的，
并且解释了 Petri nets 图语法的设计来源。

## 4.3 Concurrent process expressions

Example 4.3:

```c
process P = concurrent {
  scope (a) concurrent {
    choice {
      a.Q1
      b.Q2
    }
    [a]
  }
  choice {
    [b].R1
    [a].R2
  }
}

// one result

concurrent {
  scope (a) Q1
  choice {
    [b].R1
    [a].R2
  }
}

// another result

concurrent {
  scope (a) concurrent {
    Q2
    [a]
  }
  R1
}
```

```scheme
(define P
  (concurrent
   (scope (a)
     (concurrent
      (choice
       [a Q1]
       [b Q2])
      (op a)))
   (choice
    [(op b) R1]
    [(op a) R2])))

;; one result

(concurrent
  (scope (a) Q1)
  (choice
   [(op b) R1]
   [(op a) R2]))

;; another result

(concurrent
 (scope (a)
   (concurrent
    Q2
    (op a)))
 R1)
```
