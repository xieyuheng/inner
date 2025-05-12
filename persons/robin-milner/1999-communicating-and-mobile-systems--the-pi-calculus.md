---
title: communicating and mobile systems
subtitle: the pi calculus
author: robin milner
year: 1999
---

# My Motive

[2025-05-09] 为了看懂 philip wadler 的 2014-propositions-as-sessions，
需要先了解一下 pi calculus。

# 1 Introduction

# 2 Behaviour of Automata

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
process A(a, b) = options {
  a.A(a, b)
  b.B(a, a)
}

process B(c, d) = c.d.0
```

```scheme
(define (A a b)
  (options
    (=> a (A a b))
    (=> b (B a a))))

(define (B c d)
  (=> c d 0))
```

这样看来，process calculus 的表达式，
也可以用来描述 automata。

## 3.5 Boolean buffer

```c
process Buff2 = options {
  in(0).Buff2(0)
  in(1).Buff2(1)
}

process Buff2(i) = options {
  out(i).Buff2
  in(0).Buff2(0, i)
  in(1).Buff2(1, i)
}

process Buff2(i, j) = options {
  out(j).Buff2(i)
}
```

```scheme
TODO
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
    option_sum(X, (j) => options {
      finish(j).Sched(i, remove(X, j))
      start(i).Sched(mod(add1(i), n), add(X, i))
    })
  }
}
```

```scheme
TODO
```

## 3.7 Counter

```c
process Count = Count(0)
process Count(n) = if (equal(n, 0)) options {
  inc.Count(1)
  [zero].Count(0)
} else options {
  inc.Count(add1(n))
  [dec].Count(sub1(n))
}
```

# 4 Concurrent Processes and Reaction

TODO
