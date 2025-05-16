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

纯粹从外部行为来定义 automata 之间的等价关系，
也可以用 black box 这个直觉来理解。

transition graph 来自 labeled transition system，
类似 rewrite system 但是每个 rewrite step 带有 label，
label 可以理解为 event 或者输入，
因此这种 system 所建模的是 interactive system。

还要记住 process expression 所形成的 process calculus，
这比直接描述 automata 所形成的 graph 的表达能力要强很多，
用 expression 来构造 automata，其特点就是适于 composition，
可以用递归了描述非有限的 automata。

process expression 的想法大致来自于，
为了求 automata 所对应的正则表达式而解方程。
解方程是所用的代数结构就是 process expression 的来源。

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

还有一些关于理论证明的 ideas。

保持语法结构的等价关系称作 congruent。
congruent 可以追溯到其几何起源。

每一个 calculus 都有一个语法层的基础等价关系叫做 structure congruent，
这个等价关系一般会带上 alpha equivalence。

定义 rewriting 而引出语义层的等价关系时，会用到这个基础等价关系，
从而使得语义层的等价关系成为这个基础等价关系的加粗
（更多的 expression 划归为同一等价类了）。

这种发展 calculus 理论的方式，
显然是以 lambda calculus 为范例的。
例如这里的 process calculus 也在很多地方使用了 substitution。

在一个 labeled transition system 中，
任意一个保持 transition 的二元关系就称作一个 simulation。
从这个「保持 transition」已经可以推出很多属性了。
注意，二元关系与二分图等价。

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

## 3.1 Labelled transition systems

这一章定义 labelled transition system。

> An LTS can be thought of as an automaton
> without a start state or accepting states.

```scheme
(define-class labelled-transition-system-t ()
  (claim state-t type-t)
  (claim action-t type-t)
  (claim transition (relation state-t action-t state-t)))
```

## 3.2 Strong simulation

用 automata 所能接受的语言，可以定义 automata 之间的等价关系。
这里想要定义新的等价关系，来区分更多的 automata。
想要区分的重点，恰好和 petri net 中的两种 or 类似，
即 deterministic choice 和 non-deterministic choice。

可以用 black box 模型来理解这里的 simulation，
如果以任意一个状态为起点，
一个 black box 的所有操作（所有按钮序列），
都可以被另一个 black box 模仿（可以执行相同的按钮序列）而不卡住（deadlock），
就说后一个 black box 就可以 simulate 前一个。

注意，这里要求对应的按钮相同，
但是 black box 会隐藏 automata 的状态。

为了用数学语言定义上面的等价关系，
作者引入了两个 automata state 之间的，
保持 transition 的二元关系。

```scheme
(define-class strong-simulation-t ()
  (claim lts labelled-transition-system-t)
  (claim simulation (relation lts:state-t lts:state-t))
  (claim simulation-respect-transition
    (forall ((p lts:state-t)
             (q lts:state-t)
             (r (simulation p q)))
      (forall ((a lts:action-t)
               (p* lts:state-t)
               (t (lts:transition p a p*))))
      (exists ((q* lts:state-t)
               (s (lts:transition q a q*)))
        (simulation p* q*)))))
```

给出 simulation 这个二元关系的第一个参数的 transition，
来找第二个参数的 transition，因此是后者在 simulate 前者。

## 3.3 Strong bisimulation

如果一个 simulation 作为二元关系的逆关系也是 simulation，
那么这个 simulation 就称作是 bisimulation。
bisimulation 显然是等价关系。

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

在 lisp 语法中，
overload 函数作用语法为 `(out)`，
并且用 `(@)` 代替 `(in)`：

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
  [out(i)].Buff2
  in(0).Buff2(0, i)
  in(1).Buff2(1, i)
}

process Buff2(i, j) = choice {
  [out(j)].Buff2(i)
}
```

```scheme
(define Buff2
  (choice
   [(@ in 0) (Buff2 0)]
   [(@ in 1) (Buff2 1)]))

(define (Buff2 i)
  (choice
   [(out i) Buff2]
   [(@ in 0) (Buff2 0 i)]
   [(@ in 1) (Buff2 1 i)]))

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
    choice_flat_map(X, (j) => choice {
      finish(j).Sched(i, remove(X, j))
    })
  } else {
    choice_flat_map(X, (j) => choice {
      finish(j).Sched(i, remove(X, j))
      start(i).Sched(mod(add1(i), n), add(X, i))
    })
  }
}
```

```scheme
(define Scheduler (Sched 1 {}))

(define (Sched i X)
  (if (in? i X)
    (choice-flat-map X
     (lambda (j)
       (do (finish j) (Sched i (remove X j)))))
    (choice-flat-map X
     (lambda (j)
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
  (if (equal? n 0)
    (choice
     [(@ inc) (Count 1)]
     [(zero) (Count 0)])
    (choice
     [(@ inc) (Count (add1 n))]
     [(dec) (Count (sub1 n))])))
```

# 4 Concurrent Processes and Reaction

## 4.2 Observations and reactions

这里有解释 Petri nets 是如何作为 automata 的推广的，
并且解释了 Petri nets 图语法的设计来源。

## 4.3 Concurrent process expressions

Example 4.3:

```c
process P = concurrent {
  fresh (a) concurrent {
    choice { a.Q1 b.Q2 }
    choice { [a] }
  }
  choice { [b].R1 [a].R2 }
}

// one result

concurrent {
  fresh (a) Q1
  choice { [b].R1 [a].R2 }
}

// another result

concurrent {
  fresh (a) concurrent {
    Q2 [a]
  }
  R1
}
```

```scheme
(define P
  (concurrent
   (fresh (a)
     (concurrent
      (choice
        [(@ a) Q1]
        [(@ b) Q2])
      (do (a))))
   (choice
     [(b) R1]
     [(a) R2])))

;; one result

(concurrent
 (fresh (a) Q1)
 (choice
   [(b) R1]
   [(a) R2]))

;; another result

(concurrent
 (fresh (a)
   (concurrent
    Q2
    (do (a))))
 R1)
```

# 8 What is Mobility?

要定义 mobility 就要定义 location，
用 process 之间的 link 来定义相对的 location，
这符合一维的拓扑学。

把 location 和 person 都当作 process，
person 和 location 之间的某种连接就可以被理解为「person 在 location」。

## 8.2 Mobile phones

```scheme
(define (transmitter talk switch gain lose)
  (choice
    [(@ talk) (transmitter talk switch gain lose)]
    [(@ lose t s) (switch t s) (idle-transmitter gain lose)]))

(define (idle-transmitter gain lose)
  (@ gain t s)
  (transmitter t s gain lose))

(define (car talk switch)
  (choice
    [(talk) (car talk switch)]
    [(@ switch t s) (car t s)]))

(define (system)
  (fresh (talk1 switch1 gain1 lose1
          talk2 switch2 gain2 lose2
          control1 control2)
    (concurrent
     (! (do (@ control1)
            (lose1 talk2 switch2)
            (gain2 talk2 switch2)
            (control2)))
     (! (do (@ control2)
            (lose2 talk1 switch1)
            (gain1 talk1 switch1)
            (control1)))
     (car talk1 switch1)
     (transmitter talk1 switch1 gain1 lose1)
     (idle-transmitter gain2 lose2)
     (control1))))
```

# 9 The pi-Calculus and Reaction

## 9.1 Names, actions and processes

这里用 pi 来代表 action prefix，
这就是 pi-calculus 中 pi 的来源。

Example 9.2 Illustrating reaction:

```scheme
(define P
  (fresh (z)
    (concurrent
     (choice [(x y)] [(@ z w) (w y)])
     (do (@ x u) (u v))
     (do (x z)))))
```

```scheme
;; P -> P1

(fresh (z)
  (concurrent
   (do (y v))
   (do (x z))))

;; P -> P2

(fresh (z)
  (concurrent
   (choice [(x y)] [(@ z w) (w y)])
   (do (z v))))

;; P2 -> P3

(fresh (z)
  (concurrent
   (do (v y))))
```

## 9.2 Structural congruence and reaction

## 9.3 Mobility

在画一组 concurrent 的 process 之间的连接图（这里称作 flowgraph）时，
只要两个 choice expression 顶层的 pi（action prefixes）之间有共用的变元，
就认为两个 process expression 之间有 link。
mobility 就要被理解为这种 graph 中 connection 的变化。

另外，link 可以根据 input 和 output 分为两种，
即在图中用被 channel label 的有向边。

但是这种图示是有歧义的，与其用有歧义的图示，
不如好好设计一个没有歧义的图示。

TODO 设计一个没有歧义的 pi-calculus 的图示。

## 9.4 The polyadic pi-calculus

注意，这里看似聪明的用只接受 atomic name 的单参数 channel
实现多参数 channel 的方式，假设了每个 channel 有固定的 arity。

这种假设是合理的，可以简化所需要支持的数据类型。
另外也可以用 tuple 的 pattern match 来实现多参数 channel。

## 9.5 Recursive definitions

用 channel 来 encoding named process！

Exercise 9.23 Consider the buffer defined in Section 8.3:

```scheme
(define (B l r) (do (@ l x) (C x l r)))
(define (C x l r) (do (r x) (B l r)))

;; no recursion

(define (B l r)
  (fresh (b c)
    (concurrent
     (do (@ l x) (c x l r))
     (! (do (@ c x l r) (r x) (b l r)))
     (! (do (@ b l r) (@ l x) (c x l r))))))

(define (C x l r)
  (fresh (b c)
    (concurrent
     (do (r x) (b l r))
     (! (do (@ c x l r) (r x) (b l r)))
     (! (do (@ b l r) (@ l x) (c x l r))))))
```

## 9.6 Abstractions

这一章又用 `(x).F` 这种语法，
把 lambda 加入到表达式中来了。

这里有一个感想是，process calculus 起源于 regular expression 的代数，
而 lisp 的纯前缀表达式语法并不适合描述带有结合律的二元运算。

也许更适合用 forth 语法？

这一节所作的就是给语法设计做进一步的 refactoring，
把 binding 相关的语法都用 lambda abstraction 来实现。

```
in x (y) P == x (y) P
-- reference 一个 channel x 会取出里面的值，保存在 y 中。

new y P == new (y) P
-- new 会生成一个 fresh name 保存在 y 中。
```

这里 binding 的 concatenation 让人想起 forth，
但是与 forth 不同的是，这里的 function application
还是用前缀表达式 `F<y>` 来表示的，比如 `(x).F<y>`。

如果用 `[]` 表示把值放到栈中（而不是 joy 中的 quote），
那么上面的函数作用也可以用纯粹的后缀表达式 `[y] (x) F`，
这应该和 automath 类似。

按照这个思路尝试设计一下后缀表达式的语法：

```forth
// () -- binding
// [] -- put values on stack

define B (l r) l (x) [x l r] C end
define C (x l r) [x] r [l r] B end

// | -- concurrent
// ; -- choice
// {} -- quote

// use pure postfix and list processing
// to build structural values.

define B (l r)
  fresh (b) fresh (c)
  null
    { l (x) [x l r] c } cons
    { c (x l r) [x] r [l r] b } replication cons
    { b (l r) l (x) [x l r] c } replication cons
  concurrent
end

define C (x l r)
  fresh (b) fresh (c)
  null
    { [x] r [l r] b } cons
    { c (x l r) [x] r [l r] b } replication cons
    { b (l r) l (x) [x l r] c } replication cons
  concurrent
end
```

# 10 Applications of the pi-Calculus

## 10.2 Unique handling

> Let us say that `P` _handles_ `x` if `P` has a free occurrence of `x`
> in an action prefix of the form `(x ...)` or `(v ... x ...)`.

`x` 出现在 `P` 的 output 的位置，称作 `P` handles `x`。
在 process 的 flowgraph 图示中画作出边 `[P]-x->`。

## 10.3 Data revisited

这一节模仿 lambda 演算中用 lambda expression 编码数据类型的过程。

Definition 10.11 Truth values (ephemeral):

```scheme
(define (true l) (do (@ l t f) (t)))
(define (false l) (do (@ l t f) (f)))
```

参数 `l` 是 location 的缩写。

另外这里其实不需要 `(do)`，因为 function body 默认就是 do 的语义。
process 的语义来自对 function application 的 overload，而不是来源于 `(do)`。
只有 `(concurrent)` 内需要 `(do)`。

```scheme
(define (true l) (@ l t f) (t))
(define (false l) (@ l t f) (f))
```

```scheme
;; given t f

(define (menu l)
  (l t f)
  (choice
    [(@ t) P]
    [(@ f) Q]))

;; given l

(concurrent (true l) (menu l)) => P
(concurrent (false l) (menu l)) => Q
```

所有的 enum 都可以用类似的方式编码：

``` scheme
(define (monday l)
  (@ l d1 d2 d3 d4 d5 d6 d7)
  (d1))
```

把上面的具体 `menu` 定义为一个通用的 `conditional`：

```scheme
(define ((conditional P Q) l)
  (fresh (t f)
    (l t f)
    (choice
      [(@ t) P]
      [(@ f) Q])))

;; given P Q l

(concurrent (true l) ((conditional P Q) l)) => P
(concurrent (false l) ((conditional P Q) l)) => Q
```

在介绍 `(node)` 的数据类型的 flowgraph 中，
从一个 channel `k` 中取值，画作 `-k->[node]`；
沿着某个 channel `c` 把 `v` 和 `l` 送走，
画作 `[node]-v->` 和 `[node]-l->`。

```scheme
(define (null k) (@ k n c) (n))
(define (node k v l) (@ k n c) (c v l))
(define ((cons V L) k)
  (fresh (v l)
    (concurrent
     (node k v l)
     (V v)
     (L l))))
```

```scheme
(same-as-chart
 ((cons false null) k)
 (fresh (v0 l0)
   (concurrent
    (node k v0 l0)
    (false v0)
    (null l0)))
 (fresh (v0 l0)
   (concurrent
    (do (@ k n c) (c v0 l0))
    (do (@ v0 t f) (f))
    (do (@ l0 n c) (n)))))

(same-as-chart
 ((cons true (cons false null)) k)
 (fresh (v1 l1)
   (concurrent
    (node k v1 l1)
    (true v1)
    ((cons false null) l1)))
 (fresh (v1 l1)
   (concurrent
    (do (@ k n c) (c v1 l1))
    (do (@ v1 t f) (t))
    (fresh (v0 l0)
      (concurrent
       (do (@ l1 n c) (c v0 l0))
       (do (@ v0 t f) (f))
       (do (@ l0 n c) (n))))))
 (fresh (v0 l0 v1 l1)
   (concurrent
    (do (@ k n c) (c v1 l1))
    (do (@ v1 t f) (t))
    (do (@ l1 n c) (c v0 l0))
    (do (@ v0 t f) (f))
    (do (@ l0 n c) (n)))))
```

## 10.4 Programming with lists

```scheme
(define ((list-which P F) k)
  (fresh (n c)
    (k n c)
    (choice
      [(@ n) P]
      [(@ c v l) (F v l)])))
```

这里书中错写成了和 `conditional` 相同的：

```scheme
(define ((list-which P F) k)
  (fresh (n c)
    (k n c)
    (choice
      [(@ n) P]
      [(@ c) F])))
```

```scheme
(same-as-chart
 (concurrent
  (null k)
  ((list-which P F) k))
 (concurrent
  (do (@ k n c) (n))
  (fresh (n c)
    (k n c)
    (choice
      [(@ n) P]
      [(@ c v l) (F v l)])))
 (fresh (n c)
   (concurrent
    (do (@ k n c) (n))
    (do (k n c)
        (choice
          [(@ n) P]
          [(@ c v l) (F v l)]))))
 (fresh (n c)
   (concurrent
    (n)
    (choice
      [(@ n) P]
      [(@ c v l) (F v l)])))
 (fresh (n c)
   (concurrent
    P))
 (fresh (n c)
   P)
 P)
```

```scheme
(same-as-chart
 (concurrent
  (cons V L k)
  ((list-which P F) k))
 (concurrent
  (fresh (v l)
    (concurrent
     (node k v l)
     (V v)
     (L l)))
  (fresh (n c)
    (k n c)
    (choice
      [(@ n) P]
      [(@ c v l) (F v l)])))
 (fresh (v l n c)
   (concurrent
    (node k v l)
    (V v)
    (L l)
    (do (k n c)
        (choice
          [(@ n) P]
          [(@ c v l) (F v l)]))))
 (fresh (v l n c)
   (concurrent
    (do (@ k n c) (c v l))
    (V v)
    (L l)
    (do (k n c)
        (choice
          [(@ n) P]
          [(@ c v l) (F v l)]))))
 (fresh (v l n c)
   (concurrent
    (c v l)
    (V v)
    (L l)
    (choice
      [(@ n) P]
      [(@ c v l) (F v l)])))
 (fresh (v l n c)
   (concurrent
    (V v)
    (L l)
    (F v l)))
 (fresh (v l)
   (concurrent
    (V v)
    (L l)
    (F v l))))
```

假设 `(match)` 可以根据 `null` 和 `cons` 的定义，
展开成上面的 `(choice)`：

```scheme
(define ((list-which P F) k)
  (match k
    [(null) P]
    [(cons v l) (F v l)]))

;; expand to:

(define ((list-which P F) k)
  (fresh (n c)
    (k n c)
    (choice
      [(@ n) P]
      [(@ c v l) (F v l)])))
```

TODO `list-copy`
TODO `list-join`

TODO `list-move`
TODO `list-append`

## 10.5 Persistent and mutable data

TODO

# 11 Sorts, Objects, and Functions

TODO

# 14 Discussion and related work

这一章介绍了很多其他人的工作，现在没法马上去看，
可以经常回来重新读读，然后找感兴趣的看。

> The pi-calculus has been introduced as a new and fundamental way of
> thinking about mobile interactive processes, and one which is
> amenable to rigorous treatment.

后面所强调的 amenable to rigorous treatment，
是对比 actor model 而言的。

> A very general question is: How should we rigorously express the
> specification or required behaviour of a system, perhaps in terms of
> some logical formalism;

也许是 philip wadler 在 2014-propositions-as-sessions
中所引入的 linear logic。

> and then what does it means for a system -- perhaps represented by a
> pi-calculus term -- to satisfy such a specification?  There is no
> reason why the system and its specification should be expressed in
> the same formalism.

> A fortiori, there is no reason to expect that the satisfaction of a
> specification by a system should always be expressed as the
> equivalence between two terms in the same formalism.  But the study
> of behavioural equivalence, as far as it goes, has the advantage
> that only a single formalism is required.  Furthermore, equivalence
> is in a certain sense fundamental; to echo the introductory chapter,
> until we know what constitutes equivalent behaviour we do not
> reallynunderstand what the term 'behaviour' means!

其实就算是从逻辑和类型系统的角度看，等价关系也是非常重要的，
用 NbE 实现 dependent type 就是一个例子。

> But as we seriously address the problem of modelling mobile
> communicating systems we get a sense of completing a model which was
> previously incomplete; for we can now begin to describe what goes on
> _outside_ a computer in the same terms as what goes on _inside_ --
> i.e. in terms of interaction.  Turning this observation inside-out,
> we may say that we inhabit a global computer, an informatic world
> which demands to be understood just as fundamentally as physicists
> understand the material world.
