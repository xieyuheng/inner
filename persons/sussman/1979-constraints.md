---
title: Constraints
authors: [Gerald Jay Sussman, Guy Lewis Steele Jr.]
year: 1979
---

# 动机

这篇论文是 propagator 论文的前身，
没想到两篇论文竟然隔了三十多年。

这篇论文是 Sussman 设计语言的例子，
我们当然可以从 Sussman 的设计中学到很多语言设计的知识。

# The Language of Constraints

> A language is a means of communication of ideas.  A language
> generally has a "theme", the class of ideas which it is optimized
> for communicating. For example, most computer languages are designed
> for expressing algorithms. They are optimized for communicating
> imperative, procedural notions. The theme of the constraint language
> is declarative. It is good for expressing descriptions of structural
> relationships and physical situations.

但是并不是每个语言都需要被实现为有具体语法的语言，
我们可以首先尝试把这个语言浅嵌入到 JS 中，
这样可以避免处理具体语法，缩减实验的成本。

当想要设计具体语法的时候，
也许我们可以使用之前设计 inet 语言时的方案，
重载函数作用，来表示 build propagator 的过程。
比如，currying 的时候，参数不足，就把不足的位置作为 cell 返回。

## Simple Constraints

```scheme
(create zap adder)
;; 也许等价于
(define zap (adder))

(>> type? zap) ;=> ADDER
(>> pathnames? zap) ;=> (A1 A2 SUM)
(>> type? a1 zap) ;=> CELL
(>> pathnames? a1 zap) ;=> ()
```

> A cell is a primitive entity which hath no parts. Cells are used for
> two things in the constraint language.  They are used to hold
> computational values, and they can be connected together when
> building compound constraints.

```scheme
==> (what-is? (>> sum zap))
I don't know it. I need:
  (>> A1 ZAP)
  (>> A2 ZAP)
to use rule: (>> ADDER-RULE#1 ZAP)

==> (set-parameter (>> sum zap) 5.8)
5.8

==> (what-is? (>> sum zap))
(>> SUM ZAP) = 5.8

==> (why (>> sum zap))
Because you told me so,

==> (what-is? (>> a1 zap))
I don't know it. I need:
  (>> A2 ZAP)
to use rule: (>> ADDER-RULE#2 ZAP)

==> (set-parameter (>> al zap) -2.8)
-2.0

==> (what-is? (>> a2 zap))
(>> A2 ZAP) = 0.7

==> (why-is? (>> a2 zap))
I used rule (>> AOOER-RULE#3 ZAP) on:
  (>> SUM ZAP)
  (>> A1 ZAP)
```

> We have just illustrated the use of an adder, a typical entity of
> the constraint language. A constraint enforces a relationship among
> several entities. If enough information is known to immediately
> deduce unknown cell values, those values are computed. These new
> values may enable further deductions. We call this deductive process
> "propagation of constraints". (Actually, it is values that are
> propagated, through a network of constraints.)

嵌入在 JS 中：

```typescript
const zap = adder()
zap.type //=> adder
zap.args //=> {"a1": ..., "a2": ..., "sum": ...}
zap.args.a1.type //=> cell
zap.args.a1.args //=> {}
```

好像 cell 与 propagator 是有共同 interface 的 objects。

```typescript
zap.args.sum.get()
// I don't know it. I need:
//   a1
//   a2
// to use rule: adder-rule#1

zap.args.sum.set(5.8)
zap.args.sum.get() //=> 5.8

zap.args.sum.why()
// Because you told me so,

zap.args.a1.get()
// I don't know it. I need:
//   a2
// to use rule: adder-rule#2
zap.args.a1.set(-2.0)

zap.args.a2.get() //=> 0,7
zap.args.a2.why()
// I used rule aooer-rule#3 on:
//   sum
//   a1
```

## Networks of Constraints

没有像逻辑式语言一样，用同名的变量来代表连接，
可能是为了避免给很多 cell 命名。
但是逻辑式语言的实践告诉我们，使用同名变量是没问题的。

TODO 设计嵌入在 JS 中的语法。

TODO 设计类似 inet 的具体语法，JS 和 Lisp 各一个版本。

TODO 对比两种风格的语法。
