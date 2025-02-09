---
title: 1. Introduction
---

# CODING

> To me, development consists of two processes that feed each other.
> First, you figure out what you want the computer to do.  Then, you
> instruct the computer to do it. Trying to write those instructions
> inevitably changes what you want the computer to do, and so it goes.
>
> In this model, coding isn’t the poor handmaiden of design or
> analysis.  Coding is where your fuzzy, comfortable ideas awaken in
> the harsh dawn of reality. It is where you learn what your computer
> can do. If you stop coding, you stop learning.
>
> We aren’t always good at guessing where responsibilities should go.
> Coding is where our design guesses are tested. Being prepared to be
> flexible about making design changes during coding results in
> programs that get better and better over time.

一个例子是这段代码：

```smalltalk
Station>>computePart: aPart
  ^self multiplyPartTimesRate: aPart

Station>>multiplyPartTimesRate: aPart
  ^Part
    amount: aPart amount * self rate
    date: aPart date
```

> I said, “we seem to be using a lot of the Part’s data in
> multiplyPartTimesRate:. Why don’t we move this code into Part?”
> “But we didn’t design Parts to do arithmetic!” “Since the code
> seems to be telling us to do this, let’s try it.”

```smalltalk
Part>>* aRate
  ^Part
    amount: amount * aRate
    date: date

Station>>computePart: aPart
  ^aPart * self rate
```

Some of the biggest improvements come from figuring out how to eliminate:

- Duplicate code (even little bits of it)
- Conditional logic
- Complex methods
- Structural code (where one object treats another as a data structure)

想要消除 structural code 换成 object 的 message passing，
这与我目前的 belief 相冲突了，我的 belief 是：

- 尽量使用 record + functions，而不用 class + methods。

我以前也是 message passing，但是慢慢转成 record + functions 了。

因此回顾这里的 message passing 风格，应该是很有益的体验。

## Example in TypeScript

```typescript
class Part {
  amount: number
  date: Date
}

class Station {
  rate: number

  computePart(aPart) {
    return this.multiplyPartTimesRate(aPart)
  }

  multiplyPartTimesRate(aPart) {
    return Part({
      amount: aPart.amount * this.rate,
      date: aPart.date,
    })
  }
}
```

改成：

```typescript
class Part {
  amount: number
  date: Date

  mul(aRate) {
    return Part({
      amount: this.amount * aRate,
      date: this.date,
    })
  }
}

class Station {
  rate: number

  computePart(aPart) {
    return aPart.mul(this.rate)
  }
}
```

## Example in scheme

```scheme
(define-class part ()
  (claim amount number)
  (claim date date)
  (define (mul a-rate)
    (new part
      :amount (number-mul amount a-rate)
      :date date)))

(define-class station ()
  (claim rate number)
  (define (compute-part a-part)
    (a-part:mul rate)))
```

可以发现，在想像中的 cicada-lisp 中，
如果用之前的 cicada 的语义，
那么 `(claim date date)` 这种 class 中的 statement 是有问题的。
因为这相当于是引入局部变量，而不是引入一个 property name。

在 cicada-lisp 中，`define-class` 时，
不能有 define，只能有 claim。

```scheme
(define-class part ()
  :amount number
  :date date)

(define (part-mul (a-part part) (a-rate number))
  (new part
    :amount (number-mul a-part:amount a-rate)
    :date a-part:date))

(define-class station ()
  :rate number)

(define (station-compute-part
         (a-station station)
         (a-part part))
  (part-mul a-part a-station:rate))
```

函数类型应该在 claim 中声明，
而不应该在 define 中声明：

```scheme
(define-class part ()
  :amount number
  :date date)

(claim part-mul (-> part number part))
(define (part-mul a-part a-rate)
  (new part
    :amount (number-mul a-part:amount a-rate)
    :date a-part:date))

(define-class station ()
  :rate number)

(claim station-compute-part (-> station part part))
(define (station-compute-part a-station a-part)
  (part-mul a-part a-station:rate))
```

# GOOD SOFTWARE

> The patterns here form a system; one that I have developed during my
> years as a Smalltalk programmer. Most of it is really the work of
> the Smalltalkers who came before me and left their wisdom in the
> image. Some small part is my own invention.  I consider myself part
> of a culture. As with any culture, there is a core set of values
> that drives what the culture sees as good and what it sees as
> bad. What are those values?

也就是「使命、愿景、价值观」中的价值观
-- 用来判断好与坏的准则。

> I can’t say it often enough -- the bottlenecks throughout
> development come from limitations in human communication. Over and
> over in the patterns, you will read “You could do this or you could
> do that, but this over here communicates best, so that’s what you
> should do.” If there’s a radical thought here, that’s it; that
> when you program, you have to think about how someone will read your
> code, not just how a computer will interpret it.

关于 human communication，
还有 [Conway's law](https://en.wikipedia.org/wiki/Conway%27s_law)。

# STYLE

There are a few things I look for that are good predictors of whether
a project is in good shape. These are also properties I strive for in
my code.

- Once and only once -- In a program written with good style,
  everything is said once and only once.

- Lots of little pieces -- Only by factoring the system into many
  small pieces of state and function can you hope to satisfy the
  “once and only once” rule.

- Replacing objects -- In a really good system, every time the user
  says “I want to do this radically different thing,” the developer
  says, “Oh, I’ll have to make a new kind of X and plug it in.”

- Moving objects -- objects can be easily moved to new contexts.

- Rates of change -- don’t put two rates of change together.
