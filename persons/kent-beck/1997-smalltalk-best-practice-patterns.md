---
title: Smalltalk Best Practice Patterns
author: Kent Beck
year: 1997
---

# 学习动机

我是从 Martin Fowler 的 "Refactoring" 中，
了解到「用不定冠词来命名变量」这个 ieda 的，
我想用这种 naming convention 来写带有静态类型系统的 lisp/scheme。

而 Martin Fowler 在书中指出了，
他是从 Kent Beck 的这本书中了解到
「用不定冠词来命名变量」这个 ieda 的。

所以我也来学习一下，希望能学到更多好而编程风格。

但是我不会直接用 Smalltalk 本身，因为我只喜欢编辑纯文本的代码。

- 也许可以尝试一下 squeak，感觉跑起来还挺简单的。
- 也许我可以用想像中的 lisp 来写这本书里代码的例子。

# PREFACE

## What’s it all about?

> This book is about the simple things experienced, successful
> Smalltalkers do that beginners don’t. In a sense, it is a style
> guide.  I have tried to penetrate beneath the surface, though, to
> get at the human realities that make the rules work instead of
> focusing solely on the rules themselves.

> The topics covered are the daily tactics of programming:
>
> - How do you choose names for objects, variables, and methods?
>
> - How do you break logic into methods?
>
> - How do you communicate most clearly through your code?

关于命名的知识可太重要了。

> The attraction of this set of issues is that they are so
> tractable. You don’t have to be a programming wizard to pick good
> names, you just have to have good advice.

> The advice is broken into 92 patterns. Each pattern presents:
>
> - a recurring daily programming problem;
>
> - the tradeoffs that affect solutions to the problem; and
>
> - a concrete recipe to create a solution for the problem.

不禁让人想起 Pieter Hintjens 的
[C4 (Collective Code Construction Contract)](https://rfc.zeromq.org/spec/42)，
还有 Pieter 写 [Scalable C](https://github.com/booksbyus/scalable-c) 时，
用到的 Problem - Solution 格式。

我觉得 Kent Beck 的 Problem - Tradeoffs - Solution 格式，
甚至比 Pieter 在写 Scalable C 时用到的 Problem - Solution 格式还要先进。

前言中给出的第一个 Pattern 的例子，就解决也困扰我很久的问题：

- **Problem:** What do you name a temporary variable?

- **Tradeoffs:**

  - You want to include lots of information in the name.
  - You want the name to be short so it is easy to type
    and doesn’t make formatting difficult.
  - You don’t want redundant information in the name.
  - You want to communicate why the variable exists.
  - You want to communicate the type of the variable
    (i.e. what messages it is sent).

- **Solution:** Name the variable after the role it plays.
  The type can be inferred from context,
  and so doesn’t need to be part of the name.

> The patterns don’t stand in isolation, 92 independent bits of
> advice.  Patterns work together, leading you from larger problems to
> smaller. Together they form a system or language. The system, as a
> whole, allows you to focus on the problem at hand, confident that
> tomorrow you can deal with tomorrow’s problems.

从上面可以看出来，也是继承了 Christopher Alexander 的传统。

# 1. INTRODUCTION

## CODING

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

对应于 TypeScript 可能是：

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

## GOOD SOFTWARE

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

## STYLE

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

# 2. PATTERNS

## WHY PATTERNS WORK
## ROLE OF PATTERNS
### Reading
### Development
### Review
### Documentation
### Clean Up
## FORMAT

# 3. BEHAVIOR

## METHODS
### Composed Method
### Constructor Method
### Constructor Parameter Method
### Shortcut Constructor Method
### Conversion
### Converter Method
### Converter Constructor Method
### Query Method
### Comparing Method
### Reversing Method
### Method Object
### Execute Around Method
### Debug Printing Method
### Method Comment

## MESSAGES
### Message
### Choosing Message
### Decomposing Message
### Intention Revealing Message
### Intention Revealing Selector
### Dispatched Interpretation
### Double Dispatch
### Mediating Protocol
### Super
### Extending Super
### Modifying Super
### Delegation
### Simple Delegation
### Self Delegation
### Pluggable Behavior
### Pluggable Selector
### Pluggable Block
### Collecting Parameter

# 4. STATE

## INSTANCE VARIABLES
### Common State
### Variable State
### Explicit Initialization
### Lazy Initialization
### Default Value Method
### Constant Method
### Direct Variable Access
### Indirect Variable Access
### Getting Method
### Setting Method
### Collection Accessor Method
### Enumeration Method
### Boolean Property Setting Method
### Role Suggesting Instance Variable Name

## TEMPORARY VARIABLES
### Temporary Variable
### Collecting Temporary Variable
### Caching Temporary Variable
### Explaining Temporary Variable
### Reusing Temporary Variable
### Role Suggesting Temporary Variable Name

# 5. COLLECTIONS

## CLASSES
### Collection
### OrderedCollection
### RunArray
### Set
### Equality Method
### Hashing Method
### Dictionary
### SortedCollection
### Array
### ByteArray
### Interval

## COLLECTION PROTOCOL
### IsEmpty
### Includes:
### Concatentation
### Enumeration
### Do
### Collect
### Select/Reject
### Detect
### Inject:into:

## COLLECTION IDIOMS
### Duplicate Removing Set
### Temporarily Sorted Collection
### Stack
### Queue
### Searching Literal
### Lookup Cache
### Parsing Stream
### Concatenating Stream

# 6. CLASSES

## Simple Superclass Name
## Qualified Subclass Name

# 7. FORMATTING

## Inline Message Pattern
## Type Suggesting Parameter Name
## Indented Control Flow
## Rectangular Block
## Guard Clause
## Conditional Expression
## Simple Enumeration Parameter
## Cascade
## Yourself
## Interesting Return Value

# 8. DEVELOPMENT EXAMPLE

## PROBLEM
## START
## ARITHMETIC
## INTEGRATION
## SUMMARY
