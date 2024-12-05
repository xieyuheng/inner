---
title: tidy first?
subtitle: a personal exercise in empirical software design
author: kent beck
year: 2023
---

# 学习动机

[2024-12-03] 我正在写一个基于 x window system 的 pixel canvas 程序，
其中目前有 `canvas_t` 和 `canvas_window_t` 两个 class，
前者是不依赖 x window system 的，
后者包含 canvas，并且处理所有和 window 相关的逻辑。
但是这样导致了 API 使用起来不是很方便，
因此我想把两个 class 融合，也就是进一步增加 coupling。
但是这样做是否正确呢？这本书的理论可以给出答案。

# Foreword

前言的作者是 Larry Constantine，
coupling 和 cohesion 概念的提出者。

> In theory, there is no difference between theory and practice, while
> in practice there is.

> That core theory is simply this: that the complexity of computer
> code depends on how it is organized into parts, on how coupled those
> parts are with each other and on how cohesive the parts are in
> themselves.

> Coupling and cohesion are simply measures of the complexity of
> computer code, not from the perspective of the computers executing
> the programs but that of human beings trying to understand the
> code. To understand any program, whether to create it or to correct
> it or to change it, requires understanding the piece of code
> immediately in front of you as well as those other pieces to which
> it is connected, which it depends on or affects or is affected
> by. It is easier to understand the immediate piece of code if it all
> hangs together, if it makes sense as a whole, if it forms what
> cognitive psychologists call a gestalt. That’s cohesion. It is also
> easier to understand it in terms of its relationships with other
> pieces of code if these relationships are few and relatively weak or
> highly constrained. That’s coupling. Coupling and cohesion are
> really all about how your brain deals with complicated systems.

# Preface

> As a book, Tidy First? practices what it proposes -- delivering
> these tidyings in small chunks and suggesting when and where you
> might apply them. So, instead of trying to master tidying all at
> once, you can try out a few tidyings that make sense for your
> problem. Tidy First? also begins describing the theory behind
> software design: coupling, cohesion, discounted cash flows, and
> optionality.

# Introduction

> Helping folks learn to design safely contributes to my mission.
> Hence, you will see frequent references to working in small, safe
> steps throughout these pages. I’m not interested in short-term
> acceleration. Software design creates value, when it creates value,
> that is realized over time.

时间。

标题 "tidy first?" 带有问号，
代表疑问是否此时应该 tidy。

# Part I. Tidyings

> My general learning strategy is to go from concrete to abstract.
> Therefore, we’ll start with a catalog of little design “moves”
> you can make when faced with messy code you have to change.

> Tidyings are a subset of refactorings. Tidyings are the cute, fuzzy
> little refactorings that nobody could possibly hate on.

以 tidyings 这些使用的技巧为诱饵，
来介绍 coupling 和 cohesion 相关的理论。

## 1. Guard Clauses

> You see some code like this:

```
if (condition)
    ...some code...
```

> Or even better, this:

```
if (condition)
    if (not other condition)
        ...some code...
```

> As a reader, it’s easy to get lost in nested conditions.  Tidy the
> above to:

```
if (not condition) return
if (other condition) return
...some code...
```

> This is easier to read. It says, “Before we get into the details of
> the code, there are some preconditions we need to bear in mind.”

## 2. Dead Code

> Delete it. That’s all.
> If the code doesn’t get executed, just delete it.

## 3. Normalize Symmetries

其实用一种重复的复制粘贴的 pattern 代码，
虽然保持了一致性也不是好的解决方案，
最好是能遇到 pattern 时抽一个函数出来。

但是有的语言不能把某些 pattern 抽成函数，
这就是语言的问题了。

## 4. New Interface, Old Implementation

> So you need to call a routine, and the interface makes it
> difficult/complicated/confusing/tedious. Implement the interface you
> wish you could call and call it. Implement the new interface by
> simply calling the old one (you can inline the implementation later,
> after migrating all other callers).

比如 c 标准库中糟糕的命名规则，
此时可以直接给 c 函数套一层更好的名字。

## 5. Reading Order

> Reorder the code in the file in the order in which a reader
> (remember, there are many readers for each writer) would prefer to
> encounter it.

> You’re a reader. You just read it. So you know.

这是否要求一个语言要能够引用在后面的定义的东西？
还是只能先定义再引用比较好？

> No single ordering of elements is perfect. Sometimes you want to
> understand the primitives first and then understand how they
> compose. Sometimes you want to understand the API first and then
> understand the details of implementation.

c 带有 header file，同时又要求先定义再引用。
这样就算是同时解决了上面的两个问题。

## 6. Cohesion Order

> Reorder the code so the elements you need to change are adjacent.

> Cohesion order works for routines in a file: if two routines are
> coupled, put them next to each other.  It also works for files in
> directories: if two files are coupled, put them in the same
> directory. It even works across repositories: put coupled code in
> the same repository before changing it.

> Why not just eliminate the coupling?  If you know how to do that, go
> for it. That’s the best tidying of all, assuming:

```
cost(decoupling) + cost(change) < cost(coupling) + cost(change)
```

但是完全消除 coupling 是不可能的，总是有需要同时修改的东西。
比如 coupling 有时给人以方便的 API。

> Tidying can increase cohesion enough to make behavior changes
> easier. Sometimes the increased clarity from slightly better
> cohesion unlocks whatever is blocking you from decoupling.
> Sometimes better cohesion helps you live with the coupling.

## 7. Move Declaration and Initialization Together

> Here’s what this tidying looks like. Imagine you have some code like this:

```
fn()
    int a
    ...some code that doesn't use a
    a = ...
    int b
    ...some more code, maybe it uses a but doesn't use b
    b = ...a...
    ...some code that uses b
```

> Tidy this by moving the initialization up to the declaration:

```
fn()
    int a = ...
    ...some code that doesn't use a
    ...some more code, maybe it uses a but doesn't use b
    int b = ...a...
    ...some code that uses b
```

> You can’t just put variables and code that sets them in any old
> order. You must respect the data dependencies between variables.
> If you use a to initialize b, you have to initialize a first.
> As you’re executing this tidying, remember that you have to
> maintain the order of the data dependencies.

## 8. Explaining Variables

> Some expressions grow. Even if they start small, they grow. And they
> grow and they grow. And then along you come with your reading
> glasses on, and you try to understand what’s happening.

不要直接写一个 tree 在代码里,
而是一点一点构造这个 tree。
这对于构造 graph 的语言也适用。

> When you understand a part of a big, hairy expression, extract the
> subexpression into a variable named after the intention of the
> expression.

> You’ll see this frequently in graphics code:

```
return new Point(
    ...big long expression...,
    ...another big long expression...
)
```

> Before changing one of those expressions, consider tidying first:

```
x := ...big long expression...
y := ...another big long expression...
return new Point(x, y)
```

> Or maybe the expressions mean something more specific, like width
> and height, top and left, run and rise.

> In this tidying you are taking your hard-won understanding and
> putting it back into the code. This sets you up to change either one
> of those expressions more easily (because now they are separated),
> and to read them more quickly next time the code needs to change.

## 9. Explaining Constants

TODO

## 10. Explicit Parameters
## 11. Chunk Statements
## 12. Extract Helper
## 13. One Pile
## 14. Explaining Comments
## 15. Delete Redundant Comments

# Part II. Managing

## 16. Separate Tidying
## 17. Chaining
## 18. Batch Sizes
## 19. Rhythm
## 20. Getting Untangled
## 21. First, After, Later, Never

# Part III. Theory

## 22. Beneficially Relating Elements
## 23. Structure and Behavior
## 24. Economics: Time Value and Optionality
## 25. A Dollar Today > A Dollar Tomorrow
## 26. Options
## 27. Options Versus Cash Flows
## 28. Reversible Structure Changes
## 29. Coupling
## 30. Constantine’s Equivalence
## 31. Coupling Versus Decoupling
## 32. Cohesion
## 33. Conclusion

# Appendix: Annotated Reading List and References
