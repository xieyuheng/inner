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

与上一章的原则一样。

```
if response.code = 404
    ...blah blah blah...
```

```
PAGE_NOT_FOUND := 404

if response.code = PAGE_NOT_FOUND
    ...blah blah blah...
```

> You’re reading. You understand. You’re putting that understanding
> into the code so you don’t have to hold it in your head.

我们太熟悉 HTTP 的某些 code 了，以至于会忽略上面的修改。
但是对于 404 之外的其他 code，其实找一个地方集中定义一下是不错的。

> There are a few tidyings downstream of this one about putting
> constants that change together or need to be understood together in
> one place and separating them from constants that cluster for other
> reasons.

## 10. Explicit Parameters

> You’re reading some code you want to change, and you notice that
> some of the data it works on wasn’t passed explicitly to the
> routine. How do you make the inputs clear?

> Split the routine. The top part gathers the parameters and passes
> them explicitly to the second part.

> For example, if you see this:

```
params = { a: 1, b: 2 }
foo(params)

function foo(params)
    ...params.a... ...params.b...
```

> Make the parameters explicit by splitting foo:

```
function foo(params)
    foo_body(params.a, params.b)

function foo_body(a, b)
    ...a... ...b...
```

什么时候应该直接传参数，
什么时候应该用 record 传，
这也是个问题。

在 js 中用 record 很方便，所以很多人倾向于用 record。
但是在 c 中用 record 不太方便。

## 11. Chunk Statements

> This one wins the prize for simplest tidying. You’re reading a big
> chunk of code and you realize, “Oh, this part does this and then
> that part does that.” Put a blank line between the parts.

想要用一致的方式做好这件事也不容易。

在增加空行时需要考虑各种情况，
比如：

- 跨越多行的一个函数调用
- `if`
- `while`

## 12. Extract Helper

> You see a block of code inside a routine that has an obvious purpose
> and limited interaction with the rest of the code in the routine.
> Extract it as a helper routine.  Name the routine after the purpose
> (not how the routine works).

> I want to mention a couple of special cases of extracting a
> helper. One is when you have to change a couple of lines within a
> larger routine. Extract those lines as a helper, change just the
> lines in the helper, then, if it makes sense, inline the helper back
> into the calling routine. (Usually you’ll find yourself growing
> fond of the helper and keeping it around.)

为了修改先 extract，再修改，再 inline。
这可能是 Kent 在实验 TCR 时发现的技巧。
这是我没做过的。

> Another case for extracting a helper is expressing temporal coupling
> (`a()` needs to be called before `b()`). If you see:

```
foo.a()
foo.b()
```

> frequently, then create:

```
ab()
  a()
  b()
```

这种是我没注意到的 helper function 的情形！

> Fondness is not the only reason to keep helpers around. Frequently
> you’ll find yourself wanting to use your new helper again hours or
> even minutes after you’ve created it. Interfaces become tools for
> thinking about problems. New interfaces emerge when we’re ready to
> think more abstractly, to add words to our design vocabulary.

## 13. One Pile

> Sometimes you read code that’s been split into many tiny pieces,
> but in a way that hinders you from understanding it. Inline as much
> of the code as you need until it’s all in one big pile. Tidy from
> there.

一般的建议是推迟 abstraction。
这里的更好的建议是，
不要害怕 abstraction，
需要的时候随时 inline 回来。
总之，重点是不要害怕 inline，
不要忘记 inline 也是设计的一个方向。

> The biggest cost of code is the cost of reading and understanding
> it, not the cost of writing it. Tidy first has a bias toward lots of
> little pieces, both theoretically, to increase cohesion as a path to
> reducing coupling, and practically, to reduce the amount of detail
> that needs to be held in your head at any one time.

> The goal of this bias toward small pieces is to enable the code to
> be understood a little at a time. Sometimes, though, this process
> goes wrong. Because of how the small pieces interact, the code is
> harder to understand. To regain clarity, the code must first be
> mooshed together so new, easier-to-understand parts can then be
> extracted.

Sandi Metz 也经常提到 "small pieces"
与 "easy to understand as a whole"
有时是相互冲突的。

> Some symptoms you’re looking for are:
>
> - Long, repeated argument lists
> - Repeated code, especially repeated conditionals
> - Poor naming of helper routines
> - Shared mutable data structures

我觉得重点还是要记住 inline 也是一个 option。
有时候小规模的 inline 并不能形成 One Pile 的景象，
但是也是很实用的。

## 14. Explaining Comments

TODO

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
