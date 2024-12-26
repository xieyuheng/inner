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

[2024-12-12] 经过融合上面的两个 class，API 确实简化了很多。
现在新的问题是给 inet 实现 GUI debug 功能时，
代码结构应该如何组织？
目前是 `debug` has `net_layout` as `node_layout_list`，
有更好的结构吗？

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

> You know that moment when you’re reading some code and you say,
> "Oh, so that’s what’s going on!" That’s a valuable moment.
> Record it.

注释有很多种类：

- 解释背后的模型。
- 解释一些 coupling。
- 等等。

> It’s not ideal to have that coupling in your code.  Eventually,
> you’ll have to learn how to eliminate it, but in the meantime,
> it’s much better to add the comment that points out the coupling
> issue, rather than leaving it buried in the sand.

## 15. Delete Redundant Comments

> When you see a comment that says exactly what the code says,
> remove it.

> Tidyings often chain together. A previous tidying may have made a
> comment redundant. For example, the original code might look like
> this:

```
if (generator)
    ...a bunch of lines of code to set up the generator...
else
    # no generator, return the default
    return getDefaultGenerator()
```

> After tidying with a guard clause, the code looks like this:

```
if (! generator)
    # no generator, return the default
    return getDefaultGenerator()

...a bunch of lines of code to set up the generator...
```

我才发现 guard clause 可以用来避免原本看似有用的注释。

# Part II. Managing

> This section on managing tidying discusses how to fit tidying into a
> personal development workflow:
>
> - When do you start tidying?
> - When do you stop tidying?
> - How do you combine tidying,
>   changing the structure of the code,
>   with changing the behavior of the system?

## 16. Separate Tidying

> Folks learning to tidy seem to go through predictable phases.
> In the first phase we’re just making changes, and we begin with an
> undifferentiated mass of changes.

> After learning the tidyings, it’s as if our picture under the
> microscope snaps into focus. Some of those changes were changing the
> behavior of the program, its attributes as observed from the running
> of the program. Some of those changes, though, were changing the
> structure of the program. Those changes can only be observed by
> looking at the code.

> After a bit of this, we start noticing the common flows. Chunking
> statements leads to explaining helpers leads to an easier time
> making behavior changes. Now programming is more like chess, and you
> can guess how the game will play out several moves ahead.

关于 pull-request 的讨论，可以参考 ZMQ 的 c4。

## 17. Chaining

> Tidyings are like potato chips. You eat one, and you’ll want
> another. Managing the urge to keep tidying is a key tidying
> skill. You just tidied; should you tidy more? It depends.

这一章有很多 tidying/refactoring 之间的引出关系。

> **Explicit parameters** After making parameters explicit, you may be
> able to group a set of parameters into an object and move code into
> that object. This is out of the scope of tidying, but be on the
> lookout for new abstractions revealed as you tidy. Some of the most
> powerful abstractions you will ever discover derive from running
> code. You would never have created them on speculation.

这种抽象出 object 的观点是我第一次听说。
"powerful abstractions may be discovered from running code."
也是第一次听说。

> **Delete redundant comments** Since change is the dominant cost of
> software development and understanding code is the dominant cost of
> change, communicating the structure and intent of working code is
> one of the most valuable skills you can exercise. Comments are a
> form of communication, but tidying lets you explore the limits of
> communicating through the other elements of programming.

对其他技术文献的学习也是类似，理解是最废时间的。

- 表达清晰帮助读者理解。
- 在遇到表达不好的文献时，想办法帮助自己理解。

理解的结果是什么？
理解的结果不是笔记，而是想象力。

- 能够清楚无误地想像出来整个系统的运行过程。
  可能对于计算模型而言尤其是如此。
- 能够知道系统的重要不变量（invariant）。
- 能够衍生出来新的想法。

## 18. Batch Sizes

这里所对比的 tradeoff 是说如果有很多小 tidy，
可能会增加 review 的时间。
其实这是一个团队组织问题而不是代码结构的问题。
因为实际上 tidy 几乎不需要 review。

ZMQ 的 C4 甚至定义了什么是「正确的」修改，
并且规定「正确的」修改都不需要 review。

## 19. Rhythm

structure 和 behavior change 应该交替进行。

## 20. Getting Untangled

再次强调了 structure 和 behavior change 应该分开。
并且建议如果乱了，就 revert 然后重写。

## 21. First, After, Later, Never

> Let’s talk about the timing of tidying with respect to a behavior
> change in the system.

Never 需要强调这也是一个选项，
如果代码不需要修改的话。
但是注意，很少有代码是不需要修改的。

Later 在于人们认为自己没有时间，
其实是有时间的，把眼光放长远一些。

After 就像解题之后的 lookback。

First 这是默认的选择。
注意这里的前提是你知道如何 tidy。

# Part III. Theory

## 22. Beneficially Relating Elements

> **Elements**

> Substantial structures have parts.
> Organelle → organ → organism.
> Atoms → molecules → crystals.
> In our world: tokens → expressions → statements →
> functions → objects/modules → systems.

> Elements have boundaries. You know where they start and end.

> Elements contain subelements. In our world we like to have
> homogeneous hierarchies (à la Composite pattern).
> Natural hierarchies, like previous examples, are not homogeneous.
> Contained subelements differ from the container.

这也许解释了为什么 generic dispatching 很重要。
因为这种技术允许我们突破 homogeneous hierarchies 的限制。

> **Relating**

> Okay, so we have a hierarchy of elements. Those elements exist in
> relation to each other. One function calls another. The functions
> are the elements. “Calls/called by” is the relationship. In the
> natural world, we have relationships like “eats,” “shades,” and
> “fertilizes.”

> **Beneficially**

> Here’s where the magic happens. One design is to have a single
> gigantic soup of tiny subelements. Think assembly language with a
> global namespace. This program would work. It would behave from the
> point of view of an external observer exactly the same as a
> well-designed program. Quickly, however, we would be unable to
> change it.  There would be too many relationships, often implicit,
> between the elements.

> When we design, creating intermediate elements between the machine
> instructions and the whole, those intermediate elements begin
> benefitting each other. Function A can be simpler because function B
> takes care of the complexity of a part of the calculation.

这里已经能体会到一点儿，
为什么 Kent 会说  “software design is an exercise in human relationships.” 了。
因为人也要 benefitting each other。

> **Beneficially Relating Elements**

> One reading of the phrase “beneficially relating elements” starts
> with “the design is….” What is the design? It’s the elements,
> their relationships, and the benefits derived from those
> relationships.

> Another reading starts with “designers are….” What do designers
> do? They beneficially relate elements. From this perspective,
> software designers can only:

> - Create and delete elements.
> - Create and delete relationships.
> - Increase the benefit of a relationship.


> Take one of my favorite examples. I have an object that invokes
> another object twice in one function:

```
caller()
    return box.width() * box.height()
```

> The calling function has two relationships with the box, those of
> invoking two functions. Let’s move the expression into the box:

```
caller()
    return box.area()

Box>>area()
    return width() * height()
```

> From a design standpoint, we have created a new element, Box.area(),
> and adjusted the relationship between the caller and the box. Now
> they are related by a single function invocation, with the benefit
> that the calling function is simpler and the cost that Box is one
> function bigger.

这里的设计与 dot 语法无关，
对于 scalable c 也同样适用：

```
caller()
    return box_width(box) * box_height(box)
```

```
caller()
    return box_area(box)

box_area(box)
    return box_width(box) * box_height(box)
```

> When I talk about the structure of the system, I’m talking about:

> - The element hierarchy
> - The relationships between elements
> - The benefits created by those relationships

> Now we can make a firmer distinction between the structure and the
> behavior of the system.

这里 element hierarchy 可以理解为类型系统，
即类型作为集合之间的包含关系。

## 23. Structure and Behavior

> Software creates value in two ways:
>
> - What it does today
> - The possibility of new things we can make it do tomorrow

> Behavior can be characterized in two ways:
>
> - Input/output pairs
> - Invariants

> Behavior creates value.

> This is the secret it took me decades to absorb. I didn’t have to
> change the behavior of my system to make it more valuable. As soon
> as I added to the options of what it could do next, I had already
> made money. (I went down the rabbit hole of options pricing formulas
> to really cement this understanding, but I trust you to figure out
> how to convince yourself.)

用期权来理解软件设计的经济学原理。

option 定义为在未来做选择的权力。

> One of the coolest thing about options is that the more volatile the
> environment is, the more valuable options become.

> As a young engineer, I was terrified when a seemingly settled
> situation turned chaotic. As I learned to enhance optionality, I saw
> chaos as an opportunity.

> The structure of the system doesn’t matter to its behavior.
> The structure creates options.

structure changes 和 behavior changes 之间有很多区别，
其中最重要的区别就是 reversibility。

## 24. Economics: Time Value and Optionality

> The nature [of money] I learned consisted of two surprising
> properties:
>
> - A dollar today is worth more than a dollar tomorrow, so earn
>   sooner and spend later.
>
> - In a chaotic situation, options are better than things, so create
>   options in the face of uncertainty.

## 25. A Dollar Today > A Dollar Tomorrow

> More is more and less is less, right? Depends.
> With money, it depends on:
>
> - When
> - How sure

> In the scope of this book, the time value of money encourages tidy
> after over tidy first. If we can implement a behavior change that
> makes us money now and tidy after, we make money sooner and spend
> money later.

## 26. Options

> The previous chapter modeled the economic value of a software system
> as the sum of the discounted future cash flows. We create value when
> we change those flows:
>
> - Earning money more, sooner, and with greater likelihood
> - Spending money less, later, and with less likelihood

> There’s another, sometimes conflicting, source of value: optionality.

> I can’t implement all those algorithms [about options pricing] for
> you, but I can report the lessons I learned (I encourage you to try
> the exercise if you really want to “get it”):

> - “What behavior can I implement next?” has value all on its own,
>   even before I implement it. This surprised me. I thought I was
>   getting paid for what I had done (as per the previous chapter).
>   I wasn’t. I was mostly getting paid for what I could do next.

> - “What behavior can I implement next?” is more valuable the more
>   behaviors are in the portfolio. If I can increase the size of the
>   portfolio, I have created value.

> - “What behavior can I implement next?” is more valuable the more
>   the behaviors in that portfolio are valuable. I can’t predict
>   which behavior will be most valuable, nor how valuable it will be,
>   but…

> - I don’t have to care which item will be most valuable, as long as
>   I keep open the option of implementing it.

> - (This is the best one.) The more uncertain my predictions of value
>   are, the greater the value of the option is (versus just
>   implementing it). If I embrace change, I maximize the value I
>   create in exactly those situations where (then) conventional
>   software development fails most spectacularly.

> [a story about potatoes]

> What does this mean for software design? Software design is
> preparation for change; change of behavior. The behavior changes we
> could make next are the potatoes from the story. Design we do today
> is the premium we pay for the “option” of “buying” the behavior
> change tomorrow.

> Thinking of software design in terms of optionality turned my
> thinking upside-down.  When I focused on balancing creating options
> and changing behavior, what used to scare me now excited me:
>
> - The more volatile the value of a potential behavior change, the better.
> - The longer I could develop, the better.
> - Yes, the cheaper I could develop in future, the better,
>   but that was a small proportion of the value.
> - The less design work I could do to create an option, the better.

## 27. Options Versus Cash Flows

> Here we have the economic tug-of-war that makes “tidy first?” such
> an interesting question:

> - Discounted cash flow tells us to make money sooner with greater
>   likelihood and spend money later with less likelihood. Don’t tidy
>   first. That’s spending money sooner and earning money
>   later. Maybe don’t even tidy after or later.

> - Options tell us to spend money now to make more money later (even
>   if we don’t currently know exactly how). Absolutely tidy first
>   (when it creates options). Tidy after and later too.

简单地说 Tidy first？ 取决于：

- 取决于软件是否要长期维护；
- 软件是否要在未来变化以适应新的需求。

在下面这种情况下，不用想也需要 tidy first：

```
cost(tidying) + cost(behavior change after tidying) <
                cost(behavior change without tidying)
```

在下面这种情况下，则取决于软件是否需要长期维护：

```
cost(tidying) + cost(behavior change after tidying) >
                cost(behavior change without tidying)
```

也就是取决于我们对未来的判断。

## 28. Reversible Structure Changes

Structure Change 是 Reversible 的，
与 Behavior Changes 相比几乎没有成本，
因此可以大胆地去做去尝试，
在决定 structure change 上考虑太多，
反而是不对的，是浪费时间。

## 29. Coupling

> To prepare to write their classic text Structured Design, Ed Yourdon
> and Larry Constantine examined programs to find out what made them
> so expensive. They noticed that the expensive programs all had one
> property in common: changing one element required changing other
> elements. The cheap programs tended to require localized changes.

> They dubbed this change infection property “coupling.”
> Two elements are coupled with respect to a particular change
> if changing one element necessitates changing the other element.

```
coupled(E1, E2, Δ) := ΔE1 -> ΔE2
```

可以画成 graph with labelled edges：

- node 是 E1 E2；
- edge 的 label 是 Δ。

> For example, a calling function is coupled to a called function with
> respect to changes to the name of the called function.  To say
> something useful, we have to also say coupled with respect to which
> changes.  If two elements are coupled with respect to a change that
> never happens, then they aren’t coupled in a way that should
> concern us.

> Analyzing coupling cannot be done simply by looking at the source
> code of a program. We need to know what changes have happened and/or
> are likely to happen before we can tell whether two elements are
> coupled. (For an experiment, see which pairs of files tend to show
> up together in commits. Those files are coupled.)

> If coupling were only ever between two elements, then it wouldn’t
> haunt our nightmares. Instead, coupling has two properties that drag
> it center stage:

> - 1–N
>   One element can be coupled with any number of other elements with
>   respect to a change.

> - Cascading
>   Once a change has rippled from one element to another, that
>   implied change can trigger another round of changes, which can
>   themselves trigger changes of their own.

关于 changing program 的动力学。

> Cascading changes are the bigger issue. As we will see in the next
> book, the cost of changes follows a power law distribution. This
> distribution is created by the cost of cascading changes. You will
> be using software design to reduce the probability and magnitude of
> cascading changes.

> In large, complex systems, coupling can be subtle. Indeed, when we
> say a system is “complex,” we mean that changes have unexpected
> consequences.

甚至人们对复杂的主观理解也是与 coupling 相关的。

> What does coupling mean for answering the question, “Should I tidy
> first?” Sometimes when you’re staring at a messy change, it’s
> coupling that’s harshing your mellow: “But if I change this, then
> I’ll have to change all those too.” Messy. Take a minute to go
> through the list of tidyings and see which of them would reduce
> coupling.

## 30. Constantine’s Equivalence

TODO

## 31. Coupling Versus Decoupling

TODO

## 32. Cohesion

TODO

## 33. Conclusion

TODO

# Appendix: Annotated Reading List and References

TODO
