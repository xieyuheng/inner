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
也可以说成是 Problem - Constraints - Solution 格式，
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
