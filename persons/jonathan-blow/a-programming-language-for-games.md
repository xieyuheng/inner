---
title: a programming language for games
author: jonathan blow
playlist:
- "https://www.youtube.com/playlist?list=PLmV5I2fxaiCKfxMBrNsU1kgKJXD3PkyxO"
- "https://space.bilibili.com/550104600/lists/8503162"
---

# My Motive

[2026-07-06] 虽然设计了 meta-lisp，
但是带有 GC 的语言有局限，
没法用来写游戏一类的程序。

可以参照 jai 的设计，设计一个 system-lisp。
同样适用 HM 类型系统，并且使用 region-based 内存管理。

# [2014-09-26] Declarations and Factorability


为了 Factorability 应该支持 local function definition。
也应该支持带有独立 scope 的 local block。

这个 talk 开始设计 jai 的语法。

```c
f : float;
f : float = 1;
f := 1;
f = 1; // reassign
```

jai 的特点是，全局与局部定义使用同样的 definition 语法，
为了方彼岸 refactor。

在 scheme 的语法框架下。

全局：

```scheme
(claim f float-t)
(define f 1)
```

局部：

```scheme
(: f float-t)
(= f 1)
```

这个阶段的函数语法设计是：

```c
square := (x: float) -> float { return x * x; };
```

省略返回类型：

```c
square := (x: float) { return x * x; };
```

模仿 c++ 的 lambda capture：

```c
f := (x: float) -> float [y] { return x * x + y; };
```

可以说 jonathan blow 所使用的语法框架是 c 的语法框架，
同时纠正了 c 定义复合类型时的错误设计。

```c
                                { ... };
                      [capture] { ... };
     (float) -> float [capture] { ... };
f := (float) -> float [capture] { ... };
```

这里有对 c 语法框架内语法设计的讨论，比如：

- 类型声明
- lambda capture 的位置

从不同方面来讨论不同的设计方案，比如：

- 就语法设计的一致性而言
- 就编辑代码的便利性而言

不同的语法设计有不同的 affordance：

- lambda capture 设计为和 block 一组，而不是（像 c++）与 type 一组。
  就可以用 capture 来限制 block 所能引用的上层 scope 中的变量。
  方便理解代码与 refactor。

没有通用的设计，因为 affordance 只有在设计所处的文化环境下，才有意义。

在 scheme 语法框架内也可以有类似的讨论。

# 2014-10-31 Demo： Base language, compile-time execution

这是第一个带有语言实现的 talk。

TODO
