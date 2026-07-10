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

首先是关于使用 `:=` 的语法来定义 function，
这里不应该追求一致性，因为定义函数与定义全局变量意义是完全不同的。
因此之后函数定义的设计换成了 `::`。

有意义的差异值得用语法差异表示出来。

这个 talk 所展示的主要语言 feature 是，
编译时可以通过运行任意函数。

如果要设计 explicit 的 system-lisp，
就需要 explicit `(ref)` 和 `(deref)`，
并且生成 accessors 和 modifiers：

```scheme
(define-struct node-t
  (left (ref-t node-t))
  (right (ref-t node-t))
  (value int-t))

(claim main (-> void-t void-t))

(define (main)
  (= node (new node-t))
  (: node (ref-t node-t))
  (: (deref node) node-t)
  (node-put-left node (new node-t))
  (node-put-right node (new node-t))
  (node-put-value node 1))
```

accessors 和 modifiers 的参数都必须是 `(ref)`。
这样就不用些很多 `(deref)` -- 这是主要使用情况，
而是需要在处理 struct 本身的时候加上 `(ref)`。

# 2014-12-11 Demo： Iteration and arrays, uninitialized values, enums

## part 1

通过编译时运行任意函数，
外加命令行指定所需要运行的函数，
可以完成 build，来代替外部的 build system。

## part 2

设计 pointer 和 array 的语法：

```c
N :: 10;

e : Entity;

pointer : ^Entity;
pointer = ^e;

values: [N] float;
e_pointer: ^ Entity
e_pointers: [N] ^ Entity

e2 : Entity = *pointer;

e_pointer = new Entity;
e_pointers[0] = new Entity;
```

```scheme
(define N 10)

(: e entity-t)

(: pointer (ref-t entity-t))
(= pointer (ref e))

(: values (array-t float-t N))
(: e-pointer (ref-t entity-t))
(: e-pointers (array-t (ref-t entity-t) N))

(: e2 entity-t)
(= e2 (deref pointer))

(= e-pointer (new entity-t))
(array-put e-pointers 0 (new entity-t))
```

但是注意这里 `(=)` 的语义是初始化或 reassign，
与 meta-lisp 中单纯的初始化不同。

也许应该分离出来一个不常用的 `(assign)` 语法，
专门代表 reassign。

## part 3: default values, implicit uninitialization

指默认把 struct 初始化为对应类型的 0。
用 `---` 语法代表 uninitialized。

```scheme
f : float = ---

Vector3 :: struct {
  x : float = ---
  y : float = ---
  z : float = ---
}

v1 : Vector3
v1.x
v1.y
v1.z

array : [100] Vector3
v2 := array[50]
```

```scheme
(: f float-t)
(= f #uninitialized)

(define-struct vector3-t
  (x float-t #uninitialized)
  (y float-t #uninitialized)
  (z float-t #uninitialized))

(: v1 vector3-t)
(vector3-x (ref v1))
(vector3-y (ref v1))
(vector3-z (ref v1))

(: array (array-t vector3-t 100))

(: v2 vector3-t)
(= v2 (array-get array 50))
```

## part 4: array type

也许 system-lisp 可以要求 array 类型必须带有长度参数，
而 list 是 array + 长度 + auto grow 功能。

- `(array-ref-t T)`
  -- 意思是区分作为 array 的 pointer 和 value 的 pointer `(ref-t T)`。
- `(array-t T N)`
- `(list-t T)`

但是这样不行，
因为如果想要使用 HM 类型系统，就需要完全区分所有类型，
而不同的类型需要不同的 API 函数。
三组完全类似的 API 函数，看起来不太对。

方案 A：

- 区分一般指针 `(ref-t T)` 与 array 指针。
  要求 `(array-t T)` 的类型中没有长度参数，实现为长度 + 指针。
  初始化 array 时带有长度参数 `(new-array <type> N)`。
  如果想要在 stack 上分配内存，可以配置特定的 allocator。

jai 在这里区分了两种 array 类型：

```c
int[] -- 长度 + 指针
int[N] -- 编译时长度 + 指针
```

函数的类型参数总是 `int[]`，
初始化为 `int[N]` 的变量，
在传递给函数的时候，会利用类型信息 elaborate 成 `int[]`。

## part 5: iteration

array 是 builiin type，就可以设计特殊的 for 循环语法。

```scheme
(for* ((x array)  ;; 外层循环
       (y array)) ;; 内层循环
  ...)
```

区分并行循环 `for` 和嵌套循环 `for*`。
并行循环也是常用的，比如 zip。
还可以设计类似的 `for-ref` 和 `for-ref*`，
不是 by-value 而是 by-ref。

jai 的设计：

- for loop 中的 in-place remove，我觉得太复杂了。
- for loop 中的 implicit variable `it` 和 `it_index`，
  从 schemer 的角度看，也是错误的设计。

可以从例子里看出来 jai 的 dynamic array 是 auto grow 的，
而不是简单的 slice（length + pointer）：

- 有 `array_add` API。
- 在某个位置 remove 的时候，把末尾的元素补充进来。
- non-local break 和 continue，
  可以用 loop variable 来代表 loop block，
  并且用 `break j` 和 `continue j` 退出指定的 look block。

## part 6: enum

可能 system-lisp 应该直接支持 ADT 而避免 c-like enum。

jai 的 enum 带有 runtime meta data，有 `<enum-type>.names` 之类的 API。

这与 scheme 为某个生成定义生成 API 函数的方式很相似，
比如可以想象给 meta-lisp 的 `(define-struct)` 和 `(define-enum)`，
生成更多的关于 meta-data 的 API 函数。

因此在 system-lisp 中把「使用 `define-*` 来生成 API 函数」的方式，
作为核心的设计框架是正确的，这样很灵活。
缺点是生成的名字要占用命名空间。

## part 7: inlining

inline 应该被设计为命令，而不是编译器的优化 hint。
