---
title: stricty pretty
author: christian lindig
year: 2000
---

# My Motive

[2025-10-06]
大概读了 persons/philip-wadler/1997-a-prettier-printer，
但是没有完全看懂。
看看这个 ocaml 版本的会不会好懂一些。

[2025-10-07] 我发现对这种方案的更简单的理解方式是 markup language。
PPML -- pretty print markup language。

# Abstract

> Pretty printers are tools for formatting structured text. A recently
> taken algebraic approach has lead to a systematic design of pretty
> printers. Wadler has proposed such an algebraic pretty printer
> together with an implementation for the lazy functional language
> Haskell. The original design causes exponential complexity when
> literally used in a strict language. This note recalls some
> properties of Wadler’s pretty printer on an operational level and
> presents an efficient implementation for the strict functional
> language Objective Caml.

所谓 "on an operational level" 。

# 2 Philip Wadler’s Pretty Printer

用 XML 和 markup language 来理解这里所定义的数据类型，
就非常清晰了 -- PPML -- pretty print markup language。

markup 语言和 render 方式的设计有两个重点：

- 一是是 `group` 这个 markup，
  一个 group 中的所有 node，包括 sub-node，
  要么被 print 成一行，要么这一个 group 会被分行，
  sub-node 继续递归处理。

- 二是处理 indentation 的方式，
  是根据当前的 context，在 newline 后面加空格。
  而不是给整体 print 的结果前面的每一行加空格。

# 3 Implementation

这一章出现了 "pp.ml" 这个词，
吓了我一跳，我还以为作者也意识到这是 markup language 了，
其实这是文件名，"pp" 代表 pretty print，"ml" 代表 ocaml。

> The explicit implementation of groups is the main difference to the
> lazy implementation. It encodes groups implicitly by unfolding a
> group into two alternative documents: a flat one, where all breaks
> are rendered as spaces; and a broken one, where all breaks are
> rendered as newlines.

> Since this expansion is done lazily it does not lead to an
> exponential growth as it would do in a strict language. To avoid the
> exponential growth with the number of nested groups the strict
> implementation must encode groups explicitly.

在 wadler 的 algebraic 实现中，
group 并不是一个 data constructor，
而是一个函数。

作者还设计了一个中间结构：

> Documents of type `doc` are not printed directly but transformed
> into simpler documents of type `sdoc`. During this transformation
> the layout for each group is decided, as `sdoc` does no longer
> provide groups.

```ocaml
type sdoc =
  | SNil
  | SText of string * sdoc
  | SLine of int * sdoc (* newline + spaces *)
```

其实就是 list of parts，
一个 part 可以是 string 或者 indented newline。

> Wadler has shown that every complex document can be transformed into
> an equivalent simple document which is straight forward to print.

所以说这种设计不是为了方便，
而至为了迎合 wadler algebraic 风格中的 normal form。

那么对正常实现来说，
这种中间结构不一定是有必要的了。

注意，为了检查一个 group 是否能 rendered 在一行内，
不是 render 再比较宽度，而是把宽度作为参数，
一点一点消耗宽度。

这里递归函数 `fits` 处理 `DocCons`（其实意思是 `DocAppend`）的方式很有意思，
因为先处理 append 的左边再处理右边对于 consume 宽度参数 `w` 来说是很不方便的。

但是 `fits` 所处理的 list 元素，为什么要带有 `mode`？
其实是必要的，只有 `format` 函数需要。
同样地，`i` 对于 `fits` 来说也是多余的。

这里 `format` 的定义在需要调用 `fits` 的时候写错了，
应该只把当前 group 的 `doc` 传给 `fits`，
而这里还带上了后面的 `z` -- rest list of `doc`s。

> The `fits` function of the lazy implementation does not work on
> complex documents but on simple documents instead. The laziness of
> Haskell permits to transform the alternative group variants into
> simple documents and check them which is easier than checking
> complex documents. The strict implementation does not expand groups
> beforehand and thus must check the width of complex documents.

没想到 lazy 语言还有这种优点。

# 4 Examples

为了处理例子，定义了一些 ppml 的 combinator 作为辅助函数。

# A Missing Parts

> The document doc to be printed is enclosed on the outermost level by
> a virtual [flat] group.
