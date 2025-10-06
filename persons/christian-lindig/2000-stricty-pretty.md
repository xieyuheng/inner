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

TODO

# 3 Implementation

TODO

# 4 Examples

TODO

# 5 Conclusion

TODO
