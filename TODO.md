# making-dynamic-language-s-beautiful-api-well-typed.md

也许动态类型语言可以使得我们这样做

- 更认真地学习 laravel & php 和 ruby & rails
- 用中文的童谣来讲解 OOP
  - https://en.wikipedia.org/wiki/99_Bottles_of_Beer
    - Knuth, Donald. "The Complexity of Songs"
- 制作类似 https://exercism.org/ 的网站来让大家对比同一个习题的不同解法

# anti-conditional-programming.md

和 Sandi Metz 学到了 anti-conditional programming。

每当在非 object creation 相关的代码中使用了 conditional，
都审问自己，为什么这样做，给出让自己安心的理由。

# extending Martin Löf's Type theory

- 在 Martin Löf 的基础的 Type theory 中引入新的类型有多种方式，
  inductive datatype 只是其中一种，higher inductive datatype 可能是另一种，
  那么一般的引入心类型的形式是什么呢？

- structural typing 和简单的 key value top level 可以简化理论
  - 也许可以参考 theory of object，
    还有历史上其他关于 structural typing 的论文

# implementation notes

- write chinese notes about implicit arguments and inductive type -- like the note about implementation patterns

# problem solving

- polya: ~/inner/person/polya/mathematics-and-plausible-reasoning
  - vol-2--patterns-of-plausible-inference.md

# software design

- practice design patterns -- functional style and class style
- laravel: https://github.com/xieyuheng/summer.php
- tdd: ~/watching/tdd/james-shore

# language design

- categorical-semantics: ~/watching/person/bartosz-milewski
  - https://www.youtube.com/user/DrBartosz/playlists
  - Category Theory 4.1: Terminal and initial objects
  - Category Theory 4.2: Products
  - Category Theory 5.1: Coproducts, sum types

# projects

- `@xieyuheng/partech` instead of `@cicada-lang/partech`

- `@xieyuheng/langdev` language framework

  - use `@xieyuheng/partech`

- `@xieyuheng/apidev` api framework
  - extract from `@xieyuheng/summer`
