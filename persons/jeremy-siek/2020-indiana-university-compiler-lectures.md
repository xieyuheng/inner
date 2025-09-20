---
title: indiana university compiler lectures
authors: Jeremy G. Siek
year: 2020
github: "https://github.com/IUCompilerCourse/IU-P423-P523-E313-E513-Fall-2020"
video-backup: "https://space.bilibili.com/550104600/lists/4735899"
---

# My Motive

[2025-09-21] 已经在用我自己的 x-lisp 来实现课程中的编译器了，书也读到了第三章。
跟一下这里个课程视频，因为课程中有一些书里没有的细节。
同时也记录一下教学的技巧。

# 2020-08-25

- 介绍课程概况，介绍教科书的章节。

- 介绍「具体语法」与「抽象语法」的差异。

- 43:38 我才知道是从这一年的课程开始，
  才放弃直接把 sexp 当作 AST 的。
  这一年开始用 racket 的 struct 实现 AST。

- 由于没有用 sexp，
  导致很多学生在这里问 racket struct 的问题，
  其实还是应该保持简单，就用 sexp。

  还有一个方式是用 中用 ADT（ algebraic data type）来实现 AST。
  但是这通常要求使用静态类型语言，
  除非像 x-lisp 一样，在动态类型语言也支持 ADT。

- 介绍什么是 language，
  以及如何用 grammer 来定义 language。
  使用 EBNF 来写 grammer。
  以第一个语言 R0 为例子介绍它的 concrete syntax grammer
  与 abstract syntax grammer。

  种类适合与集合论联系起来讨论。
  集合论可以给出这些概念的精确定义。

- 介绍 structural recursion 与 pattern matching。
  给出一个简单的 structural recursion 函数的例子，
  即计算 exp 的深度。

# 2020-08-27

- 想要学习如何写编译器，先要学习如何写解释器。
  解释器可以用来定义语言。

  这种定义方式让人想到范畴论中的 diagram chasing。
  只要让学生考虑所有 language 所构成的范畴，
  就能引入范畴论。

- 介绍第一个语言的 R0 的解释器。

- 介绍 R1 语言 -- 带有 `(let)`。

- 介绍 x86 的构架，并且把这个构架定义成一个语言。

- 59:46 停下来问学生「想要编译的源语言与目标 x86 语言的差异是什么」，
  因为编译器的本质就是 bridging the differences。
  如果学生未来要自己写编译器，这个问题是最需要独立思考的。

  回答这个问题的同时，介绍如何用编译器的 passes 处理 differences。

- 1:17:39 除了对 source 语言有解释器，
  编译器 passes 用到的各种中间语言也有解释器，
  比如可以写 x86 的解释器（或者说模拟器），
  这是 dan 开始就有的传统，
  就是写着玩，作为练习。

# 2020-09-01

- 正式介绍所有 passes。
  这些 passes 将会被用作学生的作业。
  因此讲解的时候要给出例子，
  而不是直接给出代码。

  我可以通过用谓词刻画语言的子集，
  从而把 compiler 所 bridge 的 differences
  明确地形式化地表示出来，
  并且用这种形式化表示来指导 structural recursion 的实现。

  当然，既然语言使用 grammer 定义的，
  这里用 grammer 而不是用谓词来刻画子集也可以。

  但是由于 x-lisp 中定义新的数据类型就会得到谓词，
  所以用谓词来刻画比较自然。

  如果说刻画的子集是真子集，就可以用来做测试。

- 讲解 uniquify。

- 讲解 remove-complex-opera*。
  书里是 remove-complex-operands，也就是不包含 operator。

  operator -- opera -- rator -- rand，
  可能是从 Dan 开始就玩的文字游戏，
  rator 和 rand 经常被用来在写程序的时候做命名。

  这个 pass 的 structural recursion 可依据返回类型来写：

  ```bnf
  <atom-operand-exp>
    ::= (var-exp <symbol>)
      | (int-exp <int>)
      | (let-exp <symbol>
          <atom-operand-exp>
          <atom-operand-exp>)
      | (prim-exp <op> [<atom-exp> ...])
  ```

  - `<atom-operand-exp>` -- `(rco-exp)`
  - `<atom-exp>` -- `(rco-atom)`

- 介绍中间语言 C0，并且讲解 explicate-control。

  这里定义的函数除了翻译成 C0 这个职责，
  还返回了变量的列表，
  这是两个职责应该分两组函数实现。

  这个 pass 的 structural recursion，
  可以先把输入的 `<atom-operand-exe>` 分成两类
  -- `<tail-exp>` v.s `<non-tail-exp>`：

  ```bnf
  <atom-operand-exe> ::= <tail-exp>
  <tail-exp>
    ::= (var-exp <symbol>)
      | (int-exp <int>)
      | (prim-exp <op> [<atom-exp> ...])
      | (let-exp <symbol>
          <non-tail-exp>
          <tail-exp>)
  <non-tail-exp>
    ::= (var-exp <symbol>)
      | (int-exp <int>)
      | (prim-exp <op> [<atom-exp> ...])
      | (let-exp <symbol>
          <non-tail-exp>
          <non-tail-exp>)
  ```

  - `<tail-exp>` -- `(explicate-tail)`
  - `<non-tail-exp>` -- `(explicate-assign)`

# 2020-09-03

TODO

# 2020-09-08
# 2020-09-10
# 2020-09-15
# 2020-09-17
# 2020-09-22
# 2020-09-24
# 2020-09-29
# 2020-10-01
# 2020-10-06
# 2020-10-08
# 2020-10-13
# 2020-10-15
# 2020-10-20
# 2020-10-22
# 2020-10-27
# 2020-10-29
# 2020-11-03
# 2020-11-05
# 2020-11-10
# 2020-11-12
# 2020-11-17
# 2020-11-19
# 2020-12-01
# 2020-12-08
# 2020-12-10
