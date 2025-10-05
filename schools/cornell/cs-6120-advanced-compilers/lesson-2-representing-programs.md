---
title: lesson 2 -- representing programs
source: "https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/2/"
---

[2025-10-05]

- 不用 AST 而使用 list of instructions 来表示程序。
  就是类似 LLVM IR 的一种语言。

  ```c
  <dest>: <type> = <operator> <operand> ...;
  ```

  必须要求 `<operator>` 出现，
  甚至不能直接写 literal 和 variable：

  ```c
  v1: int = const 1;
  result: int = id v1;
  ```

  其中 `id` 是 identity operator，
  `const` 是 constant operator。

  operator 不一定对应汇编指令的 opcode，
  比如下面的例子中有 `print` 这个 operator。

  也许通过扩展 operator 和 type，
  我们可以有一个相对 high level 的这种语言，
  适合直接把 lisp 代码翻译成 instruction list。

  而不是只在中间语言才用这种 instruction list 语言。

  这类 instruction list 语言应该叫什么名字？
  SSA -- static single-assignment form 吗？
  但是目前老师所描述的语言好像还没有 single assignment 这个限制。

- CFG -- control flow graph

  是我们从 instruction list 所表示的 program 中提取出来的信息。

  作为 graph 的性质：

  - directed graph
  - vertex = instruction
  - edge = possible control flow
  - one entry vertex, one exit vertex

- 介绍一些从 instruction list 构造 CFG 的例子。

- 介绍 CFG 的一种自然演化，
  就是为最常见的 chain of instructions
  而优化 graph 的表示方式。

  就是引入 basic block 的概念，
  使得 vertex 从 instruction 变成 basic block。

  注意老师如何渐进地引入概念，
  而不是一开始就介绍 basic block。

  一个 basic block：

  - jump 与 branch -- 称作 terminator，
    只能出现在 basic block 的结尾。
    注意 call 不是 terminator。

  - label 的对象是（也就是 jump 或 branch 的 target）
    只能 basic block 而不是 instruction。

- 可以设想设计 lisp 语法，
  直接表示 basic block 所构成的 CFG。

  也许可以叫做 b-lisp，
  或者 baby-lisp -- basic-block-lisp。

- 下面介绍如何设计类似汇编的语言，
  也就是从线性的 instruction list 开始，
  解析出来 basic block 的 CFG。

  最好直接把 label 保存在 block 中：

  ```scheme
  (define-data block?
    (basic-block (label symbol?) (instrs (list? instr?))))
  ```

  所需要的函数类型是：

  ```scheme
  (-> (list? (union instr? label?)) (list? block?))
  ```

  其实既然是在设计新的中间语言，
  就可以直接设计表示 CFG 的语言，
  而不必再从 instruction list 解析。

  这与 lisp 直接用 sexp 而避免语法解析是一样的道理。

- 然后介绍如个构造 CFG。

  所需要的函数类型是：

  ```scheme
  (-> (list? block?) (digraph? block?))
  ```

  有一个特殊情况是不带 terminator 的 block，
  要有 edge 到下一个 block。

  当然，设计语言的时候，也可以考虑完全禁止这种情况。

- 我发现这位老师好像很擅长写类似 python 的伪代码。

  可能在现代的 CS 专业，上了很多算法课之后就会如此。
