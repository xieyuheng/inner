---
title: indiana university compiler lectures
authors: Jeremy G. Siek
year: 2020
github: "https://github.com/IUCompilerCourse/IU-P423-P523-E313-E513-Fall-2020"
video-backup: "https://space.bilibili.com/550104600/lists/6478233"
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
  -- `<tail-exp>` vs `<non-tail-exp>`：

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

- 这节课介绍后续的 passes。

- 讲解 select-instructions。

  这里有一些对 special case 的简单优化
  -- 对 self add 的优化，
  这种优化被称作 special casing。
  但是这种 self add 好像不会出现在目前所生成的 c-exp 中。

- 讲解 assign-homes。

  需要用到调用栈里的内存了，
  因此需要讲解 calling conventions。
  推迟到需要的时候才讲是正确的。

- 有同学问如何 debug assembly code，介绍 gdb 和 lldb。

  助教说，nanopass compiler 的意义就在于方便 debug，
  可以少用 gdb。

- 讲解 patch-instructions。

  除了这里预留 rax 寄存器的方案，
  还介绍了循环跑寄存器分配的方案，
  我还没理解这第二个方案。

  预留寄存器比较简单，并且速度比较快。
  据说编译器的诸多 passes 主要的瓶颈就是寄存器分配，
  别的 passes 都是 O(n)，只有寄存器分配是 O(n^2)。

- 讲解 print-x86。

  这里和书上的最后一个 pass 有差异。
  书中这之前还有一个 prelude-and-conclusion。

  这里需要用到 calling conventions 的知识。

  在我的实现中，这个两个函数分别是：
  format-x86-program 和 prolog-and-epilog。

  课程中想要直接在汇编中定义 main，
  其实可以在 runtime 中定义 main，
  然后调用汇编中的 callback 函数。

# 2020-09-08

- 这节课是学生代码的 code review。

- 评价 uniquify。

  说 racket 的 for/list 比 map 好用。
  要我说一点也不好，
  因为每次看到这些语法关键词都要查文档，
  比如在 for/list 之外，下面的代码还用到了 for/lists。

  学生的代码处理 let 的 rhs 时候有 bug。

- 评价 remove-complex-opera*。

  强调 structural recursion。

  但是 structural recursion 一般都是从外层到内层来实现，
  也就是 top-down design，
  但是这里的代码都是 bottom-up。

  解释为什么在 scheme function 内部用大量 define，
  因为为了避免 let 所引起的 indentation。

  这里介绍了一个写 structural recursion 的建议，
  就是不要在 extract 出来的辅助函数中做递归调用。
  递归调用都应该在主函数中清晰凸显出来。
  这个建议的极端范例就是 OOP。

- 评价 select-instructions。

  这里学生的代码对某些特殊情况进行了优化，
  但是也许这些优化应该在后面的 pass 做。

- 评价 assign-homes。

  这里学生的代码没有用 structural recursion，
  而是只写了一个递归函数。
  这也与数据类型定义没有用 ADT，
  而是用了自由的 racket struct 有关。

- 评价 patch-instructions。

  这里学生代码的命名没有遵循 structural recursion。

- 评价 print-x86。

  介绍 macos 的限制：

  - 汇编 label 必须要加上 `_` 前缀。
  - 在函数调用之前 stack 必须 16 byte align。

# 2020-09-10

- 这节课将寄存器分配，语言不变，算是优化，只有三个 passes。

  重点是如何把三个问题联系起来：

  - 寄存器分配
  - 图染色
  - 数独

- 首先最简单的方案是按顺序分配。

  但是人们马上就发现
  「有时候两个变量可以共用一个寄存器」，
  前提就是它们不被同时用到。
  对这个「是否同时用到」的分析就是 liveness analysis。

- live 的定义。
  强调 live 是就指令之间的位置（或者说时间）定义的。

  为什么要定义 live？
  因为如果两个变量 live 的时间不一样，
  就可以共用一个寄存器。

- 下面要计算一个例子中每个位置的 liveness。

  此时可以问学生为什么要从后面向前计算。
  答案就在 live 的定义中。
  重点在于教学生看定义，
  就像 polya 的 how to solve it 所说的。

  也许应该先 learn working example，
  然后再回头分析如何自己解决这个问题。

- 讲解 interference graph。

  在同一时刻 live 的两个 variables，
  不能共用一个寄存器。
  variable 之间的这种关系叫做 interfere。

  把 variable 视为点，
  把 liveness set 视为 hyperedge，
  就得到了 hypergraph，
  寄存器分配问题就转化为了图中点的着色问题。
  限制条件是相邻的点不能有同一种颜色。

  hypergraph 的结构就是 list of hyperedges，
  而一个 hyperedge 就是 set of nodes。
  这是极为常见的抽象数据结构，
  也可以在这里介绍一下。

- 有同学提问，不知道为什么需要解决寄存器分配问题。
  所以开始的时候就应该介绍内存访问和寄存器访问的时间差异，
  可以直接用真实数据介绍。

- 讲解如何 build interference graph。

  其实现有的数据已经形成 hypergraph 了，
  但是转化成 graph 可以让判断 edge 的谓词速度更快。

  这个过程中会 forget 一些 hypergraph 中的信息，
  具体就是 forget 了 hyperedge，
  也就是在那一个时刻发生了 interference 的信息。

- 前面转化成 liveness set
  其实也是忘记了具体 instructions 的信息，
  忘记信息就是做抽象。
  这一点也可以给同学讲讲。

- 为了 build interference graph，
  还需要有聪明的算法来减少这个转化的时间复杂度。

  聪明的算法就是再次遍历指令列表，
  以每个指令的 write set 和 live-after set 来构建 graph，
  因为大部分指令只有一个 write dest，
  所以这样可以避免一个对 set size 的复杂度。

  但是为什么这样做不会有遗漏？

  按照课程的介绍看来，
  并不是简单的从 hypergraph 构建 graph，
  而是还要用到具体的 instruction 信息，
  比如 `movq x, y` 时，不需要增加 x 到 y 的 edge。

  看书里的对应章节应该会解决这里的疑问。

# 2020-09-15

[2025-09-28]

- 介绍如何把数独问题转化成图染色问题。
  展示图染色问题的一般性。

- 思考图染色问题的解决方案。
  比如可以用 constraint solving 的方式来解决图染色问题。

- 强调需求：

  - 算法复杂度不能太高，否则编译器速度太慢。
  - 寄存器不够了可以用栈。
  - 可以不是最优解。

- 介绍 DSatur 算法。

- 用一个例子演示 DSatur 算法。

- 消除 register 之间的 self move。

- 为了能消除更多的 self move，
  介绍 move biasing 优化。
  此时可以再用一个 move-graph。

  bias 会影响的步骤是：

  - 选择哪个变量是下一个需要着色的；
  - 选择这个变量应该分配到哪个颜色。

- 介绍 move biasing 的例子。

- 介绍 register 和 memory 的速度差异，之前忘记介绍了。

- 我感觉这个优化可以推迟到，
  有很多代码例子可以用来评价优化的有效性之后。

  课程中讲这个优化是对的，
  但是实现的时候可以先保持简单。

- 关于可以使用的寄存器，
  已知我们需要保留 rax rsp rbp，
  另外还需要保留 r15，未来要用。

# 2020-09-17

[2025-10-04]

- 这节课讲 conditional 了，
  而下一节课才是前面三个 pass 的 code review，
  所以可以先看下一节课。

[2025-10-05]

- 介绍 conditional 相关的具体语法。

- 介绍 conditional 相关的解释器。

  这里有一个错误是用 match（以及之后的 cond）
  来做了 implicit assertion。
  这里有同学因为对此困惑而产生了疑问。
  正确的做法是直接写 assertion。

- extract `interp-op`。

- 强调了这里的解释器代码是课程提供的。

  这里的哲学是：解释器用来定义语言，为编译器提供测试标准。

- 下面讲类型检查。

  因为带有 conditional 之后，我们将加上静态类型系统。

  以已有的语言为例子 -- racket 和 typed racket。
  介绍 runtime error 和 comptime error。

- 之所以加静态类型系统，
  是因为这可以让编译器实现起来更简单，
  也就是不需要对 64 bit value 做 runtime tag 编码。

- TODO

# 2020-09-22

[2025-10-04]

- 这节课是对寄存器分配中的三个 pass 的 code review。

- 在 code review 之前，用 running example 介绍生成代码的例子。

- 再次介绍 16 bytes stack alignment：

  - call 之前要 16 bytes stack align。
  - call 之后保存了返回地址，所以 rsp 前进 8 bytes。
  - call 的 prelude 保存当前 rbp，所有 rsp 又前进 8 bytes。
  - 又回到 16 bytes stack alignment 的状态了，
    后面就只需要考虑 local variable 所需要的 alignment 了。

- 这里同学问：「在 prelude 中，
  为什么先处理 rbp 再处理 callee-saved 寄存器的 push？」
  老师回答这是一个惯例，是模仿 GCC 的做法，
  使得学生以后读 GCC 所生成的代码时更方便。

- 还有同学问：「在 prelude 中，
  为什么不先给所有 spilled variable 留下空间，
  再处理 callee-saved 寄存器的 push？」
  老师回答这都可以，只是一种随意的选择，
  也许某一种选择实现编译器的时候写起来更容易。

- 介绍 live range splitting。

  deepseek 说「SSA + Linear Scan 可以自动获得这种效果。」

- 下面开始 code review。

- review uncover-live：

  这里的代码没有按照书里描述的方式处理 jmp 和 retq。

- review build-interference：

  所有的 block 用来 build 了一个 graph，
  保存到了 x86 program 的 info 中。
  这是不对的。
  应该每个 block build 一个 graph，
  保存到 block 的 info 中。

  另外这里在构造 graph 的时候，其实可以避免，
  先用 list-product/no-diagonal 获得所有的 edge，
  然后直接从 edge 构造 graph。

- review allocate-registers：

  这里好的一点是重用了 assign-homes。
  但是是直接调用了 assign-homes-block，
  而不是保持 assign-homes 这个 pass。

  是不是这也可以在 pass 上体现出来？
  也就是说，不是在 allocate-registers 中直接调用 assign-homes，
  而是用 info 传递 assign-homes 所需要的信息。

  这是我第一次发现可以 nanopass 的地方我没有 nanopass。

- 这里为了做 move biasing 相关的优化，
  所以没有用一般的 graph-coloring 函数。

  这里老师承认自己重写了四次之后代码还是一坨。

- 我发现这种上课方式很不错：

  - 首先老师讲解问题
  - 然后学生回家完成实现
  - 最后一起 code review

  因为这可以让学生对比出来，
  自己的代码和老师的代码之间的优劣。

  自己先实现一遍很重要，
  因为这就像是 lattice 的元素的 join，
  想要得到更好的解决方案，
  需要自己的方案中有一些新 idea，
  然后和老师的方案 join。

  但是这其实就是最简单的上课方式：

  - 老师讲课
  - 学生回家写作业
  - 最后一起对答案

- 关于 assign-homes 我发现：

  everybody home, but after then
  they found that variables in the same home
  can not live together at all!

  everybody-home 是 kent 讲这门课的时候，
  对 assign-homes 这个 pass 的命名。

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
