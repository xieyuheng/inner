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

  注意，未来的章节还要讲解如何实现 tag 编码的动态类型语言，
  到时候可以保留静态类型检查，二者并不冲突。

- 介绍了三种错误处理：

  - comptime
  - runtime
  - 不处理

  有趣的是，x86 就是不处理的例子。
  没人抱怨什么。

  也许 BCPL 这种完全不处理的高阶语言也是有价值的。
  可以作为编译器的中间语言。

- 介绍 type checker 的实现。

  为了保持简单，课程中个的实现总是假设返回类型是 int，
  因为这方便实现与测试。

  老师的借口是：c 的 main 函数就是如此。

  但是这样并不好。
  正确的做法是加上 `the` 这个语法关键词。

- 这里还有一些错误：

  - 把 type cheker 的 `context` 参数命名为 `env`。
  - 把 `type-infer-exp` 函数命名为 `type-check-exp`。
    没有 infer 和 check 的区别，
    bidirectional type checking 的结构就不清晰了。

- 这里有同学问：「`read` 的 type 是什么」？

  其实目前还没有函数类型，
  primitive operator 的类型检查是以临时地方式实现的。

  这里还解释了，为了保持简单 `read` 只能返回 int。
  既然如此，为什么不直接叫 `read-int` 呢？
  这样就可以避免解释了。

- 介绍 if 的 type checking 的时候，
  强调了 if 的两个 branch 必须是同一个 type。

  - 想要突破这个限制就必须要实现集合论意义上的 union type，
    这是非常困难的，比如要处理递归定义的 type 中带有 union 的情况。

  有同学问「如果 if 的一个 branch infer 了 int，
  另一个 branch infer 了 bool，
  可以不可以 cast bool 到 int？」

  老师的回答是，不可以，
  因为 type checker 的设计其实是受到已有的 interpreter 的约束的，
  也就是受到语言 runtime 行为的定义的约束的。
  可以说 type checker 在「预测」interpreter 的行为。

  interpreter 中没有 cast，
  那么 type checker 也不能 cast。

  比如问「为什么 if 的 conditional 必须被 check 成 bool？」的时候，
  答案必须从 interpreter 对语言的定义中找到。

  这里的 type checker 要能检查出来
  interpreter 中所有 undefined behavior。

  这里是学生问的问题，
  其实在自己设计语言的类型系统时，
  也会有类似的迟疑，
  重点就是记住要「回到定义」去找答案。

- 这里有学生问「为什么 if 不能被实现为 primitive operator？」

  这是 lisp 的基础知识了，
  应该在整个课程开始之前有一些预习。

  在用 sexp 设计语法时，
  主要情况是用 `()` 代表函数作用，
  也就是要为组合子演算设计一个语言。

  但是在代表函数作用的过程中，
  我们让渡了一部分 sexp 表达式出来，
  代表特殊的语法关键词，比如 quote lambda 和 if。

  if 有必要被实现为一个语法关键词，
  是因为它不能被处理为一般的函数作用，
  因为一般的函数作用要 eval 所有的参数位置的 exp，
  而 if 只应该 eval 一个 branch 的 exp。

  如果学生的问题意思是，
  在实现中，为什么不用 prim-exp 来处理 if，
  答案就是：meaningful distinctions deserve to be maintained -- bishop。

  这个学生之所以问，
  是因为发现 and 和 or 被老师实现为了 primitive operator，
  这是错误的。

- 下面要讲和 conditional 相关的第一个 pass 了。

  这个 pass 是 shrink，或者叫 desugar，
  意思是，在集合论的意义上，
  把一个大语言缩小为一个小语言。

  具体例子是：

  ```scheme
  (- e1 e2) => (+ e1 (- e2))
  ```

  但是这显然只是一个教学的例子，
  实现的时候不能浪费 x86 的 `subq` instruction。

  下面的 desugar 是合理的：

  - 假设我们主要保留 `(< e1 e2)`。

  ```scheme
  (> e1 e2) => (< e2 e1)
  (<= e1 e2) => (not (> e1 e2)) => (not (< e2 e1))
  ```

  但是上面这些变换有个问题，
  就是按照目前的 interpreter，参数的位置决定了 eval 的顺序，
  而上面的变换改变了参数的顺序，也就改变了 eval 的顺序。

  正确的翻译要用到 `(let)`。

  但是有个学生想到了：

  ```scheme
  (> e1 e2) => (< (- e1) (- e2))
  ```

  还有同学想到了把所有比较都翻译成 sub 相关的算术运算。

  但是翻译成 `(let)` 是最简单的。

  注意，类似的问题在 `partial-eval` 中也会出现，
  也要保证 eval 的顺序不变。

- 预告了一下，后面的难点是：

  - explicate-control
  - allocate-registers

- 聊到了大家上课用到的 web app 的功能：

  - 线上做题，自动打分，但是要隐藏答案和得分，
  - 在某个固定的时间一起公布答案。
    这样能避免学生之间利用提前公布的答案和得分。

  也是不错的 web app 需求的例子。

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

[2025-10-05]

- 介绍编译 conditional 所需要的 x86 指令。

- 介绍关于 conditional 的 instruction selection。

- 介绍为了支持 conditional，需要如何扩展中间语言 -- C。
  其实这里非常类似 SSA 了，但是没有直接说是 SSA。
  比如，if 其实是 SSA 中受限制的 br。

- 介绍如何把 S 的 conditional 编译到 C。

  给出一个 running example，
  然后介绍如何做 explicate-control。

  这里的 running example 是在 if 的 condition 位置嵌套了另一个 if。
  这种嵌套是需要在 explicate-control pass 消除的。

- 这里老师说在编译代码的过程中，
  一定要避免 duplicate 某段代码，
  因为很容易构造输入程序，
  使得这种 duplication 指数爆炸。

  使用 graph 而不用 tree，
  就可以避免这种 duplication。

  因为与 tree 相比，graph 中的 node 可以有多个 parent node。

- unnest if 的过程不能在 AST 的阶段完成，
  就是因为不能 duplication code。

  但是这样 explicate-control 就和 unnest-if 被放在一个 pass 中了。

  也许可以给 S 加一个 `let-block` 语法，
  使得可以把 unnest-if 作为一个 pass
  从 explicate-control 中 extract 出来。

  - 给 S 增加语法也许是不对的，
    因为 explicate-control 在于，
    用 CPS 处理所有的 control flow。

- 介绍如何修改 explicate-control 的中的递归函数，
  以支持 conditional。

- 直接对 running example 手动 trace `explicate-tail` 函数。
  看看上面所修改的递归函数的运行效果。

  能够手动 trace 递归函数是非常牛的技能，
  是 the little book 所使用的核心技能。

# 2020-09-29

[2025-10-06]

- 介绍 explicate-control 的难点，
  其实这个 pass 是在用 CPS 来把所有的 control 都变成 explicate，
  而难点就在于，对于初学者来说 CPS 比较难理解。

  一旦熟悉了 CPS 这个 pass 看起来就很简单。

  这里由于两节课之间间隔了一段时间，所以在课程的开头，
  再次讲解 explicate-control 和 conditional 还有 CPS，
  非常不错。

  在递归函数中：

  - explicate-assign 切换到 `(let)` 的 context 中。
  - explicate-pred 切换到 `(if)` 的 context 中。

  具体实现的时候的思想模型，
  就是在不同的语法 context 中切换。

  另外，每个递归函数的参数不再是简单的一个 block，
  而是需要在某些时候添加新的 block 到 CFG 中，
  所以 CFG 本身也要作为参数（为了方便可以用副作用）。

  具体在代码中，我们应该不会用 CFG 这个词，
  因为它其实是一个 record 而不是 graph，
  graph 的信息还没有被明显地提取出来。

- 介绍 conditional 的 select-instructions。

- 介绍 conditional 的 allocate-registers。

- 首先是 uncover-live。

  生成了很多 block 之后，
  block 之间是 share variable 的，
  所以 liveness 要一起分析。

  注意：

  - liveness 要从后向前分析。
  - block 末尾是 branch 到两个 block 的时候，
    要 union 两个 block 的 live-before sets。

- 介绍上面用到的 order 是 reverse topological order，
  所谓 topological order 就是必须沿着 edge 给点排序。

  这里说实现的时候，可以选择先做有向图的拓扑排序，
  然后用一个 for loop 就可以处理所有的 liveness 了。

- 介绍 build-interference。

  注意这里将会有多个 block share 同一个 interference-graph 的情况，
  因此也会 share 同一个 coloring，实现的时候要避免重复 coloring。

  新增的 case 是：

  - `movzbq` -- 与 `movq` 类似。
  - `al` -- 可以视为 `rax`。

- 介绍 patch-instructions。

  新增的 case 是：

  - `cmpq` -- 第二个参数不能是 immediate。
  - `movzbq` -- dest 参数必须是 register。

- 下面介绍和 jump 有关的优化。

  explicate-control 会给出 goto 一个 block，
  但是这个 block 只有一个 statement，
  这个 statement 也是 goto 一个 block，
  这显然应该被合并。

  这里的一个解决方案是用 `walk` 函数，
  类似于在实现 unification 的时候对 logic variable 的 walk。

  可以 walk 每个 goto statement，
  直到遇到 non-trivial block。

  最后删除没有被引用到的 block。
  这可以作为一个独立的 pass。

  另外一个优化是，如果一个 block 只有一个 in-edge，
  那么这个 block 可以直接合并到 jump 到它的 parent block。

  这里有同学问「这两个优化有什么区别？」
  老师通过画 ASCII art 图来解释，非常不错。

- 下面要进入下一个章节的内容了。
  就是 "tuples and garbage collection"。

  我实现完 conditional 之后再来看。

  注意，书中在这之前还有一章
  "loops and dataflow analysis"，
  在课程视频，这一章节的内容放到了函数相关的内容之后。

[2025-10-18]

- 介绍 vector 和 void 数据类型。

  这里的 vector 真的应该用 tuple，
  因为 `(vector 1 2 #t)` 的类型是 `(Vector Int Int Bool)`。

- 这里学生问 void 相关的问题，
  为什么要使用 void。

  老师强调了课程想要编译的语言是 racket 的一个子集。
  并且所有的 expression 都要有一个 value，
  这与 c 这种带有 statement 的语言不同。

- tuple 是 reference value 与之前的 atom value（int 和 bool）不同。
  reference value 的特点是可以有 alias。
  通过副作用这种观察手段，可以区分 alias 与 copy。
  alias 与 副作用 同时出现时，语言就很难静态分析。
  SSA 有 alias 但是没有对变量的副作用。

- 这里有同学质疑了老师给出的例子，
  没有揭示出来 "tuple live forever"（因此需要 heap），
  非常不错。

- 其实 alias 和 live-forever 都可以用 atom value vs. reference value 来解释。
  这种对 value 的分类程序员应该是很熟悉的。

- 下面介绍垃圾回收的概念。

- 老师介绍了之前这门课是要求学生自己写垃圾回收器的，
  但是太难了。

# 2020-10-01

[2025-10-18]

- 这节课继续将 tuple 与垃圾回收器。

- 将要实现的就是简单的 copying GC。

- 现代的动态语言都用 generational GC，
  比如 java 和 chez-scheme。

  generational GC 是可以在 copying GC 基础上做的一个重要的优化。

- 我好奇在没有 runtime tag 的前提下如何实现 GC。
  如何区分 pointer 与 int。

- 下面介绍 copying GC 的工作原理。

  老师在画图的时候用黑点表示 pointer，
  这应该是 lisp 的传统，很不错。

  copying GC 的一个问题就是 copy 之后 pointer 变了，
  因此需要 update 很多 pointer。

  我想我可能会实现以 malloc 为基础的 mark and sweep GC，
  这样可以保证稳定的 pointer，
  也不用担心内从中的 gap 问题。

  其实 copying GC 在同时解决两个问题：

  - 一个是避免内存泄漏。
  - 一个是消除 gap。

  我们可以考虑分层解决这两个问题，
  把「消除 gap」的问题放到底层解决。

- copying GC 的特点是不用 free dead data，
  而只需要 copy live data。

- copying GC 本质上是重建同构的 digraph 的问题，
  或者说 pointer 本质上都是 digraph 问题。

- 既然转化成了 digraph 的问题，
  就又到了使用 queue 来遍历 graph 的时候了。

  这里介绍的技巧是，
  在从 from-space copy 到 to-space 的过程中，
  只要给 to-space 增加一个指针作为 queue 的 front pointer，
  就可以直接把 to-space 本身当作 queue。
  不必再额外分配内存做一个 queue 出来。

  这将会形成广度优先遍历。
  这种实现方式叫做 cheney 算法。

  注意，要避免一个 from-space 中的 data
  被 enqueue 两次（被 copy 两次）。

  - 我能想到的方案是，在每个 data 上留一个 field，
    记录自己被 copy 到哪里了。

    其实不需要额外 field，因为 copy 之后，
    可以在这个 data 的位置上留下信息，
    说这个 data 已经被 copy 到了什么地方。

    但是这种方案可能也是需要有 tag 才方便，
    静态类型还是需要预留 field。

- 区分 stack 中 pointer 和 int 的方式是，
  维护一个 shadow stack（root stack），
  其中只有 pointer。

  这其实挺复杂的，
  首先正常的运行时要维护一个只有在 copy 时才会用到的 root stack，
  其次在 copy 之前，要处理寄存器中保存着 pointer 的情况。

  动态类型的 tag 编码，
  就可以解决这个「寻找 root」的问题。

- 区分 heap 中 pointer 和 int 的方式是，
  在每个 data 开头加上个 header，
  来保存关于 pointer 的 metadata。

  - 1 bit -- forwarding -- 这个 data 是否已经被 copy 了，
    数据中保存的是 forwarding address。

  - 6 bits -- tuple length (vector length) -- 限制 length 最大是 64。
    这样看实现这个 tuple 还不如直接实现古典的 lisp cons 呢，
    起码能保持简单。

  - 50 bits -- pointer mask -- 有进一步限制 tuple lisp 最大是 50。

  - 7 bits -- 未使用。
    可以想象这些 bits 可以用来实现任意长度的 vector。

- 区分寄存器中的 pointer 和 int 的方式，
  是在寄存器分配的时候做的，
  但是细节我没理解。

  可能是任何类型为 vector 的变量，
  都要避免被分配到寄存器中？

- 下面介绍说，要给 type check 加上 elaboration 了，
  需要让 type check 返回的表达式都带有类型。

- 介绍 `expose-allocation` pass。

  要处理嵌套的 allocation，
  也就是嵌套的 `(vector ...)` expression。

  - 但是这不是在 `remove-complex-operands` 中处理过了吗？

    我知道了，因为这个 `expose-allocation` pass 需要被放在
    `remove-complex-operands` 之前。

    也就是说重复了后面 pass 所作的工作，
    这可能是一些线索，告诉我们应该把 `(vector ...)`
    实现成 runtime 中的函数，
    而不是在 comptime 处理。

  另外这个 pass 要把 `(vector ...)` expression
  都翻译成更底层的操作 GC 的 expression。

  - 我认为这些完全可以在 runtime 中实现，
    而不用编译器来实现，GC 的实现方式应该尽量少地暴露给编译器，
    最好是完全不暴露给编译器。

    看到这里 compile 出来的代码是带有 if 的，
    就已经有问题了，因为这与运行时处理每什么差别。

  - 我想老师可能是知道，
    这里 runtime 和 comptime 的 trade-off
    已经偏向 runtime 了。

    但是在教学中可能不想让学生碰 runtime 代码。
    或者是在教学中给学生一些 comptime 的挑战。

- 介绍如何修改后续 passes。

- 老师说，由于加了 elaboration 的类型信息，
  他差点给每个 expression 都加上了 info field，
  来保存 metadata。

  这里可以看到老师在做很多设计决策，
  为什么没有直接继承 IU 已有的课程中的决策？

- 我不想再无效 refactor 了。
  可能要看到「动态类型」的章节再上手继续实现。

- 这节课没有讲完后续的 passes 如何修改。

# 2020-10-06

[2025-10-18]

- 这节课是 conditional 部分的 code review。

- review `rco-atom` 的 if case 时候，
  再次遇到了结果导向的结构递归问题。

- 感觉我可以总结一下目前学到的知识，
  看看对于编译 x-lisp 而言，还差什么知识。
  感觉很多难点都已经过了。

  - TODO 像总结解释器和类型检查器的实现知识一样，
    总结编译器的实现知识。

  并且如果有了 frontend 和 backend 的分层的话，
  整个项目的复杂度还能再度降低。

- TODO

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
