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

  注意，这里将会有多个 block share 同一个 interference-graph 的情况，
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

  - 还欠缺的知识：

    - 如何编译 closure。

  - TODO 像总结解释器和类型检查器的实现知识一样，
    总结编译器的实现知识。

    - 首先关键词是 unnest，要去掉各种嵌套的东西：

      - 函数作用参数位置的嵌套
      - let lhs 位置嵌套 let
      - if condition 位置嵌套 if

      unnest 的过程非常像是我之前编译到 forth 的过程，
      但是感觉比 SSA 要简单一些。
      那么 forth 是否可以作为 SSA 之前的一个中间语言呢？

      注意，unnest if 的过程需要 control flow graph。

      forth 的 control 已经是 explicit 的了，
      比如 tail-call 就真的在 tail 的位置。

    - 如果增加 forth 之后，
      可能也需要在 frontend 和 backend 之间增加一个阶段。

      也就是说，frontend 和 backend 其实是语言所切分出来的阶段，
      每增加一个语言，都会多分出来一个阶段。

    - 另外一个关键的点是 basic block 所形成的 control flow graph，
      这很适合 codegen。

    - 还有就 SSA，这 append-only 的数据结构适合优化。

  并且如果有了 frontend 和 backend 的分层的话，
  整个项目的复杂度还能再度降低。

[2025-10-19]

- 下面 review `explicate-control`。

  老师认为这是目前遇到的最难的一个 pass。
  老师觉得难，主要是因为 CPS。

  主要是新增的 if expression 之后：

  - `explicate-tail` -- 要增加 if case。
  - `explicate-assign` -- 要增加 if case。
  - `explicate-if` -- 主要处理 if case 的同时，还要处理 nested let。

  也就是说新增一个 expression，
  需要修改的代码的位置是 n。

  也许用 rewrite system 可以更好地处理这个问题。

  可能也不行，应为就算在 rewrite system 中不用修改已有的递归函数，
  新增的 case 数量还是 n。

- 这里有的学生的代码因为没有遵循 CPS，
  不是传递 continuation 而是传递了 label。

  这使得代码变得难以理解并且是错误的。

  这有点像批作业，
  但是修改有很多地方都是错误的代码，
  是很难的。

- 下面 review 对 jump 的优化。

  思路类似 unification 算法中的 walk 函数，
  对于一个 trivial block，
  walk 直到 non-trivial block。

  老师的实现方式是，
  先构造一个 hash table，
  保存 walk 函数的结果。

- 注意，如果区分了 frontend 和 backend，
  那么这个优化应该属于 backend。

- 下面 review `select-instructions`。

- 后面的几个 passes 没有讲，留给学生自己看了。

# 2020-10-08

[2025-10-19]

- 这节课继续介绍 GC。

- 注意，这里的 GC 是可以支持没有 runtime tag 的静态类型语言。

  并且后续的动态类型语言实现中，
  动态类型部分要和静态类型部分混用，
  这对于优化来说非常重要。

  所以这种实现 GC 的方式是很重要的知识。

- 介绍类型检查和 `HasType`。
  将会在后面用类型信息保证类型为 vector 的变量不会被分配到寄存器，
  而是被分配到 stack。

  这是我觉得有些没法接受的。
  一个解决方案是使用精确垃圾回收（precise garbage collection）。
  意思是编译出来函数的同时，
  保留一些函数的 metadata，
  记录那些寄存器和局部变量的类型是 pointer。

- 介绍 `(vector ...)` 的 `expose-allocation` pass。

  给出一个具体的例子，
  展示生成出来的新表达式如何被后续的 pass 处理。

  我认为 vector 不应该被编译，
  而应该放在 runtime 中实现。

  除了这一点我要修改之外，
  其他部分的实现都可以暂时跟着教程走。

- 这里有个 `uncover-locals` pass 用来收集变量的数据类型。

  目前我的实现中是 `check-c-program` 在做这件事，有点乱了。

- 介绍 `select-instructions` pass。

  为了编译 `(vector-set! vector n value)`，
  又保留了一个临时寄存器 `r11`。
  因为这里生成的 instruction，
  可能需要被 `patch-instructions` pass 处理。

  为了编译 `allocate`，
  用到了 pc（program counter）relative addressing -- `free_ptr(%rip)`。
  global variable 和 function 都可以用这种 addressing。

- 介绍 `allocate-registers` pass。

  这里需要处理保存 vector 的变量，
  如果在调用 GC 的时候，
  某个保存 vector 的变量是 live 的，
  那么这个变量就不能被分配到寄存器，
  而是必须用 root stack。

- 提醒写 runtime c 代码的时候，
  初始化的内存要清零。

- 介绍这一章的三个挑战：

  - 实现类似 racket 的 struct。
    但是 getter 和 setter 都是函数，
    所以这个功能应该在函数之后再实现。

  - 实现 array。

  - 实现 generational GC。

- 介绍 generational GC。

  heuristic 是：新 allocated 数据经常会被马上 free。

  活过了几次 copy 的数据，
  会被复制到一个专门为 old generation 准备的 from space。

  old generation 不必经常 copy。

  注意，当 old space 和 new space 的数据之间有相互引用的时候，
  情况会非常复杂。

# 2020-10-13

[2025-10-28]

- 这节课讲如何编译函数。
  首先是不带 closure 的函数。

- 有学生问，我们今天讲的函数是否支持递归，
  其实编译到 x86 之后，x86 的 label 易经支持相互递归了。

- 老师在解释器中实现 top-level 函数相互递归的方式太糟糕了，
  要在 pass 两次不说，还要在 lambda 的 env 上做副作用。

  还介绍说这种技术叫做 back patching。

  正确的方式是让 lambda 的 closure 在 env 之外还带有 module（mod）。
  在处理 top-level 的时候对当前 module 做副作用。

- 编译函数的时候需要用到 PC-relative addressing：

  ```asm
  leaq add1(%rip), %rbx
  callq *%rbx
  ```

  但是既然已经知道是 `add1` 这个 label 了，
  为什么不直接 call？这不是和 call `read` 一样吗？

- TODO 可能在编译 x-lisp 到 basic-lisp 的时候，
  basic-lisp 除了 `call` 之外还需要 `indirect-call`。

  因为目前 `call` 的第一个参数只能是全局函数。

- 这里说为了 tail-call，当参数个数超过 6 时，
  最后一个传递参数的寄存器应该用 tuple 保存多余的参数，
  而不是将多余的参数保存在 stack 上。

  这就是说 tail-call 将会用到 GC。

- 老师提到在早期就要用一个 pass `limit-function`，
  来处理超过 6 个的参数问题。

  我认为这最好可以在中间语言 basic-lisp 中处理，
  因为不同构架参数个数限制不同。

- 再次介绍函数调时 stack 中 frame 的情况。
  注意，除了 x86 的 call stack，
  还需要处理我们的 root stack。

- 关于寄存器，我们已经保证了所有 call-live variables
  （在一个 call 指令时 live 的 variables），
  不会被分配到 caller-saved register。
  因此在使用 call 的时候，不用再保存寄存器。

  在函数的 prolog，还是需要保存所有用到了的 callee-saved registers。

- 关于 tail-call。

  由于所有的参数都保存在寄存器中，
  所以准备好 tail-call 的参数之后，
  可以放心 pop return stack。

  注意，pop return stack 之前是需要 function epilog 的。
  这里三个操作具体的顺序是什么？

  好像只有可能是：

  - 准备 arguments。
  - 执行 epilog 中的部分代码，但是不要 ret。
  - jmp。

  为什么需要 epilog，然后再 jump 到函数 label，
  而不是直接 jump 到函数的 body label？

  因为 tail-call 的可能不是这个函数，而是另一个函数！
  tail-call 这个函数自己的时候，确实可以直接 jump 到 body label。

# 2020-10-15

[2025-10-29]

- 这节课介绍为了编译 function 需要修改哪些 pass。

- 有同学问 `HasType` 应该放在 assignment statement 的什么地方，
  老师回答应该放在 rhs，并且这就是 `HasType` 的目的，
  放在 rhs 之后就可以实现 `uncover-locals`。

  注意，书里没有出现 `uncover-locals`。

- 首先是 `shrink` 要把 top-level expression 放到 `main` 函数中。

- 新的 `reveal-functions` pass，
  在编译时就把所有到函数的引用，
  与对局部变量的引用区分开。

  `(Var f) => (FunRef f)`

  注意，只需要对 top-level function 做这个处理。

  考虑语言的完整性，
  这可能也意味着我们需要给 value 添加 function 类型
  -- `function-t`。

- 老师在这里介绍了自己实现的对函数定义的类型检查。
  可以发现需要两个 stage，那么还不如把 `claim` 和 `define` 区分开。

- 下面介绍 `limit-functions` pass。

  就是当参数多余 6 个时，
  把第 6 个和多余的参数放到 tuple 里。

  对于 x-lisp 来说，这个 pass 完全可以放在 basic-lisp 这个 backend 里。

- 对 `remove-complex-operands` pass 的影响。

  `FunRef` 和 `Apply` 都算是 complex operand。

  `Apply` 的参数必须都是 atom-exp。

  为什么 `FunRef` 需要 unnest 呢？
  因为对于每个 `FunRef`，我们都会用 lea 计算出地址，
  然后把函数地址保存到 variable 中。

  但是这是必要的吗？`FunRef` 已经是地址了。

  unnest operand 的原则是什么？
  operand 必须是 x86 的那三种 rand。

- `explicate-control` pass。

  每个 context 下都要增加 `FunRef` 而 `Apply` 的 case。
  其中 tail context 中 `Apply` 要翻译成 tail-call。

- 由于在 program 和 block 之间新增了 funtion 一层，
  所以结构递归的开头要新增一个 helper funtion 来处理，
  听起来也不是很难修改。

- 介绍 `select-instructions` pass。

  这里由于所有的 `FunRef` 被当作 complex operand 提取出去了，
  所以被提取出去的 assignment 被翻译成了 `lea` 来计算函数地址，
  call 被翻译成了 indirect-call。

  TODO 实现的时候可以试试对于已知的 FunRef 用 direct-call。

  翻译 tall-call 的时候使用了 tail-jump 这个 pseudo instruction，
  因为寄存器分配之后，才能知道如何做 epilog。

  还要处理函数定义中 parameter 到参数寄存器的映射，
  就是在函数的开头，把通过寄存器传递的参数，
  保存到 parameter 变量中。

  - 注意，这些指令是要放在函数的 entry block 前面的，
    而不是 prolog 前面。

  - move biasing 那个优化，
    可以在分配寄存器的时候优先考虑 move，
    因此优化掉上面的 move。

  TODO 我有一个问题是 `allocate-registers`
  可否放在 `select-instructions` 之前，
  也就是放在中间语言中来做。
  因为对于中间语言，我们有丰富的 control flow graph 处理工具，
  而对于 x86 的 block，我们不想重复实现这些工具。

  这样 `select-instructions` 可以变成所有 pass 的最后一步。
  保持这个是最简单的一步，才能方便我们 port 到其他构架。

- 关于 `build-interference` pass。

  之前编译 tuple 的时候，
  为了能让 `collect` 找到所有的 root。
  必须让所有保存了 tuple 的 variables
  与所有的 callee-saved registers interference。
  就是说不要把 tuple 保存到寄存器中，
  总是在 root stack 中找到 tuple。

  这里老师提到，既然所有的函数都有可能调用 `collect`，
  那么所有的函数都应该导致上面的 interference。

  这其实预示着老师的方案是有问题的，
  因为这个 interference 会传递到所有的函数。

  实际上具体会出现的情况是，
  在调用 `collect` 时，
  caller-saved registers 已经被保存了，
  但是有一些 callee-saved registers，
  可能是 `collect` 没有用到的，
  因此不会被 `collect` 保存，
  但是这些寄存器里保存着 caller 的数据，
  这些数据可能是 tuple。
  因此只要在 `collect` 之前保存所有寄存器就可以了。

- 我突然感觉这种渐进式的编译器教学有点折磨，
  每个修改都要修改一串 pass。

  但是这可能代表了实际开发时增加 feature 的过程。

  增加 feature 是如此，
  但是初始开发的时候并非如此，
  初始开发的时候既然知道了所有的信息，
  就要避免无效的 refactoring。

- 关于 `patch-instructions` pass。

  新增的限制是：

  - `leap` 的 `dest` 必须 register。
  - `tail-jmp` 所用的寄存器因该是 `rax`。
    或者说用 `rax` 是安全的。

- 下面介绍 `print-x86` pass。

  新增的情况是：

  - `(FunRef label) => label(%rip)`
  - `(IndirectCall arg) => call *arg`
  - `(TailJmp rax)` 的处理非常复杂，老师在这里处理了。
    我认为应该放在 `patch-instructions` 中处理，
    或者放在 `prolog-and-epilog` 中处理，
    因为是和 `epilog` 有关的。
    或者独立一个 pass。

# 2020-10-20

[2025-10-30]

- 这节课前半部分讲一个编译函数的例子。

- TODO 可以实现完 function 再来看。

[2025-10-30]

- 最后十分钟，讲后面要实现的 lambda。

  只是对 lambda 的基本介绍，
  因为有的同学可能还不知道 lambda。

  我想到了我开始学 lambda 的时候，
  还不会写 lambda 演算的解释器的时候，
  这还是非常难的概念。

# 2020-10-22

[2025-10-30]

- 这节课讲 lambda 的 closure conversion。
  这是这一整个课程中最重要的技巧。
  编译器的高光时刻。

- 介绍如何用 closure 实现 lambda 的解释器。

  介绍了如果不用 closure，每就会形成 dynamic scope。

  dynamic scope 很容易在解释器中实现，
  只需要在 apply 的时候 extend 当前的 env，
  而不是 closure 中的 env，
  就可以了。

  但是，在编译器中，如何实现 dynamic scope？
  首先要用特殊的变量语法，
  比如 scheme 的 `make-parameter` 和 `parameterize`。
  好像给每个 parameter 一个 stack，就可以实现。
  如果是多线程的 lisp，
  可以设计成每个 thread 对于每个 parameter 都有一个 stack。

  老师说 dynamic scope 很难用，
  其实是大部分时候应该用 lexical scope，
  但是 dynamic scope 在很多情况下很好用，
  比如函数的 options，可以直接用 dynamic scope 实现。
  典型的例子是 the little learner 中用来实现机器学习中的参数。

  另外，假设我要实现一个 system-lisp，其中有多种 allocator，
  并且要求每次 allocation 都要指明使用哪个 allocator，
  这时 allocator 就可以被实现为 dynamic scope 的变量。
  这将会非常好用！

  老师在谈 dynamic scope 的应用时，
  举例是类似 canvas 的画图程序，
  这就是有大量的 options 的程序的例子。
  dynamic scope 可以让我们灵活地临时调整这些参数。

- 老师又讲了如何在解释器里处理相互递归函数，
  并且提到了 Y-combinator。

  老师说，课程所实现的静态类型语言没法表达 Y-combinator，
  因为类型检查不过（因为没有 polymorphic type）。
  但是动态类型的版本可以。

- 在看 lambda 的类型检查时，
  老师在这里说他修改了类型检查器的实现，
  不用在所有的地方都增加 `HasType` 了。

  我觉得这样反而搞乱了 elaboration 这个步骤。

  老师讲了他也不知道如何做 dynamic scope 的 type checking，
  但是如果是使用 `make-parameter` 和 `parameterize` 来实现 dynamic scope，
  用类似检查全局变量的方式就可以了。

- 下面介绍 free variable，
  lambda conversion 依赖于，
  找到 lambda 的所有的 free variables。

  free variable 其实是用 bound variable 的否定来定义的。

- 介绍在 runtime 中实现 closure 的方式。

  这种实现 closure 的方式，称作 "flat-closure"。
  老师说这是 luca cardelli 发明的。
  kent 也声称自己独立发明了这个 idea。

  可以看出，GC 是 lambda 的前提，好像没法避免。
  因为 lambda 也可以通过副作用形成对自身的循环引用，
  也就是 lambda 所编译成的 tuple 到自身的循环引用。

  除非是没有副作用的语言，
  因为没有副作用的语言可以用引用计数来做自动内存管理。

- 这里有学生问嵌套的 lambda 如何处理。
  老师给出 K combinator 的例子。

- 这里把 lambda 编译成 top-level 的方案 A 是：
  让 function 的第一个参数是 flat-closure 自身，
  然后用 let 把参数从 flat-closure 中取出来。

  有点像是 object 的 method 的 self（或者说 this）参数。

  另外一种方案 B 是，lambda 所编译成的 top-level function，
  带有所有 free variable 为参数，
  在 apply flat-closure 的时候，
  先准备好对参数，再调用 function。

  - 第二种方案可能更有利于寄存器分配，
    因为少了一个用来保存 flat-closure 自身的变量。

  - 第二种方案其实很复杂，
    因为为函数准备参数，需要从 vector 中取值出来，
    这需要在每个 apply exp 的位置生成 let。

    - 方案 A 也需要在 apply exp 的位置生成 let，
      但是比较简单。

  如果使用方案 A，
  也许可以约定 lambda 内有 self 这个变量，
  或者 `recur` 这个变量（因为 self 在 system-lisp 中有他用）。
  这样可以方便 lambda 递归调用自身。

- 我还是选择方案 B，
  因为方案 B 所生成的代码，
  类似于 refactor 过程中的人工手动生成的代码。

# 2020-10-27

[2025-10-30]

- 这节课 code review 之前实现的 tuple。

- TODO

# 2020-10-29

[2025-10-30]

- 这节课讲为了实现 lambda 需要哪些 pass。

  书中有两个 pass `convert-assignments` 和 `convert-to-closures`，
  前者是处理 flat-closure 对 `set!` 的影响的。

  - 我认为这又是一个我们不应该支持 `set!` 的证据。

  这里我们只需要关心 `convert-to-closures`。
  在课程中这个 pass 叫做 `closure-conversion`。

- 介绍 `closure-conversion` pass。

  重点是这个 pass 在 `reveal-functions` 之后。

  具体实现方式其实很简单。
  就是把 lambda exp 翻译成 literal vector exp，
  然后增加一个函数到 top-level，
  注意，增加到 top-level 的函数需要被递归处理，
  因为里面可能还有 lambda。

  注意，需要在这个翻译过程中保持类型。
  也就是说，收集 free variable 的函数，还需要收集类型。

- 在 x-lisp 实现中，我需要把 lambda exp 翻译成 curried exp，
  注意，我还需要知道 curried exp 的 arity。

  curried exp 将会被求值为 curried value，
  它有自己的 API，而不是 vector。

- [2025-11-02] 不对。
  因为 flat-closure 的自我引用是在 apply 的时候才形成的，
  所以不能用 curried 来代替 flat-closure。

  老师的方案不需要在运行时判断 apply，
  是正常的函数也被处理为 closure 了。

- 注意，`closure-conversion` 在解释器中也很有用，
  因为把 lambda 翻译成 top-level function 的过程，
  会给每个 lambda 一个名字，
  这能够帮助 error report。

- 下面介绍一个具体的的例子。

  TODO 我可以等实现到这里的时候再看。

# 2020-11-03

[2025-10-19]

- 这节课讲动态类型语言的编译器设计。

  x-lisp 是动态类型语言，
  所以我跳过来看一下。

- 再次介绍动态语言与静态语言的差异。

- 通过 `(inject exp type)` 和 `(project exp type)` operator，
  在 value 和 tagged value 和之间转换。

  这样是不是可以做到运行时的 C 代码，
  不用知道我们对 value 的 runtime encoding 是什么？

  看起来有机会在很多地方都直接用 value。

  如果是这样的话，
  那么作为中间语言的 basic-lisp
  中的 value 就应该不带 tag 了。

  但是这种带有 tag 的 value
  和不带 tag 的 value 共存的设计，
  还不一定对。

- 介绍如何用 lower 3-bits 来做 tag。

- 介绍我们会保留之前的实现，然后新增一个 `any` type。

  但是这样也不太对，
  因为如果能让所有的 value 都带有 tag，
  就可以简化 GC。

  但是如果保留了之前的实现，
  GC 在增加 any 后就更复杂了。

  但是明显地表达 inject 和 project，
  就有机会在某些地方把它们优化掉。
  如果真的有好的方法做这种优化，
  那么混合两种 value 可能就是合理的。

  也许是的！
  应该可以把大量的 inject 和 project 连用的地方优化掉。

  ```scheme
  (+ e1
     (+ e2
        e3))
  =>
  (inject
   (+ (project e1 Integer)
      (project (inject
                (+ (project e2 Integer)
                   (project e3 Integer))
                Integer) Integer))
   Integer)
  ```

  显然可以被优化成：

  ```scheme
  (inject
   (+ (project e1 Integer)
      (+ (project e2 Integer)
         (project e3 Integer)))
   Integer)
  ```

  带有 tag 的 value 与不带 tag 的 value 混用，
  并且在编译时处理动态类型的支持，是我没想到的。

  但是这样也许还是不对的，
  因为如果在实现 GC 的时候需要维护 shadow stack 来保存 GC 的 roots。
  那么优化掉的 inject 和 project，
  还不如维护这个 shadow stack 的开销大。
  毕竟大部分 inject 和 project 只是寄存器上的操作，
  而 shadow stack 是内存。

  并且按照现在的 GC 实现来看，
  好像不能把 reference value 保存在寄存器中。
  这也不太对。

- 介绍类型检查器。

  有了动态类型为什么还要类型检查？
  可能与 Gradual Typing 有关。

- 介绍解释器。

- 介绍 tag 对 value 的影响。
  尤其是对 pointer 的影响。

- 介绍 `shrink` pass。

  - `project` 被翻译成底层 API 了，并且用到了 if expression。
  - `inject` 也被翻译成底层 API 了。

- TODO 剩下的实现到这里再看。

# 2020-11-05
# 2020-11-10
# 2020-11-12
# 2020-11-17

[2025-10-28]

- 这节课讲 data-flow 分析，为下一节课编译 while loop 做准备。

- TODO

# 2020-11-19
# 2020-12-01
# 2020-12-08
# 2020-12-10
