---
title: cs 6120 advanced compilers
subtitle: the self-guided online course
sources:
- "https://www.cs.cornell.edu/courses/cs6120/2020fa"
- "https://www.cs.cornell.edu/courses/cs6120/2025fa"
---

# info

老师：https://www.cs.cornell.edu/~asampson
课程：https://www.cs.cornell.edu/courses/cs6120/2020fa/self-guided

# lesson 1 -- welcome and overview

[ [Webpage](https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/1/) ]

[2025-10-04] 我第一次听说 Proebsting’s Law，
也就是说编译器优化对软件运行速度的提升，
远远小于硬件对软件运行速度的提升。

> **Proebsting's Law: Compiler Advances Double Computing Power Every
> 18 Years**
>
> I claim the following simple experiment supports this depressing
> claim. Run your favorite set of benchmarks with your favorite
> state-of-the-art optimizing compiler. Run the benchmarks both with
> and without optimizations enabled. The ratio of of those numbers
> represents the entirety of the contribution of compiler
> optimizations to speeding up those benchmarks. Let's assume that
> this ratio is about 4X for typical real-world applications, and
> let's further assume that compiler optimization work has been going
> on for about 36 years. These assumptions lead to the conclusion that
> compiler optimization advances double computing power every 18
> years. QED.
>
> This means that while hardware computing horsepower increases at
> roughly 60%/year, compiler optimizations contribute only
> 4%. Basically, compiler optimization work makes only marginal
> contributions.
>
> Perhaps this means Programming Language Research should be
> concentrating on something other than optimizations. Perhaps
> programmer productivity is a more fruitful arena.

source: https://proebsting.cs.arizona.edu/law.html
info: https://oleksii.shmalko.com/20211028115609/

老师说，但是因为摩尔定律正在减慢，
并且有很多新的特殊硬件，
所以编译器写手还是有市场的。

# lesson 2.1 -- representing programs

[ [Webpage](https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/2/) ]

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

# lesson 2.2 -- introduction to bril

[ [Webpage](https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/2/) ]

[2025-10-05]

- bril 是老师的 IR 项目：

  - https://capra.cs.cornell.edu/bril/
  - https://github.com/sampsyo/bril

  动机：

  - bril 是为教学而设计的，保持简单。
  - bril 可以随便扩展 operator 和 type。

- 直接用 JSON 作为 AST。

  具体语法：

  ```c
  @main {
    v0: int = const 1;
    v1: int = const 2;
    v2: int = add v0 v1;
    print v2;
    v3: ptr<int> = alloc v0;
    free v3;
  }
  ```

  翻译成 JSON AST：

  ```js
  {
    "functions": [{
      "name": "main",
      "instrs": [
        { "op": "const", "type": "int", "dest": "v0", "value": 1 },
        { "op": "const", "type": "int", "dest": "v1", "value": 2 },
        { "op": "add", "type": "int", "dest": "v2", "args": ["v0", "v1"] },
        { "op": "print", "args": ["v2"] },
        { "op": "alloc", "type": { "ptr" : "int" }, "dest": "v3", "args": ["v0"] },
        { "op": "free", "args": ["v3"] },
      ]
    }]
  }
  ```

  其实用 lisp 就可以兼顾具体语法和 JSON AST 的优点：

  ```scheme
  (define (main)
    (= v0 int (const 1))
    (= v1 int (const 2))
    (= v2 int (add v0 v1))
    (print v2)
    (= v3 (ptr int) (alloc v0))
    (free v3))
  ```

- 老师所用的很多工具都是 rust 系的，比如：

  - deno instead of node
  - uv instead of pip

  不同工具是用不同语言写的，想要跑起来，
  就要安装一些列 rust 系的工具，很麻烦。

  老师解释故意用很多语言写是为了证明 JSON AST 的普适性。
  但是这其实不用什么证明，大家都了解。

  正确的策略也许是用单一的语言写一套方便的工具，
  如果 bril 真 的有价值，
  别的开发者会根据需要用各自喜欢的语言来写新的工具的。

- 由于 bril 的 JSON AST 是描述 instruction list 的，
  所以这里老师展示如何用 python 把 instruction list 转化为 CFG。

  这里展示了 unix pipe 可以很方便地用来测试 JSON AST 的处理与转化程序。

  老师直接在课堂中现场写代码，这种教学方式也不错。
  重点也是学生需要暂停视频，先独立给出自己的实现。

  与 IU 的 homework + code review 的方式相比，
  现场写代码的方式只能用于视频课程，
  因为一般的课程没法暂停。

- 老师介绍了一个自己写的 snapshot testing 工具，
  叫 turnt：https://github.com/cucapra/turnt

  实际上按照我的经验，
  snapshot testing 用 unix pipe + git diff 就足够了，
  根本没必要再依赖额外的工具。

# lesson 6 -- llvm

[ [Webpage](https://www.cs.cornell.edu/courses/cs6120/2020fa/lesson/6/) ]

[2025-10-15]

这节课的内容是用 llvm 的 cpp api，
来写一个简单的 pass：

- 把所有的 binary operator 都改成 mul。

- 实现这个 pass 的方式不是修改已有的 binary instruction，
  而是创造一个修改了的新的 instruction 节点，
  然后找到所有用到了原来 binary instruction 结果的地方，
  把变量换成新的变量。

- 实现过程中展现了 SSA 哲学，下面三个东西被视为是等同的：

  - instruction
  - 所生成的结果
  - 保存结果的 variable

SSA 这么简单的东西，
被 llvm 包装成这么复杂的 OOP api，
这太荒谬了。

就像 clojure 经常说的那样，
应该用数据，而不应该用 OOP。

bril 就是数据驱动的例子。
