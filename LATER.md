# actor-lisp

actor model 最适合写的就是 user login，
然后 user 访问资源，修改资源。

各种 login 和 register 机制，
可以直接集成到语言的标准库中。

# scheme

sussamn/1987-lisp-a-language-for-stratified-design.pdf

# tael

使用 x-lisp 和 tael 的方案：

- 像 vue 一样，将 x-lisp 嵌入 xml。

  - 用 x-if x-for 等等。
  - 用 `@(...)` 代替  `{{ ... }}` -- 因为 lisp 代码已经带有 `()` 了。
    - 或用 `$(...)` -- 模仿 bash。

  模仿 vue 的 template，
  但是语义上完全模仿 php 的 server side rendering。

  也可以模仿 alpine.js 的设计，而不是 vue，
  因为其对 template 的理解更具有一致性。
  比如 x-if 需要 template，x-show 不需要。
  只支持 x-if 而不支持 x-else -- 因为这需要把 xml 当作语法来解析。

  或者不知模仿 alpine.js，而是模仿 php 的 SSR，
  和 alpine.js 的前端配合。
  SSR 可以只是传数据给 alpine.js template，
  而不需要任何 if 和 for 的 control flow。

- 如果直接用 tael 生成 xml，
  那么 markup language 和 template language 就都是 lisp。
  就像 @quasiquote 和 @unquote 一样，
  也像 @pattern 和 @escape 一样，
  具有一致性。

- 自己设计 pixel 风格的浏览器，直接以 tael 为 markup language。
  可以写 uxn 风格的 app，比如 flash card app。
  markup language 可以直接支持类似现代 web 前端框架中的 component。

- 比浏览器更低一级的 API 就是 2D game engine 的 API。
  此时 x-lisp 可以是 game engine 的脚本语言，
  类似 lua 之存在。

# richard-bird

algebra-of-programming.djvu
algorithm-design-with-haskell.pdf
introduction-to-functional-programming.djvu
introduction-to-functional-programming.pdf
pearls-of-functional-algorithm-design.pdf
the-algebra-of-programming.pdf
thinking-functionally-with-haskell.pdf

# basic-lisp.c

create basic-lisp.c project

[diray] 2025-10-06 the plan for self-reliance

开启一个 basic-lisp.c 项目，来帮助 x-lisp 走向独立自主。

在印第安纳大学的编译器课程的基础上，
又发现了康奈尔大学关于编译器优化的课程，
我感觉编译器的实现又简单了一个数量级。
因为这个课程中所设计的中间语言非常简洁且通用。

其简洁性使得扩展这个中间语言的方式只有增加 data type 和 operator。
正如扩展 lisp 的方式是 overload application。
这种局限使得设计方向变得明确。

目前 x-lisp 的实现计划：

- 用 js 写 x-lisp 的解释器 x-lisp.js。
- 用 x-lisp.js 写 x-lisp 的编译器，编译到 x86。

改进后 x-lisp 的实现计划：

- 用 js 写 x-lisp 的解释器 x-lisp.js 和编译器，编译器到 basic-lisp。
- basic-lisp 代码可以暂时由 basic-lisp.c 解释运行。
- 用 x-lisp 写 x-lisp 的编译器，编译到 basic-lisp。

这样的好处是：

- 我们可以尽早脱离 js 的 runtime，新的 runtime 是 basic-lisp.c。
- 用 x-lisp 写自身的编译器时，就是简单的 port 已有的用 js 写的编译器代码。

# ruby

https://sinatrarb.com/
https://www.learnenough.com/courses

# graph theory

richard trudeau

- the non-euclidean revolution
- introduction to graph theory

# modelling

《模型思维》（The Model Thinker - Scott E. Page）
《计算思维》（Jeannette M. Wing）
《设计数据密集型应用》（Designing Data-Intensive Applications - Martin Kleppmann）

# compiler

persons/nils-m-holm -- https://t3x.org/index.html

# programmer's life as a game

读书如同 roguelike，可以做笔记，
但是可能要不断重读，每次读都有不同的收获。

程序员本身就像是 RPG 角色：

| RPG  | programming                        |
|------|------------------------------------|
| 智力 | 函数式编程                         |
|      | combinator，monad                  |
| 力量 | C 和汇编                           |
| 敏捷 | 让程序员能快速相应变化的知识       |
| 体质 | 让程序员更有信心，增加容错率的知识 |
|      | refactoring, design pattern, TDD   |

项目如同大关卡，只有完成项目击败最后的 boss 才算完成。

# partial evaluation

topics/type-theory/partial-evaluation/partial-evaluation-and-automatic-program-generation.pdf

# hardware design

> learn hardware design language -- where everything runs in parallel

# functional programming

persons/scott-wlaschin/2018-domain-modeling-made-functional.pdf
persons/alexander-granin/2025-pragmatic-type-level-design.pdf
persons/alexander-granin/2025-functional-design-and-architecture.pdf
langs/haskell/readings/2016-haskell-programming-from-first-principles.pdf

# 通过写作来学习

- 关于可计算性与集合论

  - 主要就是关于一个集合的基础等词的讨论，
    可以以程序语言中的问题来作为例子：
    - lambda term 的等价性
    - 带有递归的类型之间的等价性
    - 带有递归的 lambda term 的等价性

  - 模仿 ray 的 blue book，因为这和形式系统有关。
    或者说形式系统就是我想要讨论的主题。
    或者说形式系统就是我想要要讨论的集合论的重要应用领域。

- 如果实现了 occam-lisp 到自己的编译器，
  使得 occam-lisp 成为了实用的语言。

  我就可以：

  - 用 occam-lisp 来写书介绍 functional programming。
  - 用 occam-lisp 来写后端服务，甚至用 occam-lisp 来写 htmx web app。

    也许可以学 scott-wlaschin/2018-domain-modeling-made-functional.pdf 来写后端

  总的来说，如果实现了 occam-lisp，
  可以让我用初学者的心态去学习之前没有学好的东西。
  因为我把「使用自己的语言」当成了很多学习的前提。

# occam-lisp

learn API design from fp library in js

- https://ramdajs.com/docs
- https://lodash.com
- https://underscorejs.org/docs/underscore-esm.html

learn API design from clojure

# meta-lisp.js

> this is where we can test luca's idea of subtype of recursive types.

> read papers about LCF

persons/robin-milner/1984-the-use-of-machines-to-assist-in-rigorous-proof
persons/robin-milner/1972-logic-for-computable-functions-description-of-a-machine-implementation
persons/robin-milner/1976-models-of-lcf
persons/robin-milner/1978-a-metalanguage-for-interactive-proof-in-lcf
persons/robin-milner/1979-edinburgh-lcf--a-mechanised-logic-of-computation
persons/robin-milner/1979-edinburgh-lcf--a-mechanised-logic-of-computation

# luca cardelli

> luca cardelli's work is very close to my research interests in meta-lisp.

persons/luca-cardelli/1985-on-understanding-types-data-abstraction-and-polymorphism
persons/luca-cardelli/1987-basic-polymorphic-typechecking
persons/luca-cardelli/1989-typeful-programming
persons/luca-cardelli/1991-operations-on-records
persons/luca-cardelli/1994-an-extension-of-system-f-with-subtyping
persons/luca-cardelli/1996-a-theory-of-objects

persons/david-macqueen/1984-an-ideal-model-for-recursive-polymorphic-types.md

- how to view type is ideal of domain

# hindley-milner

univalent analysis of relation -- wikiepdia page of HM

# lazy evaluation and dan

read Dan's language-and-programmer.pdf

- call/cc vs let-cc
  let-cc is better than call/cc, why?
  maybe because it removed one level of indirect.

https://en.wikipedia.org/wiki/Lazy_evaluation
https://en.wikipedia.org/wiki/Daniel_P._Friedman

# lambda diagrams

it is important to have many ways to imagine lambda calculus.

https://tromp.github.io/cl/diagrams.html

# evaluation strategy

read henk-barendregt again to study evaluation strategy

persons/henk-barendregt/1984-the-lambda-calculus.pdf
persons/henk-barendregt/2010-lambda-calculus-with-types.pdf

# tree calculus

see if it is related to tree automata

topics/computer-science/tree-calculus/2021-reflective-programs-in-tree-calculus--barry-jay.pdf
webside: https://treecalcul.us

# tree automata

study/topics/computer-science/automata/tree-automata/2008-tree-automata-techniques-and-applications.pdf
study/topics/computer-science/automata/tree-automata/2015-tree-automata--ferenc-gécseg--magnus-steinby.pdf

study/topics/computer-science/automata/tree-automata/1992-tree-automata-and-languages.pdf
study/topics/computer-science/automata/tree-automata/2010-foundations-of-xml-processing--the-tree-automata-approach.pdf

# lattice theory

type system as lattice

persons/george-grätzer/1978-general-lattice-theory.md

computational analysis of formal system -- logic of finite observations

# random

[DHH: Future of Programming, AI, Ruby on Rails, Productivity & Parenting | Lex Fridman Podcast #474](https://www.youtube.com/watch?v=vagyIcmIGOQ)

persons/david-heinemeier-hansson/2006-getting-real.pdf
persons/david-heinemeier-hansson/2007-restful-web-services.pdf
persons/david-heinemeier-hansson/2018-it-doesnt-have-to-be-crazy-at-work.epub
persons/david-heinemeier-hansson/2018-it-doesnt-have-to-be-crazy-at-work.pdf

2008-the-seven-virtues-of-simple-type-theory.pdf

# combinatory-logic

> combinatory logic and dependent type theory

using combinatory logic as the domain of language with dependent type:
- a type (set) is a predicate-like element
- only need one judgement `|-`
- all bound variable can be compiled out: lambda, pi, sigma
- infinite sum and product can be handled by high order functions:
  - first argument is a type, second argument is a function

persons/j-roger-hindley/1997-basic-simple-type-theory
persons/j-roger-hindley/2008-lambda-calculus-and-combinators--an-introduction
persons/haskell-curry/1951-outlines-of-a-formalist-philosophy-of-mathematics
persons/haskell-curry/1963-foundations-of-mathematical-logic
persons/haskell-curry/1956-combinatory-logic--volume-1
persons/haskell-curry/1970-combinatory-logic--volume-2

> dependent type and combinatory logic

maybe dependent type is more easy to develop in combinatory logic.

> the S of combinatory logic is much like the dup of interaction nets

# foundation of mathematics

learn from hilbert

- my understanding of the logic of mathematics is shaped by type theory now,
  maybe i should learn the other way.

# recursion theory

study recursion theory -- to understand kleene's mu operator

- https://en.wikipedia.org/wiki/mu_operator
- https://en.wikipedia.org/wiki/Computability_theory
- https://plato.stanford.edu/entries/recursive-functions/

# denotational semantics

persons/joseph-e-stoy/1977-denotational-semantics--the-scott-strachey-approach-to-programming-language-theory

# algebraic-subtyping

topics/type-theory/2016-algebraic-subtyping--tephen-dolan.pdf

# inet and propagator

persons/robin-milner/2009-the-space-and-motion-of-communicating-agents

# cicada

persons/peirce/1880-on-the-algebra-of-logic -- early denotational semantics for logic

# kan complex

2011-simplicial-structures-in-topology--davide-l-ferrario--renzo-a-piccinini.pdf
2023-an-elementary-illustrated-introduction-to-simplicial-sets--greg-friedman.pdf
2023-higher-categories-and-homotopical-algebra--denis-charles-cisinski.pdf

# constraint processing

结合所实现的逻辑式语言，读 "constraint processing"

# pattern

读 Christopher Alexander 的书，思考知识被固定的方式

# cell-complex

回顾 cell-complex + cobordism + lisp 的项目 -- 实现几何引擎

# study

topics/computer-science/interaction-nets/the-optimal-implementation-of-functional-programming-languages--andrea-asperti.pdf
projects/others/HigherOrderCO/HVM/paper/HVM2.pdf
[maybe] persons/jean-yves-girard/proof-nets--the-parallel-syntax-for-proof-theory--1995.pdf

2017-arithmetic.pdf -- what is computation
2012-measurement.pdf
2009-the-existential-graphs-of-charles-s-peirce--don-d-roberts.djvu
2006-proof-nets-and-the-identity-of-proofs--straßburger.pdf
2023-essentials-of-compilation-racket.pdf

# projects

propagator -- a language for propagator model -- to experiment with type checking
"essentials of compilation" -- learn about compilation -- using x-forth
learn pytorch and tinygrad -- tinygrad vs. little learner
readonly.link -- be like sourcehut

# category

[category] 范畴论的一个特点是不从最基础的具体公理开始构建理论，
而是在数学实践中，在证明定理的过程中，发现理论中的 pattern 再总结成公理。

# x-puzzle

[x-puzzle] expression-based puzzle games

- 把 to mock a mockingbird 做成一个游戏。
- 支持纯文字版本，因此需要 grammar

# fidb

Should NOT just use file as config, should use code.
i.e. should be a library instead of db app.

# mimor

受到 kimi 启发，重新设计 mimor 使得它的 UX 更简单。

- 在做新的 web app 之前首先要有一个稳定的数据层，
  回到 fidb 项目，这次不完全以 JSON 为 config
  来生成一个 database 的 HTTP API，
  而是用以 fidb 为 database library，
  在这之上手写 HTTP API。
