---
title: Essentials of Compilation
subtitle: An Incremental Approach in Racket
authors: Jeremy G. Siek
year: 2023
---

# 学习动机

动机首先来自 Indiana 大学的传统，
来自 Dan 介绍解释器的 EOPL。

其次我想模仿 uxn 写一个 vm，
但是与之以 byte 为 value 的设计不同，
我想要：

- 64-bits 的 word。
- tagged-value。
- garbage collection。

为了这个目标，可以从这本书里学：

- runtime tagged-value encoding
- garbage collection 实现方式

等等。

另外，这里的实现的语言是相对传统的 lambda 演算，
用的实现方式也是相对传统的 expression-based。
因此，还有一个学习动机，
就是看这里的传统知识可以给
propagator 和 inet 的实现带来什么启发。

[2025-05-14] 在 inet-lisp 和 inet-forth 中，
我已经尝试了 runtime tagged-value encoding，
但是我是编译到一个类似 forth 的 byte code interpreter，
现在想要学会如何编译到 native code 并且符合 C 的 calling convention。

读 pi calculus 时，
想要写解释器来试验里面层层递进所介绍的各种语言，
但是没有合适的语言来实现，
所以需要自己的动态类型的 lisp，
所以需要学如何写编译器来处理 closure 和 call/cc。

[2025-05-24] 也许更适合用来为 pi calculus 之类的语言写解释器的，
是 simply typed scheme，而不是 generic dispatching。

# Preface

> We take you on a journey through constructing your own compiler for
> a small but powerful language. Along the way we explain the
> essential concepts, algorithms, and data structures that underlie
> compilers.

继承了 Abdulaziz Ghuloum 的论文
"An Incremental Approach to Compiler Construction"。

> We develop your understanding of how programs are mapped onto
> computer hardware, which is helpful in reasoning about properties at
> the junction of hardware and software, such as execution time,
> software errors, and security vulnerabilities.

> For those interested in pursuing compiler construction as a career,
> our goal is to provide a stepping-stone to advanced topics such as
> just-in-time compilation, program analysis, and program
> optimization.

> For those interested in designing and implementing programming
> languages, we connect language design choices to their impact on the
> compiler and the generated code.

就是我了！

> A compiler is typically organized as a sequence of stages that
> progressively translate a program to the code that runs on
> hardware. We take this approach to the extreme by partitioning our
> compiler into a large number of _nanopasses_, each of which performs
> a single task. This enables the testing of each pass in isolation
> and focuses our attention, making the compiler far easier to
> understand.

还继承了 chez-scheme 的 nanopass。

<question>
What is a compiler?
<answer>
A compiler is typically organized as a sequence of stages that
progressively translate a program to the code that runs on
hardware.
</answer>
</question>

> The most familiar approach to describing compilers is to dedicate
> each chapter to one pass. The problem with that approach is that it
> obfuscates how language features motivate design choices in a
> compiler. We instead take an _incremental_ approach in which we build
> a complete compiler in each chapter, starting with a small input
> language that includes only arithmetic and variables. We add new
> language features in subsequent chapters, extending the compiler as
> necessary.

<question>
What is an incremental approach to build a compiler?
<answer>
In an incremental approach,
we build a complete compiler in each chapter,
starting with a small input language
that includes only arithmetic and variables.
We add new language features in subsequent chapters,
extending the compiler as necessary.
</answer>
</question>

# 1 Preliminaries

> The program-as-text representation is called _concrete syntax_.
> We use concrete syntax to concisely write down and talk about
> programs. Inside the compiler, we use abstract _syntax trees_ (ASTs)
> to represent programs in a way that efficiently supports the
> operations that the compiler needs to perform. The process of
> translating concrete syntax to abstract syntax is called _parsing_.

> This book does not cover the theory and implementation of parsing.

直接用 sexp。

> ASTs can be represented inside the compiler in many different ways,
> depending on the programming language used to write the compiler.
>
> - We use Racket’s struct feature to represent ASTs (section 1.1).
>
> - We use grammars to define the abstract syntax of programming
>   languages (section 1.2)
>
> - and pattern matching to inspect individual nodes in an AST
>   (section 1.3).
>
> - We use recursive functions to construct and deconstruct ASTs
>   (section 1.4).

## 1.1 Abstract Syntax Trees

> By using a tree to represent the program, we can easily follow the
> links to go from one part of a program to its subparts.

但是与 propagator model 相比，
这里的 tree 能也只能从 parent to children。

> We define a Racket `struct` for each kind of node. For this chapter
> we require just two kinds of nodes: one for integer constants (aka
> literals) and one for primitive operations. The following is the
> `struct` definition for integer constants.

在 Charles H. Moore 1970 年的文章
"Programming a problem-oriented-language" 中，
设计语言的起点也是 primitive data 和 primitive operations。

```racket
(struct Int (value))
```

> The following is the struct definition for primitive operations.

```racket
(struct Prim (op args))
```

> We have made a design choice regarding the Prim structure. Instead
> of using one structure for many different operations (`read`, `+`,
> and `-`), we could have instead defined a structure for each
> operation, as follows:

```racket
(struct Read ())
(struct Add (left right))
(struct Neg (value))
```

> The reason that we choose to use just one structure is that many
> parts of the compiler can use the same code for the different
> primitive operators, so we might as well just write that code once
> by using a single structure.

解释设计决策与 trade-off。

> We often write down the concrete syntax of a program even when we
> actually have in mind the AST, because the concrete syntax is more
> concise. We recommend that you always think of programs as abstract
> syntax trees.

## 1.2 Grammars

> A programming language can be thought of as a _set_ of programs.

这种 set theory 的观点承自 EOPL。

Jeremy 也有一个演讲是介绍这种观点的：
- "Crash Course on Notation in Programming Language Theory - λC 2018"
  - [Part 1](https://www.youtube.com/watch?v=vU3caZPtT2I)
  - [Part 2](https://www.youtube.com/watch?v=MhuK_aepu1Y)

一个 grammar 会定义一个集合。

定义一个集合在于：

- （1）可以判断元素是否属于这个集合；
- （2）一个基础等价关系，
  使得给出这个集合中的任意两个元素，
  可以判断它们是否相等。

而元素，可以理解为一个预先定义的
[Domain of discourse](https://en.wikipedia.org/wiki/Domain_of_discourse)
中的元素。例如：
- 所有 JSON 数据的集合。
- 所有 sexp 表达式的的集合。
等等。

> The set is infinite (that is, one can always create larger
> programs), so one cannot simply describe a language by listing all
> the programs in the language. Instead we write down a set of rules,
> a _context-free_ grammar, for building programs.

> Grammars are often used to define the concrete syntax of a language,
> but they can also be used to describe the abstract syntax.

> As an example, we describe a small language, named `LangInt` , that
> consists of integers and arithmetic operations.

我们没法用字体来区分 grammer 中的 terminal 与 nonterminal，
因此我们用 `<Exp>` 来表示 nonterminal。

```bnf
<Exp> ::= (Int <Int>)
<Int> ::= "An int is a sequence of decimals (0 to 9),
           possibly starting with – (for negative integers),"
<Exp> ::= (Prim 'read ())
<Exp> ::= (Prim '- (<Exp>))
<Exp> ::= (Prim '+ (<Exp> <Exp>))
<Exp> ::= (Prim '- (<Exp> <Exp>))
```

注意上面所使用的 quote，
这意味着在定义 grammer 时，
已经有 racket 这个 meta-language 了。

Example exp in `LangInt`：

```racket
(Prim '+ ((Prim 'read ()) (Prim '- ((Int 8)))))
```

上面对 quote 的用法有歧义，
如果想要成为 meta-language 中合法的表达式，
必须加上 `list`：

```racket
(Prim '+ (list (Prim 'read (list)) (Prim '- (list (Int 8)))))
```

> The last grammar rule for `LangInt` states that there is a `Program`
> node to mark the top of the whole program:

```bnf
<LangInt> ::= (Program '() <Exp>)
```

> The Program structure is defined as follows:

```racket
(struct Program (info body))
```

> where `body` is an expression. In further chapters, the `info` part
> is used to store auxiliary information, but for now it is just the
> empty list.

The concrete syntax for `LangInt`：

```bnf
TODO
```

The abstract syntax for `LangInt`：

```bnf
TODO
```

## 1.3 Pattern Matching

TODO

# 2 Integers and Variables

# 3 Register Allocation

# 4 Booleans and Conditionals

# 5 Loops and Dataflow Analysis

# 6 Tuples and Garbage Collection

# 7 Functions

# 8 Lexically Scoped Functions

# 9 Dynamic Typing

# 10 Gradual Typing

# 11 Generics

# A Appendix
