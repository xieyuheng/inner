---
title: Essentials of Compilation
subtitle: An Incremental Approach in Racket
authors: Jeremy G. Siek
year: 2023
---

# My Motive

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

[2025-08-12] 已经有了自己的动态类型 lisp
-- occam-lisp：https://github.com/xieyuheng/occam-lisp.js
现在想要用 occam-lisp 写自己的编译器。

[2025-08-30] 已经把目前写的 racket 代码翻译到了 occam-lisp。

# My Notes

[2025-06-05] 作为渐进的开发与教学的著作，
作者在两个方面践行了渐进：

- 多个 pass 渐进。
- 多个 lang 渐进。

多个 pass 渐进很容易实现。

但是实现多个 lang 渐进的时候，
作者用到了 class 与 open recursion。
在实际的实现中，
根本不必保留多个 lang 的历史，
因此不用使用这些更为复杂的语言功能。

还有其他保持开发历史的方式：

- 复制粘贴每个阶段的语言。
- git repo + tags。

[2025-06-06] 上面说不需要 lang 之间的渐进，
但是 c-program 的 exp 是 program 的 exp 的子集。

```bnf
<exp> ::= (Int <int>)
        | (Var <var>)
        | (Prim 'read ())
        | (Prim '- (<exp>))
        | (Prim '+ (<exp> <exp>))
        | (Prim '- (<exp> <exp>))
        | (Let <var> <exp> <exp>)
<program> ::= (Program '() <exp>)
```

```bnf
<atm> ::= (Int <int>)
        | (Var <var>)
<exp> ::= <atm>
        | (Prim 'read ())
        | (Prim '- (<atm>))
        | (Prim '+ (<atm> <atm>))
        | (Prim '- (<atm> <atm>))
<stmt> ::= (Assign (Var <var>) <exp>)
<tail> ::= (Return <exp>)
         | (Seq <stmt> <tail>)
<c-program> ::= (CProgram <info> ((<label> . <tail>) … ))
```

如果要区分 c-program 与 program，并且还想要重用代码，
就还是需要 class 与 open recursion。

有没有办法避免这种情况？

[2025-08-12] 也许不应该在任何时候都避免重复代码，
在适当的时候重复代码可以大大降低理解整体代码的复杂性。

[2025-08-30] 目前的实现方式就是重复代码：

`program.lisp`:

```scheme
(define-data program?
  (cons-program
   (info anything?)
   (body exp?)))

(define-data exp?
  (var-exp (name symbol?))
  (int-exp (value int?))
  (prim-exp (op symbol?) (args (list? exp?)))
  (let-exp (name symbol?) (rhs exp?) (body exp?)))
```

`c-program.lisp`:

```scheme
(define-data c-program?
  (cons-c-program
   (info anything?)
   (seqs (record? seq?))))

(define-data seq?
  (return-seq (result c-exp?))
  (cons-seq (stmt stmt?) (tail seq?)))

(define-data stmt?
  (assign-stmt (var var-c-exp?) (rhs c-exp?)))

(define-data c-exp?
  (var-c-exp (name symbol?))
  (int-c-exp (value int?))
  (prim-c-exp (op symbol?) (args (list? c-atom?))))

(define c-atom? (union var-c-exp? int-c-exp?))
```

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

直接从 sexp 开始，生成用 ADT 表示的 AST。

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
这里的 tree 能也只能从 parent 到 children，
而不能从 children 到 parent。

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

这种 set theory 的观点继承自 EOPL。

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

并且对于（1）和（2）中的「判断」，可以按照算法的复杂度进一步分类：

- 对于 grammar 所定义的集合而言，（1）和（2）都有高效的算法。
- 对于 program 所定义的集合而言，（1）有高效的算法，但是（2）没有。
  即没有算法可以判断两个程序是否等价，
  程序之间的等价定义为所能观察到的行为相同。

考虑上面的定义中集合的「元素」时，
可以理解为有一个预先定义的全集
[Domain of discourse](https://en.wikipedia.org/wiki/Domain_of_discourse)。

在这个全集中取元素，例如：

- 所有 JSON 数据的集合。
- 所有 sexp 表达式的的集合。

> The set is infinite (that is, one can always create larger
> programs), so one cannot simply describe a language by listing all
> the programs in the language. Instead we write down a set of rules,
> a _context-free_ grammar, for building programs.

> Grammars are often used to define the concrete syntax of a language,
> but they can also be used to describe the abstract syntax.

> As an example, we describe a small language, named `LInt` , that
> consists of integers and arithmetic operations.

<question>
What is context-free grammar rule?
<answer>
Each rule has a left-hand side and a right-hand side.
If you have an AST node that matches the right-hand side,
then you can categorize it according to the left-hand side.
</answer>
</question>

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

Example exp in `LInt`：

```racket
(Prim '+ ((Prim 'read ()) (Prim '- ((Int 8)))))
```

上面对 quote 的用法有歧义，
如果想要成为 meta-language 中合法的表达式，
必须加上 `list`：

```racket
(Prim '+ (list (Prim 'read (list)) (Prim '- (list (Int 8)))))
```

> The last grammar rule for `LInt` states that there is a `Program`
> node to mark the top of the whole program:

```bnf
<LInt> ::= (Program '() <Exp>)
```

> The Program structure is defined as follows:

```racket
(struct Program (info body))
```

> where `body` is an expression. In further chapters, the `info` part
> is used to store auxiliary information, but for now it is just the
> empty list.

> **Figure 1.1**: The concrete syntax for `LInt`.

```bnf
<type> ::= Integer
<exp> ::= <int>
        | (read)
        | (- <exp>)
        | (+ <exp> <exp>)
        | (- <exp> <exp>)
<LInt> ::= <exp>
```

> **Figure 1.2**: The abstract syntax for `LInt`.

```bnf
<type> ::= Integer
<exp> ::= (Int <int>)
        | (Prim 'read ())
        | (Prim '- (<exp>))
        | (Prim '+ (<exp> <exp>))
        | (Prim '- (<exp> <exp>))
<LInt> ::= (Program ’() <exp>)
```

## 1.5 Interpreters

> The behavior of a program is defined by the specification of the
> programming language. ... In this book we use interpreters to
> specify each language that we consider. An interpreter that is
> designated as the definition of a language is called a _definitional
> interpreter_ (Reynolds 1972).

```scheme
(define (interpret-exp exp)
  (match exp
    [(Int n) n]
    [(Prim 'read (list))
     (define r (read))
     (cond [(fixnum? r) r]
           [else (error 'interpret-exp "read expected an integer" r)])]
    [(Prim '- (list e))
     (fx- 0 (interpret-exp e))]
    [(Prim '+ (list e1 e2))
     (fx+ (interpret-exp e1) (interpret-exp e2))]
    [(Prim '- (list e1 e2))
     (fx- (interpret-exp e1) (interpret-exp e2))]))

(define (interpret-lang-int program)
  (match program
    [(Program (list) exp) (interpret-exp exp)]))
```

## 1.6 Example Compiler: A Partial Evaluator

```scheme
(define (pe-lang-int program)
  (match program
    [(Program (list) exp)
     (Program (list) (pe-exp exp))]))

(define (pe-exp exp)
  (match exp
    [(Int n) (Int n)]
    [(Prim 'read (list)) (Prim 'read (list))]
    [(Prim '- (list e)) (pe-neg (pe-exp e))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]
    [(Prim '- (list e1 e2)) (pe-sub (pe-exp e1) (pe-exp e2))]))

(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [_ (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-sub r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx- n1 n2))]
    [(_ _) (Prim '- (list r1 r2))]))
```

# 2 Integers and Variables

## 2.1 The LVar Language

> **Figure 2.1**: The concrete syntax for `LVar`.

```bnf
<type> ::= Integer
<exp> ::= <int>
        | (read)
        | (- <exp>)
        | (+ <exp> <exp>)
        | (- <exp> <exp>)
------
<exp> ::= <var>
        | (let ([<var> <exp>]) <exp>)
<LVar> ::= <exp>
```

> **Figure 2.2**: The abstract syntax for `LVar`.

```bnf
<type> ::= Integer
<exp> ::= (Int <int>)
        | (Prim 'read ())
        | (Prim '- (<exp>))
        | (Prim '+ (<exp> <exp>))
        | (Prim '- (<exp> <exp>))
------
<exp> ::= (Var <var>)
       | (Let <var> <exp> <exp>)
<LVar> ::= (Program ’() <exp>)
```

### 2.1.1 Extensible Interpreters via Method Overriding

> To prepare for discussing the interpreter of LVar, we explain why we
> implement it in an object-oriented style. Throughout this book we
> define many interpreters, one for each language that we
> study. Because each language builds on the prior one, there is a lot
> of commonality between these interpreters. We want to write down the
> common parts just once instead of many times. A naive interpreter
> for LVar would handle the cases for variables and let but dispatch
> to an interpreter for LInt in the rest of the cases.

这是这本书最大的错误。
不应该为了极少量「代码复用」而引入 class 之间复杂的依赖关系。

> The problem with this naive approach is that it does not handle
> situations in which an LVar feature is nested inside an LInt
> feature.

> To make our interpreters extensible we need something called open
> recursion, in which the tying of the recursive knot is delayed until
> the functions are composed.  Object-oriented languages provide open
> recursion via method overriding.

用 method overriding 所带来的 open recursion，
把一个 recursive function 拆开成相互继承的一串 class。
是一串 class 甚至不是 tree，为了未来的代码复用而使用了复杂的语言 feature，
但是复用代码的情况并没有出现。

这带来的认知复杂负担太大了。
说这是这本书的最大错误，
是因为这是为了教学而写的书，
设想如果没有这些额外的复杂度，
这本书中的 idea 会更容易为学生所理解。

Dan 在讲授编译器实现的时候，
肯定会保持代码朴素，而不会用这些 OOP 的 feature，

### 2.1.2 Definitional Interpreter for LVar

> The interpreter for LVar adds two new cases for variables and
> let. For let, we need a way to communicate the value bound to a
> variable to all the uses of the variable. To accomplish this, we
> maintain a mapping from variables to values called an _environment_.

这一节强调了，在有了 Definitional Interpreter 之后，
编译器的正确性可以定义为下面两个函数等价：

```scheme
interp-Lvar = (compose interp-x86int compile)
```

在研究形式系统（formal system）之间的关系时，编译器起到重要作用。
程序语言的编译器只是这种知识最实用的一面。

注意，所谓「研究形式系统」就在于设计很多新的形式系统，
并且研究它们之间的关系。

## 2.2 The x86Int Assembly Language

> **Figure 2.6**: The syntax of the x86Int assembly language (AT&T syntax).

```bnf
<reg> ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi
        | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
<arg> ::= $<int>
        | %<reg>
        | <int>(%<reg>)
<instr> ::= addq <arg>, <arg>
          | subq <arg>, <arg>
          | negq <arg>
          | movq <arg>, <arg>
          | pushq <arg>
          | popq <arg>
          | callq <label>
          | retq
          | jmp <label>
          | <label>: <instr>
<x86Int> ::= .globl main
             main: <instr> …
```

> We exhibit the use of memory for storing intermediate results in the
> next example.  Figure 2.8 lists an x86 program that computes (+ 52
> (- 10)). This program uses a region of memory called the _procedure
> call stack_ (_stack_ for short).  The stack consists of a separate
> _frame_ for each procedure call. The memory layout for an individual
> frame is shown in figure 2.9.

> **Figure 2.9** Memory layout of a frame.

| Position  | Contents       |
|-----------|----------------|
| 8(%rbp)   | return address |
| 0(%rbp)   | old rbp        |
| -8(%rbp)  | variable 1     |
| -16(%rbp) | variable 2     |
| ...       | ...            |
| 0(%rsp)   | variable n     |

这里对 stack 的展示方式是错误的。
stack 之所以被设计成 push 的时候 index 减少，
也就是向低地址位置延伸，就是为了让人们画图的时候，
在「先画低地址，再画高地址」的前提下，
可以把栈画成生活中堆叠的栈的样子，即向上延伸。
此时 stack 的 top 和 bottom 有自然的解释。

但是 Figure 2.9 还是把图画成了向下延伸。

正确的画法是：

| Position  | Contents       |
|-----------|----------------|
| 0(%rsp)   | variable n     |
| ...       | ...            |
| -16(%rbp) | variable 2     |
| -8(%rbp)  | variable 1     |
| 0(%rbp)   | old rbp        |
| 8(%rbp)   | return address |

注意，为了使用 procedure call stack，
人们用了两个保留的 register：

- rsp 用来指向 stack top；
- rbp 用来指向当前 frame 的开始。

call 和 ret 指令只会操作 rsp，
而 rbp 是 c calling convention 的一部分。

> In the program shown in figure 2.8, consider how control is
> transferred from the operating system to the `main` function. The
> operating system issues a `callq main` instruction that pushes its
> return address on the stack and then jumps to `main`. In x86-64, the
> stack pointer `rsp` must be divisible by 16 bytes prior to the
> execution of any `callq` instruction, so that when control arrives
> at main, the `rsp` is 8 bytes out of alignment (because the callq
> pushed the return address).

这个 16 byte alignment 是需要  caller 处理的，
在 `main` 函数中不用管。

> **Figure 2.8** An x86 program that computes (+ 52 (- 10)).

```asm
start:
  movq $10, -8(%rbp)
  negq -8(%rbp)
  movq -8(%rbp), %rax
  addq $52, %rax
  jmp conclusion

.globl main
main:
  pushq %rbp
  movq %rsp, %rbp
  subq $16, %rsp
  jmp start

conclusion:
  addq $16, %rsp
  popq %rbp
  retq
```

> The first three instructions are the typical _prelude_ for a
> procedure.
>
> - The instruction `pushq %rbp` first subtracts 8 from the stack
>   pointer `rsp` and then saves the base pointer of the caller at
>   address `rsp` on the stack.
>
> - The next instruction `movq %rsp, %rbp` sets the base pointer to
>   the current stack pointer, which is pointing to the location of
>   the old base pointer.
>
> - The instruction `subq $16, %rsp` moves the stack pointer down to
>   make enough room for storing variables. This program needs one
>   variable (8 bytes), but we round up to 16 bytes so that rsp is
>   16-byte-aligned, and then we are ready to make calls to other
>   functions.

注意，这里预留 stack 空间，并且计算好 `rsp` 的 alignment，
这样可以使得不用在每次 `callq` 的时候重新处理 `rsp` 的 alignment。
如果不是为了这个 alignment，预留 stack 空间是没必要的。

> The last instruction of the prelude is `jmp start`, which transfers
> control to the instructions that were generated from the expression
> (+ 52 (- 10)).

这个 `jmp` 是没必要的，只是为了展示 `start` 这部分代码是来自 `(+ 52 (- 10))`。

把 `main` 和 `conclusion` 写在一起，
也是为了使得「预备代码」和「善后代码」的对应关系更明显：

```asm
main:
  pushq %rbp
  subq $16, %rsp

conclusion:
  addq $16, %rsp
  popq %rbp
```

> **Figure 2.10**: The abstract syntax of x86Int assembly.

```bnf
<reg> ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi
        | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
<arg> ::= (Imm <int>)
        | (Reg <reg>)
        | (Deref <reg> <int>)
<instr> ::= (Instr addq (<arg> <arg>))
          | (Instr subq (<arg> <arg>))
          | (Instr negq (<arg>))
          | (Instr movq (<arg> <arg>))
          | (Instr pushq (<arg>))
          | (Instr popq (<arg>))
          | (Callq <label> <int>)
          | (Retq)
          | (Jmp <label>)
<block> ::= (Block <info> (<instr> … ))
<x86Int> ::= (X86Program <info> ((<label> . <block>) … ))
```

解释给汇编语言设计 AST 时需要注意的问题，
比如 label 对应于 block，以及 call 带有 arity。

## 2.3 Planning the Trip to x86

> To compile one language to another, it helps to focus on the
> differences between the two languages because the compiler will need
> to bridge those differences. What are the differences between LVar
> and x86 assembly? Here are some of the most important ones:

> 1. x86 arithmetic instructions typically have two arguments and
> update the second argument in place. In contrast, LVar arithmetic
> operations take two arguments and produce a new value. An x86
> instruction may have at most one memory-accessing argument.
> Furthermore, some x86 instructions place special restrictions on
> their arguments.

> 2. An argument of an LVar operator can be a deeply nested
> expression, whereas x86 instructions restrict their arguments to be
> integer constants, registers, and memory locations.

> 3. The order of execution in x86 is explicit in the syntax, which is
> a sequence of instructions and jumps to labeled positions, whereas
> in LVar the order of evaluation is a left-to-right depth-first
> traversal of the abstract syntax tree.

> 4. A program in LVar can have any number of variables, whereas x86
> has 16 registers and the procedure call stack.

> 5. Variables in LVar can shadow other variables with the same
> name. In x86, registers have unique names, and memory locations have
> unique addresses.

从 bridging the differences 的角度来看，编译器的领域就宽广多了。
渐进地开发编译器，一定也是渐进地发现新的需要 bridging 的 differences。

编译器不只是一类程序，它同时还代表了一种解决问题的思路，
即通过翻译来解决问题，并且在实现翻译的时候可以用多层 pass 来控制复杂度。

看了上面的 differences 列表，可以停下来想一下应该如何实现 passes。

> We ease the challenge of compiling from LVar to x86 by breaking down
> the problem into several steps, which deal with these differences
> one at a time. Each of these steps is called a _pass_ of the
> compiler. This term indicates that each step passes over, or
> traverses, the AST of the program. Furthermore, we follow the
> nanopass approach, which means that we strive for each pass to
> accomplish one clear objective rather than two or three at the same
> time.

在动态类型的语言中，
我们可以把每次 pass 前后的不变性表达为谓词，
注意这些不变性经常是不能用类型表达的。

> The next question is, in what order should we apply these passes?
> This question can be challenging because it is difficult to know
> ahead of time which orderings will be better (that is, will be
> easier to implement, produce more efficient code, and so on), and
> therefore ordering often involves trial and error. Nevertheless, we
> can plan ahead and make educated choices regarding the ordering.

不是简单介绍这个编译器怎么写，
而是教授在遇到需要用编译器来解决的新问题时，
解题思路是什么。

### 2.3.1 The CVar Intermediate Language

> **Figure 2.12**: The concrete syntax of the CVar intermediate language.

```bnf
<atm> ::= <int> | <var>
<exp> ::= <atm>
        | (read)
        | (- <atm>)
        | (+ <atm> <atm>)
        | (- <atm> <atm>)
<stmt> ::= <var> = <exp>;
<tail> ::= return <exp>; | <stmt> <tail>
<CVar>::= (<label>: <tail>) …
```

> **Figure 2.13**: The abstract syntax of the CVar intermediate language.

```bnf
<atm> ::= (Int <int>)
        | (Var <var>)
<exp> ::= <atm>
        | (Prim 'read ())
        | (Prim '- (<atm>))
        | (Prim '+ (<atm> <atm>))
        | (Prim '- (<atm> <atm>))
<stmt> ::= (Assign (Var <var>) <exp>)
<tail> ::= (Return <exp>)
         | (Seq <stmt> <tail>)
<CVar> ::= (CProgram <info> ((<label> . <tail>) … ))
```

## 2.4 Uniquify Variables

```scheme
(let ([x 32])
  (+ (let ([x 10]) x) x))
=>
(let ([x.1 32])
  (+ (let ([x.2 10]) x.2) x.1))
```

```scheme
(let ([x (let ([x 4])
           (+ x 1))])
  (+ x 2))
=>
(let ([x.2 (let ([x.1 4])
             (+ x.1 1))])
  (+ x.2 2))
```

```scheme
(define (freshen name)
  (gensym (string-append (symbol->string name) ".")))

(define ((uniquify-exp env) exp)
  (match exp
    [(Var name)
     (define found-name (dict-ref env name #f))
     (Var (or found-name name))]
    [(Int n)
     (Int n)]
    [(Let name rhs body)
     (define fresh-name (freshen name))
     (define new-env (dict-set env name fresh-name))
     (Let fresh-name
          ((uniquify-exp env) rhs)
          ((uniquify-exp new-env) body))]
    [(Prim op args)
     (Prim op (map (uniquify-exp env) args))]))

(define (uniquify program)
  (match program
    [(Program info body)
     (Program info ((uniquify-exp (list)) body))]))
```

这里的 Exercise 2.2 介绍了一个依赖 convention 的 testing framework，
这也是错误的，因为太复杂了。

简洁优雅的语言，也要看给什么人用，
不会写简介代码的人，就算用 scheme 还是不会写。

书中关于编译器核心的代码，
很多都继承自传统的印第安纳大学课程，
是真正有经验的程序员写的。

## 2.5 Remove Complex Operands

> **Figure 2.15**: LVarMon is LVar with operands restricted to
> atomic expressions.

```bnf
<atm> ::= (Int <int>)
        | (Var <var>)
<exp> ::= <atm>
        | (Prim 'read ())
        | (Prim '- (<atm>))
        | (Prim '+ (<atm> <atm>))
        | (Prim '- (<atm> <atm>))
        | (Let <var> <exp> <exp>)
<LVarMon> ::= (Program '() <exp>)
```

```scheme
(let ([x (+ 42 (- 10))])
  (+ x 10))
=>
(let ([x (let ([tmp.1 (- 10)])
           (+ 42 tmp.1))])
  (+ x 10))
```

> The atomic expressions are pure (they do not cause or depend on side
> effects) whereas complex expressions may have side effects, such as
> `(Prim 'read ())`.  A language with this separation between pure
> expressions versus expressions with side effects is said to be in
> monadic normal form (Moggi 1991; Danvy 2003), which explains the mon
> in the name LVarMon.

这里的两个引用：

- Moggi, Eugenio. 1991. “Notions of Computation and Monads.”
- Danvy, Olivier. 2003. “A New One-Pass Transformation into Monadic Normal Form.”

> We recommend implementing this pass with two mutually recursive
> functions, `rco-atom` and `rco-exp`. The idea is to apply `rco-atom`
> to subexpressions that need to become atomic and to apply `rco-exp`
> to subexpressions that do not. Both functions take an LVar
> expression as input. The `rco-exp` function returns an expression.
> The `rco-atom` function returns two things: an atomic expression and
> an alist mapping temporary variables to complex subexpressions.

返回 exp + alist 代表 `rco-atom` 用了 writer monad.

这里的 remove-complex-operands 也许可以称作 unnest。

> In the example program with the expression `(+ 42 (- 10))`, the
> subexpression `(- 10)` should be processed using the `rco-atom`
> function because it is an argument of the + operator and therefore
> needs to become atomic. The output of `rco-atom` applied to `(- 10)`
> is as follows:

```scheme
(- 10)  =>  tmp.1
            ((tmp.1 . (- 10)))
```

> In debugging your compiler, it is often useful to see the
> intermediate programs that are output from each pass. To print the
> intermediate programs, place `(debug-level 1)` before the call to
> `interp-tests` in `run-tests.rkt`.

我不用 `(debug-level 1)` 而是专门给每个 pass 写 snapshot test。

## 2.6 Explicate Control

> The `explicate-control` pass compiles `LVar` programs into `CVar`
> programs that make the order of execution explicit in their syntax.
> For now this amounts to flattening `let` constructs into a sequence
> of assignment statements. For example, consider the following `LVar`
> program:

```scheme
(let ([y (let ([x 20])
           (+ x (let ([x 22]) x)))])
  y)
```

逐步编译的过程如下：

```scheme
000 (program () (let ((y (let ((x 20)) (+ x (let ((x 22)) x))))) y))
010 (program () (let ((y₁ (let ((x₁ 20)) (+ x₁ (let ((x₂ 22)) x₂))))) y₁))
020 (program () (let ((y₁ (let ((x₁ 20)) (let ((x₂ 22)) (+ x₁ x₂))))) y₁))
030 (c-program () (:start ((= x₁ 20) (= x₂ 22) (= y₁ (+ x₁ x₂)) (return y₁))))
```

> Recall that the right-hand side of a let executes before its body,
> so that the order of evaluation for this program is to assign 20 to
> `x₁`, 22 to `x₂`, and `(+ x₁ x₂)` to `y₁`, and then to return `y₁`.
> Indeed, the output of `explicate-control` makes this ordering explicit.

下面要依据 tail position 来写递归函数。

> **Definition 2.1** The following rules define when an expression is
> in tail position for the language `LVar`.
>
> 1. In `(Program () e)`, expression `e` is in tail position.
> 2. If `(Let x e1 e2)` is in tail position, then so is `e2`.

既然都说是 "tail position" 了，
那 tail 在代码中的体现就应该是一个 field 的名字，
而不是一个 data type 的名字。

> The `explicate-assign` function is in accumulator-passing style: the
> `cont` parameter is used for accumulating the output. This
> accumulator-passing style plays an important role in the way that we
> generate high-quality code for conditional expressions in
> chapter 4. The abbreviation `cont` is for continuation because it
> contains the generated code that should come after the current
> assignment. This code organization is also related to
> continuation-passing style, except that `cont` is not what happens
> next during compilation but is what happens next in the generated
> code.

## 2.7 Select Instructions

TODO

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
