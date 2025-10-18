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

> In this section we consider a compiler that translates `LInt` programs into `LInt`
> programs that may be more efficient. The compiler eagerly computes the parts
> of the program that do not depend on any inputs, a process known as _partial
> evaluation_ (Jones, Gomard, and Sestoft 1993). For example, given the following
> program

这里的引用是：

- "Partial Evaluation and Automatic Program Generation".
  Jones, Neil D., Carsten K. Gomard, and Peter Sestoft. 1993.
  Prentice Hall.

感觉也是值得一看的。

```scheme
(+ (read) (- (+ 5 3)))
```

> our compiler translates it into the program

```scheme
(+ (read) -8)
```

> **Figure 1.5** A partial evaluator for `LInt`.

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

> In figure 1.5, the structural recursion over exp is captured in the
> `pe-exp` function, whereas the code for partially evaluating the
> negation and addition operations is factored into three auxiliary
> functions: `pe-neg`, `pe-add` and `pe-sub`. The input to these
> functions is the output of partially evaluating the children. The
> `pe-neg`, `pe-add` and `pe-sub` functions check whether their
> arguments are integers and if they are, perform the appropriate
> arithmetic. Otherwise, they create an AST node for the arithmetic
> operation.

这种递归的组合算是可以被命名为 partial evaluation 的 pattern。

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

> In the `select-instructions` pass we begin the work of translating
> from `CVar` to `x86Var`. The target language of this pass is a
> variant of x86 that still uses variables, so we add an AST node of
> the form `(Var var)` to the arg nonterminal of the `x86Int` abstract
> syntax.

这里需要处理的 difference 是 x86 汇编 operator 作用于寄存器时的限制。

## 2.8 Assign Homes

> The `assign-homes` pass compiles `x86Var` programs to `x86Int`
> programs that no longer use program variables. Thus, the
> `assign-homes` pass is responsible for placing all the program
> variables in registers or on the stack.

先不考虑寄存器分配，
把所有的参数都保存在 stack 中，
写一个简单的编译器出来。

## 2.9 Patch Instructions

> The patch-instructions pass compiles from `x86Int` to `x86Int` by
> making sure that each instruction adheres to the restriction that at
> most one argument of an instruction may be a memory reference.

```scheme
(let ((a 42))
  (let ((b a))
    b))
```

```asm
movq $42, -8(%rbp)
movq -8(%rbp), -16(%rbp)
movq -16(%rbp), %rax
```

> The second movq instruction is problematic because both arguments
> are stack locations. We suggest fixing this problem by moving from
> the source location to the register `rax` and then from `rax` to the
> destination location, as follows.

```asm
movq -8(%rbp), %rax
movq %rax, -16(%rbp)
```

> There is a similar corner case that also needs to be dealt with. If
> one argument is an immediate integer larger than 2^16 and the other
> is a memory reference, then the instruction is invalid. One can fix
> this, for example, by first moving the immediate integer into `rax`
> and then using `rax` in place of the integer.

## 2.10 Generate Prelude and Conclusion

> The last step of the compiler from `LVar` to x86 is to generate the
> main function with a prelude and conclusion wrapped around the rest
> of the program, as shown in figure 2.8 and discussed in section 2.2.

回顾 **Figure 2.8**： An x86 program that computes (+ 52 (- 10)).

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

## 2.11 Challenge: Partial Evaluator for LVar

首先把之前对 `LInt` 的 partial evaluator 扩展到 `LVar`。

然后处理这种情况：

```scheme
(+ 1 (+ (read) 1))
=>
(+ 2 (read))
```

> To accomplish this, the `pe-exp` function should produce output in
> the form of the `<residual>` nonterminal of the following
> grammar. The idea is that when processing an addition expression, we
> can always produce one of the following:
>
> - (1) an integer constant,
>
> - (2) an addition expression with an integer constant on the
>   left-hand side but not the right-hand side,
>
> - (3) an addition expression in which neither subexpression is a
>   constant.

```bnf
<inert> ::= <var>
          | (read)
          | (- <var>)
          | (- (read))
          | (+ <inert> <inert>)
          | (let ((<var> <residual>)) <residual>)

<residual> ::= <int>
             | (+ <int> <inert>)
             | <inert>
```

也就是说利用加法的交换性，把所有的常量都放到左边。
因此，排除了 `(+ (read) 1)` 这种情况。

也许这里所描述的技巧，可以在之前的引用中找到更系统的叙述：

- "Partial Evaluation and Automatic Program Generation".
  Jones, Neil D., Carsten K. Gomard, and Peter Sestoft. 1993.
  Prentice Hall.

同时，这种优化也让我想到，
是否 forth 的纯后缀表达式对于优化而言更具有优势，
这一点可以参考 de bruijn 所发现的，后缀表达式的 lambda 演算语法中，
可以发现更多的规律，找到更多的等价的 lambda term。

> The `pe-add` and `pe-neg` functions may assume that their inputs are
> residual expressions and they should return `<residual>` expressions.

> Once the improvements are complete, make sure that your compiler
> still passes all the tests. After all, fast code is useless if it
> produces incorrect results!

注意，这里的术语 inert 和 residual 模仿了化学反应的术语，
这里所作的 partial evaluation 类似 rewrite until inert，
确实类似化学反应。

注意，这种 partial evaluation 与 normalization by evaluation (NbE) 不同。

# 3 Register Allocation

> The goal of register allocation is to fit as many variables into
> registers as possible. Some programs have more variables than
> registers, so we cannot always map each variable to a different
> register. Fortunately, it is common for different variables to be in
> use during different periods of time during program execution, and
> in those cases we can map multiple variables to the same register.

> **Figure 3.1** A running example for register allocation.

```scheme
(let ((v 1))
  (let ((w 42))
    (let ((x (iadd v 7)))
      (let ((y x))
        (let ((z (iadd x w)))
          (iadd z (ineg y)))))))
```

After pass 040:

```scheme
((movq $1 v₁)
 (movq $42 w₁)
 (movq v₁ x₁)
 (addq $7 x₁)
 (movq x₁ y₁)
 (movq x₁ z₁)
 (addq w₁ z₁)
 (movq y₁ _₁)
 (negq _₁)
 (movq z₁ %rax)
 (addq _₁ %rax)
 (jmp epilog))
```

写成带有变量的 x86 伪代码：

```asm
start:
       movq $1, v
       movq $42, w
       movq v, x
       addq $7, x
       movq x, y
       movq x, z
       addq w, z
       movq y, _
       negq _,
       movq z, %rax
       addq _, %rax
       jmp epilog
```

> The program is almost completely in the x86 assembly language, but
> it still uses variables. Consider variables `x` and `z`. After the
> variable `x` has been moved to `z`, it is no longer in use.
> Variable `z`, on the other hand, is used only after this point, so
> `x` and `z` could share the same register.

> The topic of section 3.2 is how to compute where a variable is in
> use. Once we have that information, we compute which variables are
> in use at the same time, that is, which ones _interfere_ with each
> other, and represent this relation as an undirected graph whose
> vertices are variables and edges indicate when two variables
> interfere (section 3.3). We then model register allocation as a
> graph coloring problem (section 3.4).

这一章最重要的知识，就是问题层面的「编译器」，
即如何把编译器分配问题转化为图的染色问题。

感觉又是逻辑式编程可以派上用场的时候，
但是这里应该是手写的。

未来 x-lisp 稳定了，可以在 x-lisp 中嵌入逻辑式编程语言。

> If we run out of registers despite these efforts, we place the
> remaining variables on the stack, similarly to how we handled
> variables in chapter 2. It is common to use the verb _spill_ for
> assigning a variable to a stack location. The decision to spill a
> variable is handled as part of the graph coloring process.

> We make the simplifying assumption that each variable is assigned to
> one location (a register or stack address). A more sophisticated
> approach is to assign a variable to one or more locations in
> different regions of the program. For example, if a variable is used
> many times in short sequence and then used again only after many
> other instructions, it could be more efficient to assign the
> variable to a register during the initial sequence and then move it
> to the stack for the rest of its lifetime.  We refer the interested
> reader to Cooper and Torczon (2011) (chapter 13) for more
> information about that approach.

这里的引用是：

- Cooper, Keith, and Linda Torczon. 2011.
  Engineering a Compiler. 2nd edition.
  Morgan Kaufmann.

对比这里的第 3章，那里要到第 13 章才处理这个问题。

我想 IU 和 Dan 的写书风格应该是给每个问题一个具体的方案，
然后去讲解这个方案的实现方式，以帮助学生入门，
而不是系统地分析这个问题的所有方案。

## 3.1 Registers and Calling Conventions

> As we perform register allocation, we must be aware of the calling
> conventions that govern how function calls are performed in
> x86. Even though `LVar` does not include programmer-defined
> functions, our generated code includes a `main` function that is
> called by the operating system and our generated code contains calls
> to the `read_int` function.

> Function calls require coordination between two pieces of code that
> may be written by different programmers or generated by different
> compilers. Here we follow the System V calling conventions that are
> used by the GNU C compiler on Linux and MacOS (Bryant and
> O’Hallaron 2005; Matz et al. 2013).

注意，前面在生成汇编代码的时候，
已经需要考虑一部分 calling conventions 了，
比如：

- `rsp` 作为 stack pointer。
- `rbp` 作为 stack base pointer。
- `rax` 传递返回值。

但是这些应该是 x86 的 calling conventions，
而不是 System V calling conventions。

为了方便在教学过程中渐进地引入概念，
System V calling conventions 中关于寄存器的部分，
可以推迟到这一章才介绍。

要想真的对 calling conventions 有更深刻的理解，
还是需要尝试更多 alternatives，只看 System V 是不够的。

> The calling conventions include rules about how functions share the
> use of registers.  In particular, the caller is responsible for
> freeing some registers prior to the function call for use by the
> callee. These are called the _caller-saved registers_ and they are
>
>     rax rcx rdx rsi rdi r8 r9 r10 r11
>
> On the other hand, the callee is responsible for preserving the
> values of the _callee-saved registers_, which are
>
>     rsp rbp rbx r12 r13 r14 r15

首先 `rax` 必须是 caller-saved，
因为 callee 需要用 `rax` 来传递返回值给 caller。

其次 `rsp` 和 `rbp` 作为 stack 相关的指针，必须是 callee-saved，
因为 caller 需要保证返回时这些指针不变。

为什么额外的寄存器 conventions 是必要的？
假设除了 `rax` 之外所有的寄存器都是 callee-saved，
此时 caller 可以假设除了 `rax` 之外的寄存器在调用函数后都不变，
而 callee 如果用到了某个寄存器，就必须保存这个寄存器。
这是可行的，但是如果均匀分配保存寄存器的职责，
caller 和 callee 在使用寄存器的时候就有了偏好，
这样在大部分情况下，caller 和 callee 都不用保存寄存器！

> We can think about this caller/callee convention from two points of
> view, the caller view and the callee view, as follows:
>
> - The caller should assume that all the caller-saved registers get
>   overwritten with arbitrary values by the callee. On the other
>   hand, the caller can safely assume that all the callee-saved
>   registers retain their original values.
>
> - The callee can freely use any of the caller-saved registers.
>   However, if the callee wants to use a callee-saved register, the
>   callee must arrange to put the original value back in the register
>   prior to returning to the caller. This can be accomplished by
>   saving the value to the stack in the prelude of the function and
>   restoring the value in the conclusion of the function.

> In x86, registers are also used for passing arguments to a function
> and for the return value. In particular, the first six arguments of
> a function are passed in the following six registers, in this order.
>
>     rdi rsi rdx rcx r8 r9
>
> We refer to these six registers are the argument-passing registers.
> If there are more than six arguments, the convention is to use space
> on the frame of the caller for the rest of the arguments.

这就意味着这六个寄存器也必须是 caller-saved，
因此除了必须 caller-saved 寄存器之外，
就只有 `r10` 和 `r11` 是额外的 conventions。

为了方便记忆，列举一下这前四个寄存器的缩写：

| reg | original meaning  |
|-----|-------------------|
| di  | destination index |
| si  | source index      |
| dx  | data register     |
| cx  | count register    |

deepseek 说基础这些参数寄存器的顺序的顺口溜是：
"Diane's Silly Dog Chases Rabbits Ruthlessly."
哈哈。

> The next question is how these calling conventions impact register
> allocation.  Consider the `LVar` program presented in figure 3.2. We
> first analyze this example from the caller point of view and then
> from the callee point of view. We refer to a variable that is in use
> during a function call as a _call-live variable_.

注意，说 call-live variable 时，
指的是分配寄存器之前带有名字的变量，
而 call-live 是这个变量就分配寄存器这个问题而言的属性。

> **Figure 3.2** An example with function calls.

```scheme
(let ((x (read)))
  (let ((y (read)))
    (+ (+ x y) 42)))
```

> Generated x86 assembly:

```asm
start:
        callq read_int
        movq %rax, %rbx
        callq read_int
        movq %rax, %rcx
        addq %rcx, %rbx
        movq %rbx, %rax
        addq $42, %rax
        jmp conclusion

        .globl main
main:
        pushq %rbp
        movq %rsp, %rbp
        pushq %rbx
        subq $8, %rsp
        jmp start

conclusion:
        addq $8, %rsp
        popq %rbx
        popq %rbp
        retq
```

> The program makes two calls to read. The variable `x` is call-live
> because it is in use during the second call to `read`; we must
> ensure that the value in `x` does not get overwritten during the
> call to `read`. One obvious approach is to save all the values that
> reside in caller-saved registers to the stack prior to each function
> call and to restore them after each call. That way, if the register
> allocator chooses to assign `x` to a caller-saved register, its
> value will be preserved across the call to `read`. However, saving
> and restoring to the stack is relatively slow. If `x` is not used
> many times, it may be better to assign `x` to a stack location in
> the first place. Or better yet, if we can arrange for `x` to be
> placed in a callee-saved register, then it won’t need to be saved
> and restored during function calls.

figure 3.2 的汇编代码中用的是 callee-saved `rbx`，
来临时保存第一次调用 `read` 的返回值。

> On the other hand, for variables that are not call-live, we prefer
> placing them in caller-saved registers to leave more room for
> call-live variables in the callee-saved registers.

> Returning to the example in figure 3.2, let us analyze the generated
> x86 code on the right-hand side. Variable `x` is assigned to `rbx`,
> a callee-saved register. Thus, it is already in a safe place during
> the second call to `read_int`. Next, variable `y` is assigned to
> `rcx`, a caller-saved register, because `y` is not a call-live
> variable.

> We have completed the analysis from the caller point of view, so now
> we switch to the callee point of view, focusing on the prelude and
> conclusion of the `main` function.

> - As usual, the prelude begins with saving the `rbp` register to the
>   stack and setting the `rbp` to the current stack pointer. We now
>   know why it is necessary to save the `rbp`: it is a callee-saved
>   register.

> - The prelude then pushes `rbx` to the stack because (1) `rbx` is a
>   callee-saved register and (2) `rbx` is assigned to a variable
>   (`x`). The other callee-saved registers are not saved in the
>   prelude because they are not used.
>
> - The prelude subtracts 8 bytes from the `rsp` to make it
>   16-byte aligned.
>
> - Shifting attention to the conclusion, we see that `rbx` is
>   restored from the stack with a `popq` instruction.

也就是说，当前函数 `main` 作为 callee，
只有用到某个寄存器时才需要保存这个寄存器，
这里为了保存 read 的返回值用到了 `rbx`，
而只要用到了 `callq`，就隐式地用到了 `rsp` 和 `rbp`。

另外注意，这里 16-byte alignment
是需要根据当前 stack 的使用情况来具体处理的。

## 3.2 Liveness Analysis

> The `uncover-live` pass performs _liveness analysis_; that is, it
> discovers which variables are in use in different regions of a
> program. A variable or register is _live_ at a program point if its
> current value is used at some later point in the program.

<question>
What is the meaning of a variable or register is _live_ at a program point?
<answer>
A variable or register is _live_ at a program point if its
current value is used at some later point in the program.
</answer>
</question>

变量和寄存器，就是为了保存计算过程中的临时结果，
然后在后面的计算中用到的，如果其中保存的值在后面真的能用到，
就算是 live。

目前说的还是 "will be used later"，
加入 conditional 之后，就要说 "might be used later" 了。

另外注意，判断 live 的点，
是指令执行的前后间隙时间。

> We refer to variables, stack locations, and registers collectively
> as _locations_. Consider the following code fragment in which there
> are two writes to `b`. Are variables `a` and `b` both live at the
> same time?

```asm
 1 | movq $5, a
 2 | movq $30, b
 3 | movq a, c
 4 | movq $10, b
 5 | addq b, c
```

> The answer is no, because `a` is live from line 1 to 3 and `b` is
> live from line 4 to 5.  The integer written to `b` on line 2 is
> never used because it is overwritten (line 4) before the next read
> (line 5).

可以想象在为 instruction 建立数据库，
每一个 instruction 有一个 id，
L-after 和 L-before 就是这个 id 的属性。

> The live locations for each instruction can be computed by
> traversing the instruction sequence back to front (i.e., backward in
> execution order). Let `I(1), … , I(n)` be the instruction
> sequence. We write `L-after(k)` for the set of live locations after
> instruction `I(k)` and write `L-before(k)` for the set of live
> locations before instruction `I(k)`.

总有 `L-after(k) = L-before(k + 1)`：

```
  L-before(1)
I(1)
  L-after(1)
  L-before(2)
I(2)
  L-after(2)
  L-before(3)
I(3)
  L-after(3)
...
```

这里先给出直觉上的 liveness 定义，
再找到合适的数学工具给出形式定义，
并且给出计算方式。

这个过程非常类似小学和初中的「应用题」，
只不过这里的题是真的有所应用的。

我认为这种解决问题的能力是比具体学习某个技术更核心的能力。

这种用 before 和 after 两个属性，
来描述 sequence 的间隔中的数据的方式很不错。
这类似于解决平面几何问题是做的「辅助线」，
也类似于 DOM element 的 API：

```js
const targetElement = document.getElementById('myElement')
const previousElement = targetElement.previousElementSibling
const nextElement = targetElement.nextElementSibling
```

假设 `I(n)` 是最后一条指令，从后向前的计算如下：

```
L-after(n) = {}
L-before(k) = L-after(k) - W(k) + R(k)
```

其中 `-` 代表 `set-difference`，`+` 代表 `set-union`。

想要理解为什么要从后向前计算？只需要注意 live 的定义：
"A variable or register is _live_ at a program point if its
current value is used at some later point in the program."
一个变量在某一点 live 与否，是由其后面的所有指令决定的，
所以要从最后一条指令开始计算，这是递归的初始步骤，此时后面的指令列表为空。

> where `W(k)` are the locations written to by instruction `I(k)`,
> and `R(k)` are the locations read by instruction `I(k)`.

因此向一个 location 写数据，就是 kill 这个 location，
从一个 location 读数据，就是依赖这个 location。

> There is a special case for `jmp` instructions. The locations that
> are live before a `jmp` should be the locations in `L-before` at the
> target of the jump. So, we recommend maintaining an alist named
> `label->live` that maps each label to the `L-before` for the first
> instruction in its block.

> For now the only `jmp` in a `x86Var` program is the jump to the
> conclusion. (For example, see figure 3.1.) The conclusion reads from
> `rax` and `rsp`, so the alist should map `conclusion` to the set
> `{rax, rsp}`.

就是说就计算 `L-before` 和 `L-after` 而言，
`jmp` 将两串指令拼起来，
类似 instruction 之间的 unification。

但是为什么 `jmp conclusion` 的 `L-before` 是 `{rax, rsp}`？

```asm
conclusion:
  addq $16, %rsp
  popq %rbp
  retq
```

重点是要知道每个指令的 `W(k)` 和 `R(k)` 如何计算。

| instr          | write      | read       |
|----------------|------------|------------|
| addq $16, %rsp | {rsp}      | {rsp}      |
| popq %rbp      | {rsp, rbp} | {rsp}      |
| retq           | {rsp}      | {rsp, rax} |

注意：

- `addq` 会同时读写 destination location。
- `retq` 读写 `rsp` 的同时，会假设 `rax` 中已有的值为返回值，
  这个虽然不是直接读 `rax` 再做什么，
  但是也可以被理解为依赖 `rax`，因此和读等价。

```asm
conclusion:
                  {rsp, rax} - {rsp} + {rsp} = {rsp, rax}
  addq $16, %rsp
                  {rsp, rax} - {rsp, rbp} + {rsp} = {rsp, rax}
  popq %rbp
                  {} - {rsp} + {rsp, rax} = {rsp, rax}
  retq
                  {}
```

回到本节开头的例子：

```asm
   |               {a} - {a} + {} = {}
 1 | movq $5, a
   |               {a} - {b} + {} = {a}
 2 | movq $30, b
   |               {c} - {c} + {a} = {a}
 3 | movq a, c
   |               {b, c} - {b} + {} = {c}
 4 | movq $10, b
   |               {} - {c} + {b, c} = {b, c}
 5 | addq b, c
   |               {}
```

**Exercise 3.1** 计算 the running example 的 liveness，
即 figure 3.1 中带有变量的 x86 伪代码 的 liveness：

TODO figure 3.1 liveness

```asm
start:
                     {rsp, v} - {v} + {} = {rsp}
       movq $1, v
                     {rsp, w, v} - {w} = {rsp, v}
       movq $42, w
                     {rsp, w, x} - {x} + {v} = {rsp, w, v}
       movq v, x
                     {rsp, w, x} - {x} + {x} = {rsp, w, x}
       addq $7, x
                     {rsp, y, w, x} - {y} + {x} = {rsp, w, x}
       movq x, y
                     {rsp, y, w, z} - {z} + {x} = {rsp, y, w, x}
       movq x, z
                     {rsp, z, y} - {z} + {w, z} = {rsp, y, w, z}
       addq w, z
                     {rsp, z, _} - {_} + {y} = {rsp, z, y}
       movq y, _
                     {rsp, _, z} - {_} + {_} = {rsp, z, _}
       negq _,
                     {rsp, _, rax} - {rax} + {z} = {rsp, _, z}
       movq z, %rax
                     {rsp, rax} - {rax} + {_, rax} = {rsp, _, rax}
       addq _, %rax
                     {rsp, rax}
       jmp epilog
```

> **Exercise 3.2** Implement the `uncover-live` pass.  Store the
> sequence of live-after sets in the _info_ field of the `Block`
> structure. We recommend creating an auxiliary function that takes a
> list of instructions and an initial live-after set (typically empty)
> and returns the list of live-after sets.
>
> Hints:
>
> - The `callq` instruction should include all the caller-saved
>   registers in its write set `W` because the calling convention says
>   that those registers may be written to during the function call.
>
> - Likewise, the `callq` instruction should include the appropriate
>   argument-passing registers in its read set `R`, depending on the
>   arity of the function being called. (This is why the abstract
>   syntax for `callq` includes the arity.)

## 3.3 Build the Interference Graph

> On the basis of the liveness analysis, we know where each location
> is live. However, during register allocation, we need to answer
> questions of the specific form: are locations `u` and `v` live at
> the same time? (If so, they cannot be assigned to the same
> register.)

> To make this question more efficient to answer, we create an
> explicit data structure, an _interference graph_.

> An interference graph is an undirected graph that has a node for
> every variable and register and has an edge between two nodes if
> they are live at the same time, that is, if they interfere with each
> other.

> A straightforward way to compute the interference graph is to look
> at the set of live locations between each instruction and add an
> edge to the graph for every pair of variables in the same set. This
> approach is less than ideal for two reasons. First, it can be
> expensive because it takes `O(n^2)` time to consider every pair in a
> set of n live locations. Second, in the special case in which two
> locations hold the same value (because one was assigned to the
> other), they can be live at the same time without interfering with
> each other.

简单地从 hypergraph 转化到 graph 是不行的，
还是需要用到 instruction 的具体信息。

> A better way to compute the interference graph is to focus on writes
> (Appel and Palsberg 2003). The writes performed by an instruction
> must not overwrite something in a live location.

这里的引用是：

- Appel, Andrew W., and Jens Palsberg. 2003.
  Modern Compiler Implementation in Java.
  Cambridge University Press.

这样为什么不会遗漏？
之所以觉得会遗漏，是因为认为从 liveness 的 hypergraph，
转化成 graph 才是全部的信息。
其实不是的，不应该只通过 live 来定义 interference，
而是要看 interference 的具体定义。
其实具体想要避免的情况，
就是一个寄存器被分配给了两个变量 -- 比如 `x` `y`，
前面已经对 `x` 赋值了，
但是后面在不能删除 `x` 的值的前提下，对 `y` 赋值了，
此时由于这两个变量分配到了同一个寄存器，
所以就会 overwrite 之前的值。

> So for each instruction, we create an edge between the locations
> being written to and the live locations.

为什么这里应该用 L-after 而不是 L-before？
因为只有在 L-after 中，当前这条指令的信息才能被包含进来，
而 interference 的定义是「假设运行了这条指令会有干扰」。

> - For the `callq` instruction, we consider all the caller-saved
>   registers to have been written to, so an edge is added between
>   every live variable and every caller-saved register.

> - Also, for `movq` there is the special case of two variables
>   holding the same value. If a live variable `v` is the source of
>   the `movq`, then there is no need to add an edge between `v` and
>   the destination, because they both hold the same value.

两个变量有相同的值，当然就可以公用一个寄存器。

## 3.4 Graph Coloring via Sudoku

> We come to the main event discussed in this chapter, mapping
> variables to registers and stack locations. Variables that interfere
> with each other must be mapped to different locations. In terms of
> the interference graph, this means that adjacent vertices must be
> mapped to different locations. If we think of locations as colors,
> the register allocation problem becomes the graph coloring problem
> (Balakrishnan 1996; Rosen 2002).

这里的两个引用是：

- Balakrishnan, V. K. 1996.
  Introductory Discrete Mathematics.
  Dover.

- Rosen, Kenneth H. 2002.
  Discrete Mathematics and Its Applications.
  McGraw-Hill Higher Education.

介绍如何把 sudoku 转化为 graph coloring 问题，
作为 graph coloring 的例子。

此处有一个引用：

- Brélaz, Daniel. 1979.
  “New Methods to Color the Vertices of a Graph.”
  Commun. ACM 22 (4):251–256.

将要介绍的算法中 saturation 定义为：
colors already used by neighbors。

> However, register allocation is easier than sudoku, because the
> register allocator can fall back to assigning variables to stack
> locations when the registers run out. Thus, it makes sense to
> replace backtracking with greedy search: make the best choice at the
> time and keep going.

也就是上面论文中提到的 DSATUR graph coloring algorithm。

## 3.5 Patch Instructions

> **Exercise 3.5**
>
> Update the `patch-instructions` compiler pass to delete trivial
> moves. Run the script to test the `patch-instructions` pass.

## 3.6 Generate Prelude and Conclusion

> Recall that this pass generates the prelude and conclusion
> instructions to satisfy the x86 calling conventions (section
> 3.1). With the addition of the register allocator, the callee-saved
> registers used by the register allocator must be saved in the
> prelude and restored in the conclusion.

要在 allocate-registers 传信息给 prelude-and-conclusion。

另外需要移动 rsp 给保存 used callee-saved registers
和 spilled variable locations，保留 stack 空间。

如果是 macos 还要在调用函数之前 16 bytes align stack space。

## 3.7 Challenge: Move Biasing

这个优化当有很多例子能测试优化的有效性时再做。

## 3.8 Further Reading

> Earlier editions of the compiler course at Indiana University
> (Dybvig and Keep 2010) were based on the algorithm of Briggs,
> Cooper, and Torczon (1994).

这里的两个引用是：

- Dybvig, R. Kent, and Andrew Keep. 2010.
  P523 Compiler Assignments.
  Technical report. Indiana University.

- Briggs, Preston, Keith D. Cooper, and Linda Torczon. 1994.
  “Improvements to Graph Coloring Register Allocation.”
  ACM Trans. Program. Lang. Syst. 16 (3): 428–455.

Dybvig 的课程资料很难在网络上找到。

后面这个引用的作者也写了关于 compiler 的书：

- Cooper, Keith, and Linda Torczon. 2011.
  Engineering a Compiler. 2nd edition.
  Morgan Kaufmann.

> The smallest-last ordering algorithm is one of many _greedy_
> coloring algorithms. A greedy coloring algorithm visits all the
> vertices in a particular order and assigns each one the first
> available color.

关于 graph coloring，mit 的课程也很精彩：

- [lecture 6 / graph theory and coloring](https://www.youtube.com/watch?v=h9wxtqoa1jY)

> An _offline_ greedy algorithm chooses the ordering up front, prior
> to assigning colors. The algorithm of Chaitin et al. (1981) should
> be considered offline because the vertex ordering does not depend on
> the colors assigned. Other orderings are possible. For example, Chow
> and Hennessy (1984) ordered variables according to an estimate of
> runtime cost.

Chaitin 相关的引用：

- Chaitin, G. J. 1982.
  “Register Allocation & Spilling via Graph Coloring.”
  In SIGPLAN ’82: Proceedings of the 1982 SIGPLAN Symposium
  on Compiler Construction, 98–105.
  Association for Computing Machinery.

- Chaitin, Gregory J., Marc A. Auslander,
  Ashok K. Chandra, John Cocke, Martin E. Hopkins,
  and Peter W. Markstein. 1981.
  “Register Allocation via Coloring.”
  Computer Languages 6:47–57.

关于 Chow 和 Hennessy 的引用：

- Chow, Frederick, and John Hennessy. 1984.
  “Register Allocation by Priority-Based Coloring.”
  In Proceedings of the 1984 SIGPLAN Symposium on Compiler Construction, 222–232.
  Association for Computing Machinery.

> An _online_ greedy coloring algorithm uses information about the
> current assign- ment of colors to influence the order in which the
> remaining vertices are colored.  The saturation-based algorithm
> described in this chapter is one such algorithm. We choose to use
> saturation-based coloring because it is fun to introduce graph
> coloring via sudoku!

> In situations in which compile time is of utmost importance, such as
> in just-in- time compilers, graph coloring algorithms can be too
> expensive, and the linear scan algorithm of Poletto and Sarkar
> (1999) may be more appropriate.

这里的引用为：

- Poletto, Massimiliano, and Vivek Sarkar. 1999.
  “Linear Scan Register Allocation.”
  ACM Trans. Program. Lang. Syst. 21 (5): 895–913.

# 4 Booleans and Conditionals

> The `LVar` language has only a single kind of value, the
> integers. In this chapter we add a second kind of value, the
> Booleans, to create the `LIf` language.

> With the addition of `if`, programs can have nontrivial control
> flow, which impacts `explicate-control` and liveness analysis.
> Also, because we now have two kinds of values, we need to handle
> programs that apply an operation to the wrong kind of value, such as
> `(not 1)` and `(car 1)`.

> The `LIf` language performs type checking during compilation just as
> Typed Racket. In chapter 9 we study the alternative choice, that is,
> a dynamically typed language like Racket.

先学静态类型，是因为就写编译器而言，静态类型更简单。
这与写解释刚好相反，就写解释器而言，动态类型更简单。

> We keep the type checker for `LIf` fairly simple because the focus
> of this book is on compilation and not type systems, about which
> there are already several excellent books (Pierce 2002, 2004; Harper
> 2016; Pierce et al. 2018).

- Pierce, Benjamin C. 2002.
  Types and Programming Languages.
  MIT Press.

- Pierce, Benjamin C., ed. 2004.
  Advanced Topics in Types and Programming Languages.
  MIT Press.

- Harper, Robert. 2016.
  Practical Foundations for Programming Languages. 2nd edition.
  Cambridge University Press.

- Pierce, Benjamin C., and many. 2018.
  Programming Language Foundations. Vol. 2. Software Foundations.

## 4.1 The LIf Language

尽管对 argument 的求值方式是 lazy 的，
`and` 和 `or` 还是被实现为了 operator。
也许 operator 可以是 lazy 的。

定义语言就是写解释器，
所以这一章写解释器。

## 4.2 Type Checking LIf Programs

> It is helpful to think about type checking in two complementary
> ways.
>
> - A type checker predicts the type of value that will be produced by
>   each expression in the program.
>
> - A second way to think about type checking is that it enforces a
>   set of rules about which operators can be applied to which kinds
>   of values.

第一个其实是 infer，第二个是 check。
但是按照书里这样说，就是没有把 bidirectional type checking 说清楚。
可能是因为类型检查不是教学的主线，所以不细讲。

> It [type checker function] returns an expression because there are
> situations in which we want to change or update the expression.

这是类型系统中的 elaboration，也没有直接点出来。

## 4.3 The CIf Intermediate Language

增加 goto 和 branch。

## 4.4 The x86If Language

> As x86 does not provide direct support for Booleans, we take the
> usual approach of encoding Booleans as integers, with True as 1 and
> False as 0.

用 xor 来实现 not -- `(not x) = (xor 1 y)`

> The abstract syntax for the set instruction differs from the
> concrete syntax in that it separates the instruction name from the
> condition code.

所谓 condition code 就是这里的 cc -- `set<cc>` 和 `jmp<cc>`，

    cc ::= e | l | le | g | ge

> Because the conditional jump instruction relies on the EFLAGS
> register, it is common for it to be immediately preceded by a `cmpq`
> instruction to set the EFLAGS register.

## 4.5 Shrink the LIf Language

> The `shrink` pass translates some of the language features into
> other features, thereby reducing the kinds of expressions in the
> language.

把 language 当成集合，就是集合论意义上的 shrink。

目前只有两个：

```scheme
(and e1 e2) => (if e1 e2 #f)
(or e1 e2)  => (if e1 #t e2)
```

从语法上看，减少语法就是 macro 展开。

## 4.6 Uniquify Variables

> Add cases to `uniquify-exp` to handle Boolean constants and if
> expressions.

## 4.7 Remove Complex Operands

> The output language of `remove-complex-operands` is `Lmon-if`
> (figure 4.11), the monadic normal form of `LIf`. A Boolean constant
> is an atomic expression, but the if expression is not. All three
> subexpressions of an `if` are allowed to be complex expressions, but
> the operands of the `not` operator and comparison operators must be
> atomic.

> Add cases to the `rco-exp` and `rco-atom` functions for the new
> features in `LIf`. In recursively processing subexpressions, recall
> that you should invoke `rco-atom` when the output needs to be an
> `atm` (as specified in the grammar for `Lmon-if` ) and invoke
> `rco-exp` when the output should be `exp`.

> Regarding if, it is particularly important NOT to replace its
> condition with a temporary variable, because that would interfere
> with the generation of high-quality output in the upcoming
> `explicate-control` pass.

也可以这样看：

- unnest-operand -- remove-complex-operands
- unnest-let -- explicate-control
- unnest-if -- explicate-control

其中 unnest-operand 与 if 无关，
所以这里就是简单地递归下去。

## 4.8 Explicate Control

> Recall that the purpose of `explicate-control` is to make the order
> of evaluation explicit in the syntax of the program. With the
> addition of if, this becomes more interesting.

> The `explicate-control` pass translates from LIf to CIf.

> The main challenge to overcome is that the condition of an if can be
> an arbitrary expression in `LIf`, whereas in `CIf` the condition
> must be a comparison.

明确所要 bridging 的 difference。

> As a motivating example, consider the following program that has an
> if expression nested in the condition of another if:

```scheme
(let ((x (read)))
  (let ((y (read)))
    (if (if (< x 1) (eq? x 0) (eq? x 2))
      (+ y 2)
      (+ y 10))))
```

简单地处理嵌套的 if 表达式，
而编译成下面的汇编是不对的：

```asm
cmpq $1, x
setl %al
movzbq %al, tmp
cmpq $1, tmp
je .then_branch_1
jmp .else_branch_1
```

应该编译成：

```asm
cmpq $1, x
je .then_branch_1
jmp .else_branch_1
```

一个思路是把内部 condition 位置的 if 换出来：

```scheme
(let ((x (read)))
  (let ((y (read)))
    (if (< x 1)
      (if (eq? x 0)
        (+ y 2)
        (+ y 10))
      (if (eq? x 2)
        (+ y 2)
        (+ y 10)))))
```

但是这样会重复代码，所以也是不行的。

TODO 如果先转化成 SSA，
是否可以在 SSA 层面处理这里遇到的问题？

- 我们现在学习的其实是 compile with CPS 的简化版本，
  也就是 Felleisen 提出的 A-normal form。
  而 CPS ANF SSA 这三者是非常类似的，
  但是又有差异的编译技术。

> How can we apply this transformation without duplicating code? In
> other words, how can two different parts of a program refer to one
> piece of code? The answer is that we must move away from abstract
> syntax _trees_ and instead use _graphs_.

需要 graph of basis block 了，
可是在代码中，`block` 这个类型名字被 x86 部分占用了，
而 c 部分原本应该称作 block 的类型，
被命名为了 `seq`（或书中的 `tail`）。

- 这里只是练习代码，可以这样。
  自己实现编译器的时候，
  这种混乱的命名是绝对不允许的。

`explicate-tail`：

- 在进入 let 的 context 时调用 `explicate-assign`
- 在进入 if 的 context 时调用 `explicate-pred`

`explicate-pred` 称作 `explicate-if` 更好，
因为是在 if 的 context 中。

`explicate-assign` 可以继续称作 assign，
因为在 x-lisp 中 assign 来自 body 中的 `=`，
而不是来自 `let`。

> The `explicate-pred` function should match on `cnd` (condition) with
> a case for every kind of expression that can have type Boolean.

所谓 explicate control 中的 control，
就是先执行哪个后执行哪个。
用解释器直接解释 expression 的时候，
这个执行顺序是没有在 expression 中明显表达出来的。
如果顺着解释器可能的执行顺序走一遍，
可以发现记录下来的就是 explicate control 的输出。

注意，由于 `shrink` 会把 `and` 和 `or` 翻译成 `if`，
所以正常的代码中会有很多在 `if` 的 condition 位置出现嵌套的 `if` 的情况。

## 4.9 Select Instructions

新增的情况非常少：

- operator -- not
- operator -- cmp-ops -- eq? lt? lteq? gt? gteq?
- tail in seq -- branch-seq, goto-seq

注意 bool 用 1 和 0 编码。

## 4.10 Register Allocation

`uncover-live` 需要对所有 basis block
按照 control flow graph 做拓扑排序，
然后从后向前逐个处理。

TODO 这里说 control-flow graph 是 multidigraph，
但是为什么我观察是 digraph？

TODO 可否直接用带有副作用的递归函数处理 graph 之间的依赖关系？
因为这与程序语言 module 之间的依赖关系非常相似。

后面的 5.2 所介绍的 dataflow analysis
可以解决带有 loop 的 graph 的问题。

## 4.11 Patch Instructions

新增的 case 是：

- `cmpq` -- 第二个参数不能是 immediate。
- `movzbq` -- dest 参数必须是 register。

## 4.12 Challenge: Optimize Blocks and Remove Jumps
## 4.13 Further Reading

# 5 Loops and Dataflow Analysis

## 5.2 Cyclic Control Flow and Dataflow Analysis

`uncover-live` 需要对所有 basis block 做拓扑排序，
然后才能从后向前处理，而不遇到依赖问题。

但是循环会导致 control flow graph 中有圈。

解决方案是先假设空的 live-after sets
然后在循环中逐步逼近正确答案。

> This approach of iteratively analyzing a control-flow graph is
> applicable to many static analysis problems and goes by the name
> _dataflow analysis_. It was invented by Kildall (1973) in his PhD
> thesis at the University of Washington.

- Kildall, Gary A. 1973.
  “A Unified Approach to Global Program Optimization.”

TODO

# 6 Tuples and Garbage Collection
# 7 Functions

先实现不带 lexical scope 的函数。

## 7.1 The LFun Language

这里表示函数类型的具体语法设计成了 `(type ... -> type)`
而不是 `(-> type ... type)`。

这也是错误的，因为引入脱离前缀表达式的语法是错误的。
尤其是在在教学中，更不应该引入这种语法。

TODO

## 7.4 Reveal Functions and the LFunRef Language

reveal 和 uncover，都是使得语言中某些信息变得明显的 pass 的名字。

> The syntax of `LFun` is inconvenient for purposes of compilation in
> that it conflates the use of function names and local
> variables. This is a problem because we need to compile the use of a
> function name differently from the use of a local variable. In
> particular, we use leaq to convert the function name (a label in
> x86) to an address in a register. Thus, we create a new pass that
> changes function references from `(Var f)` to `(FunRef f n)` where
> `n` is the arity of the function. This pass is named
> `reveal-functions` and the output language is LFunRef.

> Placing this pass after `uniquify` will make sure that there are no
> local variables and functions that share the same name.  The
> `reveal-functions` pass should come before `remove-complex-operands`
> pass because function references should be categorized as complex
> expressions.

# 8 Lexically Scoped Functions
# 9 Dynamic Typing
# 10 Gradual Typing
# 11 Generics
# A Appendix
