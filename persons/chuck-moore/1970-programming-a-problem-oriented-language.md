---
title: Programming a problem-oriented-language
author: Charles H. Moore
year: 1970
---

# 学习动机

[2024-11-07]

在实现 c 版本的 inet 时，
不知道如何设计 rule 编译出来的 bytecode 了。
也许 forth 的 inner interpreter 可以给一些启发。

比如，如何命名这些数据类型？

- word？
- opcode？

我很久之前就读过这本书，但是现在又忘记了。

readonly.link 整理的版本（有些排版没整理完）：
- <https://readonly.link/books/https://books.xieyuheng.com/programming-a-problem-oriented-language/book.json/-/1-introduction.md>

# Preface

> This is an unpublished book I wrote long ago. Just after I'd written
> the first versions of Forth. Perhaps it explains the motivation
> behind Forth. There is some interest in it, and publishing is no
> longer relevant since I can post it on my website.

# 1 Introduction

## 1.1 The Basic Principle

> **Basic Principle: Keep it Simple.**

> As the number of capabilities you add to a program increases, the
> complexity of the program increases exponentially. The problem of
> maintaining compatibility among these capabililties, to say nothing
> of some sort of internal consistency in the program, can easily get
> out of hand.  You can avoid this if you apply the Basic Principle.
> You may be acquainted with an operating system that ignored the
> Basic Principle.

> It is very hard to apply. All the pressures, internal and external,
> conspire to add features to your program. After all, it only takes a
> half-dozen instructions; so why not? The only opposing pressure is
> the Basic Principle, and if you ignore it, there is no opposing
> pressure.

> **Corollary 1: Do Not Speculate!**

> **Corollary 2: Do It Yourself!**

> Before you can write your own subroutine, you have to know how.
> This means, to be practical, that you have written it before; which
> makes it difficult to get started. But give it a try. After writing
> the same subroutine a dozen times on as many computers and
> languages, you'll be pretty good at it. If you don't plan to be
> programming that long, you won't be interested in this book.

> But suppose everyone wrote their own subroutines? Isn't that a step
> backward; away from the millenium when our programs are machine
> independent, when we all write in the same language, maybe even on
> the same computer? Let me take a stand: I can't solve the problems
> of the world. With luck, I can write a good program.

## 1.2 Preview

> I'm going to tell you how to write a program. It is a specific
> program; that is, a program with a specific structure and
> capabilities. In particular, it is a program that can be expanded
> from simple to complex along a well-defined path, to handle a wide
> range of problems, likewise varying from simple to complex. One of
> the problems it considers is exactly the problem of complexity. How
> can you control your program so that it doesn't grow more
> complicated than your application warrants?

"how to write a program. It is a specific program ..."
这里的 a program 其实就是 forth。

> First I'll define "input", and mention some general rules of
> programming that apply to all programs, whether they have input or
> not. Actually we will be almost exclusively concerned with input, so
> I've not much to say about programs lacking input.

这里的带有 "input"，就是 forth 的外部解释器。

> By admitting input, a program acquires a control language by which a
> user can guide the program through a maze of possibilities.
> Naturally, this increases the flexibility of the program, it also
> requires a more complex application to justify it. However it is
> possible to achieve a considerable simplification of the program, by
> recognising that it needs a control language as a tool of
> implementation.

这里说的 "control language" 就像给用户提供 forth words，
让用户能自己通过 compose words 来灵活地解决问题。

> The next step is a problem-oriented-language. By permitting the
> program to dynamically modify its control language, we mark a
> qualitative change in capability. We also change our attention from
> the program to the language it implements. This is an important, and
> dangerous, diversion. For it's easy to lose sight of the problem
> amidst the beauty of the solution.

"dynamically modify its control language"
就是用户能定义新的 forth word。

> In a sense, our program has evolved into a meta-language,
> which describes a language we apply to the application.

# 2 Programs without input

> The simplest possible program is one that has no input. That is a
> somewhat foolish statement, but if you'll give me a chance to
> explain we can establish some useful definitions.

> First consider the word "input". I want to use it in a specific sense:
>
> - Input is information that controls a program.

让用户用调用 C 函数的方式使用程序，看来是不合理的。
但是让用户使用后缀表达式的 word 来使用程序看来是合理的，
比如后缀表达式的计算器。
可以以这种计算器为典型例子，来理解这里提到的 input。

> In particular, I do not consider as input:
>
> - Moving data between media within the computer. For instance,
>   - copying tape onto disk, or disk into core.
> - Reading data into the computer. This is really a transfer between media:
>   - from card to core.

> In order to sharpen your recognition of input, let me describe a
> program that has input. Consider a program that fits a smooth curve
> through measured data points. It needs a lot of information in order
> to run: the number of data points, the spacing between points, the
> number of iterations to perform, perhaps even which function to fit.
> This information might be built into the program; if it is not, it
> must be supplied as input. The measured data itself, the object of
> the entire program, is not input; but must be accompanied by input
> in order to to intelligible.

这个例子非常适合 "The Little Learner"，
因为 learner 就是在 fits curve，
而在 learner 中区分了 parameters 和 arguments，
这里的 "input" 就类似 parameters。
"The measured data itself,
the object of the entire program, is not input;"
那应该就是 arguments 了。

> A program that has no input may be extremely complex. Lacking input
> simply means the program knows what to do without being told.
> That built into the code is all the information needed to run. If
> you are willing to re-compile the program, you can even modify it
> without input.

> But I'll be viewing programs from the input side. I'll be ranking
> programs according to the complexity of their input and I plan to
> demonstrate that a modest increase in the complexity of input can
> provide a substantial decrease in the complexity of the program.

## 2.1 Choosing a language

> ... we won't be talking about problems at the language level.

> I want to talk about problems common to all programs in a
> machine-independent and language-independent manner. I will leave to
> you the details of implementation. I am not going to write a
> program, I am going to show you how to write a program.

> Now let's look at the major failing of higher-level languages.  In
> attempting to achieve machine-independence and to be applicable to a
> wide range of applications, they only give you acess to a fraction
> of the capabilities of your computer. If you compare the number of
> loop control instructions on your computer to the number of loop
> constructs in your language, you'll see what I mean.

批评了高阶语言隐藏了 computer 的能力。

后面还批评了复杂的 calling convention，比如需要保存寄存器之类的。
相比之下简单的 calling convention 就是用一个传递 arguments 的 stack。

> You will have to code in assembler! Not the whole program, if you
> insist, but the important parts that we'll be concentrating on. You
> might be able to do some of these in FORTRAN, but it simply isn't
> worth the effort.

这里说的是 FORTRAN，但是对 C 而言也是如此。
但是在现在这个时代，为了让简化实现，我还是用了 C。

## 2.2 Choosing a computer

> Of course I don't expect that you're in a position to choose a
> computer. Nor am I going to discuss hardware at all. But I do have a
> mental image of the kind of computer, and explaining it may help you
> understand some of my comments.

> Most applications can be programmed very nicely on a small computer:
> say 4K of 16-bit words with a typical instruction set,
> floating-point hardware if needed. If, that is so, the computer is
> augmented with random access secondary memory, which I will call
> disk. The capacity of disk is unimportant, even a small disk
> providing plenty for our purposes, and is determined by the
> application.  However, it is important to be able to copy the disk
> onto another disk, or tape, for back-up. Thus I envisage a small
> computer with 2 secondary memories, and of course a keyboard or
> card-reader and printer or scope for input and output.

> Instead of running applications in serial on a small computer, you
> can run them in parallel on a large one. I see no advantage, for the
> amount of core and disk you can afford to use for a single
> application is about that available on a small computer. You don't
> gain speed, you suffer from a complex operating system, and you have
> a enormous capital investment. But the configuration I have in mind
> remains the same:
>
> **4K of core, secondary memory and input/output device.**

用现在的术语是：

- 4K of RAM
- disk
- input/output device

## 2.3 Arrangement and formatting

> Now I'm going to tell you how to write a program, independent of
> language or computer. Things you ought to be doing already, but
> probably aren't because noone ever told you to. Little things. but
> if you don't do them you won't have a good program; and we're going
> to write a good program.

这里是关于编程风格的讨论。

> Remember the Basic Principle!
>
> **Keep it Simple.**

> Declare all variables. Even in FORTRAN when you don't have to.
> Everyone likes to know what parameters you are using, presumably
> need to use; likes to count them, to see if they could use fewer; is
> annoyed if you slip one in without mentioning it.

> Define everything you can before you reference it. Even in FORTRAN
> when you don't have to. Why not? You don't like to read a program
> backwards either. 'Everything you can' means everything except
> forward jumps. You better not have many forward jumps.

> Make the variables as GLOBAL as possible. Why not? You can save some
> space and clarify your requirements. For instance, how many Is, Js
> and Ks do you need? In most cases a single copy in COMMON would
> suffice (you have to declare them, remember, and may as well put
> them in COMMON); you can redefine it locally if you must; and it is
> of interest that you must.

我很少使用 global variables。
在看似需要 global variables 的地方，
我总是通过给所有函数增加一个变量来解决。
但是这会让函数的参数变多。
也许我应该尝在某些时候试使用 global variables 试试。

对于单线程的计算来说，
全局变量其实是非常本质的，
比如说，看似在写 C 代码时我没有用全局变量，
但是其实我用了 C 的 malloc，
而 malloc 一定是用很多全局变量来实现的。

想要避免使用 malloc，就要自己使用全局变量。
比如在 data-oriented programming 中，
我们可以为每个 struct 设置多个全局的 allocator，
每个 allocator 有自己的 managed memory。

> Indent! High-level languages, even modern assemblers, fail to insist
> that you start in column x. But you do! The unbelievable appeal of a
> straight left margin! Paper is 2-dimensional. Use it! If you indent
> all statements inside a loop, it's obvious at a glance the extent of
> the loop. If you indent conditionally executed statements you'll
> find that nested conditions sort themselves out - automatically. If
> you indent little statements you wish you didn't have to include (I
> = I) you'll find they intrude less as you glance through the
> listing. Always indent the same amount, 3 spaces/level is good. Be
> consistant and be accurate.  Sloppy indenting is obvious.

"The unbelievable appeal of a straight left margin!"

所以也许这种 single-char-rune 外加 2-space indentation，
是不错的设计。

```inet
* (zero) -- value!
* (add1) prev -- value!
* (add) target! addend -- result

! (zero)-(add)
  (add)-addend result-(add)

! (add1)-(add)
  (add)-addend (add1)-prev add
  add1 result-(add)
```

```inet
* (null) -- value!
* (cons) head tail -- value!
* (append) target! rest -- result

! (null)-(append)
  (append)-rest result-(append)

! (cons)-(append)
  (append)-rest (cons)-tail append
  (cons)-head cons result-(append)
```

## 2.4 Mnemonics

> You will find as you read, that I have strong opinions on some
> subjects and no opinion of others. Actually I have strong opinions
> on all, but sometimes I can't make up my mind which to express.
> Fortunately it leaves you some decisions to make for yourself.

这里是延续对编程风格的讨论，
并且是最重要的关于命名的风格。

> Use words with mnemonic value. Unfortunately what is mnemonic to you
> may not be mnemonic to me; and I'm the one who judges. Also
> unfortunately, mnemonic words tend to be long, which conflicts with:
> Use short words. You don't want to type long words, and I don't want
> to read them. In COBOL this means avoid dashes and avoid
> qualification, though both can be useful upon occasion.

"dashes" 指 lisp-case naming convention for multi-part word；
"qualification" 应该是指 `module::name` 或 `module.name` 之类的。

> So let me suggest a compromise: abbreviate in some consistant
> fashion and stick to your own rules. I can probably figure out the
> rules you're using. You might even mention them in a comment.

用 dashes 和 qualification，
可以避免让写程序的人过度思考如何简化命名，
也可以让读程序的人避免浪费时间去猜测简化命名的方式。
比如 C 的标准库中，为了避免 dashes 而使用了很多奇怪的简化命名。
所以我还是选择用长的名字。

> Use words with the correct grammatical connotations: nouns for
> variables, verbs for subroutines, adjectives for ... Do not use
> clever words (GO TO HELL). Their cuteness wears off very fast and
> their mnemonic value is too subjective. Besides they offer an
> unwanted insight into your personality.

我早期在写 scheme 代码时，
就经常设计自己的奇怪 macro，
其实就是犯了上面提到的错误。

> Use comments sparingly! (I bet that's welcome.) Remember that
> program you looked through - the one with all the comments? How
> helpful were all those comments? How soon did you quit reading them?
> Programs are self-documenting, even assembler programs, with a
> modicum of help from mnemonics.

> What comments should say is what the program is doing. I have to
> figure out how it's doing it from the instructions anyway. A comment
> like this is welcome:
>
> - COMMENT SEARCH FOR DAMAGED SHIPMENTS

> Mnemonics apply to variables and labels. Where possible you should
> apply them to registers also. You may do well to assign several
> names to the same entity, to indicate its current use. However,
> don't waste effort naming things that don't need names. If you need
> a counter, use I, J, K; to assign a big name (EXC-CNTR) to an
> insignificant variable is no help.

## 2.5 Routines and subroutines

> There are two words I need to establish precise definitions for: A
> _subroutine_ is a set of instructions that return from whence they
> came. A _routine_ is a set of instructions that return to some
> standard place.

> To put it another way, you _jump_ to a routine, you _call_ a
> subroutine.  The difference is retained in higher-level languages:
> GO TO versus CALL or ENTER.

这一节还确立我们将要使用的 calling convention：

- 用 return-stack 处理递归的 subroutine call。
- 用 value-stack 来 pass parameters。

# 3 Programs with input

> A program without input is a program with a single task. A program
> with input may have many tasks, which it will perform as directed by
> its input. Thus I consider input to be control information, and the
> control information to define a control language.

在为了探索某个计算模型，而实现程序语言时，
最重要的 input 就是 making definition。
我需要一个灵活的 meta language 来做各种的 definition。
后面的章节应该会讨论这种 input。

> To set the stage, let me briefly outline how our program must
> operate.  You are sitting at a keyboard typing input. You type a
> string of characters that the computer breaks into words. It finds
> each word in a dictionary, and executes the code indicated by the
> dictionary entry, perhaps using parameters also supplied by the
> entry. The process of reading words, identifying them and executing
> code for them is certainly not unusual. I am simply trying to
> systematize the process, to extract the inevitable functions and see
> that they are efficiently performed.

"The process of reading words,
identifying them and executing code for
them is certainly not unusual."

- pattern match on sumtype
- dispatch on arguments
- route by request

和上面所说的 process 都是类似的。

## 3.1 Nouns and verbs

> I've mentioned the dictionary and we'll soon examine the details
> required to implement it. But first I'd like to talk a bit about
> individual entries to try and give you a feel for what we're doing.

这里 "individual entries" 中的 "entry" 指字典中的条目
-- word entries in dictionary。

- 注意，entry 和 item 这两个词，
  经常被用在 array 和 record 和 map 相关的 API 中，
  比如 javascript 用 entry，python 用 item。

  因此在实现 froth 的时候，
  entry 可能是一个意义很一般的词，
  没法被直接用来代表 dictionary entry。
  可能需要加上前缀 "word entry" 或 "dictionary entry"，
  或改成别的词，比如 "definition"。

forth 可以在编译时期就找到
word 所对应的 dictionary 中的地址，
在运行是就不用查找地址了。
但是 inet 从 active edge 到 rule 的查找，
好象是没法避免的，因为生成和消除 active edge 的过程是动态的。
相比之下 forth 的一个 word definition 是静态的。

- 在实现 inet 时，一定要优化这个动态查询的时间。
  可以通过使用嵌套的 dictionary 来实现。

> We're going to read words from your input, find them in the
> dictionary, and execute their code. A particular kind of word is a
> literal, We won't find such words in the dictionary, but we can
> identify them by their appearance.

> Each word performs its specific, limited function; independently of
> any other word. Yet the combination of words achieves something
> useful.

> This is basically the value of our program. It lets us combine
> simple operations in a flexible way to accomplish a task.

monoid composition，可以非常方便地 refactoring。

数学世界中的 factoring 方式有很多。
下面对 noun 和 verb 的讨论，
可以看成是人类语言带来的，
对如何 factoring 的一种指导。

> Let's look more closely at the words we used above. They fall into 2
> distinct classes; English even provides names for them:
>
> - Nouns place arguments onto the stack.
> - Verbs operate upon arguments on the stack.

下面描述一些系统的「不变量」来帮助人们认知系统。

> All words cause code to be executed. However in the case of nouns,
> the code does very little: simply place a number on the stack. Verbs
> are considerably more varied in their effects. They may do as little
> as add 2 arguments, or as much as type out a result -- which
> requires a great deal of code.

> In effect, nouns place arguments onto the stack in anticipation of
> verbs that will act upon them. The word anticipation is a good
> one. In order to keep our verbs simple, we promise that their
> arguments are available. We could define a verb that reads the next
> word and uses it as an argument; but in general we don't. It is not
> the business of a verb to provide its own arguments; we use nouns to
> provide arguments before we execute the verb. In fact, this
> substantially simplifies our program.

下面对 word entries 的分类，
代表了人们对类型论的最原始需求。

> We can extend the characterization of entries a little further. Verbs
> have different numbers of arguments:
>
> - Unary verbs modify the number on the stack.
> - Binary verbs combine 2 arguments to leave a single result.

> Another way of distinguishing verbs is:
>
> - Destructive verb removes its arguments from the stack.
> - Non-destructive verb leaves its arguments on the stack.

forth 处理常量和变量的方式：

> Literals are nouns. We can define other words as nouns; words that
> use their parameter field to place numbers onto the stack:
>
> - Constants place the contents of their parameter field onto the stack.
> - Variables place the address of their parameter field onto the stack.

这里可以发现一些关于如何实现的信息：

- 每个 word 从 dictionary 里找到 word entry。
- 有不同类型的 word entry。
- 不同类型的 word entry 下保存的数据不一样。
  保存数据的地方，被作者称作 "parameter field"。

这里介绍了两类 word entry，
而最重要的一类 word entry 是代表函数的 entry。
不同的 forth threaded code 方式，
就对应了函数 entry 的不同实现方式：

- 函数保存 array of opcode。
- 函数保存 array of address of word entry。

> Rather than try to distinguish function by context, as compilers do,
> we shall define 2 verbs that act upon variables:
>
> - @ replace the address on the stack with its contents.
> - = Store into the address on the stack, the value just beneath it on the stack.

上面提到 "distinguish function by context"，
forth 所做的也可以理解为 making context explicit。

## 3.2 Control loop

这里的 Control loop 指的是 forth 的 outer interpreter。

之前提到的 subroutine 和 routine 的区别，
就是 routine 可以用 RETURN jump 到这个 outer interpreter。

错误处理也会 jump 到这个 outer interpreter。

## 3.3 Word subroutine

> What is a word?
> A word is a string of characters bounded by spaces.

> The order of the words we read is significant, though their position
> is not.  We lose, however, the ability to leave a field empty, since
> we cannot recognise an empty word.

与 applicative 语法相比，
concatenative 语法没法区分 `f(x, y)` 和 `f(x)`，
因此没法做自动的 currying
（可以专门定义一些 word 来手动 currying）。

> All our data must be explicit, which is probably a good idea but a
> slow one to learn. Decide now that you will not specify input
> conventions that have optional parameters.

也没法像 `f(x)` 式的语法一样，带有可选参数。

### 3.3.1 Message I/O

> The WORD subroutine presumably examines input characters.
> Where does it get these characters?

"The WORD subroutine" 就是 forth outer interpreter 的 lexer。

> I'm going to assume that you have a keyboard to type input.

> In any case we may want to examine each character more than once, so
> we want buffered input. Even if you can process characters as they
> arrive, don't. Store them into a message buffer.

### 3.3.2 Moving characters

讨论 WORD subroutine 的实现方式。
我应该不会这样实现，
而是先用 lexer 把代码处理成 token，
然后以 token 为基础，实现 outer interpreter。

## 3.4 Decimal conversion

> After isolating and aligning a word from the input string, your
> control loop searches the dictionary for it. If it isn't in the
> dictionary, it might be a number. A number is a special kind of word
> that doesn't need a dictionary entry; by examining the word itself
> we can decide what to do with it. The code executed for a number
> will place the binary representation of the number onto the stack.

称 number 这种 word 为 literal 很合适。
尽管和 scheme 的 literal expression 不同。

### 3.4.1 Numbers

> It is very hard to state exactly what is a number and what is
> not. You will have to write a NUMBER subroutine to convert numbers
> to binary, and this subroutine is the definition of a number. If it
> can convert a word to binary, that word is a number; otherwise not.

> One of your major tasks will be to decide what kinds of numbers you
> need for your application, how you will format them, and how you
> will convert them. Each kind of number must be uniquely identifiable
> by the NUMBER subroutine, and for each you must provide an output
> conversion routine.

我选择直接用 C 的 number 相关的传统，
因为 C 的传统已经很流行了。

### 3.4.2 Input conversion

这一节讨论如何 parse number。

在我的实现中，lexer 会处理 number 的 input conversion。

### 3.4.3 Output conversion

这一节讨论如何 format number。

在我的实现中，我会给每一类数实现所需要的 format 函数。

## 3.5 Stacks

> We will be using several push-down stacks and I want to make sure
> you can implement them. A push-down stack operates in a last-in
> first-out fashion. It is composed of an array and a pointer. The
> pointer identifies the last word placed in the array.

## 3.5.1 Return stack

> This stack stores return information. One use is to store the return
> address for subroutines, when subroutine calls use an index
> register.  The last-in first-out nature of a stack is exactly the
> behavior required for nested subroutine calls. We will later
> encounter several other kinds of return information that can be
> stored in the same stack.

> It is important not to attempt to combine the return stack and the
> parameter stack. They are not synchronized.

其实 C 的 calling convention 确实是
combine the return stack and the parameter stack，
并且这种设计对于实现局部变量而言很有用，
可以在实现局部的 memory allocator，而不用垃圾回收器。

这么看来 C 的设计也是很好的。

设想实现一个类似 C 的语言，
没有 malloc，通过额外的 pointer 参数来实现返回值。
如果给函数标记出来 input 和 output 参数，
就可以实现下面的效果：

```c
infer: (ctx: ctx_t, exp: exp_t) -> (type: value_t);

// given:
// ctx: ctx_t
// exp: exp_t

type: value_t = infer(ctx, exp);

// the same as:

type: value_t;
infer(ctx, exp, type);
```

好像甚至不需要 `*` 和 `&`，
所有的参数都是 reference 而不是 value。

在设计 propagator 和 inet 的函数作用语义时，
我都用到了类似的技巧。

## 3.5.2 Parameter stack

> This stack is the one I intend when I say simply stack. Numbers,
> constants, variables are all placed on this stack, as will be
> discussed later. This stack is used to pass parameters among
> routines. Each routine can find its arguments there, regardless of
> how many other parameters are present, or how long ago they were
> placed there.

> We need some terminology:
>
> - You _place_ a word _onto_ then stack, thereby increasing its size.
> - You _drop_ a word _from_ the stack, thereby decreasing its size.
> - The word on top of the stack is called the _top_ word.
> - The word immediately below the top of the stack is called the _lower_ word.

这里的 word 代表 machine word，
比如对 64bit machine 来说，一个 word 就是 64bit。
这与 C 的混合 stack 是很不同的，
C 的 stack 没有「只能保存 machine word」的限制，
而是可以当作一种 memory management 的方式。

## 3.6 Dictionary

> Every program with input must have a dictionary. Many programs
> without input have dictionaries. However these are often not
> recognised as such. A common 'casual' dictionary is a series of IF
> ...  ELSE IF ... ELSE IF ... statements, or their equivalent. Indeed
> this is a reasonable implementation if the dictionary is small (8
> entries) and non-expandable.

if else 和 dictionary 的共同点在于，
它们都用来形成 dispatching 或者说 branching。

> It is important to acknowledge the function and existence of a
> dictionary, to concentrate it in a single place and to standardize
> the format of entries.

> The most important property of an entry is one that is usually
> overlooked. Each entry should identify a routine that is to be
> executed.

指下一节所说的 `on_call` callback。

### 3.6.1 Entry format

讨论了不同的实现 dictionary entry 的方式，
并且选择了用 linked list of entry 来实现。

> An entry has 4 fields: the word being defined, the code to be
> executed, a link to the next entry and parameters. Each of these
> warrants discussion.

在 C 中，我们可以用 hash table 来实现 dictionary。

> The code field should contain the address of a routine rather than
> an index to a table or other abbreviation. Program efficiency
> depends strongly on how long it takes to get to the code once a
> entry is identified.

code field 虽然指向一个 routine，
但是其实代表了这个 entry 的类型。

在 C 中，指向一个 routine 的 code field
相当于 entry 带有一个 `on_call` callback。
也可以用让 entry 带有一个 `kind`，
然后写函数来 dispatch 这个 `kind`。
我觉得使用 `on_call` callback 好一点。

> The parameter field will typically contain 4 kinds of information:

> - A number, constant or variable, of variable size. The nature of
>   the number is determined by the code it executes.

> - Space in which numbers will be stored - an array. The size of the
>   array may be a parameter, or may be implicit in the code executed.

> - A definition: an array of dictionary entries representing
>   virtual-computer instructions.

这一条描述的就是最经典的 indirected threaded code interpreter。
在 C 实现中与其用 dictionary entry，
也许用一个明显的 instruction 类型比较好，
此时 dictionary entry 对应 call 这个 instruction。

> - Machine instructions: code compiled by your program which is
>   itself executed for this entry. Such data must probably be aligned
>   on word boundary, the other need not.

如果使用了 instruction，这一条还是对应 call，
只不过被 call 的 definition 是 primitive definition。

### 3.6.2 Search strategies

> One basic principle applies to dictionary search: it must be
> backwards -- from latest to oldest entries. You have perhaps noticed
> that the dictionary is not arranged in any order (ie. alphabetical)
> other than that in which entries are made. This permits the same
> word to be re-defined, and the latest meaning to be obtained. There
> is no trade-off valuable enough to compromise this property.

我选择用 hash table 来实现 dictionary。
相比 linked list 更复杂了，但是搜索的速度快了。

这里还讨论了一个优化用 linked list 实现的 dictionary 的方式，
那就是用多个 linked list，
按照 word 的 hash 来决定这个 word 属于哪个 list。

把这种优化推广到极限其实就是 hash table。

> However, search time is not a important consideration, and I advise
> against multiple chains unless the dictionary is very large
> (hundreds of entries).

keep it simple。

### 3.6.3 Initialization

> The dictionary is built into your program and is presumably
> initialized by your compiler. This is centainly true if you have
> fixed-size entries.  Variable-sized entries must be linked together,
> however, and this can be beyond the ability of your compiler,
> especially if you have multiple chains.

> Other things may need initializing, particularly any registers that
> are assigned specific tasks. All such duties should be concentrated
> in this one place.

## 3.7 Control language, example

> Applications tend to be complicated before they become interesting.
> But here's a fairly common problem that shows off a control language
> to advantage. Implementation would be tricky, execution woud be
> inefficient; but the program would be simple, and its application
> flexible.

> The problem is to examine a sequential file, select certain records,
> sort them, and list them - in many different ways. Suppose these
> variables define the fields in the record:

```
NAME AGE SALARY DEPT JOB SENIORITY
```

> Let's define these verbs:

```
LIST SORT EQUAL GREATER LESS
```

> Each acts upon the temporary file produced by the previous, in
> accordance with the following examples:

> List in alphabetical order all employees in dept 6:

```
6 DEPT EQUAL NAME SORT LIST
```

> List twice, by seniority, all employees holding job 17 in dept 3:

```
17 JOB EQUAL 3 DEPT EQUAL SENIORITY SORT LIST LIST
```

> List, by age, all employees whose salary is greater than $10,000;
> and identify those whose seniority is less than 3:

```
10000 SALARY GREATER AGE SORT LIST 3 SENIORITY LESS LIST
```

> Several comments seem indicated. We can apply a logical "and" by
> using several select verbs in sequence; we cannot use a logical "or".

> Actually many other capabilities could be provided, including the
> ability to locate specific records and modify them. But rather than
> design a particular application, I just want to show how nouns and
> verbs combine to provide great flexibility with a simple program.
> Notice how even such a simple example uses all our facilities: the
> word subroutine, the number subroutine, the dictionary, the stack.
> We're not speculating, we are providing essential code.

假设所操作的是 array of JSON record，
让我来实现，函数的命名可能会是这样：

```cicada
6 :dept filter-equal :name sort-by print
17 :job filter-equal 3 :dept filter-equal :seniority sort-by print print
10000 :salary filter-greater :age sort-by print 3 :seniority filter-less print
```

# 4 Programs that grow

> So far our dictionary has been static. It contains all the entries
> you need - placed there when the program was compiled. This need not
> be.  We can define entries that will cause additional entries to be
> made and deleted.

> In fact, the purpose of your program undergoes a gradual but
> important change.  You started with a program that controlled an
> application. You now have a program that provides the capability to
> control an application.  In effect, you have moved up a level from
> language to meta-language.  This is an extremely important step.  It
> may not be productive. It leads you from talking _to_ your
> application to talking _about_ your application.

> Another way of viewing the transition is the entries in your
> dictionary. At first they were words that executed pieces of code
> that constituted your application program. A purely control
> function. Now they tend to become words that let you construct your
> application program. They constsitute a
> **problem-oriented-language**.

problem-oriented-language 标题出现！

> I suspect any application of sufficient complexity, and surely any
> application of any generality, must develop a specialized
> language. Not a control language, but a descriptive language.

要能够扩展 dictionary entry 的种类，
因为 entry 所代表的 definition
就是 descriptive language 中的 description。

> Some examples: A simulator does not want a control language. It is
> important to be able to describe with great facility the system
> being simulated. A linear-programming problem needs a language that
> can describe the problem. A compiler actually provides a descriptive
> language for use with the programs it compiles. A compiler-compiler
> describes compilers. What is a compile-compiler that can execute the
> compiler it describes and in turn execute the program it compiled?
> That is the question!

> Let me now assume that you have a problem that qualifies for a
> descriptive language. What dictionary entries do you need?

## 4.1 Adding dictionary entries

> Let us now assume that you want to expand your dictionary; that you
> have a sufficiently complex application to justify a specialized
> language. How do you make a dictionary entry?

> Recall the control loop: it reads a word and searches the
> dictionary. If you want to define a word, you must define an entry
> that will read the next word and use it before RETURNing to the
> control loop.  Let us call such an entry a _defining_ entry, its
> purpose is to define the next word.

> In principle we only need one defining entry, but we must supply as
> a parameter the address of the code to be executed for the entry it
> defines.

这个 code 代表了 entry 的类型。

> Remember that 4 fields are required for each entry:
> the word, its code address, a link, and (optionally) parameters.
> - The word we obtain from the word subroutine;
> - the link we construct;
> - the parameters we take from the stack.

> We could also take the address from the stack, but it's more
> convenient to have a separate defining word for each kind of entry
> to be constructed. That is, to have a separate defining entry for
> each address we need, that provides the address from its parameter
> field.

也就是说：

```
<parameter> ... <defining-word> <word> ...
```

如果 code 也 stack 里获得，就是：

```
<code> <parameter> ... define <word> ...
```

但是这是不合理的，因为不同的 defining word
可能对 body 的处理方式不同。

因此，其实不只是 convenient -- "but it's more convenient to have a
separate defining word for each kind of entry to be constructed."
而是必须要有不同的 defining word 来处理这里的差异。

例如：

```
<value> constant <name>
variable <name>
function <name> ... end
```

如果严格按照从 stack 中取 (optionally) parameters，
假设 `{ ... }` 可以将 lambda 放到 stack 里，
那么 function 应该是：

```
{ ... } function <name>
```

但是这也是不合理的，
因为这种方式局限了 defining word 读取 body 的方式。
defining word 读取 body 的方式不同，
让我们可以灵活地扩展语法。

这一节还提到了一个 ENTRY subroutine，
可以用来在 forth 中通过定义新的 defining word
来扩展 definition 的种类。

但是在 C 实现中，这个扩展能力可能会被 C 保留，
而不下放到 forth 中，因为在 forth 中不方便定义新的 code。

- 用汇编实现的 forth，如果定义了自己的汇编器的话，
  就可以在 forth 中定义 code。

- 是否可以设计一个自己的 VM，
  然后用这个 VM 的汇编来实现 forth 呢？
  这样就即获得了 forth 的灵活性，
  也获得了 VM 的可移植性。

  如果要实现 forth over portable VM，
  这个 VM 可以是没有 tagged value 的，
  这样可以保持 VM 的设计简单。

  如果 VM 本身就是 stack-based，
  会对上层实现的 forth 有什么影响？
  是否此时应该放弃 stack-based VM，
  而使用 register-based VM？

作者还列举了一些 defining words:

```
0 integer i
1. real x
8 array temp
0 8 index j
3 vector x 3 vector y 9 vector z
remember here
```

关于 defining word 的命名，
为了不占变量的名字，
也许应该加上 `define` 前缀：

```
<value> define-constant <name>
define-variable <name>
define <name> ... end
```

没有前缀就是 `define-function`。

给作者的例子加上 `define` 前缀看起来也好一些：

```
0 define-integer i
1. define-real x
8 define-array temp
0 8 define-index j
3 define-vector x 3 define-vector y 9 define-vector z
define-undo-point here
```

## 4.2 Deleting entries

如果用 hash table 而不用 linked-list 来实现 dictionary，
可能要用额外的技巧来实现 deleting。
或者直接不允许 redefine 和 deleting。

> There is only one feasible way to delete entries.
> That is to delete all entries after a certain point.

delete 某个 entry，必须同时 delete 所有在这个 entry 之后定义的 entries。
这不是因为 linked-list 和内存管理的问题，
而是依赖关系的问题，因为后面的 entries 可能会依赖前面的 entry。

- 尽管这里的依赖关系是不精确的，
  而是通过定义时间推导出来的保守估计。

> Deleting trailing entries is a completely satisfactory solution.
> You'll find that, in practice, you add a bunch of entries; find a
> problem; delete those entries; fix the problem; and reenter all the
> entries. Or you fill your dictionary for one application; clear it;
> and re-fill with another application.

## 4.3 Operations

> Recall that the stack is where arguments are found.  There are some
> words you may want to define to provide arithmetic capabilities.
> They are of little value to a control language, but essential to add
> power to it.

在定义数字相关的函数时，我将不用符号而是用单词，比如 `add`。
并且我需要加上类型前缀，比如 `int-add`。
不带前缀的名字应该代表 generic function。
generic function 在运行时 dispatch to handler by predicates，
注意，因为 concatenative 语法中函数的参数不带括号，
因此 generic function 必须有固定的 arity。

> Your stack must have a fixed word-length. However the operations
> mentioned above might apply to several kinds of numbers: integers,
> fixed-point fractions, floating-point fractions, double-precision
> fractions, complex numbers, vectors of the above kinds. The truth
> values are only 1 bit. Clearly, the stack must be able to hold the
> largest number you expect to use. Less clear is how you should
> distinguish among various kinds of numbers.  One way is to define
> separate operations for each kind of number:
>
> - `+` integer and fixed-point add (they are the same).
> - `+F` floating-point add.
> - `+D` double-precision add.

> Another is to make a stack entry long enough to contain a code
> identifying the kind of number. This makes the code defining each
> operation more elaborate and raises the problem of illegal arguments.

这里甚至讨论的 tagged value 的方案，
但是 tag 是在 machine word 之外的；
而 scheme 之类的动态类型语言，
tag 是在 machine word 之内的。

> I recommend not checking arguments and defining separate
> operations, for reasons of simplicity. Actually, you are working with
> one kind of number at a time and the problem may never arise.

我们可以用 generic function 来处理这里的问题。
不只是为了简化命名，generic function 本身的表达能力很强，
可以让代码更容易扩展。

> Do not bother with mixed-mode arithmetic. You _never need_ it, and it's
> not convenient often enough to be worth the great bother. With
> multiple word numbers (complex, double-precision) you may put the
> address of the number on the stack. However, this leads to 3-address
> operations with the result generally replacing one of the arguments.
> And this, in turn, leads to complications about constants.

用 3-address operation 来实现 pass-by-reference 的 binary function，
在 C 里是常用的，原来在 forth 里也可以用。

但是，有两点会导致用起来不如 C-like 语言方便：

- 由于 concatenative 语法不能对 overload over arity。
- 由于没有像 C 一样的 stack-based memory management，
  不能像 C 一样使用 local variable。
  - 这一点也许可以通过在 return stack 的 frame 上做文章来解决。

> In general, the number of things you might do with numbers increases
> indefinitely.  Many of these are mutually incompatible.
> Basic Principle!

## 4.4 Definition entries

> I must now describe an entry more complicated than any so far,
> though not the most complicated that you'll see. It is also
> exceptional in that it's not optional. For this ability is required
> for any effective application language: to be able to define one
> word in terms of others.  To abbreviate, if you will. You recall
> that I characterised words as being simple in themselves, but
> powerful in combination. Well here is a way to combine words.

> A definition consists of a defining entry ":" followed by a series
> of words terminated by ";". The intention is that the word defined
> by ":" has the meaning expressed by the words that follow. For
> example:

```
: ABS DUP 0 LESS IF MINUS THEN ;
```

我选择不用任何标点，因此代码是：

```
define abs dup 0 less if minus then end
```

带上缩进：

```
define abs
  dup 0 less if
    minus
  then
end
```

> You may consider this a rather clumsy definition of ABS. Especially
> since there is an instruction on your computer that does exactly
> that.  you're quite right, definitions tend to be clumsy. But they
> let us use words that we hadn't the foresight to provide entries
> for. Given certain basic words we can construct any entry we
> need. Definitions provide a succinct distinction betwen a control
> language and an application language: The control language must have
> all its capabilities built in; the application language can
> construct those capabilities it needs.

作者只管这种 word entry 叫做 definition，

但是我想用 "definition" 代替 "entry"，
并且把这种 definition 叫做 "function definition"
或者 "program definition"。

> Another viewpoint is concealed in an abbreviation I use: I speak of
> "executing a word", when I really mean executing the code associated
> with the word. Or even more precisely, executing the code whose
> address is stored in the dictionary entry for the word. The
> abbreviation is not only convenient, it suggests that a word is an
> instruction that can be executed. And indeed, it is helpful to think
> of a word as an instruction: an instruction for a computer that is
> being simulated by our real computer. Let's call that imaginary
> computer the "virtual computer". Thus when you type words you are
> presenting instructions to the virtual computer. The control loop
> becomes the instruction fetch circuitry of the virtual computer.

> If we extend this analogy to definitions, a definition becomes a
> subroutine for the virtual computer. And the process of defining a
> definition is equivalent to compiling this subroutine.

作者对 "definition" 的理解是 "defining one word in terms of others"，
但是其实 "defining one word by a primitive function"
或者 "defining one word by a constant value"
或者 "defining one word as a variable"
也都可以以算是 definition。

> Definitions are extremely powerful. Why, is hard to explain, hard even
> to comprehend. Their value is best appreciated by hindsight. You
> complete a ludicrously simple implementation of an application,
> discover that you used a dozen definitions and nested them 8 deep.
> The definitions appear responsible for the simplicity.

我觉得是 "easy to factor" 的力量。

### 4.4.1 Defining a definition

> The defining entry ":" acts just like any other. It passes the
> address EXECUTE to the ENTRY subroutine. I'll discuss that code in
> the next section.

> It then sets a switch STATE. The control loop must be changed to
> test STATE: if it is 0, words are executed as I've already
> described; if it is 1, words are compiled.

这里的全局 STATE 是我想要避免的。
全局 STATE 导致每个 defining word 处理 compilation 的方式是固定的，

> To compile a word is simple. After finding it in the dictionary, you
> have the address of its dictionary entry. Deposit this address in the
> parameter field.

我需要让每个 compiling word 都有机会处理不同的语法。

> Before showing how to compile a number, let me define
> pseudo-entries. A pseudo-entry is a dictionary entry that is not in
> the dictionary. That is, it has the format of an entry but it is not
> linked to other entries. Thus it would never be found during a
> dictionary search.

> As you've probably guessed, in order to compile a literal you
> compile a pseudo-entry. You then follow it by the number itself;
> that is, you compile the number also. The result is a double-length
> virtual-computer instruction.  The code executed for the
> pseudo-entry must fetch the number and place it onto the stack. Thus
> literals that are compiled have the same effect, when executed, as
> if they were executed immediately.

我用一个独立的 operation 类型来给 entry 加一层 indirect，
而不是直接在函数体内保存 entry 的地址。

- 我用 "operation" 而不用 "instruction"，
  因为 "operation" 有一个简单好认的缩写 "op"，
  而 "instruction" 没有什么好的缩写。

这一节还介绍了用 `:R` 来做递归定义，
保持 `:` 简单。

### 4.4.2 Executing a definition

我没看懂这一节。

### 4.4.3 Conditions

我用与 jump 相关的 operations，
外加 `define` 识别 if else then 来实现 condition。

TODO

### 4.4.4 Loops

TODO

### 4.4.5 Implementation

TODO

## 4.5 Code entries

> I've explained definitions and how they, in effect, compile
> instructions for the virtual-computer. What about compiling code for
> your real computer then? Of course you can. But you probably won't.

> The Basic Principle intrudes. If you add code entries to your
> program, you add enormous power and flexibility. Anything your
> computer can do, any instructions it has, any tricks you can play
> with its hardware are at you fingertips. This is fine, but you
> rarely need such power.  And the cost is appreciable. You'll need
> many entries (say 10) to provide a useful compiler; plus all the
> instruction mnemonics.  Moreover you'll have to design an
> application language directed at the problem of compiling code.

从这本书的开始就有强调，
我们的目标写应用，而不是实现语言。
需要实现一个语言是在思考如何写好应用的过程中，
用逻辑推导出的结果。

与其让语言更强大，不如先去写好应用。

TODO

# 5 Programs with memory

TODO

# 6 Programs with output

TODO

# 7 Programs that share

TODO

# 8 Programs that think

TODO

# 9 Programs that bootstrap

TODO
