---
title: PROGRAMMING A PROBLEM-ORIENTED-LANGUAGE
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

确立我们将要使用的 calling convention：

- 用 return-stack 处理递归的 subroutine call。
- 用 value-stack 来 pass parameters。

# 3 Programs with input

> A program without input is a program with a single task. A program
> with input may have many tasks, which it will perform as directed by
> its input. Thus I consider input to be control information, and the
> control information to define a control language.

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

forth 可以在编译时期就找到
word 所对应的 dictionary 中的地址，
在运行是就不用查找地址了。
但是 inet 从 active edge 到 rule 的查找，
好象是没法避免的，因为生成和消除 active edge 的过程是动态的。
相比之下 forth 的一个 word definition 是静态的。

> We're going to read words from your input, find them in the
> dictionary, and execute their code. A particular kind of word is a
> literal, We won't find such words in the dictionary, but we can
> identify them by their appearance.

> Each word performs its specific, limited function; independently of
> any other word. Yet the combination of words achieves something
> useful.

> This is basically the value of our program. It lets us combine
> simple operations in a flexible way to accomplish a task.

monoid composition -- 可以自由的 refactor。

下面对 noun 和 verb 的讨论是关于如何 refactor 的。

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

没法区分 `f(x, y)` 和 `f(x)`，
因此没法做自动的 currying。

> All our data must be explicit, which is probably a good idea but a
> slow one to learn. Decide now that you will not specify input
> conventions that have optional parameters.

也没法像 `f(x)` 式的语法一样，带有可选参数。

### 3.3.1 Message I/O

> The WORD subroutine presumably examines input characters.
> Where does it get these characters?

> I'm going to assume that you have a keyboard to type input.

> In any case we may want to examine each character more than once, so
> we want buffered input. Even if you can process characters as they
> arrive, don't. Store them into a message buffer.

### 3.3.2 Moving characters

TODO

## 3.6 Dictionary

> Every program with input must have a dictionary. Many programs
> without input have dictionaries. However these are often not
> recognised as such. A common 'casual' dictionary is a series of IF
> ...  ELSE IF ... ELSE IF ... statements, or their equivalent. Indeed
> this is a reasonable implementation if the dictionary is small (8
> entries) and non-expandable.

> The most important property of an entry is one that is usually
> overlooked. Each entry should identify a routine that is to be
> executed.

> An entry has 4 fields: the word being defined, the code to be
> executed, a link to the next entry and parameters. Each of these
> warrants discussion.

> The code field should contain the address of a routine rather than
> an index to a table or other abbreviation. Program efficiency
> depends strongly on how long it takes to get to the code once a
> entry is identified.

code field 虽然指向一个 routine，
但是其实代表了这个 entry 的类型。

> The parameter field will typically contain 4 kinds of information:
>
> - A number, constant or variable, of variable size. The nature of
>   the number is determined by the code it executes.
>
> - Space in which numbers will be stored - an array. The size of the
>   array may be a parameter, or may be implicit in the code executed.
>
> - A definition: an array of dictionary entries representing
>   virtual-computer instructions.
>
> - Machine instructions: code compiled by your program which is
>   itself executed for this entry. Such data must probably be aligned
>   on word boundary, the other need not.

最重要的是 "a definition" that
"representing virtual-computer instructions"。

### 3.6.2 Search strategies

TODO

# 4 Programs that grow

TODO

## 4.4 Definition entries

TODO

### 4.4.1 Defining a definition

> To compile a word is simple. After finding it in the dictionary, you
> have the address of its dictionary entry. Deposit this address in the
> parameter field.

TODO

### 4.4.2 Executing a definition

TODO

# 5 Programs with memory
# 6 Programs with output
# 7 Programs that share
# 8 Programs that think
# 9 Programs that bootstrap
