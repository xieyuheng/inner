---
title: fundamental concepts in programming languages
author: christopher strachey
year: 1967
---

# My Motive

[2025-06-08] 据说下面这些概念都是从这篇论文来的：

- R-value and L-value
- ad hoc polymorphism
- parametric polymorphism
- referential transparency

所以要读一下。

# My Summary

[2025-06-08] 这篇论文看起来是想要为程序语言的数学奠基，
即从对程序语言设计中的现象做解释说明（explication），
过度到使用数学模型来解释这些现象，
即 denotational semantic。

这个课程中有很多对名词的定义，
很适合写成 mimor 卡片来回顾。

# Foreword

Foreword 的作者是 Peter Mosses 写于 2000。

> Strachey’s paper starts with some philosophical remarks about the
> need to focus on semantic issues in the design of programming
> languages, and to “recognise and isolate the central concepts --
> things analogous to the concepts of continuity and convergence in
> analysis”.

这为程序语言理论的 Edinburgh 学派奠定了基础：

- [The History of Standard ML: Ideas, Principles, Culture](https://www.youtube.com/watch?v=NVEgyJCTee4&ab_channel=ICFPVideo)

类比 continuity 和 convergence 之类的概念，
就是把布尔巴基结构主义的研究方法带到程序语言理论中来。

> Strachey then proceeds to give a clear and incisive exposition of
> many of his insights into programming language design and formal
> semantics, covering the following main topics:
>
> - assignment commands, L- and R-values;
> - expression evaluation and environments;
> - commands and sequencing;
> - modes of parameter-passing and variable binding;
> - functions and routines as data items;
> - types and (parametric) polymorphism; and
> - compound data structures and pointers.

> He also indicates how to model some of these concepts
> using λ-expressions.

> The style of semantics proposed by Strachey in this paper was
> further developed (and put on a firm mathematical foundation) in his
> collaboration with Dana Scott, which started in 1969; initially
> referred to as ‘mathematical semantics’, or simply as
> ‘Scott-Strachey semantics’, the framework has since become known as
> denotational semantics.

> Thus Strachey’s paper has undoubtedly a high degree of historical
> interest and relevance. His practical experiences with programming
> throughout the early days of computing (he had an excellent
> reputation as a master programmer) together with his deep
> involvement in programming-language design, had made him
> exceptionally well-qualified to address the fundamental concepts of
> programming languages.

> One potential difficulty with reading Strachey’s paper is that most
> of the illustrations of programs are given in a lesser-known
> language called CPL, which had been developed by Strachey and his
> colleagues during the mid-1960’s.

可以尝试把 CPL 的例子改成 lisp 语法。

> Another possible source of confusion is that the semantic functions
> L and R for expressions, introduced in Sect. 3.3.2, do not
> explicitly take any environment arguments. However, Strachey clearly
> explains that “we speak of evaluating an expression in an
> environment (or sometimes relative to an environment) which provides
> the values of component [identifiers]” (Sect. 3.2.2), and moreover,
> the representation of functions in Sect. 3.5.2 makes explicit
> reference to such an environment.

也许是在实现时把 environment 当作全局变量了，
但是如果实现为全局变量，就没法在递归调用中更改 environment 了。

# Abstract

> This paper forms the substance of a course of lectures given at the
> International Summer School in Computer Programming at Copenhagen in
> August, 1967. The lectures were originally given from notes and the
> paper was written after the course was finished.

> In spite of this, and only partly because of the shortage of time,
> the paper still retains many of the shortcomings of a lecture
> course. The chief of these are an uncertainty of aim -- it is never
> quite clear what sort of audience there will be for such
> lectures—and an associated switching from formal to informal modes
> of presentation which may well be less acceptable in print than it
> is natural in the lecture room.

带有 informal mode 是好事情。

> There are numerous references throughout the course to CPL
> [1–3]. This is a programming language which has been under
> development since 1962 at Cambridge and London and Oxford. It has
> served as a vehicle for research into both programming languages and
> the design of compilers.

> The lack of a precise formulation for CPL should not cause much
> difficulty in this course, as we are primarily concerned with the
> ideas and concepts involved rather than with their precise
> representation in a programming language.

# 1 Preliminaries

## 1.1 Introduction

> Any discussion on the foundations of computing runs into severe
> problems right at the start. The difficulty is that although we all
> use words such as ‘name’, ‘value’, ‘program’, ‘expression’ or
> ‘command’ which we think we understand, it often turns out on closer
> investigation that in point of fact we all mean different things by
> these words, so that communication is at best precarious.

研究程序语言的语义的动机，
首先是想要把自己的想法在与同事的交流之中表达楚。
让人想起 Peirce 的 "how to make our ideas clear"。

> These misunderstandings arise in at least two ways. The first is
> straightforwardly incorrect or muddled thinking. An investigation of
> the meanings of these basic terms is undoubtedly an exercise in
> mathematical logic and neither to the taste nor within the field of
> competence of many people who work on programming languages.  As a
> result the practice and development of programming languages has
> outrun our ability to fit them into a secure mathematical framework
> so that they have to be described in ad hoc ways. Because these
> start from various points they often use conflicting and sometimes
> also inconsistent interpretations of the same basic terms.

程序语言领域缺乏良好的数学框架做基础。

> A second and more subtle reason for misunderstandings is the
> existence of profound differences in philosophical outlook between
> mathematicians.

## 1.2 Philosophical considerations

> The important philosophical difference is between those
> mathematicians who will not allow the existence of an object until
> they have a construction rule for it, and those who admit the
> existence of a wider range of objects including some for which there
> are no construction rules. (The precise definition of these terms is
> of no importance here as the difference is really one of
> psychological approach and survives any minor tinkering.)

即直觉主义数学或者说构造主义数学，与形式主义数学的区别。
注意，由于数学对象是在想象中的，
所以这里说的是 "a construction rule for it"，
而不是真的 construction。

> This may not seem to be a very large difference, but it does lead to
> a completely different outlook and approach to the methods of
> attacking the problems of programming languages.

构造对形式的差异如何影响人们解决程序语言中的问题？
在程序语言领域除了构造主义好像不能有别的主义存在，
毕竟当面对机器的时候人们是不得不构造的。

> The advantages of rigour lie, not surprisingly, almost wholly with
> those who require construction rules. Owing to the care they take
> not to introduce undefined terms, the better examples of the work of
> this school are models of exact mathematical reasoning.

"models of exact mathematical reasoning"
指证明论中的各种 logic system。

> Unfortunately, but also not surprisingly, their emphasis on
> construction rules leads them to an intense concern for the way in
> which things are written -- i.e., for their representation,
> generally as strings of symbols on paper -- and this in turn seems
> to lead to a preoccupation with the problems of syntax.

研究构造主义数学和证明论的人，
需要构造逻辑系统，这就需要设计语法。
但是人们经常陷在语法相关的问题中，而忽略了语义。

> This is probably an unfair criticism, for, as will become clear
> later, I am not only temperamentally a Platonist and prone to
> talking about abstracts if I think they throw light on a discussion,
> but I also regard syntactical problems as essentially irrelevant to
> programming languages at their present stage of development. In a
> rough and ready sort of way it seems to me fair to think of the
> semantics as being what we want to say and the syntax as how we have
> to say it.

lisp 可以缓解这一点，
即减少人们在处理语法上所花费的时间。

> In these terms the urgent task in programming languages is to
> explore the field of semantic possibilities.

semantic possibilities 在我看来，
很大一部分包括探索各种计算模型。

即便是在现在，对计算模型的探索还在初级阶段，
很多模型都没有被实现为能够让人满意地编程的语言。
比如：

- interaction nets
- petri nets
- pi calculus
- propagator
- ...

> When we have discovered the main outlines and the principal peaks we
> can set about devising a suitably neat and satisfactory notation for
> them, and this is the moment for syntactic questions.

> But first we must try to get a better understanding of the processes
> of computing and their description in programming languages.
> In computing we have what I believe to be a new field of mathematics
> which is at least as important as that opened up by the discovery
> (or should it be invention?) of calculus.

> We are still intellectually at the stage that calculus was at when
> it was called the ‘Method of Fluxions’ and everyone was arguing
> about how big a differential was.

> We need to develop our insight into computing processes and to
> recognise and isolate the central concepts -- things analogous to
> the concepts of continuity and convergence in analysis.

如果人们对于程序语言的理解，
正如作者所言还在如此初期，
那么发展的潜力就还很大。
对比无穷小分析的发展，感觉确实如此。

> To do this we must become familiar with them and give them names
> even before we are really satisfied that we have described them
> precisely. If we attempt to formalise our ideas before we have
> really sorted out the important concepts the result, though possibly
> rigorous, is of very little value -- indeed it may well do more harm
> than good by making it harder to discover the really important
> concepts. Our motto should be ‘No axiomatisation without insight’.

这里可以看出，发展理论的过程和软件开发的过程非常类似。
比如这里说的很多建议类似 Sendi Metz 和 Kent Beck 关于软件开发的讨论。
‘No axiomatisation without insight’ 对应的就是「不要过早抽象」。

> However, it is equally important to avoid the opposite of perpetual
> vagueness. My own view is that the best way to do this in a rapidly
> developing field such as computing, is to be extremely careful in
> our choice of terms for new concepts. If we use words such as
> ‘name’, ‘address’, ‘value’ or ‘set’ which already have meanings with
> complicated associations and overtones either in ordinary usage or
> in mathematics, we run into the danger that these associations or
> overtones may influence us unconsciously to misuse our new terms --
> either in context or meaning.

这是认知语言学的观点。

> For this reason I think we should try to give a new concept a
> neutral name at any rate to start with. The number of new concepts
> required may ultimately be quite large, but most of these will be
> constructs which can be defined with considerable precision in terms
> of a much smaller number of more basic ones. This intermediate form
> of definition should always be made as precise as possible although
> the rigorous description of the basic concepts in terms of more
> elementary ideas may not yet be available. Who when defining the
> eigenvalues of a matrix is concerned with tracing the definition
> back to Peano’s axioms?

> Not very much of this will show up in the rest of this course. The
> reason for this is partly that it is easier, with the aid of
> hindsight, to preach than to practice what you preach. In part,
> however, the reason is that my aim is not to give an historical
> account of how we reached the present position but to try to convey
> what the position is. For this reason I have often preferred a
> somewhat informal approach even when mere formality would in fact
> have been easy.

early formality 类似于 early abstraction。

# 2 Basic concepts

## 2.1 Assignment commands

> One of the characteristic features of computers is that they have a
> store into which it is possible to put information and from which it
> can subsequently be recovered. Furthermore the act of inserting an
> item into the store erases whatever was in that particular area of
> the store before -- in other words the process is one of
> overwriting. This leads to the assignment command which is a
> prominent feature of most programming languages.

> The simplest forms of assignments such as

```
x := 3
x := y + 1
x := x + 1
```

```scheme
(set! x 3)
(set! x (add1 y))
(set! x (add1 x))
```

> The following assignment commands show this danger.

```
            i := a > b j, k
         A[i] := A[a > b j, k]
A[a > b j, k] := A[i]
   a > b j, k := i
```

```scheme
(set! i (if (> a b) j k))
(vector-set! A i (vector-ref A (if (> a b) j k)))
(vector-set! A (if (> a b) j k) (vector-ref A i))
(set! (if (> a b) j k) i) ;; not valid in standard scheme
(if (> a b) (set! j i) (set! j k))
```

就赋值命令而言，等号左边的表达式和等号右边的表达式意义不同。

就 vector（或 list）而言，
区分 L-value 和 R-value 只是为了把
`vector-set!` 和 `vector-ref` 这两个函数，
强行捏在一起，使用一个语法元素来表达 `:=`。

但是就 variable 而言，
`set!` 确实要区分 L-value 和 R-value，
因此这个概念是重要的。
毕竟 store 和 load 确实是计算机构架中最本质的概念。

尽管如此，但是假设我设计的是纯函数语言，没有 `set!`，
那么就可以完全消除 L-value 和 R-value 的概念了。
其实不用是纯函数式（没有副作用），
只要排除对 variable 的 `set!` 这一种副作用，
就可以消除 L-value 和 R-value 的概念。

> Roughly speaking an expression on the left stands for an ‘address’
> and one on the right for a ‘value’ which will be stored there.

> We shall therefore accept this view and say that there are two
> values associated with an expression or identifier. In order to
> avoid the overtones which go with the word ‘address’ we shall give
> these two values the neutral names: L-value for the address-like
> object appropriate on the left of an assignment, and R-value for the
> contents-like object appropriate for the right.

发明 L-value 和 R-value 这对术语是为了实践前文提到的认知语言学。

## 2.2 L-values and R-values

> An L-value represents an area of the store of the computer. We call
> this a location rather than an address in order to avoid confusion
> with the normal store-addressing mechanism of the computer.

> The two essential features of a location are that it has a content
> -- i.e. an associated R-value -- and that it is in general possible
> to change this content by a suitable updating operation.

## 2.3 Definitions

> In CPL a programmer can introduce a new quantity and give it a value
> by an initialised definition such as
>
>     let p = 3.5
>
> This introduces a new use of the name p (ALGOL uses the term
> ‘identifier’ instead of name), and the best way of looking at this
> is that the activation of the definition causes a new location not
> previously used to be set up as the L-value of p and that the
> R-value 3.5 is then assigned to this location.

> The relationship between a name and its L-value cannot be altered by
> assignment, and it is this fact which makes the L-value important.

> In CPL, but not in ALGOL, it is also possible to have several names
> with the same L-value.  This is done by using a special form of
> definition:
>
>    let q ≃ p
>
> which has the effect of giving the name of the same L-value as p
> (which must already exist).  This feature is generally used when the
> right side of the definition is a more complicated expression than a
> simple name. Thus if M is a matrix, the definition
>
>    let x ≃ M[2,2]
>
> gives x the same L-value as one of the elements of the matrix. It is
> then said to be sharing with M[2,2], and an assignment to x will
> have the same effect as one to M[2,2].
>
> It is worth noting that the expression on the right of this form of
> definition is evaluated in the L-mode to get an L-value at the time
> the definition is obeyed. It is this L-value which is associated
> with x.

可以看出 C 的 pointer 就继承自这里。

> M[i,i] is an example of an anonymous quantity i.e., an expression
> rather than a simple name—which has both an L-value and an
> R-value. There are other expressions, such as a+b, which only have
> R-values. In both cases the expression has no name as such although
> it does have either one value or two.

## 2.4 Names

> ALGOL 60 uses ‘identifier’ where we have used ‘name’ ...

> It seems to me wiser not to make a distinction between the meaning
> of ‘name’ and that of ‘identifier’ and I shall use them
> interchangeably. The important feature of a name is that it has no
> internal structure at any rate in the context in which we are using
> it as a name.  Names are thus atomic objects and the only thing we
> know about them is that given two names it is always possible to
> determine whether they are equal (i.e., the same name) or not.

## 2.5 Numerals

> We use the word ‘number’ for the abstract object and ‘numeral’ for
> its written representation.  Thus 24 and XXIV are two different
> numerals representing the same number.

> There is often some confusion about the status of numerals in
> programming languages. One view commonly expressed is that numerals
> are the ‘names of numbers’ which presumably means that every
> distinguishable numeral has an appropriate R-value associated with
> it. This seems to me an artificial point of view and one which falls
> foul of Occam’s razor by unnecessarily multiplying the number of
> entities (in this case names). This is because it overlooks the
> important fact that numerals in general do have an internal
> structure and are therefore not atomic in the sense that we said
> names were in the last section.

> An interpretation more in keeping with our general approach is to
> regard numerals as R-value expressions written according to special
> rules. Thus for example the numeral 253 is a syntactic variant for
> the expression
>
>     2 × 102 + 5 × 10 + 3

> Local rules for special forms of expression can be regarded as a
> sort of ‘micro-syntax’ and form an important feature of programming
> languages. The micro-syntax is frequently used in a preliminary
> ‘pre-processing’ or ‘lexical’ pass of compilers to deal with the
> recognition of names, numerals, strings, basic symbols
> (e.g. boldface words in ALGOL) and similar objects which are
> represented in the input stream by strings of symbols in spite of
> being atomic inside the language.

比如 lisp 的 `'symbol` 和 `:keyword`，
还有各种 `#` 开头的 reader macro。

> With this interpretation the only numerals which are also names are
> the single digits and these are, of course, constants with the
> appropriate R-value.

## 2.6 Conceptual model

> It is sometimes helpful to have a picture showing the relationships
> between the various objects in the programming language, their
> representations in the store of a computer and the abstract objects
> to which they correspond. Figure 1 is an attempt to portray the
> conceptual model which is being used in this course.

> Figure 1. The conceptual model.
>
> | language component | L-value (address) | R-value (value) |
>
> On the left are some of the components of the programming
> language. Many of these correspond to either an L-value or an
> R-value and the correspondence is indicated by an arrow terminating
> on the value concerned. Both L-values and R-values are in the
> idealised store, a location being represented by a box and its
> contents by a dot inside it. R-values without corresponding L-values
> are represented by dots without boxes, and R-values which are
> themselves locations (as, for example, that of a vector) are given
> arrows which terminate on another box in the idealised store.

> R-values which correspond to numbers are given arrows which
> terminate in the right hand part of the diagram which represents the
> abstract objects with which the program deals.

不只表示了 name 到 address，以及 address 之间的指针图，
还同时表示出了表达式到 value 的 evaluation，
以及 name 在 evaluation 过程中，经过 address 得到 value。

尽管这里的 conceptual model 总结的很不错，
但是对比 SICP 中没有引入副作用之前的 conceptual model，
还是后者简单。
但是这种简单的 model 不能包含计算机科学的全部，
甚至不能包含一个语言实现的全部，
因为要写 scheme 的编译器，
还是需要认识汇编中的副作用。

# 3 Conceptual constructs

## 3.1 Expressions and commands

> All the first and simplest programming language—by which I mean
> machine codes and assembly languages—consist of strings of commands.
> When obeyed, each of these causes the computer to perform some
> elementary operation such as subtraction, and the more elaborate
> results are obtained by using long sequences of commands.

> In the rest of mathematics, however, there are generally no commands
> as such. Expressions using brackets, either written or implied, are
> used to build up complicated results.  When talking about these
> expressions we use descriptive phrases such as ‘the sum of x and y’
> or possibly ‘the result of adding x to y’ but never the imperative
> ‘add x to y’.

> As programming languages developed and became more powerful they
> came under pressure to allow ordinary mathematical expressions as
> well as the elementary commands.  It is, after all, much more
> convenient to write as in CPL, x := a(b+c)+d than the more
> elementary
>
>    CLA b
>    ADD c
>    MPY a
>    ADD d
>    STO x
>
> and also, almost equally important, much easier to follow.

> To a large extent it is true that the increase in power of
> programming languages has corresponded to the increase in the size
> and complexity of the right hand sides of their assignment commands
> for this is the situation in which expressions are most valuable.

比如写 complier 的时候，有一个 pass 就是，
消除 right hand sides 的 complex expression。

> In almost all programming languages, however, commands are still
> used and it is their inclusion which makes these languages quite
> different from the rest of mathematics.

> There is a danger of confusion between the properties of
> expressions, not all of which are familiar, and the additional
> features introduced by commands, and in particular those introduced
> by the assignment command. In order to avoid this as far as
> possible, the next section will be concerned with the properties of
> expressions in the absence of commands.

## 3.2 Expressions and evaluation

### 3.2.1 Values

> The characteristic feature of an expression is that it has a _value_.

> We have seen that in general in a programming language, an
> expression may have two values -- an L-value and an R-value. In this
> section, however, we are considering expressions in the absence of
> assignments and in these circumstances L-values are not
> required. Like the rest of mathematics, we shall be concerned only
> with R-values.

> One of the most useful properties of expressions is that called by
> Quine [4] _referential transparency_. In essence this means that if
> we wish to find the value of an expression which contains a
> sub-expression, the only thing we need to know about the
> sub-expression is its value. Any other features of the
> sub-expression, such as its internal structure, the number and
> nature of its components, the order in which they are evaluated or
> the colour of the ink in which they are written, are irrelevant to
> the value of the main expression.

这就是在引入副作用之前
SICP 的 conceptual model 很简单的原因。

### 3.2.2 Environments

> In order to find the value of an expression it is necessary to know
> the value of its components. Thus to find the value of a + 5 + b/a
> we need to know the values of a and b. Thus we speak of evaluating
> an expression in an environment (or sometimes relative to an
> environment) which provides the values of components.

> One way in which such an environment can be provided is by a
> _where-clause_. Thus
>
>     a + 3/a where a = 2 + 3/7
>     a + b − 3/a where a = b + 2/b
>
> have a self evident meaning.
> An alternative syntactic form which has the same effect
> is the initialised definition:
>
>     let a = 2 + 3/7; a + 3/a
>     let a = b + 2/b; a + b − 3/a
>
> Another way of writing these is to use λ-expressions:
>
>     (λa. a + 3/a)(2 + 3/7)
>     (λa. a + b − 3/a)(b + 2/b)

> All three methods are exactly equivalent and are, in fact, merely
> syntactic variants whose choice is a matter of taste. In each the
> letter a is singled out and given a value and is known as the _bound
> variable_. The letter b in the second expression is not bound and
> its value still has to be found from the environment in which the
> expression is to be evaluated. Variables of this sort are known as
> _free variables_.

### 3.2.3 Applicative structure

> Another important feature of expressions is that it is possible to
> write them in such a way as to demonstrate an _applicative
> structure_ -- i.e., as an operator applied to one or more
> operands. One way to do this is to write the operator in front of
> its operand or list of operands enclosed in parentheses. Thus
>
>     a + b corresponds to +(a, b)
>     a + 3/a corresponds to +(a, /(3, a))

> Expressions written in this way with deeply nesting brackets are
> very difficult to read.  Their importance lies only in emphasising
> the uniformity of applicative structure from which they are built
> up. In normal use the more conventional syntactic forms which are
> familiar and easier to read are much to be preferred -- providing
> that we keep the underlying applicative structure at the back of our
> minds.

上面这些论点，只对带有结合律的算术函数有效。

### 3.2.4 Evaluation

> We thus have a distinction between _evaluating_ an operator
> and _applying_ it to its operands.

> ... the general rule for evaluating compound expressions
> in the operator-operand form viz:
>
> 1. Evaluate the operator and the operand(s) in any order.
> 2. After this has been done, apply the operator to the operand(s).

> The interesting thing about this rule is that it specifies a partial
> ordering of the operations needed to evaluate an expression. Thus
> for example when evaluating
>
>     (a + b)(c + d/e)
>
> both the additions must be performed before the multiplication, and
> the division before the second addition but the sequence of the
> first addition and the division is not specified. This partial
> ordering is a characteristic of algorithms which is not yet
> adequately reflected in most programming languages.

interaction nets 可以做到让这个 partial order 中，
所有能并行的计算都并行。

> There is a device originated by Schönfinkel [5], for reducing
> operators with several operands to the successive application of
> single operand operators. Thus, for example, instead of +(2, p)
> where the operator + takes two arguments we introduce another adding
> operator say +' which takes a single argument such that +'(2) is
> itself a function which adds 2 to its argument. Thus (+' (2))(p) =
> +(2, p) = 2 + p. In order to avoid a large number of brackets we
> make a further rule of association to the left and write +' 2 p in
> place of ((+' 2) p) or (+' (2))(p). This convention is used from
> time to time in the rest of this paper. Initially, it may cause some
> difficulty as the concept of functions which produce functions as
> results is a somewhat unfamiliar one and the strict rule of
> association to the left difficult to get used to. But the effort is
> well worth while in terms of the simpler and more transparent
> formulae which result.

这里在介绍 currying 的时候，已经放弃了传统函数作用语法 `f(x)` 了，
但是就算是想要用 currying，也没必要放弃传统语法，
直接说 `+(2, p)` 等价于 `+(2)(p)` 就可以了。

这种不带最外层括号和逗号的函数作用语法，
可能就是 Schönfinkel 首先使用的，
而其灵感可能来自 Polish notation（不带所有括号和逗号）。

### 3.2.5 Conditional expressions

> There is one important form of expression which appears to break the
> applicative expression evaluation rule. A conditional expression
> such as
>
>     (x = 0) -> 0, 1/x

```scheme
(if (eq? x 0) 0 (div 1 x))
```

> Various devices can be used to convert this to a true applicative
> form, and in essence all have the effect of delaying the evaluation
> of the arms until after the condition has been decided. Thus suppose
> that If is a function of a Boolean argument whose result is the
> selector First or Second so that
>
>     If (True) = First and
>     If (False) = Second
>
> the naive interpretation of the conditional expression
> given above as
>
>
>     {If (x = 0)}(0, 1/x)
>
> is wrong because it implies the evaluation of both members of the
> list (0, 1/x) before applying the operator {If (x = 0)}. However the
> expression
>
>     [{If (x = 0)}({λa. 0}, {λa. 1/x})]a
>
> will have the desired effect as the selector function If (x = 0) is
> now applied to the list ({λa. 0}, {λa. 1/x}) whose members are
> λ-expressions and these can be evaluated (but not applied) without
> danger.

```scheme
(ifte true first second) => first
(ifte false first second) => second

;; wrong:
(ifte (eq? x 0)
      0
      (div 1 x))

;; right:
((ifte (eq? x 0)
       (lambda () 0)
       (lambda () (div 1 x))))
```

> Recursive (self referential) functions do not require commands or
> loops for their definition, although to be effective they do need
> conditional expressions. For various reasons, of which the principal
> one is lack of time, they will not be discussed in this course.

没有 conditional 的 recursive function 注定是发散的。
没有 sum type 的 recursive type 也注定是发散的。

## 3.3 Commands and sequencing

### 3.3.1 Variables

> One important characteristic of mathematics is our habit of using
> names for things. Curiously enough mathematicians tend to call these
> things ‘variables’ although their most important property is
> precisely that they do not vary. We tend to assume automatically
> that the symbol x in an expression such as 3x 2 + 2x + 17 stands for
> the same thing (or has the same value) on each occasion it occurs.
> This is the most important consequence of referential transparency
> and it is only in virtue of this property that we can use the
> where-clauses or λ-expressions described in the last section.

> The introduction of the assignment command alters all this, and if
> we confine ourselves to the R-values of conventional mathematics we
> are faced with the problem of variables which actually vary,
> ... Referential transparency has been destroyed, and without it we
> have lost most of our familiar mathematical tools -- for how much of
> mathematics can survive the loss of identity?

> If we consider L-values as well as R-values, however, we can
> preserve referential transparency as far as L-values are
> concerned. This is because L-values, being generalised addresses,
> are not altered by assignment commands. Thus the command x := x+1
> leaves the address of the cell representing x (L-value of x)
> unchanged although it does alter the contents of this cell (R-value
> of x). So if we agree that the values concerned are all L-values, we
> can continue to use where-clauses and λ-expressions for describing
> parts of a program which include assignments.

也许好的策略是直接放弃 `set!` 所带来的 L-value 和 R-value 之分，
副作用只能出现在 compound data structures 上，
这样它们都能用函数来实现。

### 3.3.2 The abstract store

> Our conceptual model of the computing process includes an abstract
> store which contains both L-values and R-values. The important
> feature of this abstract store is that at any moment it specifies
> the relationship between L-values and the corresponding R-values. We
> shall always use the symbol σ to stand for this mapping from
> L-values onto R-values. Thus if α is an L-value and β the
> corresponding R-value we shall write (remembering the conventions
> discussed in the last section)
>
>     β = σ α.

在 C 里，这就是取指针的值 `β = *α`。

> The effect of an assignment command is to change the contents of the
> store of the machine.  Thus it alters the relationship between
> L-values and R-values and so changes σ . We can therefore regard
> assignment as an operator on σ which produces a fresh σ . If we
> update the L-value α (whose original R-value in σ was β) by a fresh
> R-value β 0 to produce a new store σ 0 , we want the R-value of α in
> σ 0 to be β 0 , while the R-value of all other L-values remain
> unaltered. This can be expressed by the equation
>
>     (U (α, β'))σ = σ' where σ' x = (x = α) → β', σ x.

```scheme
((U α β') σ) = σ'
;; where:
(σ' x) = (if (eq? x α) β' (σ x))
```

> Thus U is a function which takes two arguments (an L-value and an
> R-value) and produces as a result an operator which transforms σ
> into σ' as defined.

> The arguments of U are L-values and R-values and we need some way of
> getting these from the expressions written in the program. Both the
> L-value and the R-value of an expression such as V[i+3] depend on
> the R-value of i and hence on the store. Thus both must involve σ
> and if ε stands for a written expression in the programming language
> we shall write L ε σ and R ε σ for its L-value and R-value
> respectively.

注意，这里已经在用 Schönfinkel 的函数作用语法了，
也就是现在 Haskell 和 ML 的函数作用语法。

在 C 里，`L ε σ` 就是取地址 `&ε`，
而 `R ε σ` 就是 `ε` 本身。

或者可以说 `L` 和 `R` 是实现解释器的时候的两个函数：

- `R` 就是 `evaluate`。
- `L` 是针对 L-value 的 `evaluate`，也许可以叫做 `locate`。

> These functions show an application to Schönfinkel’s device which is
> of more than merely notational convenience. The function R, for
> example, shows that its result depends on both ε and σ , so it might
> be thought natural to write it as R(ε, σ). However by writing R ε σ
> and remembering that by our convention of association to the left
> this means (R ε)σ it becomes natural to consider the application of
> R to ε separately and before the application of R ε to σ. These two
> phases correspond in a very convenient way to the processes of
> compilation, which involves manipulation of the text of the program,
> and execution which involves using the store of the computer. Thus
> the notation allows us to distinguish clearly between compile-time
> and execution-time processes. This isolation of the effect of σ is a
> characteristic of the method of semantic description described here.

### 3.3.3 Commands

> Commands can be considered as functions which transform σ. Thus
> the assignment
>
>     ε1 := ε2
>
> has the effect of producing a store
>
>     σ' = U(α1, β2)σ
>
> where
>
>     α1 = L ε1 σ
>
> and
>
>     β2 = R ε2 σ
>
> so that
>
>     σ' = U (L ε1 σ, R ε2 σ)σ
>
> and if θ is the function on σ which is equivalent to the original command we have
>
>     σ 0 = θσ
>
> where
>
>     θ = λσ.U(L ε1 σ, R ε2 σ)σ

> Sequences of commands imply the successive application of sequences
> of θ's. Thus, for example, if γ1, γ2, γ3 are commands and θ1, θ2, θ3
> the equivalent functions on σ, the command sequence (or compound
> command)
>
>     γ1;γ2;γ3;
>
> applied to a store σ will produce a store
>
>     σ 0 = θ3(θ2(θ1 σ))
>         = (θ3·θ2·θ1)σ
>
> where f·g is the function product of f and g.

> Conditional expressions can also be treated more naturally. The
> dummy argument introduced in the last section to delay evaluation
> can be taken to be σ with considerable advantages in
> transparency. Thus
>
>     R(ε1 -> ε2, ε3)σ = If(R ε1 σ)(R ε2, R ε3)σ
>
> and
>
>     L(ε1 -> ε2, ε3)σ = If(R ε1 σ)(L ε2, L ε3)σ

> An interesting feature of this approach to the semantics of
> programming languages is that all concept of sequencing appears to
> have vanished. It is, in fact, replaced by the partially ordered
> sequence of functional applications which is specified by
> λ-expressions.

> In the remaining sections we shall revert to a slightly less formal
> approach, and try to isolate some important ‘high level’ concepts in
> programming languages.

## 3.4 Definition of functions and routines

### 3.4.1 Functional abstractions

> In order to combine programs hierarchically we need the process of
> functional abstraction. That is to say that we need to be able to
> form functions from expressions such as
>
>     let f[x] = 5x + 3x^2 + 2/x^3

### 3.4.2 Parameter calling modes

> When the function is used (or called or applied) we write f[ε] where
> ε can be an expression. If we are using a referentially transparent
> language all we require to know about the expression ε in order to
> evaluate f[ε] is its value. There are, however, two sorts of value,
> so we have to decide whether to supply the R-value or the L-value of
> ε to the function f. Either is possible, so that it becomes a part
> of the definition of the function to specify for each of its bound
> variables (also called its formal parameters) whether it requires an
> R-value or an L-value. These alternatives will also be known as
> calling a parameter by _value_ (R-value) or _reference_ (L-value).

也许 call-by-value 和 call-by-reference 就是从这里来的。

注释中解释了 ALGOL 60 call-by-name：

> Let f be an ALGOL procedure which calls a formal parameter x by
> name. Then a call for f with an actual parameter expression ε will
> have the same effect as forming a parameterless procedure λ().ε
> and supplying this by value to a procedure f* which is derived from
> f by replacing every written occurrence of x in the body of f by
> x(). The notation λ().ε denotes a parameterless procedure whose body
> is ε while x() denotes its application (to a null parameter list).

Strachey 解释问题的方式真清晰。

### 3.4.3 Modes of free variables

讲了：

- lexical scope
- passing pointer + auto deref
- dynamic scope

### 3.4.4 Own variables

> The purpose is to allow a variable to preserve its value from one
> application of a function to the next -- say to produce a
> pseudo-random number or to count the number of times the function is
> applied. ... What we need is a way of limiting the scope of a
> variable to be the definition only.

### 3.4.5 Functions and routines

> We have so far discussed the process of functional abstrac- tion as
> applied to expressions. The result is called a function and when
> applied to suitable arguments it produces a value. Thus a function
> can be regarded as a complicated sort of expression. The same
> process of abstraction can be applied to a command (or sequence of
> commands), and the result is know in CPL as a routine. The
> application of a routine to a suitable set of arguments is a
> complicated command, so that although it affects the store of the
> computer, it produces no value as a result.

区分对 expression 和 command，
然后再区分对二者的 abstraction，
即 function 和 routine。
这样就可以把纯函数和副作用分开了。

> Functions and routines are as different in their nature as
> expressions and commands. It is unfortunate, therefore, that most
> programming languages manage to confuse them very successfully. The
> trouble comes from the fact that it is possible to write a function
> which also alters the store, so that it has the effect of a function
> and a routine. Such functions are sometimes said to have side
> effects and their uncontrolled use can lead to great obscurity in
> the program. There is no generally agreed way of controlling or
> avoiding the side effects of functions, and most programming
> languages make no attempt to deal with the problem at all -- indeed
> their confusion between routines and functions adds to the
> difficulties.

> The problem arises because we naturally expect referential
> transparency of R-values in expressions, particularly those on the
> right of assignment commands. This is, I think, a very reasonable
> expectation as without this property, the value of the expression is
> much harder to determine, so that the whole program is much more
> obscure.

> Any departure of R-value referential transparency in a R-value
> context should either be eliminated by decomposing the expression
> into several commands and simpler expressions, or, if this turns out
> to be difficult, the subject of a comment.

只有有了 L-value 和 R-value 之分之后，
才能做这种 function 和 routine 的区分。
如果用 `vector-set!` 而不同和 vector 相关的 L-value，
就没法区分 function 和 routine 了，
因为所有的副作用都是通过 L-value 完成的。

比如 scheme 没有区分 function 和 routine，
并且用了 procdure 一词来指带有副作用的 function，
这一点应该继承自 ALGOL。

### 3.4.6 Constants and variables

> There is another approach to the problem of side effects which is
> somewhat simpler to apply, though it does not get round all the
> difficulties. This is, in effect, to turn the problem inside out and
> instead of trying to specify functions and expressions which have no
> side effect to specify objects which are immune from any possible
> side effect of others.

> There are two chief forms which this protection can take which can
> roughly be described as hiding and freezing. Their inaccessibility
> (by reason of the scope rules) makes them safe from alteration
> except from inside the body of the function or routine they
> qualify. We shall be concerned in this section and the next with
> different forms of protection by freezing.

> The characteristic thing about variables is that their R-values can
> be altered by an assign- ment command. If we are looking for an
> object which is frozen, or invariant, an obvious possibility is to
> forbid assignments to it. This makes it what in CPL we call a
> _constant_. It has an L-value and R-value in the ordinary way, but
> applying the update function to it either has no effect or produces
> an error message. Constancy is thus an attribute of an L-value, and
> is, moreover, an invariant attribute. Thus when we create a new
> L-value, and in particular when we define a new quantity, we must
> decide whether it is a constant or a variable.

比如 javascript 有 let 和 const 之分，
但是没有 function 和 routine 之分。

### 3.4.7 Fixed and free

> The constancy or otherwise of a function has no connection with the
> mode in which it uses its free variables. If we write a definition
> in its standard form such as
>
>     let f ≡ λx. x + a
>
> we see that this has the effect of initialising f with a
> λ-expression. The constancy of f merely means that we are not
> allowed to assign to it. The mode of its free variables (indicated
> by ≡) is a property of the λ-expression.

> Functions which call their free variables by reference (L-value) are
> liable to alteration by assignments to their free variables.

就算是 CPL 也能通过 L-value 的 free variable
让 function 失去 referential transparency。

> The converse of a free function is a fixed function. This is defined
> as a function which either has no free variables, or if it has,
> whose free variables are all both constant and fixed.  The crucial
> feature of a fixed function is that it is independent of its
> environment and is always the same function. It can therefore be
> taken out of the computer (e.g., by being compiled separately) and
> reinserted again without altering its effect.

### 3.4.8 Segmentation

> A fixed routine or function is precisely the sort of object which
> can be compiled separately.

> Suppose R[x] is a routine which uses a, b, and c by reference as
> free variables. We can define a function R'[a,b,c] which has as
> formal parameters all the free variables of R and whose result is
> the routine R[x]. Then R' will have no free variables and will thus
> be a fixed function which can be compiled separately.

这正是编译 closure 的方法。

## 3.5 Functions and routines as data items

### 3.5.1 First and second class objects

> Historically this second class status of procedures in ALGOL is
> probably a consequence of the view of functions taken by many
> mathematicians: that they are constants whose name one can always
> recognise. This second class view of functions is demonstrated by
> the remarkable fact that ordinary mathematics lacks a systematic
> notation for functions.

### 3.5.2 Representation of functions

> If we want to make it possible to assign functions we must be clear
> what are their L-values and R-values. The L-value is simple -- it is
> the location where the R-value is stored -- but the R-value is a
> little more complicated.

这里作者说错了，L-value 和 R-value 是 expression 的属性，
而 function 已经是 value 了，所以不能有 L-value 和 R-value。
说 function 的 L-value 是 location，
其实是说 variable（expression）的 L-value 是 location。

> ... the R-value of a function contains two parts -- a rule for
> evaluating the expression, and an environment which supplies its
> free variables. An R-value of this sort will be called a _closure_.

## 3.6 Types and polymorphism

### 3.6.1 Types

> Most programming languages deal with more than one sort of object --
> for example with integers and floating point numbers and labels and
> procedures. We shall call each of these a different _type_ and spend
> a little time examining the concept of type and trying to clarify
> it.

> A possible starting point is the remark in the CPL Working Papers
> [3] that “The Type of an object determines its representation and
> constrains the range of abstract object it may be used to
> represent. Both the representation and the range may be
> implementation dependent”.

TODO 这句话有两部分，哪一部分是集合论意义上的 set？

### 3.6.2 Manifest and latent
### 3.6.3 Dynamic type determination
### 3.6.4 Polymorphism
### 3.6.5 Types of functions

## 3.7 Compound data structures

### 3.7.1 List processing
### 3.7.2 Nodes and elements
### 3.7.3 Assignments
### 3.7.4 Implementation
### 3.7.5 Programming example
### 3.7.6 Pointers
### 3.7.7 Other forms of structure

# 4 Miscellaneous topics

## 4.1 Load-Update Pairs
## 4.2 Macrogenerators
## 4.3 Formal semantics
