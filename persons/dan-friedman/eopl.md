---
title: Essentials of Programming Languages
authors: [Daniel P. Friedman, Mitchell Wand]
---

# My Motive

[2025-01-22]
很久之前就读过这本书，
但是感觉没有读透。
在实现 inet-lisp 时，
很多实现技术隐约记得这本书里有提到过，
但是想不起来了。
这次希望能利用 mimor 完全记住。

另外 inet-lisp 现在不是简单的解释器了，
而是编译到一个我自己设计的 vm，
因此 solution space 又变化了很多，
可以在这次重读时思考一下这个新的 solution space。

想要设计的 x-lisp，
其目标场景，就是适合用来实现这本书中的解释器。

# Foreword

> This book brings you face-to-face with the most fundamental idea in
> computer programming:

> **The interpreter for a computer language is just another program.**

> It sounds obvious, doesn’t it? But the implications are
> profound. If you are a computational theorist, the interpreter idea
> recalls Gödel’s discovery of the limitations of formal logical
> systems, Turing’s concept of a universal computer, and von
> Neumann’s basic notion of the stored-program machine.  If you are a
> programmer, mastering the idea of an interpreter is a source of
> great power. It provokes a real shift in mindset, a basic change in
> the way you think about programming.

> If you do this, you will change your view of your programming, and
> your view of yourself as a programmer. You’ll come to see yourself
> as a designer of languages rather than only a user of languages, as
> a person who chooses the rules by which languages are put together,
> rather than only a follower of rules that other people have chosen.

# Preface

## goal

> This book is an analytic study of programming languages. Our goal is
> to provide a deep, working understanding of the essential concepts
> of programming languages. These essentials have proved to be of
> enduring importance; they form a basis for understanding future
> developments in programming languages.

> Most of these essentials relate to the semantics, or meaning, of
> program elements. Such meanings reﬂect how program elements are
> interpreted as the program executes. Programs called interpreters
> provide the most direct, executable expression of program
> semantics. They process a program by directly analyzing an abstract
> representation of the program text. We therefore choose interpreters
> as our primary vehicle for expressing the semantics of programming
> language elements.

# 1 Inductive Sets of Data

## 1.1 Recursively Specified Data

proof by structural induction -

To prove that a proposition IH(s)
is true for all structures s,
prove the following:

1. IH is true on simple structures
   (those without substructures).
2. If IH is true on the substructures of s,
   then it is true on s itself.

## 1.2 Deriving Recursive Programs

- the smaller-subproblem principle -

  If we can reduce a problem to a smaller subproblem,
  we can call the procedure that solves the problem
  to solve the subproblem.

- follow the grammar! -

  When defining a procedure
  that operates on inductively defined data,
  the structure of the program
  should be patterned after the structure of the data.

## 1.3 Auxiliary Procedures and Context Arguments

no mysterious auxiliaries!

When defining an auxiliary procedure,
always specify what it does on all arguments,
not just the initial values.

## 1.4 Exercises

# 2 Data Abstraction

## 2.1 Specifying Data via Interfaces

- All the knowledge about how the data is represented
  must therefore reside in the code of the implementation.

  The most important part of an implementation
  is the specification of how the data is represented.

- Most interfaces will contain

  1. some *constructors*
     that build elements of the data type,

  2. and some *observers*
     that extract information from values of the data type.

## 2.2 Representation Strategies for Data Types

- the interpreter recipe -

  1. look at a piece of data.
  2. decide what kind of data it represents.
  3. extract the components of the datum
     and do the right thing with them.

## 2.3 Interfaces for Recursive Data Types

designing an interface for a recursive data type -

1. Include one constructor
   for each kind of data in the data type.
2. Include one predicate
   for each kind of data in the data type.
3. Include one extractor
   for each piece of data
   passed to a constructor of the data type.

## 2.4 A Tool for Defining Recursive Data Types

## 2.5 Abstract Syntax and Its Representation

# 3 Expressions

## 3.1 Specification and Implementation Strategy

> In this chapter, we study the binding and scoping of variables. We
> do this by presenting a sequence of small languages that illustrate
> these concepts. We write specifications for these languages, and
> implement them using interpreters, following the interpreter recipe
> from chapter 1. Our specifications and interpreters take a context
> argument, called the environment, which keeps track of the meaning
> of each variable in the expression being evaluated.

## 3.2 LET: A Simple Language

### 3.2.1 Specifying the Syntax

### 3.2.2 Specification of Values

> An important part of the specification of any programming language is
> the set of values that the language manipulates. Each language has
> at least two such sets:
>
> - **expressed values** -- the possible values of expressions.
> - **denoted values** -- the values bound to variables.

expressed values 和 denoted values 之间的区分，
是我第一次读 EOPL 时印象最深的一个 idea。

> In the languages of this chapter, the expressed and denoted values
> will always be the same. They will start out as

```
ExpVal = Int + Bool
DenVal = Int + Bool
```

> Chapter 4 presents languages in which expressed and denoted values
> are different.

对于简单的语言来说，没必要有这种区分。
但是对于某些语言特性来说，没有这种区分根本没法理解清楚。

## 3.3 PROC: A Language with Procedures

## 3.4 LETREC: A Language with Recursive Procedures

## 3.5 Scoping and Binding of Variables

## NAMELESS -- optimize searching to indexing

### 3.6 Eliminating Variable Names

- de Bruijn indices [or Lexical Address]

- This way of recording the information is useful
  because the lexical address predicts
  just where in the environment
  any particular variable will be found.

### 3.7 Implementing Lexical Addressing

# 4 State

## 4.1 Computational Effects

> So far, we have only considered the _value_ produced by a
> computation. But a computation may have _effects_ as well: it may
> read, print, or alter the state of memory or a file system. In the
> real world, we are always interested in effects: if a computation
> doesn’t display its answer, it doesn’t do us any good!

<question>
What’s the difference between producing a value and producing an effect?

<answer>
An effect is *global*: it is seen by the entire computation.

An effect affects the entire computation (pun intended).
</answer>
</question>

<question>
We will be concerned primarily with a single effect:
assignment to a location in memory.

How does assignment differ from binding?

<answer>
As we have seen, binding is local,
but variable assignment is potentially global.

It is about the sharing of values between
otherwise unrelated portions of the computation.
</answer>
</question>

<cloze>
Two procedures can share information
if they both know about <blank>the same location in memory</blank>.
</cloze>

<cloze>
A single procedure can share information
with a future invocation of itself
by <blank>leaving the information in a known location</blank>.
</cloze>

> We model memory as a finite map from _locations_ to a set of values
> called the _storable values_. For historical reasons, we call this
> the _store_. The storable values in a language are typically, but
> not always, the same as the expressed values of the language. This
> choice is part of the design of a language.

> A data structure that represents a location is called a _reference_.
> A location is a place in memory where a value can be stored, and a
> reference is a data structure that refers to that place.
> The distinction between locations and references may be seen by
> analogy: a location is like a file and a reference is like a URL.
> The URL refers to the file, and the file contains some data.

在 C 中，一个 pointer 可以被看成是一个 reference。

> References are sometimes called _L-values_. This name reflects the
> association of such data structures with variables appearing on the
> left-hand side of assignment statements. Analogously, expressed
> values, such as the values of the right-hand side expressions of
> assignment statements, are known as _R-values_.

注意 assignment 是我们要考虑的唯一 effect。

## 4.2 EXPLICIT-REFS: A Language with Explicit References

> In this design, we add references
> as a new kind of expressed value.
> So we have

```
DenVal = ExpVal = Int + Bool + Proc + Ref(ExpVal)
```

EXPLICIT 保持了 DenVal 与 ExpVal 相等，
因此可以说，与 ExpVal 不同的 DenVal，
就是为了某些 IMPLICIT 的语言特性。

- C 的 `*` 和 `&` 是 EXPLICIT-REFS 的例子。

> Here `Ref(ExpVal)` means the set of references to locations
> that contain expressed values.

> We leave the binding structures of the language unchanged, but we
> add three new operations to create and use references.

> 1. `newref` allocates a new location and returns a reference to it.

> 2. `deref` dereferences a reference it returns the contents of the
>    location that the reference represents.

> 3. `setref` changes the contents of the location that the reference
>    represents.

例子：

```sml
let x = newref(0) in
let rec even(dummy) =
  if zero?(deref(x))
  then 1
  else begin
    setref(x, -(deref(x),1));
    (odd 888)
  end
and odd(dummy) =
  if zero?(deref(x))
  then 0
  else begin
    setref(x, -(deref(x),1));
    (even 888)
  end
in begin
  setref(x,13);
  (odd 888)
end
```

换成 scheme 语法：

```scheme
(let ([x (newref 0)])
  (letrec ([(even)
            (if (zero? (deref x))
              1
              (begin
                (setref x (- ((deref x) 1)))
                (odd)))]
           [(odd)
            (if (zero? (deref x))
              0
              (begin
                (setref x (- ((deref x) 1)))
                (even)))])
    (setref x 13)
    (odd)))
```

> This style of communication is convenient when two procedures might
> share many quantities; one needs to assign only to the few
> quantities that change from one call to the next. Similarly, one
> procedure might call another procedure not directly but through a
> long chain of procedure calls. They could communicate data directly
> through a shared variable, without the intermediate procedures
> needing to know about it. Thus communication through a shared
> variable can be a kind of information hiding.

一个 class 中相互递归的 methods 也有类似的优点。

> Another use of assignment is to create hidden state through the use
> of private variables. Here is an example.

```sml
let g =
  let counter = newref(0) in
  proc (dummy) begin
    setref(counter, -(deref(counter), -1));
    deref(counter)
  end in
let a = (g 11) in
let b = (g 11) in
-(a,b)
```

```scheme
(let ([g (let ([counter (newref 0)])
           (lambda ()
             (setref counter (- (deref counter) -1))
             (deref counter)))])
  (let ([a (g)]
        [b (g)])
    (- a b)))
```

> We can think of this as the different invocations of `g` sharing
> information with each other. This technique is used by the Scheme
> procedure `gensym` to create unique symbols.

### 4.2.1 Store-Passing Specifications

> In our language, any expression may have an effect. To specify these
> effects, we need to describe what store should be used for each
> evaluation and how each evaluation can modify the store.

> We use _store-passing specfications_. In a store-passing
> specification, the store is passed as an explicit argument to
> `value-of` and is returned as an explicit result from `value-of`.
> Thus we write

```
(value-of exp env store) = (pair value new-store)
```

## 4.3 IMPLICIT-REFS: A Language with Implicit References

> The explicit reference design gives a clear account of allocation,
> dereferencing, and mutation because all these operations are
> explicit in the programmer’s code.

> Most programming languages take common patterns of allocation,
> dereferencing, and mutation, and package them up as part of the
> language.  Then the programmer need not worry about when to perform
> these operations, because they are built into the language.

> In this design, every variable denotes a reference. Denoted values
> are references to locations that contain expressed values.
> References are no longer expressed values. They exist only as the
> bindings of variables.

```
ExpVal = Int + Bool + Proc
DenVal = Ref(ExpVal)
```

当 DenVal 与 ExpVal 相同时，
在 lookup 一个 variable 只能是把其中的 ExpVal 取出来。
当 DenVal 与 ExpVal 不同时，
我们就有了设计空间，可以在 lookup 一个 variable 时做文章。

> Locations are created with each binding operation:
>
> - at each procedure call,
> - `let`,
> - or `letrec`.

> When a variable appears in an expression, we first look up the
> identifier in the environment to find the location to which it is
> bound, and then we look up in the store to find the value at that
> location. Hence we have a “two-level” system for `var-exp`.

`var-exp` 增加了一层 indirect。
从 EXPLICIT 到 IMPLICIT，是 indirectness 单调增长的过程。

> This design is called _call-by-value_, or _implicit references_.
> Most programming languages, including Scheme,
> use some variation on this design.

> Because references are no longer expressed values, we can’t make
> chains of references, as we did in the last example in section 4.2.

这也说明了 C 的 pointer 是一种 EXPLICIT-REFS，
因为可以有嵌套的 pointer。

## 4.4 MUTABLE-PAIRS: A Language with Mutable Pairs

## 4.5 Parameter-Passing Variations

### CALL-BY-REFERENCE

### Lazy Evaluation: CALL-BY-NAME and CALL-BY-NEED

# 5 Continuation-Passing Interpreters

## intro

In chapter 3,
we used the concept of environments
to explore the behavior of bindings,
which establish the data context
in which each portion of a program is executed.

Here we will do the same for the control context
in which each portion of a program is executed.

We will introduce the concept of a continuation
as an abstraction of the control context,
and we will write interpreters
that take a continuation as an argument,
thus making the control context explicit.

- a principle -
It is evaluation of operands,
not the calling of procedures,
that makes the control context grow.

## 5.1 A Continuation-Passing Interpreter

an environment is a representation of a function
from symbols to denoted values.

The continuation of an expression
represents a procedure
that takes the result of the expression
and completes the computation.

Tail Calls Don’t Grow the Continuation -
If the value of exp1 is returned as the value of exp2,
then exp1 and exp2 should run in the same continuation.

## 5.2 A Trampolined Interpreter

## 5.3 An Imperative Interpreter

## 5.4 Exceptions

## 5.5 Threads

THREADS

# 6 Continuation-Passing Style

## 6.1 Writing Programs in Continuation-Passing Style

## 6.2 Tail Form

## 6.3 Converting to Continuation-Passing Style

## 6.4 Modeling Computational Effects

# 7 Types

## intro

Our goal is to analyze a program
to predict whether evaluation of a program is *safe*,
that is, whether the evaluation will proceed
without certain kinds of errors.

Exactly what is meant by safety, however,
may vary from language to language.

we will consider languages that are similar to LETREC.
For these languages we say that an evaluation is safe
if and only if :

1. For every evaluation of a variable var,
   the variable is bound.

2. For every evaluation of a difference expression
   (diff-exp exp1 exp2),
   the values of exp1 and exp2 are both num-vals.

3. For every evaluation of an expression of the form
   (zero?-exp exp1),
   the value of exp1 is a num-val.

4. For every evaluation of a conditional expression
   (if-exp exp1 exp2 exp3),
   the value of exp1 is a bool-val.

5. For every evaluation of a procedure call
   (call-exp rator rand),
   the value of rator is a proc-val.

## 7.1 Values and Their Types

## 7.2 Assigning a Type to an Expression

## 7.3 CHECKED: A Type-Checked Language

## 7.4 INFERRED: A Language with Type Inference

# 8 Modules

## 8.1 The Simple Module System

## 8.2 Modules That Declare Types

## 8.3 Module Procedures

# 9 Objects and Classes

## 9.1 Object-Oriented Programming

## 9.2 Inheritance

## 9.3 The Language

## 9.4 The Interpreter

## 9.5 A Typed Language

## 9.6 The Type Checker
