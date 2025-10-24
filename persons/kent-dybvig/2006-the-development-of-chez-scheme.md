---
title: the development of chez scheme
author: kent dybvig
year: 2006
---

# My Motive

[2025-10-24] 我想知道 chez scheme 是否在运行时需要一直有 tagged value。

# Abstract

> Chez Scheme is now over 20 years old, the first version having been
> released in 1985. This paper takes a brief look back on the history
> of Chez Scheme’s development to explore how and why it became the
> system it is today.

# 1 Introduction

> Our primary objectives remain reliability and efficiency. A reliable
> system is one that correctly implements the entire language and
> never crashes due to a fault in the compiler or run-time
> environment. An efficient system is one that exhibits uniformly good
> performance in all aspects of its operation, with a fast compiler
> that generates fast code and does so for the widest variety of
> programs and programming styles possible.

# 2 Precursors

> Chez Scheme did not materialize out of a vacuum. What follows is a
> description of several Scheme or Lisp systems I worked on before
> Chez Scheme, systems that influenced the design and implementation
> of Chez Scheme in one way or another.

## 2.1 SDP

> Scheme Distributed Processes [13] (SDP) was a multi-threaded
> implementation of Scheme written in 1980–81 by fellow Indiana
> University graduate student Rex Dwyer and me. The system was
> primarily a vehicle for investigating the Distributed Processes
> model of concurrency proposed by Per Brinch Hansen [34]. It grew out
> of an assignment given to us by Dan Friedman in his graduate
> programming languages seminar, which he taught using the book he and
> Bob Filman were writing on concurrent programming techniques [32].

这里的引用：

- [13] Rex A. Dwyer and R. Kent Dybvig.
  A SCHEME for distributed processes.
  Computer Science Department Technical Report #107,
  Indiana University, Bloomington, Indiana, April 1981.

- [34] Per Brinch Hansen.
  Distributed processes: a concurrent programming concept.
  Communications of the ACM, 21(11):934–941, 1978.

- [35] Christopher T. Haynes and Daniel P. Friedman.
  Abstracting timed preemption with engines.
  Computer Languages, 12(2):109–121, 1987.

> SDP supported a large subset of the 1978 (revised report) version of
> Scheme [52]. In addition to its parallel processing extensions, SDP
> also supported arrays, a partial application mechanism, `dskin` and
> `dskout` functions for loading and saving definitions, and even a
> structure editor. SDP departed from Scheme’s semantics by
> distinguishing false from the empty list and also by requiring that
> the cdr of a list also be a list. SDP was written entirely in Simula
> [3] and took advantage of Simula’s run-time system, including most
> importantly its garbage collector.

也就是说是用 Simula 写的解释器。

> Although none of the code of SDP survived into any of the Scheme
> systems I wrote later, it provided me my first experience with
> Scheme and with implementing a Lisp dialect of any kind.

## 2.2 Z80 Scheme

> In 1981, while working as systems programmers in the academic
> computing center at IU, George Cohn and I decided to create an
> implementation of Scheme for the Z80 microprocessor. In so doing,
> George could teach me how to program in Z80 assembly, and I could
> teach him about Scheme. George was an incredible programmer, and I
> learned a lot from him in what I am sure was the better end of the
> bargain.

> ... We designed a Scheme Assembly Language analogous to the Lisp 1.5
> Lisp Assembly Language (LAP) [45] ...

- [45] John McCarthy, Paul W. Abrahams,
  Daniel J. Edwards, Timothy P. Hart, and Michael I. Levin.
  LISP 1.5 Programmer’s Manual.
  The MIT Press, Cambridge, Mass., 1966. second edition.

原来 LISP 1.5 带有一个 LISP 汇编语言！
在 manual 的附录中。

## 2.3 C-Scheme

> In 1982 I also began the implementation of a new dialect of Scheme,
> Curry Scheme (later abbreviated C-Scheme) [14].

- [14] R. Kent Dybvig. C-Scheme. Master’s thesis,
  Indiana University Computer Science Department Technical Report #149,
  1983.

可能是支持自动 currying 的 scheme。

## 2.4 Data General Common Lisp

为一个公司维护 Common Lisp 实现。

# 3 Chez Scheme Version 1

> As part of the design process, I profiled the C-Scheme
> implementation and discovered that most of the time was spent in
> variable lookups and stack-frame creation. It dawned on me that the
> typical implementation model for Scheme was all wrong: by heap
> allocating environments and call frames, it made closure creation
> fast at the expense of the more common variable references, and it
> made continuation operations fast at the expense of the more common
> procedure calls. In our second Z80 Scheme implementation, we opted
> to sacrifice full continuations to enable stack allocation of stack
> frames, and the designers of T had done likewise [47], but I wasn’t
> willing to go that route with Chez Scheme. Instead, I started to
> think about ways to make continuations “pay their own way,” and at
> the same time, shift the burden somehow from closure access to
> closure creation.

> Solving the closure issue was a bit trickier. ... I was able to make
> several adjustments and from the display model derived the notion of
> a _display closure_, a heap-allocated vector-like object holding a
> code pointer and the values of the free variables [16].  In addition
> to allowing constant-time access to all variables, it had the added
> benefit that closures hold on to no more of the environment than
> they require, which had the potential to make garbage collection
> more effective.

- [16] R. Kent Dybvig. Three Implementation Models for Scheme.
  PhD thesis, University of North Carolina Technical Report #87-011,
  Chapel Hill, April 1987.

难道这种用 tuple 来处理 closure 的方式是 dybvig 发明的？

> Assigned variables were a problem with this representation, since a
> variable’s value could potentially appear in multiple closures. I
> dealt with this by “boxing” assigned variables, i.e., replacing
> each assigned variable’s value with a pointer to a heap-allocated
> single-celled object, or box, holding the actual value. (A variable
> is assumed to be assigned if it appears on the left-hand side of an
> assignment somewhere in its scope.)

我可以直接禁止 assigned variable。

> I learned later that Luca Cardelli used a similar flat
> representation of closures in his ML implementation [11].

- [11] Luca Cardelli. Compiling a functional language.
  In Proceedings of the 1984 ACM Symposium on LISP and functional programming,
  pages 208–217, New York, NY, USA, 1984. ACM Press.

原来不是 dybvig 发明的，是英雄所见略同。

> In ML, variables are immutable, so there is no need for the compiler
> to introduce boxes. One can view the introduced boxes as a form of
> ML ref cell, however, the difference being that, in ML, the
> programmer must introduce the ref cells explicitly, whereas, in
> Scheme, the compiler introduces the boxes implicitly.

> Boxing assigned variables also solved the stack problem, because it
> allowed the values (or boxed values) of local variables to be stored
> directly in a stack frame without concern for the fact that the
> frame might be copied as a result of a continuation capture.

dybvig 的开发过程也是先写解释器，
再写 compiler frontend，
再写 compiler backend，
并且也害怕写 backend，
觉得代码生成很麻烦，
尤其是要自己写汇编器和 linker
（因为要实现 incremental compiler）。

但是后来发现这些其实是很简单的，没必要怕。

> In a high-level sense, the compiler was naive. It handled a small
> set of core forms and did only one thing that could charitably be
> called a high-level “optimization:” it treated the variables bound
> by a direct lambda application as local variables to avoid the cost
> of allocating a closure and calling that closure. As I tell my
> compiler students now, there is a fine line between “optimization”
> and “not being stupid.” This was really an instance of the latter.

编译器圈子的名句。

> My focus was instead on low-level details, like choosing efficient
> representations and generating good instruction sequences, and the
> compiler did include a peephole optimizer. High-level optimization
> is important, and we did plenty of that later, but low-level details
> often have more leverage in the sense that they typically affect a
> broader class of programs, if not all programs.

> Although I don’t recall the details now, Version 1 outperformed the
> other Scheme and Lisp systems to which I had access, in spite of the
> complete type and bounds checking, so I felt that the new closure
> and continuation representations had proven themselves.

这里说的 complete type checking，
意思是每个 value 都要有 tag 吗？

# 4 Chez Scheme Version 1.1

写手册，修 bug，发布 1.1 卖钱。

# 5 Chez Scheme Version 2

> We implemented different optimization levels, which were really just
> flags telling the compiler it could do certain things. At
> optimization level 1, it was allowed to spend more time. At
> optimization level 2, it was allowed to assume that the global names
> of primitives were indeed bound to those primitives. At optimization
> level 3, it was allowed to generate unsafe code. We also introduced
> some code into the compiler to optimize letrec expressions, optimize
> loops, and inline most simple primitives.

# 6 Chez Scheme Version 3

> ... a new segmented stack approach ...

为了避免 call/cc 的时候复制整个 stack，
应该是把 stack 实现为 list of segment 了。

# 7 Chez Scheme Version 4

> Shortly after Version 3 was released, we began a major overhaul of
> the system, changing the way it represents Scheme values and
> rewriting major portions of the compiler and storage management
> system.

终于讲到 value 的编码了。

> Ultimately, we decided to switch to a hybrid model [23] that uses
> tagged pointers to distinguish specific types of objects and also
> BiBOP for its various benefits. We no longer used the BiBOP
> mechanism to segregate objects by specific type but rather by
> characteristics of interest to the collector, such as whether they
> contain pointers and whether they are mutable.

- [23] R. Kent Dybvig, David Eby, and Carl Bruggeman.
  Don’t stop the BiBOP: Flexible and efficient storage management
  for dynamicallytyped languages.
  Technical Report 400, Indiana Computer Science Department, March 1994.

这标题是 bepop 谐音吗？

> For our implementation of tagged pointers, we adopted the low-tag
> model used by T [47], with a different assignment of tags. The
> hybrid mechanism allowed the fixnum size to be increased to 30 bits
> and type checking overhead to be reduced in many cases, without
> sacrificing virtual address space or the benefits of the BiBOP
> mechanism described above.

- [47] Jonathan A. Rees and Norman I. Adams IV.
  T: a dialect of Lisp or LAMBDA:
  The ultimate software tool.
  In Proceedings of the 1982 ACM symposium
  on LISP and functional programming,
  pages 114–122, New York, NY, USA, 1982. ACM Press.

如果使用 0 作为 int 的 tag 的话，
好像对于所生成的汇编而言也不是不能接受。

TODO

# 8 Chez Scheme Version 5
# 9 Chez Scheme Version 6
# 10 Chez Scheme Version 7

# 11 Parting remarks

> Developing a system like Chez Scheme is a process of successive
> refinement. Even had I known in 1983 what I know today, it would
> still have been unimaginably difficult to create Chez Scheme, as it
> exists today, in one continuous development effort.  Had I tried
> anything like that, I would likely have abandoned the effort before
> it was fairly begun. Instead, I started with a simple model for
> representing Scheme values, a small compiler that performed little
> optimization, a simple stop-and-copy garbage collector, and a small
> run-time system. From there it grew, and shrank, as features were
> added, extended, removed, and replaced.

一切复杂的系统都是从简单开始的。

> Our priorities have become more refined over time as well,
> especially where efficiency is concerned. I’ve always believed that
> efficiency is not merely a matter of the speed of compiled code, but
> also a matter of compile time, memory usage, and overall system
> responsiveness. Over the years, I have come to believe that
> uniformity and continuity of performance are important as well. I
> have also become much more concerned about handling large programs
> or programs that operate on large amounts of data. Beyond efficiency
> and reliability, additional priorities have emerged, such as
> standards conformance, ability to interact gracefully with code
> written in other languages, and overall system usability.

> Even after two decades of refinement, the system is no where near
> what I’d like it to be. There are numerous ways in which the
> performance can be improved, numerous places where the code could be
> cleaner, and numerous features I’d like to add or extend.  Our
> to-do list has hundreds of entries, ranging from straightforward
> chores to research projects. I’m not complaining. If the to-do list
> is ever empty, I’ll know it’s time to pack it in.

- deepseek：

  “Pack it in” 的字面意思是“把东西打包收拾起来”。
  想象一下在野餐或露营结束后，
  你把所有工具和物品收拾进包里准备回家。
