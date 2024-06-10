---
title: Propagation Networks
subtitle: A Flexible and Expressive Substrate for Computation
author: Alexey Andreyevich Radul
year: 2009
---

# Abstract

> The propagation paradigm replaces computing by global effects on a
> large memory with computing by networks of local, independent,
> stateless machines interconnected with stateful storage cells.

> The novel insight that should finally permit computing with
> general-purpose propagation is that a cell should not be seen as
> storing a value, but as accumulating _information about_ a value.

propagation 在 constraint processing 中很常用，
但是在 constraint processing 中，每个 propagation system 都是特殊的。
而这里要设计一个语言，使得人们可以用它实现任何 propagation system。

# 1 Time for a Revolution

> This dissertation introduces, explains, illustrates, and implements
> _general-purpose propagation._

> The key idea of propagating _mergeable_, _partial information_
> allows propagation to be used for general-purpose computation.

> The second key idea of making the merging mechanism _generic_ offers
> a new kind of modularity and composability.

> Finally a more technical contribution comes in the form of a
> structure for carrying _dependencies_, and a presentation of the
> fantastic consequences of combining dependencies with propagation.

## 1.1 Expression Evaluation has been Wonderful

> A program is a series of expressions, evaluated either
> for their value or for their effect.

其实，在设计语言时，我们无法避免 expression evaluation，
即便是要构造 graph 也需要用 expression evaluation 来表达。
用 expression 来构造 graph，也就是这里所说的 evaluated for effect。

## 1.2 But we Want More

列举了其他作者在突破传统的 expression evaluation paradigm 方面的努力：

- 2009-constraint-propagation--models-techniques-implementation--guido-tack.pdf
- 1995-logic--programming-and-prolog.pdf
- 2008-integrating-dataflow-evaluation-into-a-practical-higher-order-call-by-value-language.pdf

## 1.3 We Want More Freedom from Time

> Why? Why is evaluation of expressions, which has served us so
> faithfully for half a century, not good enough for all these people?
> The fundamental problem is time.  Time in evaluation-land is
> fundamentally constrained.  The successful evaluation of an
> individual expression, or, equivalently, the successful production
> of an individual value, inescapably marks a _point_ in time. The
> work done to produce that value came _before_; the work that will be
> done using that value comes _after_.

在之前 inet 的设计中，正是因为 call-by-value 这种具有确定时间顺序的语义，
使得我们可以用 expression evaluation 来构造 graph。

另外，既然 inet 和 propagator 都使用 expression evaluation 来构造 graph，
可否有一个一般的底层 graph builder 语言，
然后 inet 和 propagator 分别为 graph 语法的解释器呢？

## 1.4 Propagation Promises Liberty

> Fortunately, there is a common theme in all these efforts to escape
> temporal tyranny.  The commonality is to organize computation as a
> network of interconnected machines of some kind, each of which is
> free to run when it pleases, propagating information around the
> network as proves possible.  The consequence of this freedom is that
> the structure of the aggregate does not impose an order of time.
> Instead the implementation, be it a constraint solver, or a logic
> programming system, or a functional reactive system, or what have
> you is free to attend to each conceptual machine as it pleases, and
> allow the order of operations to be determined by the needs of the
> _solution_ of the problem at hand, rather then the structure of the
> problem's _description_.

也就是构造图的过程与解释图的过程分离。
构造图的 expression 不是语义，
而是语法，或者说构造出来的图是具体语法，
而解释图的程序才是语义。

这个论文要设计一般的 propagation 语言，
来统一上面提到的几个领域：

- constraint processing
- logic programming
- reactive programming

> I believe that general-purpose propagation offers an opportunity of
> revolutionary magnitude. The move from instructions to expressions
> led to an immense gain in the expressive power of programming
> languages, and therefore in understanding of computers and
> productivity when building computing artifacts. The move from
> expressions to propagators is the next step in that path. This
> dissertation merely lifts one foot -- who knows what wonders lie
> just around the corner?

把 expression 到 propagator 的发展，
与 instruction 到 expression 的发展相比较。

可能有点牵强，哈哈。

# 2 Design Principles

前一章阐释发展 propagator 之必要，这一章讲如何发展。
从之前几十年 propagator 的发展中总结一些原则出来。

在总结准则的时候，还树立了一个明确的「敌人」，即「时间」这个概念。

## 2.1 Propagators are Asynchronous, Autonomous, and Stateless

> To be explicit about the events in the system, let us say that the
> machines are not connected to each other directly, but through
> shared locations that can remember things which interested machines
> can read and write. Such locations are tradition- ally called
> **cells**. As the machines communicate with each other and perform
> their various computations, information will propagate through the
> cells of the network.  The machines are for this reason
> traditionally called **propagators**.

cells 与 propagators，
和 petri net 中的 places 与 transitions
这种二分图类似。

> The purpose of all this is to avoid too early a commitment to the
> timing of events in our computers, so that the computers can decide
> for themselves in what order to do things, guided by what they
> discover when performing the tasks we assign to them.  We should
> consequently not build into our infrastructure any timing
> commitments we can possibly avoid.

首先是 propagator 的 stateless 这一原则：

> Let us therefore posit that our propagators are of themselves
> memoryless. The idea of memory is inextricably entwined with the
> idea of time, so let us push all memory out of our computing
> propagators, and into the cells, who are dedicated to dealing with
> memory anyway. We lose no generality by doing this, because we can
> always equip any machine that needs to appear stateful with a
> private cell to hold its state.

说记忆总与时间有关，看起来确实如此。有趣。

关于 propagator 的 autonomous 和 asynchronous 这两个原则：

> Let us likewise posit that our propagators are autonomous,
> asynchronous, and always on -- always ready to perform their
> respective computations. This way, there is no notion of time
> embedded in questions of which device might do something when, for
> they are all always free to do what they wish. Since the cells are
> the system's memory, it is the cells' responsibility to provide
> interlocks appropriately to prevent any nasty memory corruption
> problems, and to prevent propagators from seeing the contents of an
> individual cell in an inconsistent state.

但是 cell 还是要有自己的机制来保证自己的状态是 consistent 的，
比如，一种方案是，用 actor model 来实现 cell 和 propagator，
propagator 和 cell 之间也用 message passing 来沟通。

## 2.2 We Simulate the Network until Quiescence

Propagator 网络可以用实际的硬件构造，
也可以用软件来模拟。
初期当然是用软件模拟。

构建 Network 就是编程，
Network 的运行结果就是计算结果。

Network 的运行有三种情况：

- 最终达到稳定状态（quiescence），可以用来代表计算结果。
- 无限趋近于一个稳定状态。
- 没有稳定状态，网络一直活跃。

> By "quiescent" I mean "no propagator will add any more information
> to any cell without further input from outside the network."

如何 simulate？这里还没提到。
可能的方案是，cell 每次变化之后，
都把自己已经变化了这件事告知一个 scheduler，
可以用 actor model 实现，也可以是单线程程序中简单的一个 queue。

## 2.3 Cells Accumulate Information

> We should think of a cell as a thing that accumulates _information
> about_ a value.

这是这篇论文的主要贡献，即用 Lattice。

> The basic philosophical reason why cells must accumulate
> incrementally refinable information is that computation in
> propagation networks is essentially multidirectional.  Since several
> propagators can point into the same cell, there is no reason to
> demand that just one of them be responsible for producing the whole
> of that cell's contents.  In fact, the whole point of building
> propagation systems is to capitalize on the possibility of
> multidirectionality -- and to do that, each cell must be able to
> accept and merge all the contributions of all the propagators that
> might write to it.

### 2.3.1 Why don't cells store values?

如果一个 cell 已经有某个 value 了，
又有 propagator 向这个 cell 输出 value 的时候，
应该如何更新 cell？

最自然的方式就是用 lattice 的 join 或 meet。

也许在实现的时候，
这个 lattice 操作应该由 cell 自身来进行，
propagator 只负责把信息发过去。

- 也就是后文说的 making the cells a little smarter.

### 2.3.2 Because trying to store values causes trouble

如果没有 lattice，遇到上面的情况时，就有三种选项：

- A. Drop the second value on the floor,
- B. Overwrite the first value, or
- C. Forbid the event (signal an error).

这些方案的特点是都不看 value 是什么，
也不需要知道 value 的结构，
但是这些方案显然都有问题。

### 2.3.3 And accumulating information is better

> We solve this pile of problems at a stroke by changing the
> mindset. Instead of thinking of a cell as an object that stores a
> _value_, think of a cell as an object that stores _everything you
> know about a value_.

赋 value 以 lattice 结构才是正确的方案。

遇到 contradiction 的时候，可以分叉出来一个新的世界观。
但这一切还在 lattice 的范围内，
只是某些特殊的高阶 lattice 而已。

> The point of this dissertation is that propagating and accumulating
> partial information is enough for a complete, expressive, and
> flexible model of computation.

lattice 作为抽象的数学结构，
是一个 interface，
cell 只要实现这个 interface 就好。

# 3 Core Implementation

我先按照 "The Art" 中的顺序实现一版 propagator，
再回到这里的顺序。

TODO

# 4 Dependencies
# 5 Expressive Power
# 6 Towards a Programming Language
# 7 Philosophical Insights



# Bibliography

[Abdelmeged et al., 2007] Ahmed Abdelmeged, Christine Hang, Daniel Rinehart, and Karl Lieberherr (2007). Superresolution and P-Optimality in Boolean MAX-CSP Solvers. Transition.

[Abelson et al., 1996] Harold Abelson, Gerald Jay Sussman, and Julie Sussman (1984, 1996). Structure and Interpretation of Computer Programs. MIT Press, Cambridge, MA.

[Allen et al., 2005] E. Allen, D. Chase, V. Luchangco, J.W. Maessen, S. Ryu, G.L. Steele Jr, S. Tobin-Hochstadt, J. Dias, C. Eastlund, J. Hallett, et al. (2005). The Fortress Language Specification. Sun Microsystems.

[Andersen, 1837] Hans Christian Andersen (1837). The Emperor’s New Clothes. In Tales for Children.

[Apt, 2003] Krzysztof R. Apt (2003). Principles of Constraint Programming. Cambridge University Press, Cambridge, UK.

[Apt et al., 1999] Krzysztof R. Apt, Victor W. Marek, Miroslaw Truszczynski, and David S. Warren, editors (1999). The Logic Programming Paradigm: a 25-Year Perspective. Springer, Berlin; New York.

[Athanassakis, 1983] Apostolos N. Athanassakis (1983). Hesiod: Theogony, Works and Days and The Shield of Heracles. Johns Hopkins University Press, Baltimore and London. p.90.

[Bacchus et al., 2003] Fahiem Bacchus, Shannon Dalmao, and Toniann Pitassi (2003). Value Elimination: Bayesian Inference via Backtracking Search. In Uncertainty in Artificial Intelligence, pages 20–28.

[Bonawitz, 2008] Keith A. Bonawitz (2008). Composable Probabilistic Inference with Blaise. CSAIL Tech Report MIT-CSAIL-TR-2008-044, MIT Computer Science and Artificial Intelligence Laboratory, Cambridge, MA.

[Borning, 1979] Alan H. Borning (1979). ThingLab–a Constraint-Oriented Simulation Laboratory. PhD thesis, Stanford University, Stanford, CA, USA.

[Borning, 1981] Alan H. Borning (1981). The Programming Language Aspects of ThingLab, a Constraint-Oriented Simulation Laboratory. ACM Transactions on Programming Languages and Systems, 3(4):353–387.

[Bricklin and Frankston, 1999] Dan Bricklin and Bob Frankston (1999). VisiCalc: Information from its Creators, Dan Bricklin and Bob Frankston. http://bricklin.com/visicalc.htm.

[Calandra, 1961] Alexander Calandra (1961). The Teaching of Elementary Science and Mathematics. Washington University Press, St. Louis.

[Calandra, 1968] Alexander Calandra (1968). Angels on a Pin. Saturday Review.

[Cooper, 2008] Gregory H. Cooper (2008). Integrating Dataflow Evaluation into a Practical Higher-Order Call-by-Value Language. PhD thesis, Brown University.

[Cooper and Krishnamurthi, 2004] Gregory H. Cooper and Shriram Krishnamurthi (2004). FrTime: Functional Reactive Programming in PLT Scheme. Computer science technical report CS-03-20, Brown University.

[Cooper and Krishnamurthi, 2006] Gregory H. Cooper and Shriram Krishnamurthi (2006). Embedding Dynamic Dataflow in a Call-by-Value Language. Lecture Notes in Computer Science, 3924:294.

[Cormen et al., 2001] T.H. Cormen, C.E. Leiserson, R.L. Rivest, and C. Stein (2001). Introduction to Algorithms. MIT Press, Cambridge, MA.

[de Kleer, 1976] Johan de Kleer (1976). Local Methods for Localizing Faults in Electronic Circuits. AI Memo 394, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[de Kleer and Brown, 1992] Johan de Kleer and John Seely Brown (1992). Model-Based Diagnosis in SOPHIE III. Readings in Model-Based Diagnosis.

[Dinesman, 1968] Howard P. Dinesman (1968). Superior Mathematical Puzzles, with Detailed Solutions. Simon and Schuster, New York, NY.

[Doyle, 1978] Jon Doyle (1978). Truth Maintenance Systems for Problem Solving. AI Memo 419, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[Doyle, 1890] Sir Arthur Conan Doyle (1890). The Sign of the Four. Lippincott’s Monthly Magazine. Available online at http://www.gutenberg.org/files/2097/2097-h/2097-h.htm.

[Driscoll et al., 1989] James R. Driscoll, Neil Sarnak, Daniel D. Sleator, and Robert E. Tarjan (1989). Making Data Structures Persistent. Journal of Computer and System Sciences, 38(1).

[Elliott and Hudak, 1997] Conal Elliott and Paul Hudak (1997). Functional Reactive Animation. In Proceedings of the second ACM SIGPLAN International Conference on Functional Programming, pages 263–273. Association for Computing Machinery, New York, NY.

[Erman et al., 1980] L.D. Erman, F. Hayes-Roth, V.R. Lesser, and D.R. Reddy (1980). The Hearsay-II Speech-Understanding System: Integrating Knowledge to Resolve Uncertainty. ACM Computing Surveys (CSUR), 12(2):213–253.

[Ernst et al., 1998] M. Ernst, C. Kaplan, and C. Chambers (1998). Predicate Dispatching: A Unified Theory of Dispatch. Lecture Notes in Computer Science, pages 186–211.

[Floyd, 1967] Robert W. Floyd (1967). Nondeterministic Algorithms. Journal of the ACM (JACM), 14(4):636–644.

[Forbus and de Kleer, 1993] Kenneth D. Forbus and Johan de Kleer (1993). Building Problem Solvers. MIT Press, Cambridge, MA.

[Friedman and Wand, 2007] Daniel P. Friedman and Mitchell Wand (2007). Essentials of Programming Languages. MIT Press, Cambridge, MA, 3rd edition.

[Goodman et al., 2008] N.D. Goodman, V.K. Mansinghka, D. Roy, K. Bonawitz, and J.B. Tenenbaum (2008). Church: a Language for Generative Models. In Uncertainty in Artificial Intelligence.

[Gosling et al., 2005] J. Gosling, B. Joy, G. Steele, and G. Bracha (2005). The Java (TM) Language Specification. Addison-Wesley Professional.

[Gupta et al., 1996] Vineet Gupta, Radha Jagadeesan, and Vijay Saraswat (1996). Models for Concurrent Constraint Programming. Lecture Notes in Computer Science, pages 66–83.

[Hand, 2009] Linda Hand (2009). Have you found all three easter eggs in this document? In Look Carefully, page 7. MIT Press, Cambridge, MA.

[Hanson, 2007] Chris Hanson (2007). Personal communication.

[Hanson et al., 2005] Chris Hanson et al. (2005). MIT/GNU Scheme Reference Manual. Massachusetts Institute of Technology, Cambridge, MA. http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/index.html. The code presented in this dissertation was run specifically on MIT/GNU Scheme Release 7.7.90.+, Snapshot 20080401, under a GNU/Linux operating system.

[Hayes-Roth, 1985] Frederick Hayes-Roth (1985). Rule-Based Systems. Communications of the ACM, 28(9):921–932.

[Heath, 1921] Thomas Little Heath (1921). A History of Greek Mathematics. Clarendon Press, Oxford.

[Hewitt, 1969] Carl E. Hewitt (1969). Planner: A Language for Proving Theorems in Robots. In Proceedings of the International Joint Conference on Artificial Intelligence, pages 295–301.

[Hudak et al., 1999] Paul Hudak, John Peterson, and Joseph H. Fasel (1999). A Gentle Introduction to Haskell 98. Online tutorial.

[Jaynes, 2003] Edwin T. Jaynes (2003). Probability Theory: The Logic of Science. Cambridge University Press, Cambridge, UK.

[Johnston et al., 2004] W.M. Johnston, J.R.P. Hanna, and R.J. Millar (2004). Advances in Dataflow Programming Languages. ACM Computing Surveys, 36(1):1–34.

[Jones, 2002] Simon L. Peyton Jones (2002). Tackling the Awkward Squad: Monadic Input/Output, Concurrency, Exceptions, and Foreign-Language Calls in Haskell. Engineering theories of software construction.

[Kelsey et al., 1998] Richard Kelsey, William D. Clinger, Jonathan Rees, et al. (1998). Revised5 Report on the Algorithmic Language Scheme. SIGPLAN Notices, 33(9):26–76.

[Kiczales et al., 1999] Gregor Kiczales, Daniel G. Bobrow, and Jim des Rivières (1999). The Art of the Metaobject Protocol. MIT Press, Cambridge, MA.

[Konopasek and Jayaraman, 1984] Milos Konopasek and Sundaresan Jayaraman (1984). The TK! Solver Book: a Guide to Problem-Solving in Science, Engineering, Business, and Education. Osborne/McGraw-Hill.

[Liang et al., 1995] S. Liang, P. Hudak, and M. Jones (1995). Monad Transformers and Modular Interpreters. In Proceedings of the 22nd ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, pages 333–343. Association for Computing Machinery, New York, NY.

[Lieberherr, 1977] Karl Lieberherr (1977). Information Condensation of Models in the Propositional Calculus and the P=NP Problem. PhD thesis, ETH Zurich. 145 pages, in German.

[Lloyd, 1987] J.W. Lloyd (1987). Foundations of Logic Programming. SpringerVerlag, New York, NY.

[McAllester, 1978] David Allen McAllester (1978). A Three Valued Truth Maintenance System. AI Memo 473, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[McAllester, 1990] David Allen McAllester (1990). Truth Maintenance. In Proceedings of the Eighth National Conference on Artificial Intelligence, volume 2, pages 1109–1116. AAAI Press.

[McCarthy, 1963] John McCarthy (1963). A Basis for a Mathematical Theory of Computation. In P. Braffort and D. Hirschberg, editors, Computer Programming and Formal Systems, pages 33–70. North-Holland, Amsterdam.

[Mikkelson and Mikkelson, 2007] Barbara Mikkelson and David Mikkelson (2007). The Barometer Problem. http://www.snopes.com/college/exam/barometer.asp.

[Milch et al., 2005] B. Milch, B. Marthi, S. Russell, D. Sontag, D.L. Ong, and A. Kolobov (2005). BLOG: Probabilistic Models with Unknown Objects. In Proceedings of the Nineteenth Joint Conference on Artificial Intelligence.

[Müller, 2001] Tobias Müller (2001). Constraint Propagation in Mozart. PhD thesis, Universität des Saarlandes, Saarbrücken.

[Nilsson et al., 2002] H. Nilsson, A. Courtney, and J. Peterson (2002). Functional Reactive Programming, Continued. In Proceedings of the 2002 ACM SIGPLAN workshop on Haskell, pages 51–64. Association for Computing Machinery, New York, NY.

[Nilsson and Małuszyński, 1995] Ulf Nilsson and Jan Małuszyński (1995). Logic, Programming and Prolog. Wiley, second edition.

[Norvig, 2004] Peter Norvig (2004). Paradigms of Artificial Intelligence Programming: Case Studies in Common LISP. Morgan Kaufmann.

[Ockham, 1340] William of Ockham (ca. 1340). Ockham’s razor.

[Park et al., 2005] S. Park, F. Pfenning, and S. Thrun (2005). A Probabilistic Language Based upon Sampling Functions. In Proceedings of the 32nd ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, pages 171–182. Association for Computing Machinery, New York, NY.

[Pfeffer, 2001] Avi Pfeffer (2001). IBAL: A Probabilistic Rational Programming Language. In International Joint Conferences on Artificial Intelligence, pages 733–740.

[Pfeffer, 2007] Avi Pfeffer (2007). The Design and Implementation of IBAL: A General-Purpose Probabilistic Language. In An Introduction to Statistical Relational Learning, pages 399–432. MIT Press, Cambridge, MA.

[Piponi, 2006] Dan Piponi (2006). You Could Have Invented Monads! (And Maybe You Already Have.). Weblog post. http://sigfpe.blogspot.com/2006/08/you-could-have-invented-monads-and.html.

[Puget, 1998] J.F. Puget (1998). A Fast Algorithm for the Bound Consistency of Alldiff Constraints. In Proceedings of the National Conference on Artificial Intelligence, pages 359–366. John Wiley & Sons Ltd.

[Radul, 2007] Alexey Radul (2007). Report on the Probabilistic Language Scheme. In DLS ’07: Proceedings of the 2007 symposium on Dynamic languages, pages 2–10. Association for Computing Machinery, New York, NY. http://hdl.handle.net/1721.1/39831.

[Radul and Sussman, 2009a] Alexey Radul and Gerald Jay Sussman (2009a). The (Abridged) Art of the Propagator. In Proceedings of the 2009 International Lisp Conference, pages 41–56, Cambridge, MA. Association of Lisp Users, Sterling, Virginia, USA.

[Radul and Sussman, 2009b] Alexey Radul and Gerald Jay Sussman (2009b). The Art of the Propagator. CSAIL Tech Report MIT-CSAIL-TR-2009-002, MIT Computer Science and Artificial Intelligence Laboratory, Cambridge, MA. http://hdl.handle.net/1721.1/44215.

[Ramsey and Pfeffer, 2002] Norman Ramsey and Avi Pfeffer (2002). Stochastic Lambda Calculus and Monads of Probability Distributions. Proceedings of the 29th ACM SIGPLAN-SIGACT symposium on Principles of Programming Languages, pages 154–165.

[Royce, 1970] Winston W. Royce (1970). Managing the Development of Large Software Systems. In Proceedings, IEEE WESCON, pages 1–9.

[Russell and Norvig, 1995] Stuart J. Russell and Peter Norvig (1995). Artificial Intelligence: a Modern Approach. Prentice-Hall, Inc. Upper Saddle River, NJ, USA.

[Schulte and Stuckey, 2008] C. Schulte and P.J. Stuckey (2008). Efficient Constraint Propagation Engines. Transactions on Programming Languages and Systems, 31(1).

[Shivers, 1991] Olin Shivers (1991). Control-Flow Analysis of Higher-Order Languages. PhD thesis, Carnegie Mellon University, Pittsburgh, PA. CMU-CS-91-145.

[Shivers, 1999] Olin Shivers (1999). SRFI 1: List Library. Scheme Requests for Implementation. http://srfi.schemers.org/srfi-1/srfi-1.html.

[Siskind and McAllester, 1993] Jeffrey Mark Siskind and David Allen McAllester (1993). Screamer: A Portable Efficient Implementation of Nondeterministic Common Lisp. Technical report IRCS-93-03, University of Pennsylvania Institute for Research in Cognitive Science.

[Sitaram, 2004] Dorai Sitaram (2004). Teach Yourself Scheme in Fixnum Days. Online at http://www.ccs.neu.edu/home/dorai/ty-scheme/ty-scheme.html.

[Smolka, 1995] Gert Smolka (1995). The Oz Programming Model. Lecture Notes in Computer Science, 1000:324–343.

[Stallman and Sussman, 1977] Richard Matthew Stallman and Gerald Jay Sussman (1977). Forward Reasoning and Dependency-Directed Backtracking in a System for Computer-Aided Circuit Analysis. Artificial Intelligence, 9:135–196.

[Steele Jr., 1980] Guy L. Steele Jr. (1980). The Definition and Implementation of a Computer Programming Language Based on Constraints. AI Memo 595, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[Steele Jr. and Sussman, 1980] Guy L. Steele Jr. and Gerald Jay Sussman (1980). Constraints-A Language for Expressing Almost-Hierarchical Descriptions. Artificial Intelligence, 14(1):1–39.

[Strachey, 1967] Christopher Strachey (1967). Fundamental Concepts in Programming Languages. Lecture notes for International Summer School in Computer Programming.

[Strachey, 2000] Christopher Strachey (2000). Fundamental Concepts in Programming Languages. Higher-Order and Symbolic Computation, 13(1):11–49.

[Stroustrup et al., 1991] Bjarne Stroustrup et al. (1991). The C++ Programming Language. Addison-Wesley Reading, MA.

[Sulzmann and Stuckey, 2007] M. Sulzmann and P.J. Stuckey (2007). HM (X) Type Inference is CLP (X) Solving. Journal of Functional Programming, 18(02):251–283.

[Sussman et al., 2001] Gerald Jay Sussman, Jack Wisdom, and M.E. Mayer (2001). Structure and Interpretation of Classical Mechanics. MIT Press, Cambridge, MA.

[Sutherland, 1963] Ivan E. Sutherland (1963). SketchPad: A Man-Machine Graphical Communication System. PhD thesis, Massachusetts Institute of Technology, Cambridge, MA.

[Tack, 2009] Guido Tack (2009). Constraint Propagation - Models, Techniques, Implementation. PhD thesis, Saarland University, Germany.

[Van Hentenryck, 1989] Pascal Van Hentenryck (1989). Constraint Satisfaction in Logic Programming. MIT Press, Cambridge, MA.

[Van Roy, 2005] Peter Van Roy (2005). Multiparadigm Programming in Mozart/Oz. Springer.

[Van Roy and Haridi, 2004] Peter Van Roy and Seif Haridi (2004). Concepts, Techniques, and Models of Computer Programming. MIT Press, Cambridge, MA.

[Waltz, 1972] David L. Waltz (1972). Generating Semantic Description from Drawings of Scenes with Shadows. PhD thesis, Massachusetts Institute of Technology, Cambridge, MA.

[Waltz, 1975] David L. Waltz (1975). Understanding Line Drawings of Scenes with Shadows. The Psychology of Computer Vision, pages 19–91.

[Wan and Hudak, 2000] Z. Wan and Paul Hudak (2000). Functional Reactive Programming from First Principles. ACM SIGPLAN Notices, 35(5):242–252.

[Zabih, 1987] Ramin Zabih (1987). Dependency-Directed Backtracking in Non-Deterministic Scheme. Master’s thesis, Massachusetts Institute of Technology, Cambridge, MA.

[Zabih, 1998] Ramin Zabih (1998). Dependency-Directed Backtracking in Non-Deterministic Scheme. AI Memo 956, MIT Artificial Intelligence Laboratory, Cambridge, MA.

[Zabih et al., 1987] Ramin Zabih, David Allen McAllester, and David Chapman (1987). Non-Deterministic Lisp with Dependency-Directed Backtracking. In Proceedings of AAAI 87, pages 59–64.
