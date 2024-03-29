---
title: Differences between miniKanren and Prolog
author: Will Byrd
---

One of the most commonly asked questions about miniKanren is how
miniKanren differs from Prolog. Here is the answer Will Byrd gave to
[this question on Stack
Overflow](https://stackoverflow.com/questions/28467011/what-are-the-main-technical-differences-between-prolog-and-minikanren-with-resp).

This is a tricky question to answer, largely because there are so many
variants of both miniKanren and Prolog. miniKanren and Prolog are
really families of languages, which makes it difficult to compare
their features, or even how they are used in practice. Because of
this, please take everything I'm about to say with caution: if I say
that Prolog uses depth-first search, be aware that many Prolog
implementations support other search strategies, and that alternate
search strategies can also be encoded at the meta-interpreter
level. Still, miniKanren and Prolog have different design
philosophies, and make different trade-offs.

Prolog is one of the two classic languages for symbolic artificial
intelligence programming (the other classic language being
Lisp). Prolog excels at implementing symbolic rule-based systems in
which declarative knowledge is encoded in first-order logic. The
language is optimized for expressiveness and efficiency for these
types of applications, sometimes at the expense of logical purity. For
example, by default Prolog does not use the "occur check" in
unification. From a math/logic standpoint, this version of unification
is incorrect. However, the occur check is expensive, and in most cases
the lack of the occur check is not a problem. This is a very pragmatic
design decision, as is Prolog's use of depth-first search, and use of
cut (!) to control backtracking. I'm sure these decisions were
absolutely necessary when running on the hardware of the 1970s, and
today are very useful when working on large problems, and when dealing
with huge (often infinite!) search spaces.

Prolog supports many "extra-logical" or "non-logical" features,
including cut, assert and retract, projection of variables for
arithmetic using is, and so forth. Many of these features make it
easier to express complex control flow, and to manipulate Prolog's
global database of facts. One very interesting feature of Prolog is
that Prolog code is itself stored in the global database of facts, and
can be queried against at run time. This makes it trivial to write
meta-interpreters that modify the behavior of Prolog code under
interpretation. For example, it is possible to encode breadth-first
search in Prolog using a meta-interpreter that changes the search
order. This is an extremely powerful technique that is not well known
outside of the Prolog world. 'The Art of Prolog' describes this
technique in detail.

Tremendous effort has gone into improving Prolog implementations, most
of which are based on the Warren Abstract Machine (WAM). The WAM uses
a side-effecting model in which values are destructively assigned to
logic variables, with these side-effects being undone upon
backtracking. Many features can be added to Prolog by extending the
instructions of the WAM. One disadvantage of this approach is that
Prolog implementation papers can be difficult to read without a solid
understanding of the WAM. On the other hand, Prolog implementer have a
common model for discussing implementation issues. There has been a
great deal of research in parallel Prolog, culminating in Andorra
Prolog in the 1990s. At least some of these ideas live on in Ciao
Prolog. (Ciao Prolog is full of interesting ideas, many of which go
far beyond the Prolog standard.)

Prolog has a beautiful unification-based "pattern-matching"-style
syntax that results in very succinct programs. Prologers love their
syntax, just like Lispers love their s-expressions. Prolog also has a
large library of standard predicates. Due to all of the engineering
that has gone into making the WAM fast, there are very capable and
mature Prolog implementations. As a result, many large knowledge-based
systems have been written entirely in Prolog.

miniKanren was designed as a minimal logic programming language, with
a small, easily understandable, and easily hackable
implementation. miniKanren was originally embedded in Scheme, and has
been ported to dozens of other host languages over the past
decade. The most popular miniKanren implementation is 'core.logic' in
Clojure, which now has many Prolog-like extensions and a number of
optimizations. Recently the core of the miniKanren implementation has
been simplified even further, resulting in a tiny "micro kernel"
called "microKanren." miniKanren can then be implemented on top of
this microKanren core. Porting microKanren or miniKanren to a new host
language has become a standard exercise for programmers learning
miniKanren. As a result, most popular high-level languages have at
least one miniKanren or microKanren implementation.

The standard implementations of miniKanren and microKanren contain no
mutation or other side-effects, with a single exception: some versions
of miniKanren use pointer equality for comparison of logic
variables. I consider this a "benign effect," although many
implementations avoid even this effect by passing a counter through
the implementation. There is also no global fact
database. miniKanren's implementation philosophy is inspired by
functional programming: mutation and effects should be avoided, and
all language constructs should respect lexical scope. If you look
carefully at the implementation you might even spot a couple of
monads. The search implementation is based on combining and
manipulating lazy streams, once again without using mutation. These
implementation choices lead to very different trade-offs than in
Prolog. In Prolog, variable lookup is constant time, but backtracking
requires undoing side-effects. In miniKanren variable lookup is more
expensive, but backtracking is "free." In fact, there is no
backtracking in miniKanren, due to how the streams are handled.

One interesting aspect of the miniKanren implementation is that the
code is inherently thread-safe and---at least in theory---trivially
parallelizable. Of course, parallelizing the code without making it
slower is not trivial, given that each thread or process must be given
enough work to make up for the overhead of parallelization. Still,
this is an area of miniKanren implementation that I hope will receive
more attention and experimentation.

miniKanren uses the occur check for unification, and uses a complete
interleaving search instead of depth-first search. Interleaving search
uses more memory than depth-first search, but can find answers in some
cases in which depth-first search will diverge/loop
forever. miniKanren does support a few extra-logical
operators---conda, condu, and project, for example. conda and condu
can be used to simulate Prolog's cut, and project can be used to get
the value associated with a logic variable.

The presence of conda, condu, and project---and the ability to easily
modify the search strategy---allows programmers to use miniKanren as
an embedded Prolog-like language. This is especially true for users of
Clojure's 'core.logic', which includes many Prolog-like
extensions. This "pragmatic" use of miniKanren seems to account for
the majority of miniKanren's use in industry. Programmers who want to
add a knowledge-based reasoning system to an existing application
written in Clojure or Python or JavaScript are generally not
interested in rewriting their entire application in Prolog. Embedding
a small logic programming language in Clojure or Python is much more
appealing. An embedded Prolog implementation would work just as well
for this purpose, presumably. I suspect miniKanren has become popular
as an embedded logic language because of the tiny and pure core
implementation, along with the talks, blog posts, tutorials, and other
educational materials that have come out since 'The Reasoned Schemer'
was published.

In addition to the use of miniKanren as a pragmatic embedded logic
programming language similar in spirit to Prolog, miniKanren is being
used for research in "relational" programming. That is, in writing
programs that behave as mathematical relations rather than
mathematical functions. For example, in Scheme the append function can
append two lists, returning a new list: the function call (append '(a
b c) '(d e)) returns the list (a b c d e). We can, however, also treat
append as a three-place relation rather than as a two-argument
function. The call (appendo '(a b c) '(d e) Z) would then associate
the logic variable Z with the list (a b c d e). Of course things get
more interesting when we place logic variables in other positions. The
call (appendo X '(d e) '(a b c d e)) associates X with (a b c), while
the call (appendo X Y '(a b c d e)) associates X and Y with pairs of
lists that, when appended, are equal to (a b c d e). For example X =
(a b) and Y = (c d e) are one such pair of values. We can also write
(appendo X Y Z), which will produce infinitely many triples of lists
X, Y, and Z such that appending X to Y produces Z.

This relational version of append can be easily expressed in Prolog,
and indeed is shown in many Prolog tutorials. In practice, more
complex Prolog programs tend to use at least a few extra-logical
features, such as cut, which inhibit the ability to treat the
resulting program as a relation. In contrast, miniKanren is explicitly
designed to support this style of relational programming. More recent
versions of miniKanren have support for symbolic constraint solving
(symbolo, numbero, absento, disequality constraints, nominal logic
programming) to make it easier to write non-trivial programs as
relations. In practice I never use any of the extra-logical features
of miniKanren, and I write all of my miniKanren programs as
relations. The most interesting relational programs are the relational
interpreters for a subset of Scheme. These interpreters have many
interesting abilities, such as generating a million Scheme programs
that evaluate to the list (I love you), or trivially generating quines
(programs that evaluate to themselves).

miniKanren makes a number of trade-offs to enable this relational
style of programming, which are very different from the trade-offs
Prolog makes. Over time miniKanren has added more symbolic
constraints, really becoming a symbolically-oriented Constraint Logic
Programming language. In many cases these symbolic constraints make it
practical to avoid using extra-logical operators like condu and
project. In other cases, these symbolic constraints are not
sufficient. Better support for symbolic constraints is one active area
of miniKanren research, along with the broader question of how to
write larger and more complex programs as relations.

In short, both miniKanren and Prolog have interesting features,
implementations, and uses, and I think it is worth learning the ideas
from both languages. There are other very interesting logic
programming languages as well, such as Mercury, Curry, and Gödel, each
of which has its own take on logic programming.
