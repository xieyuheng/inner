---
title: some philosophical aspects of combinatory logic
author: haskell curry
year: 1980
---

# My Motive

[2025-06-22] 想了解 combinatory logic 背后的历史。

# My Notes

[2025-06-23] 这篇论文中一个需要记住的 idea 来自 Schönfinkel，
即带有 application 这个二元运算的代数结构，是 free magma。
combinatory logic 的形式系统（formal system），
可以理解为是以这个 free magma 为基础发展而来的。

magma 是 applicative language 所对应的代数结构，
类似 forth 的 concatenative language 所对应的代数结构，
是 groupoid 或 category。

从代数的角度去理解形式语言，
可以让我们把类型系统理解为两个代数结构之间的关系。
比如类型系统中个的 infer 就是代数结构之间的同态：

    (infer (f a)) = ((infer f) (infer a))

如何理解 dependent type 的依赖值的 infer：

    (infer (f a)) = (compute-type (infer f) (infer a) f a)

感觉可以通过让 infer 的结果带上类型，来把等式右边转化为：

    ((infer f) (infer a))

即，通过添加元素丰富 type 的子空间。
比如 `(infer <exp>)` 的结果，
可以是类似 pair 的 `(the <type> <normal-exp>)`，
用来记住所得到的 type 是经由什么 exp infer 而来。

# Dedication

> Dedicated to Professor S. C. Kieene
> on the occasion of his 70th birthday

# Abstract

> This is a discussion of some philosophical criticisms of combinatory
> logic, preceded by a brief survey to give background.

# 0 Introduction

> Combinatory Logic is a branch of mathematical logic which is
> concerned with the ultimate foundations. It is not an independent
> system of logic, competing with the theory of types, abstract set
> theory, mereology, or what not; nor does it attempt to form a
> consistent system adequate for this or that portion of classical
> mathematics. Rather it forms a common substratum for a variety of
> such theories. This paper will discuss some philosophical questions
> which have been raised concerning it; but first I shall survey its
> principal features in order to give a background for the discussion.

# 1 Origins

> It began, so far as I am concerned, with an attempt to analyze the
> process of substitution. In practically all logical systems before
> 1930 -- and many since -- substitution is taken as a completely
> unanalyzed operation.  As such it is a highly complex process --
> vastly more so than such processes as modus ponens. In order to
> express it in terms of processes comparable in complexity to modus
> ponens, a new approach is necessary.

介绍带有 currying 的 application 的想法来自 Schönfinkel。

> ..., In it Schönfinkel introduced a binary operation since called
> _application_. He wrote the application of an object f to an object
> a as (f a).

注意，这里还是在用 binary operation 来理解 application。

# 2 The system H

> Now suppose we have a formal system H concerning certain objects
> which I shall call _obs_ (some persons prefer to call them
> terms). The obs are generated from certain primitive ones, the
> _atoms_, by the operation of application. The (elementary)
> statements of the system are formed by the binary predicate
> (statement function) = . This has the usual properties, as follows:
> for arbitrary obs X, Y, Z,
>
> - X = X (reflexiveness),
> - X = Y -> Y = X (symmetry),
> - X = Y & Y = Z -> X = Z (transitivity),
> - X = Y -> (Z X) = (Z Y) (left monotony),
> - X = Y -> (X Z) = (Y Z) (right monotony).

引入：

- Objects (Terms):
  - Atoms:
    - Constants: S, K, I。
    - Indeterminates: x, y, z, ... -- 就是变元（variables）。
  - Combinations: Objects 根据 application 形成新的 Object。
- Combinators: 不带变元的 Combinations。

我们可以把 Object 理解为 Exp，
没有 Exp 和 Value 之分，
只有 Exp 和 Exp 上定义的基础等价关系。

定义 S，K，I 的等式被视为是形式系统中的 axiom schemes。
对于 axiom scheme 也需要引入 substitution 的概念，
但是由于没有 bound variable，所以处理起来比较简单。

下面介绍如何消除 lambda abstraction。
就是找一个行为上和 lambda abstraction 等价的组合。
注意，这里只用到了 lambda abstraction 的表达式，
而没有用到 lambda calculus 相关的等价关系。

TODO 用 lisp 和 compiler 代码记录这里的算法。

这里的算法很有意思，这些算法就是 S，K，I 的来源。
也就是说 S，K，I 是为了保证这里的算法有效，列方程解出来的！

是否这种解方程的过程可以一般化，
使得我们可以在任意代数结构中找组合子？
但是注意，因为 application 不带有任何运算律的二元运算，
所以带有它的代数结构，已经是最一般的（带有一个二元运算的）代数结构了。

有什么推广的方向吗？

首先考虑多元运算。
currying 这个技巧已经告诉我们，
通过添加元素，就可以把多元运算转化为二元运算。

再考虑带有多个二元运算的代数结构（比如环）。
此时也可以通过把每个运算本身都作为一个元素添加到原结构中，
把这个代数结构化归为带有一个二元运算（application）的代数结构。

看来这个推广方向上，所有的代数结构都可以通过给原结构添加元素，
而划归到带有一个二元运算的代数结构中。

是这样的吗？

是这样的，因为不带有任何运算律的二元运算代数结构，
就是 free magma，根据 free 的定义，所有其他代数结构都可以由此扩展而来。

难怪在设计 interaction nets 和 propagator model 的语言时，
都可以通过给 applicative language 扩展而来。

那么还有什么推广的方向？
看来就是类型了。
但是注意，增加类型不会从 group 到 groupoid 或 category
（这是 concatenative language 的事情），
而是从 magma 到 typed magma。

typed lambda calculus，只是 typed magma 中非常特殊的一种。
比如对于 simple type 的 lambda calculus 而言，
(f a) 的返回类型只来自于 f 的类型，而不来自 a 的类型，
也就是说在 (infer f) 和 (infer a) 的类型之后，
用 infer 的结果计算新的类型 (infer (f a)) 时，
所用的计算规则是非常特殊的。
这是可以推广的方向！

比如 polymorphic type 和 dependent type
都是这个方向上的推广。

dependent type 时，
(infer (f a)) = (compute-type (infer f) (infer a) f a)。

实现类型系统时，context 可以用 propagator model 来处理。

想要这样实现类型系统的话，
还是需要一个简单的语言，
在遍历 Exp 的 tree 的过程中，
构造 propagator nets。

x-lisp 就是比较适合的简单语言！

# 3 Lambda-conversion

介绍 lambda 演算。

Substitution 称谓难点是因为有 bound variable。

> In this system, unlike H, variables can be bound, and complex
> restrictions have to be introduced to avoid collisions of bound
> variables; these restrictions I am ignoring here.

这里强调了 eta rule 必须被作为额外的等价规则加进来，
而不能从 alpha 和 beta rule 中推导出来。

# 4 Relations between the systems

> Evidently the notions [x]M and λxM have similar
> meanings. Rosser’s thesis (1935) initiated a study of their
> interrelations.

"A mathematical logic without variables", J. B. Rosser, 1935.

> One cannot simply identify them.

但是考虑等价关系，发现在 system H 中 (S K) 与 (K I) 不相等，
但是在 lambda calculus 中二者是相等的。

更一般的，可以说两个相等 lambda abstraction，
编译到 SKI 所得到的 exp 不一定相等，

通过增加新的 atom B 和 C，
从而引入新的 axiom schemes，
可以获得 lambda calculus 的等价关系
（也带有 eta rule）。

TODO 给出引入 B，C 后的新等价关系的例子。

由于两个系统可以相互转译，
并且尊重彼此的基础等价关系，
所以：

> Thus a system H and its corresponding λ-system are different aspects
> of essentially the same system. Consequently λ-calculus is an aspect
> of combinatory logic, and what I have to say may apply to either
> aspect. When it is desired to emphasize that we are dealing with a
> system H, I shall use the term “synthetic combinatory logic”. The
> synthetic aspect gives the more profound foundational analysis; but
> the λ-aspect is more intuitive, and thus better suited for
> applications (e.g. to computer programming).

在康德的 analytic-synthetic distinction 中：

- system H 算是 synthetic，
  因为 S 的意义由 (S f g x) 与 ((f x) (g x)) 等价给出。

- lambda calculus 算是 analytic，
  因为当把 S 定义为 (lambda (f g x) ((f x) (g x))) 时，
  其定义中已经包含了所要捕捉的等价关系。

可以看出 lambda calculus 之所以复杂，
就是因为它能以 analytic 的方式捕捉等式所定义的等价关系。

而 lambda calculus 能够被划归为公理化的 system H，是一件非常神奇的事。
这种公理化，一定让 Curry 想起了欧几里得。

# 5 The Church-Rosser theorem

> Are these systems consistent? Note that, although the symbols are
> capable of taking meanings of a very general nature, there is
> nothing akin to a theory of types. It is, therefore, necessary to
> show that not every statable equation can be proved. This was done
> in a rather crude fashion in my thesis (1930). But in 1936 Church
> and Rosser proved a more elegant theorem.

这里对 consistent 的定义，
就是 "not every statable equation can be proved"。

这里不是 rewrite system，
因为对等词相关的推演规则是没有方向的。
想要在没有方向的推演规则下做证明，一般需要搜索。

给等词相关的推演规则加上方向，
就得到了 rewrite system。
Church-Rosser theorem 就是描述这个
rewrite system 和原 system 之间的关系的。

这样看来，对于 rewrite system 而言，consistent 是非常平凡的。

这里我们应该记住的重点 idea 是：
通过 rewrite system 来研究一般的带有等词的 formal system，

类似的例子有 bidirectional type checking 中，
把 inference rules 转化为 functions。
或者说，巧妙设计 judgements（relations）与 inference rules，
使得 judgement 对于某个位置具有单值性。

注意，给定 inference rules 之后，formal system 就定下来了。
所以这里的研究一定是在设计 inference rules，
或者说尝试各种不同 inference rules 的组合。
而不是在给定 inference rules 之后，再转化为 functions。

比如，这里 Curry 在介绍 system H 和 lambda calculus 时，
都通过增减规则，或者修改规则，给出了一些列 system 的变体。

这么说来，设计 formal system 其实就是设计 relations，
只不过在逻辑式编程中所设计的 relations，
尽管不是函数，但是都可以通过搜索找到结果；
而 formal system 中所设计的 relations，
可能不能通过搜索找到结果。

按照这个思路，给 judgements + inference rules 所定义的 formal system 分类。
所分出来的类，就是判断 judgements 时的可计算与否，
还有可计算时，计算的复杂度。

除了给 formal system 分类，还要研究 formal system 之间的关系。
如果把 formal system 的语法层面本身当作数学对象（即不考虑指称语义），
formal system 之间的关系不只是传统意义上的同态与同构
（某些 formal system 甚至不包含等词）。
formal system 的一般关系，可能是由编译器所定义的更精妙的关系。

其实指称语义某种意义上就是 formal system 之间的关系，
但是不能这么说，因为 domain 本身可能不具备 formal system 的可数性。
即，从语法层面考虑，所有的表达式的集合总是可数的。

# 6 Combinatory arithmetic

介绍 Kleene 证明了可以用 Church 编码实现的算术函数，
与 partial recursive 等价。

> Thus representability in combinatory logic is a criterion of
> constructiveness which is equivalent to other well-known ones.

这里把 partial recursive 理解为 constructiveness criterion。
看来 recursion theory 值得深入学习。

# 7 Illative combinatory logic

> So far -- except for the abstract arithmetic-we have been
> considering only what is called _pure combinatory logic_.
> In order to base logic on this we need to introduce new atoms
> standing for the usual logical notions, such as implication,
> quantification, etc. Combinatory logic with the addition of such
> primitives has been called illative.

> For this we need to introduce a new predicate (statement functor),
> a unary one, which I indicate by prefixing the Frege assertion sign
> "|-". Thus if X is an ob,
>
>     |- X
>
> is the statement that X belongs to a category of obs called
> _assertions_.

> Let P, Π, Ξ, F be new obs, not necessarily all atoms,
> representing
>
> - P -- implication.
> - Π -- universality.
> - Ξ -- restricted generality (or formal implication).
> - F -- functionality (i.e., Fαβ represents being a function
>   from a domain α into a category β).

> then these will have the rules
>
> - RULE P. |- X & | (P X Y) -> |- Y (modus ponens)
> - RULE Π. |- (Π X) -> |- (X Y) for any ob Y
> - RULE Ξ. |- (Ξ X Y) & |- (X U) -> |- (Y U) for any ob U
> - RULE F. |- (F X Y Z) & |- (X U) -> |- (Y (Z U))

> It is not my purpose to go into illative combinatory logic here, but
> simply to mention its existence, and to give just enough information
> about it to give background for the philosophical criticisms which
> are the main purpose of this paper.

这类似于 dependent type 中只有 value（包含 type），而不区分 value 与 type，
作为 `|-` 参数的前提是 value 本身能被认为是 type。

# 8 The assertion sign

这一节可以看出，不同作者对 formal system 的理解方式不同。
也许应该将 formal system 直接定义为：
用某个程序语言来实现新的程序语言。

Curry 的 formal system 只有两层结构：

- Expression（value，object）。
- Judgement（statement，assertion）-- 由 predicate 作用于 object 而得。

注意，在 Martin-Löf 的定义中 judgement 与 assertion 等价，
但是 Curry 也把 predicate `|-` 称作 assertion。

> In the theory of formal systems it can be shown that one can
> reduce any system to one with a single unary predicate like `|-`.
> In combinatory logic there are two predicates, `|-` and `=` .
> One can eliminate equality by introducing a new atom Q,
> and defining `X = Y` as `Q X Y`.

# 9 Types

> A second point is that pure combinatory logic is a type-free
> structure, and some persons object to this on the ground that it
> involves a generality which transcends the theory of types.

> One answer is that the consistency of pure combinatory logic is
> guaranteed by the Church-Rosser theorem. Type theory was invented by
> Russell to explain the paradoxes; since inconsistencies (in the Post
> sense) cannot arise, types are unnecessary.

> However, pure combinatory logic is not a closed system. It is
> intended to be a basis to which we can adjoin the entities of
> ordinary logic; and in illative combinatory logic we can indeed have
> contradictions.

下面给出了一个在 illative combinatory logic 中不一致性的例子。
TODO 但是由于不熟悉 illative combinatory logic 我还没看懂这个例子。

# 10 Conceptualization

> Scott seems to argue that formalization without “conceptualization”
> is useless. What he means by this is not entirely clear; but it
> seems to mean an interpretation in terms of notions of ordinary
> mathematics.

Curry 是 Hilbert 的学生，所以也支持形式主义观点。
Curry 为形式主义辩护的论文：
1951-outlines-of-a-formalist-philosophy-of-mathematics

> One reason for his change of opinion seems to be that, after some
> effort, he discovered such interpretations -- in terms of lattices,
> sets of integers, etc. In so doing he added immensely to our
> knowledge about combinatory logic; but he did not, as he seems to
> imply, save it from damnation. While it is true that concocting
> formalisms entirely without regard to interpretation is probably
> fruitless, yet it is not necessary that there be "conceptualization"
> in terms of current mathematical intuitions. In fact, mathematical
> intuition is a result of evolution.

这种论点也很有说服力，并且是非常唯物的。

> Mathematicians depend on their intuitions a great deal; let us hope
> they always will. But the mathematical intuitions of today are not
> the same as those of a thousand years ago. Combinatory logic may not
> have had a conceptualization in what seems to be Scott’s sense; but
> it did have an interpretation by which it was motivated. The
> formation of functions from other functions by substitution does
> form a structure, and this structure it analyzed and formalized. For
> progress we need the freedom to let our intuitions develop further;
> this includes the possibility of formalizing in new ways.

这里说的也很对，因为模型论意义上的 interpretation，
其实是把语法层次的构造，当作数学世界的二等公民来看待了。

# 11 Priority of the function notion

> Another point about combinatory logic is the priority of the notion
> of function. In current mathematics the tendency is to think of a
> function in the sense of Dirichlet, i.e. essentially as a set of
> ordered pairs, whereby the first elements range over a set, the
> domain of the function, and when the first element of a pair is
> given, the second is uniquely determined. Thus the notion of set is
> more fundamental than that of function, and the domain of a function
> is given before the function itself. In combinatory logic, on the
> other hand, the notion of function is fundamental; a set is a
> function whose application to an argument may sometimes be an
> assertion, or have some other property; its members are those
> arguments for which the application has that property. The function
> is primary; its domain, which is a set, is another function. Thus it
> is simpler to define a set in terms of a function than vice versa;
> but the idea is repugnant to many mathematicians, and probably to
> Scott. This has been a great handicap and source of
> misunderstanding.

这里有一个注释：

> For a similar idea cf. the set theory of Von Neumann (1928)

引用是 1928-die-axiomatisierung-der-mengenlehre。

# 12 Finiteness of structure

> Another point worth mentioning is its essentially finite
> character. If equality is defined in terms of Q, then pure
> combinatory logic can be formulated in terms of a finite number of
> atoms, a finite number of axioms, and a finite number of simple
> rules.

> All other systems of logic and mathematics involve either a complex
> substitution process or axiom schemes involving infinitely many
> axioms, and, of course, infinitely many atoms. This is true even for
> propositional algebra. This finiteness of structure presumably holds
> for various forms of illative combinatory logic. It does not hold,
> however, even in pure combinatory logic [lambda calculus], for
> theories of reduction.

# 13 Pertinence to logic

> Another criticism by Scott is that the use of the word “logic” in
> “combinatory logic” is premature. Although this is a question of the
> usage of terms, and therefore not strictly debatable, yet it may be
> worthwhile to explain in just what sense the use of the term is
> justified. In CURRY (1963, p.l), I defined mathematical logic as the
> study of mathematical structures having some connection with the
> “analysis and criticism of thought” (W. E. Johnson), and the
> analogues of such structures. Since combinatory logic analyzes a
> process used in practically all such structures, it is a part of
> mathematical logic so defined, just as propositional logic,
> relational logic, or even Aristotelian logic is.

值得强调 Curry 对数理逻辑的定义：

I defined mathematical logic as the study of mathematical structures
having some connection with the “analysis and criticism of thought”,
and the analogues of such structures.

这里的 analogues，就是说数理逻辑是：对人类思维过程的建模。

这里提到的 William Ernest Johnson，
是受到 Peirce 实用主义强烈影响的，
也许值得一读。

> Scott’s point, however, is that pure combinatory logic -- which, at
> least the synthetic aspect of it, might perhaps better be called the
> theory of combinators -- excludes the propositional concepts which
> are central in logic as ordinarily understood. However, combinatory
> logic is broader than pure combinatory logic; it includes illative
> theories, some of which do contain those concepts.

> To be sure, illative combinatory logic is not completely settled. In
> view of the Godel theorem, we cannot prove its consistency by
> methods which it itself formalizes. But it is still a well-defined
> field for investigation. Moreover significant work has been done in
> it.

> Scott himself proposes one system of it in his paper (1975b).

这里引用的是 1975-combinators-and-classes。

> It is plausible that some systems proposed by Bunder will prove
> acceptable; and there are nonconstructive consistency proofs of some
> systems proposed by Fitch.

> Indeed, given any of the usual systems of logic, if one formalizes
> the substitution process by pure combinatory logic, and the theory
> of types by functionality, one has a system of illative combinatory
> logic.

TODO 如何用 functionality 处理 the theory of types？
在无类型的 pure combinatory logic 中处理类型论，
正是我所最关心的！

> Although this has not yet been done in most cases, it seems
> unreasonable to doubt that it is possible. In this field, in which
> our previously developed intuitions are not a safe guide, we make
> progress by trial and error.

# 14 Conclusion

> One final point must be mentioned. The history of combinatory logic
> shows that progress can result from the interaction of different
> philosophies.
>
> - One who, as I do, takes an empirical view of mathematics and
>   logic, in the sense that our intuitions are capable of evolution,
>   and who prefers constructive methods, would never discover the
>   models which Scott proposed.
>
> - On the other hand, it is doubtful if anyone with what seems to be
>   Scott’s philosophy, would have discovered combinatory logic.
>
> Both of these approaches have added to the depth of our
> understanding, and their interaction has produced more than either
> would have alone.
