# the principle of category theory

当我想 category theory 的 principle 时，
我想到的是将原本在别的 theory 里定义的，
比如在 set theory 里定义的，概念，
翻译成用元素的复合以及其等式来表示，
以求将原来的概念推广到更抽象，以求适用于别的领域。

但是，当 E Chen，考虑 category 时，
她说，考虑 category 在于表明我们是在什么 context 下来考虑 等价关系。

which is "category theory in use", which is very useful view.

the principle is:

> category theory should be viewed as a tool to help us recognize patterns.

but category theory's "explanation via pattern"
should not be viewed as the ultimate explanation.

reality escapes all concepts -- alan watts.

categorical semantics must also explain implementation in useful ways.

which means we should not get the "categorical semantics" and stop,
we should get the "categorical semantics" and use it.

# object oriented programming and functional programming are orthogonal

- structural programming -- no goto

- functional programming -- no side effects

- object oriented programming -- no function pointers -- about indirect transfer of control -- a little farfetched
  - use record type to pass group of high order functions as argument,
    and to return group of high order functions as result.

Note that, bob martin get this the "dark features" (like dark matters) series,
because his historical view starts from structural programming.

# learn "software library API design"

- this can only be learn from examples

  - learn from laravel
  - learn from rails

- we will write down some notes, but what will be a form of knowledge,
  that would be useful in day to day API design practice?

# from the type theory point of view, how lowdim project different from hott?

# bidirectional type checking

bidirectional type checking 在于检查一组关系就某一个位置的参数的单值性。

解释 intro 与 elim 之二分，终究需要范畴论。
因此解释 Bidirectional Type Checking 也需要范畴论。
用关系的单值性检验来解释，可能并不充分。

# back to little books

- `xieyuheng/list` a List library for js

# dependent type system and logic programming

dependent type system 的实现中，有哪些问题是可以用 logic programming 来解决的？

# conjunctive normal form

conjunctive normal form (clausal normal form)
"ands of ors" -- because (not A or B) == (if A then B)
"ands of (if ands then ors)"

# Ancient logics

- Aristotelian logic: https://en.wikipedia.org/wiki/Term_logic
- Stoic logic: https://en.wikipedia.org/wiki/Stoic_logic
- Anviksiki: https://en.wikipedia.org/wiki/Anviksiki
- Mohists: https://en.wikipedia.org/wiki/Mohism

# Formal semantics of natural language, also study scope and binding

- https://en.wikipedia.org/wiki/Scope_(formal_semantics)
- https://en.wikipedia.org/wiki/Binding_(linguistics)

# the concept of archetype

The archetype of categorical logic is cartesian closed category and simply typed lambda calculus.

https://en.wikipedia.org/wiki/Cartesian_closed_category
https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
https://en.wikipedia.org/wiki/Categorical_logic

# encoding computing machines in graph

- use `NodeId`, `NodeMap`, `EdgeId`, `EdgeMap` and so on -- to avoid copy whole graph

  - efficient computation requires efficient data structure

- 在 algebra of 1-dim 中嵌入一个图灵等价的计算规则

  - SKI？

# foundations of mathematics

This inquiry is beautifully formulated by Vladimir Voevodsky as the following,

> A formal deduction system together with a correspondence
> between its components and objects and actions in the world of mathematical thoughts
> which can be used to formalize all subject areas of mathematics
> is called a foundational system for mathematics or "Foundations of Mathematics".

# formalization of algebraic topology

can we use such implementation in Geometric modeling?

- find an algebraic definition of infinity groupoids
  that would satisfy the Grothendieck correspondence.

  - another example of internal language?

# formalization of category theory

can use really use such implementation in the design of programming language's type systems?

# how to solve it

当实现 record type 时，在改用 named sigma type 之后，
我的第一次尝试没能解决 inheritance 的问题，
这在于解决问题的能力不够，需要学习「怎样解题」

# entity component system

entity component system 与 sussman 的 propagator model 之间的关系是什么？

# quotient type

quotient type 要通过改变 equivalence relation 来定义，
只要给每个 datatype 属于它自己的 equivalence relation 就行了，
但是，函数的单值性检查怎么办？

方案 A：
- 允许不单值的函数
- 单值性成为 class 的 axiom 的一部分，比如：
  - Category -- compose
  - Group -- mul inv
由于单值函数的复合还是单值的，
这样对于所发展的抽象理论而言，
就不需要证明单值性了。

我们需要设计语言使得表达这些变容易。

# quotient type and CPL (constraint logic programming)

TODO

# 关于 quotient type 的现象

如果结合律与 List 这个数据类型对应，
结合律 + 交换律 与 DupSet (Set With Duplication) 这个数据类型对应，
结合律 + 交换律 + 幂等 与 Set 这个数据类型对应，
那么交换律与 什么 数据类型对应？
是否有一般的理论来描述这种对应？

注意这些运算律都是对二元运算而言的，
所以其实是在 二叉树 让引入不同的等价关系，
用 二叉树的图形 以及对 二叉树的图形 的形变（Tree rotation）（即 考虑 形变所形成的 等价关系），
可以发现 结合律 总是可以让我们把 二叉树 变为等价关系中一种特殊的代表形式（等价类的代表元） -- 即 List。

也就是说，有了 形变（等价关系）之后，找到 代表形式，
然后限制数据类型的定义，使得其只包含这种 代表形式 中的信息，
这就是我们所观察到的「现象」。

# unorder-ness

如何在数据结构中表示 unorder-ness？

如果和用 ordered pair 来表示 unordered pair？
如果元素之间有全序关系，可以通过排序来解决，
如果元素之间没有全序关系，好像没法化归到某种 代表形式。
如果没法化归到 代表形式 就只能通过实现非平凡的 等词 来处理了。

# Axiomatizing Consciousness – Setting and Postulates, by Henk Barendregt

part 1: https://www.youtube.com/watch?v=FebEjbkNWMA
part 2: https://www.youtube.com/watch?v=NbSay5wTjY8

在描述 turing machine 的时候，有 world 和 agent，然后有 interact

(W1, A1) -> (W2, A2) -> ...

对于 stack-based system 来说就是如此，

但是，对于 lambda calculus 来说，只有一个 expression

E1 -> E2 -> ...
