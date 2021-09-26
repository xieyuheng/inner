# categorical semantics & categorical logic

Take rules about product and either as examples.

# the concept of archetype

The archetype of categorical logic is cartesian closed category and simply typed lambda calculus.

https://en.wikipedia.org/wiki/Cartesian_closed_category
https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
https://en.wikipedia.org/wiki/Categorical_logic

# from the type theory point of view, how lowdim project different from hott?

# Ancient logics

- Aristotelian logic: https://en.wikipedia.org/wiki/Term_logic
- Stoic logic: https://en.wikipedia.org/wiki/Stoic_logic
- Anviksiki: https://en.wikipedia.org/wiki/Anviksiki
- Mohists: https://en.wikipedia.org/wiki/Mohism

# Formal semantics of natural language, also study scope and binding

- https://en.wikipedia.org/wiki/Scope_(formal_semantics)
- https://en.wikipedia.org/wiki/Binding_(linguistics)

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

# formalization of category theory and categorical semantics

can use really use such implementation in the design of programming language's type systems?

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

# dependent type constructor is relation

假设在 datatype 中定义数据构造子时，
其类型所代表的是纯逻辑式编程。
即所有的关系都是可逆的。

- 但是在 dependent type system 的 judgment 中，情况不是如此，
  因为 evaluate 不是可逆的。
