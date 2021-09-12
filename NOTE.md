# bidirectional-type-checking-and-univalent-relation

Bidirectional type checking and univalent relation

Univalent relation – a binary relation R that satisfies xRy ∧ xRz ⇒ y = z.

https://en.wikipedia.org/wiki/Binary_relation#Special_types_of_binary_relations

bidirectional type checking 在于检查一组关系就某一个位置的参数的单值性。

解释 intro 与 elim 之二分，终究需要范畴论。
因此解释 Bidirectional Type Checking 也需要范畴论。
用关系的单值性检验来解释，可能并不充分。

# Bidirectional Type Checking

2020-09-29

- Examples:
  link:../cicada/lang1/check.cic
  link:../cicada/lang2/check.cic

假设在 datatype 中定义数据构造子时，
其类型所代表的是纯逻辑式编程。
即所有的关系都是可逆的。
- 但是在 dependent type system 的 judgment 中，情况不是如此，
  因为 evaluate 不是可逆的。

双向的类型检查在于，关系的单值性，即关系是否是函数。
具体对于 Check 而言，假设第三个参数 t 为输出。
注意「关系是函数」与「有算法计算这个函数」还差一步。

关于双向类型检查与函数单值性，有一个有趣的 story。
首先要知道 Check 对第三个参数 t 的单值性，将给出函数 infer。
假设我想实现函数 check 来自动生成关系 Check 的证明。
- 所有的 predicate，在把它当作返回值为 bool 的函数之外，
  都可以阐释理解其为生成证明的函数。
  - 当这样做时，我们需要问 predicate
    所对应的 judgment 的 inference rules 是什么。
  - 对于 Check 而言，我们先有的是 Check 这个 judgment 与其 inference rules，
    然后再想办法去实现 check 这个 predicate。
一、我发现只有当我能 infer ap 的 target，才能 check ap，
二、而且，额外地，当我能 infer ap 的 target，我就不光能 check ap，并且也能 infer ap，
三、为了 infer ap 的 target 我必须能够 infer fn，
四、fn 的 Check 对其第三个参数 t 不具有单值性，因此无法实现 infer，
五、为了 fn 而加 annotation 这个新 Exp，并且把类型检查 judgment 分为两个方向的 infer 与 check。

为什么说「fn 的 Check 对其第三个参数 t 不具有单值性」？
通过把逻辑式转写成函数式，并且用到对变量的赋值，可以自动检验一组 judgment 对某一个参数位置的单值性。

we can use lower case letters as logic
variable, because const are in namespaces.

如果有检验关系就某一个参数，是否具有单值性的算法，
我们就可以用我们的 inference rule syntax
来表达 bidirectional type checking 相关的知识。

通过把逻辑式转写成函数式，并且用到对变量的赋值，
可以自动检验一组 judgment 对某一个参数位置的单值性。

以 Check.fn 这一个 inference rule 为例，检验其就第三个参数的单值性。

Check(ctx, Exp.fn(name, ret), Ty.arrow(arg_t, ret_t))
------------------------------------------------------- fn
ret_ck: Check(Map.extend(ctx, name, arg_t), ret, ret_t)

假设 Ty.arrow(arg_t, ret_t) 是未知的，因此 arg_t 是未知的，
因此 arg_t 不应在 ret_ck 的类型的第一个参数 Map.extend(ctx, name, arg_t) 中出现，
因此单值性检验失败。

bidirectional type checking 算法设计的问题，
可以划归到逻辑式编程中，就某一个位置的单值性检验问题。

# reversed-inference-rule style function application syntax

- normal:
  ``` cicada
  f(a: A): T
  g(f(a: A): T, b: B): R
  ```

- reversed-inference-rule style:
  ``` cicada
  T
  ---- f
  A
  ---- a

  R
  ---- g
  { T
    ---- f
    A
    ---- a }
  { B
    ---- b }
  ```

- compare our syntax with the traditional syntax of writing inference rules:
  - it (the traditional syntax) uses concrete syntax ambiguously.
  - it does not use closure.
  - it uses natural deduction instead of sequent calculus.
  - it use declarative pattern like the `(syntax-rules)` of scheme.
    - to express common collection like list and map.
  - it use mutable variables.
  - it is not purely declarative.
  - it is like the DSL for specifying grammar by grammar rules.

# TDD

- It is easier to ponder about working code, and refactor it to better code,
  than to conceive the perfect design at the beginning.

# from the type theory point of view, how lowdim project different from hott?

# back to little books

- `xieyuheng/list` a List library for js

# dependent type system and logic programming

dependent type system 的实现中，有哪些问题是可以用 logic programming 来解决的？

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

# formalization of category theory and categorical semantics

can use really use such implementation in the design of programming language's type systems?

# how to solve it

当实现 record type 时，在改用 named sigma type 之后，
我的第一次尝试没能解决 inheritance 的问题，
这在于解决问题的能力不够，需要学习「怎样解题」

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
