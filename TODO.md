# What we need from a logic system?

Guided by the principle of type theory,
a logic system is a type system.

The way we use it, is to:

- (1) write proof in it, and let machine check the proof valid.
- (2) let it search for proof of a given claim.
  maybe also explain its strategy to us.
- (3) computation of term, or normalization of term (proof).
  maybe simplify proof, compare proof for equivalence relation.

# back to little books

- `xieyuheng/list` a List library for js

# bidirectional type checking

bidirectional type checking 在于检查一组关系就某一个位置的参数的单值性。

解释 intro 与 elim 之二分，终究需要范畴论。
因此解释 Bidirectional Type Checking 也需要范畴论。
用关系的单值性检验来解释，可能并不充分。

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
