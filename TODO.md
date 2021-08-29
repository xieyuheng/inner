# cicada

- implicit
- inductive type
- quotient type -- learn from lean
- git-based wiki system

# dance with expressions

- use graph to implement lambda calculus
  - prove theorems about lambda calculus for both graph and tree syntax, and compare the proofs
  - we need to implement a graph library -- with API designed to implement lambda calculus and other rewrite systems

- try `(env: Env) => Exp` as `Value` -- no closure
- frontend dance
- use lean to prove classic theorems about lambda calculus

# back to little books

- `xieyuheng/list` a List library for js

# shallow embedding

shallow embedding of DSL in general programming language,
we get a powerful meta-language to extend the DSL.

# bidirectional type checking

bidirectional type checking 在于检查一组关系就某一个位置的参数的单值性。

解释 intro 与 elim 之二分，终究需要范畴论。
因此解释 Bidirectional Type Checking 也需要范畴论。
用关系的单值性检验来解释，可能并不充分。

# dependent type system and logic programming

dependent type system 的实现中，有哪些问题是可以用 logic programming 来解决的？

# record type and subtyping

在集合论中 subtyping 和 union 还有 intersection 都要求我们去考虑集合的元素，
而类型论的特点是不能考虑集合的元素，
但是其实使用 record type 的时候，
subtyping 和 union 还有 intersection 都不用考虑集合的元素，
而只要考虑 record 的 fields。

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
