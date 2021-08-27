# The principle of logic theories

"logic theories" or "formal system"?

logic theory v.s. model == class v.s. object

we need one abstraction level higher to get good naming.

the meta theorems about logic is defined by relation between pure syntactic inference and models
but not only between them, to define soundness and completeness,
we also need to consider the algebra -- boolean algebra.

in first-order logic, the result type of axioms is boolean.
in type theory, the result type of axioms is category.

how does this influence our consideration of meta theorems?

# relational algebra & higher dimensional algebra

we can do relational algebra in prolog,
by saying along which logic variable
(just like in SQL we join over a key).

This means relational algebra is already a algebra
in which we can product two element along different ways.

We can borrow its syntax in higher dimensional algebra.

- We should do low dimensional algebras first -- 0, 1, 2, 3.

或者应该反过来说，higher dimensional algebra 可以用来作为 logic programming 的类型系统。
to express equivalence between relations.

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
