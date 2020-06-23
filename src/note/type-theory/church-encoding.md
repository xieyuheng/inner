# Church encoding

https://en.wikipedia.org/wiki/Church_encoding

It is not possible in general to decide if two functions are extensionally equal
due to the undecidability of equivalence from Church's theorem.

One way to describe this is that extensional equality describes equality of functions,
where as intensional equality describes equality of function implementations.

https://en.wikipedia.org/wiki/Deductive_lambda_calculus#Intensional_versus_extensional_equality

对某些 lambda term 的子集来说（比如 Church numerals），
并不内涵（intensional）等价的两个函数
（以为证明分配律而需要考虑的两个函数为例），
相对于这个子集来说是外延（extensional）等价的。
这两个函数是保持这个子集的结构的。
这种属性为什么要被看作是 lambda calculus 的缺点呢？
现代数学本身就是研究保持结构的映射的。
