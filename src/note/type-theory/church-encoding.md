# Church encoding

2020-06-23

link:../../person/alonzo-church/alonzo-church.md

https://en.wikipedia.org/wiki/Church_encoding

It is not possible in general to decide if two functions are extensionally equal
due to the undecidability of equivalence from Church's theorem.

Lambda calculus is usually interpreted as using intensional equality.
There are potential problems with the interpretation of results
because of the difference between the intensional and extensional definition of equality.

the so called "potential problems" are the following:
https://en.wikipedia.org/wiki/Deductive_lambda_calculus#Intensional_versus_extensional_equality

One way to describe this is that extensional equality describes equality of functions,
where as intensional equality describes equality of function implementations.

equality of functions vs equality of function implementations.

对某些 Lambda term 的子集来说（比如 Church numerals），
并不内涵（Intensional）等价的两个函数
（以为证明分配律而需要考虑的两个函数为例），
相对于这个子集来说是外延（Extensional）等价的。
这两个函数是保持这个子集的结构的。
这种属性为什么要被看作是 Lambda calculus 的缺点呢？
现代数学本身就是研究保持结构的映射的。

https://en.wikipedia.org/wiki/Extensionality
https://en.wikipedia.org/wiki/Intension

用外在观察来定义等价性，是以来「观察手段」的，
我们通常能找到自然而明确的「观察手段」这一概念，
利用这个概念会给出很自然的定义，
比如数学中函数相等的常用定义，
- 此时观察手段限制于函数作用，
  或在一个子集中的函数作用（如上面的 Church numerals）。
- 构造主义数学（或者说计算机科学）（或者说 Bishop 的理想），
  可能还会观察函数的运行效率等等。
又比如狭义相对论中的相对性原理。
- 此时观察手段是任何可能的物理实验。
