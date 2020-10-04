# 2. Doin' What Comes Naturally

2020-10-04

## Definition of eliminator

In chapter 1, there are constructors, which build values,
and type constructors, which build types.

car, however, is neither a constructor nora type constructor.

car is an eliminator.

Constructors build values, and eliminators
take apart values built by constructors.

Another way to see the difference is that
values contain information, and eliminators
allow that information to be used.

## Definition of substitution

Consistently replacing a variable with an expression
is sometimes called substitution.

## Eliminating Functions

Applying a function to arguments is the eliminator for functions.

我们可以说 car 是 cons 的 eliminator，
然而说到 function 的时候，就只能说 "Applying a function to arguments"。

这并不是说在语义上 function 的 eliminator 有什么特殊，
只是说，在语法上，我们通常用 `f(x)` 这种语法形式，
而不是用一个像 car 的词，来代表并识别这个 eliminator。

在实现解释器时，一定有一个名字去代表这个 eliminator 的表达式（比如 `Exp.ap`）。

在某些语言的设计中，也有给 apply 之类的词来代表 function 的 eliminator。
