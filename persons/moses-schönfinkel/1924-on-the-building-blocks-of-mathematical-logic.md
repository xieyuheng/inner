---
title: on the building blocks of mathematical logic
author: moses schönfinkel
translator:  stefan bauer-mengelberg
year: 1924
---

# Forward (by Quine)

> The initial aim of the paper is reduction of the number of primitive
> notions of logic. The economy that Sheffer's stroke function [nand]
> had wrought in the propositional calculus is here extended to the
> predicate calculus, in the form of a generalized copula "U" of
> mutual exclusiveness. Then presently the purpose deepens. It comes
> to be nothing less than the general elimination of variables.

> Examples of how to eliminate variables had long been known in
> logic. The inclusion notation `F ⊆ G` gets rid of the universally
> quantified `x` of `(x)(Fx → Gx)`.

> The notation `F | G` of relative product gets rid of an
> existentially quantified `x`, since `exists(x)(Fyx and Gxz)` becomes
> `(F | G) yz`.

这里的 relative product 只关系之间的复合，函数复合可以视为其特殊情况。

> These devices and others figured already in Peirce's 1870 algebra of
> absolute and relative terms, thus even antedating any coherent logic
> of the variable itself, for such a logic, quantification theory,
> came only in Frege's work of 1879.

> The algebra of absolute and relative terms, however, or of classes
> and relations, is not rich enough to do the work of quantifiers and
> their variables altogether (see above, p. 229).

这里的 (see above, p. 229) 指本书中的另一篇文章。本书是：
1879-1931-from-frege-to-gödel--a-source-book-in-mathematical-logic

> Schönfinkel's notions, which do suffice to do the work of
> quantifiers and their variables altogether, go far beyond the
> algebra of classes and relations, for that algebra makes no special
> provision for classes of classes, relations of classes, classes of
> relations, or relations of relations.  Schonfinkel's notions provide
> for these things, and for the whole sweep of abstract set
> theory. The crux of the matter is that Schonfinkel lets functions
> stand as arguments.

就是说 Peirce 的理论缺少高阶元素。

> For Schonfinkel, substantially as for Frege, a classes are special
> sorts of functions. They are propositional functions, functions
> whose values are truth values.

> All functions, propositional and otherwise, are for Schonfinkel
> one-place functions, thanks to the following ingenious device.
> Where F is what we would ordinarily call a two-place function,
> Schonfinkel reconstrues it by treating `F x y` not as `F(x, y)` but
> as `(F x) y`. Thus F becomes a one-place function whose value is a
> one-place function. The same trick applies to functions of three or
> more places; thus `F x y z` is taken as `((F x) y) z`.

人们都会被 Schonfinkel 的天才想法震惊。

> In particular, a dyadic relation, as an ostensibly two-place
> propositional function, thus becomes a one-place function whose
> value is a class. An example is `U`, above, where `U F G` means that
> `F` and `G` are exclusive classes; `U F G` becomes `(U F) G`, so
> that `U` becomes a function whose value, for a class `F` as argument,
> is the class `U F` of all classes `G` that share no members with `F`.
> Or better, using general variables `x`, `y`, and so forth hereafter
> for classes and other functions and all other things as well, we may
> say that `U` is the function whose value `U x` is the class of all
> classes that share no members with `x`.

> Schonfinkel assumes one operation, that of application of a function
> to anargument. It is expressed by juxtaposition as in `U x` above,
> or `(U x) y`, or in general `z y`. Also he assumes three specific
> functions, as objects: `U` above, `C`, and `S`.
>
> - `C` is the _constancy_ function, such that `(C x) y` is always `x`,
>
> - and `S` is the _fusion_ function, such that `((S x) y) z` is
>   always `(x z) (y z)`.

> Any sentence that can be built up of truth functions and
> quantification and the "ε" of class membership can be translated
> into a sentence built up purely by the application operation, purely
> from "C", "S", "U", and whatever free variables the given sentence
> may have had. This is made evident in the course ofthe paper. The
> elimination of quantification and bound variables is thus
> complete. Since sentences with free variables are wanted finally
> only as ingredients of closed sentences, the notion of variables may
> indeed be said at this point to have been analyzed away
> altogether. All we have is C, S, U, and application.

> Variables seem to survive in rules of transformation,
> - as when `(C x) y` is equated to `x`,
> - and `((S x) y) z` to `(x y) (x z)`.

> But here the variables may be seen as schematic letters for
> metalogical exposition. If one cared to formalize one's metalanguage
> in turn, one could subject it too to Schiinfinkel's elimination of
> variables.

注意，这里所说的不是「消除所有 bound variable」，而是「消除所有 variable」。
实际上在考虑程序语言的时候，所有相对内层 scope 的 free variable，
对于某个外层 scope 来说，都是 bound variable。

在 lambda 演算发明之前，人们提到 variable，
主要指在用变元代替等式中的元素，以形成方程。

做定义的时候，需要给出名字，和其所满足的方程。
lambda 演算使得等式本身变成了语法元素，
但是也因此使得 substitution 变复杂了，
因为需要考虑变元的 scope。


# 1

> It is in the spirit of the axiomatic method as it has now received
> recognition, chiefly through the work of Hilbert, that we not only
> strive to keep the axioms as few and their content as limited as
> possible but also attempt to make the number of fundamental
> undefined _notions_ as small as we can; we do this by seeking out
> those notions from which we shall best be able to construct all
> other notions of the branch of science in question.

这里的 motion 是概念的统称，
axiom 只是其中一个方面。
我们在追求精简的 axioms 的同时，
也要追求精简的 notions。

这里给出的简化 propositional connectives，
就是简化 notions 的例子。

这篇论文的目标是消除逻辑中的「变量」这个概念。
因为变量只不过是用来标记出某些位置的元素是相同的，
这与逻辑的核心概念相比，只不过是个辅助概念。

> It seems to me remarkable in the extreme that the goal we have just
> set can be realized also; as it happens, it can be done by a
> reduction to three fundamental signs.

# 2

首先需要重现发展关于函数的理论。

介绍高阶函数，
以及如何用高阶函数处理多元函数，
即后人所说的 currying。

> As is well known, by function we mean in the simplest case a
> correspondence between the elements of some domain of quantities,
> the argument domain, and those of a domain of function values
> (which, to be sure, is in most cases regarded as coinciding with the
> former domain) such that to each argument value there corresponds at
> most one function value.

> We now extend this notion, permitting functions themselves to appear
> as argument values and also as function values. We denote the value
> of a function `f` for the argument value `x` by simple juxtaposition
> of the signs for the function and the argument, that is, by
>
>     f x.

省略最左边的括号，
把 `(f x) y` 写成 `f x y` 的想法，
来自无穷级数理论。

> In the foregoing case, therefore, `f x` is the value of a function
> that, upon substitution of a value for `x`, does not yet yield an
> object of the fundamental domain (if indeed such an object was
> intended as the value of `F(x, y)`) but yields another function,
> whose argument now is `y`; ...

考虑 domain 的话，就需要扩张 domain。
只有使用类型论的想法，才能把 domain 的变化描述清楚。

# 3

TODO

# 4

TODO

# 5

TODO

# 6

TODO
