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

TODO

# 1

TODO

# 2

TODO

# 3

TODO

# 4

TODO

# 5

TODO

# 6

TODO
