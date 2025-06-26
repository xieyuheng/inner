---
title: on the building blocks of mathematical logic
author: moses schönfinkel
translator:  stefan bauer-mengelberg
year: 1924
---

# My Motive

[2025-06-25] Combinatory logic 的开创性论文。

# My Notes

[2025-06-25] 可以说 Schönfinkel 是 combinatory logic 真正的发明人。

其贡献如下：

- 考虑 application 所构成的 magma 代数结构，
  并且为这个代数发明语法糖，即后世所称的 currying。

- 找到消除变元的算法。

Curry 的贡献可以说是进一步仔细研究，
这个 magma 代数结构中的等价关系。

# Forward (by Quine)

> The initial aim of the paper is reduction of the number of primitive
> notions of logic. The economy that Sheffer's stroke function [nand]
> had wrought in the propositional calculus is here extended to the
> predicate calculus, in the form of a generalized copula "U" of
> mutual exclusiveness. Then presently the purpose deepens. It comes
> to be nothing less than the general elimination of variables.

> Examples of how to eliminate variables had long been known in
> logic. The inclusion notation `F ⊆ G` gets rid of the universally
> quantified `x` of `(x) (Fx → Gx)`.

> The notation `F | G` of relative product gets rid of an
> existentially quantified `x`, since `exists (x) (Fyx and Gxz)` becomes
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
> relations, or relations of relations.  Schönfinkel's notions provide
> for these things, and for the whole sweep of abstract set
> theory. The crux of the matter is that Schönfinkel lets functions
> stand as arguments.

就是说 Peirce 的理论缺少高阶元素。

> For Schönfinkel, substantially as for Frege, a classes are special
> sorts of functions. They are propositional functions, functions
> whose values are truth values.

> All functions, propositional and otherwise, are for Schönfinkel
> one-place functions, thanks to the following ingenious device.
> Where F is what we would ordinarily call a two-place function,
> Schönfinkel reconstrues it by treating `F x y` not as `F(x, y)` but
> as `(F x) y`. Thus F becomes a one-place function whose value is a
> one-place function. The same trick applies to functions of three or
> more places; thus `F x y z` is taken as `((F x) y) z`.

人们都会被 Schönfinkel 的天才想法震惊。

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

> Schönfinkel assumes one operation, that of application of a function
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
>     f x

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

> Now a sequence of _particular functions_ of a very general nature
> will be introduced.  I call them:
>
> - `I` -- the identity function [Identitatsfunktion]
> - `C` -- the constancy function [Konstanzfunktion]
> - `T` -- the interchange function [Vertauschungsfunktion]
> - `Z` -- the composition function [Zusammensetzungsfunktion]
> - `S` -- the fusion function [Verschmelzungsfunktion]

> (1) By the _identity function_ `I` we mean that completely
> determined function whose argument value is not subject to any
> restriction and whose function value always coincides with the
> argument value, that function, in other words, by which each object,
> as well as each function, is associated with itself. It is therefore
> defined by the equation
>
>     I x = x

> (2) Now let us assume that the argument value is again arbitrary
> without restriction, while, regardless of what this value is, the
> function value will always be the fixed value `a`. This function is
> itself dependent upon `a`; thus it is of the form `C a`. That its
> function value is always `a` is written
>
>     (C a) y = a
>
> And by now letting `a`, too, be variable we obtain
>
>     (C x) y = x, or C x y = x
>
> as the defining equation of the _constancy function_ `C`.
>
> This function `C` is obviously of the kind considered on page 360
> [section 2]; for only when we substitute a fixed value for `x` does
> it yield a function with the argument `y`. In practical applications
> it serves to permit the introduction of a quantity `x` as a "blind"
> variable.

> (3) Conversely, we can obviously always think of an expression like
>
>     f x y
>
> as having been obtained from
>
>     F(x, y)
>
> where `F` is uniquely determined by the given `f`.  If, however, we
> now rewrite this expression as
>
>     g y x
>
> taking `y` as a parameter, then this new function, too, is uniquely
> given by `F` and therefore indirectly also by `f`.
>
> Hence we may think of the function `g` as the value of a function
> `T` for the argument value `f`. This _interchange function_ `T` has
> as its argument a function of the form `φ x y`, and the function
> value
>
>     ψ = T φ
>
> is that function `ψ x y` whose value `ψ x y` coincides with `φ y x`
> for all argument values `x` and `y` for which `φ y x` has
> meaning. We write this definition briefly as
>
>     (T φ) x y = φ y x
>
> where the parentheses may again be omitted.
>
> The function T makes it possible to alter the order of the terms of
> an expression, and in this way it compensates to a certain extent
> for the lack of a commutative law.

由于引入了全新的函数作用语法，所以 Schönfinkel 在耐心解释，
而不是只是列出定义这些函数的等式。

> (4) If in the argument place of a function `f` there occurs the
> value (dependent upon `x`) of another function, `g`, then
>
>     f (g x)
>
> obviously also depends upon `x` and can in consequence be regarded
> as the value of a third function, `F`, which is uniquely determined
> by `f` and `g`.  ... and we call `F` the function "compounded" from
> `f` and `g`. The function `F` is thus itself the value of a certain
> function `Z'` of `f` and `g`.
>
> We could therefore define
>
>     (Z'(φ, χ))x = φ (χ x)
>
> But, following our earlier convention, we prefer to replace `Z'` by
> the corresponding function of one argument, and we consequently
> obtain
>
>     Z φ χ x = φ (χ x)
>
> as the defining equation of the _composition function_ `Z`.
>
> By means of the function `Z` parentheses can be shifted (not really
> eliminated, since they must always be thought of as still being
> there) within a more comprehensive expression; its effect is
> therefore somewhat like that of the associative law, which is not
> satisfied here either.

这里提到结合律，是因为函数复合总是满足结合律。

    Z f (Z g h) = Z (Z f g) h

```
Z f (Z g h) x
= f ((Z g h) x)
= f (g (h x))
```

```
Z (Z f g) h x
= (Z f g) (h x)
= f (g (h x))
```

> (5) If in
>
>     f x y
>
> we substitute the value of a function `g` for `y`, and in particular
> the value taken for the same `x` as that which appears as argument
> of `f`, we come upon the expression
>
>     f x (g x)
>
> or, as we shall write it for the moment to make it clearer,
>
>     (f x) (g x)
>
> This, of course, is the value of a function of `x` alone; thus
>
>     (f x) (g x) = F x
>
> where
>
>     F = S'(f, g)
>
> again depends in a completely determined way upon the given
> functions `f` and `g`. Accordingly we have
>
>     (S'(φ, χ))x = (φ x) (χ x)
>
> or, if we carry out the same transformation as in the preceding case,
>
>     S φ χ x = (φ x) (χ x)
>
> as the defining equation of the _fusion function_ `S`.

虽然下面用例子说明了 `S`，
但是也只是说明了它的定义，
而没有解释为什么有这样的定义。
想要解释「为什么」就需要了解，
消除表达式中变量的算法。

> It will be advisable to make this function more intelligible by
> means of a practical example. If we take for `f x y`, say, the value
> `log x y` (that is, the logarithm of `y` to the base `x`) and for `g
> z` the function value `1 + z`, then `(f x) (g x)` obviously becomes
> `log x (l + x)`, that is, the value of a function of `x` that is
> univocally associated with the two given functions precisely by our
> general function `S`.

> Clearly, the practical use of the function `S` will be to enable us
> to reduce the number of occurrences of a variable -- and to some
> extent also of a particular function -- from several to a single
> one.

# 4

> It will prove to be relevant to the solution of the problem that we
> have raised concerning the symbolism of logic that the five
> particular functions of the function calculus that were defined
> above, `I`, `C`, `T`, `Z`, and `S`, are not mutually independent,
> that, rather, two of them, namely `C` and `S`, suffice to define the
> others. In fact, the following relations obtain here.

> (l) According to the definitions of the functions `I` and `C`,
>
>     I x = x = C x y
>
> Since `y` is arbitrary, we can substitute any object or any function
> for it, hence, for example, `C x`. This yields
>
>     I x = (C x) (C x)
>
> According to the definition of `S`, however, this means
> so that we obtain `S C C x`,
>
>     I = S C C

这里有一个注释说明对于 Schönfinkel 来说，
在这种函数作用所构成的代数结构中解方程，
也不是一开始就那么简单的：

> [1]: This reduction was communicated to me by Boskowitz;
> some time before that, Bernays had called the somewhat less simple
> one `(S C) (C C)` to my attention.

也许 Schönfinkel 发现 `S` 的契机，
并不是为了消除表达式中变量的算法，
而是为了上面所提到的：
二元函数在一个参数位置与一元函数复合，
从而消除一个参数。

> The last `C`, incidentally, does not occur in the expression `S C C`
> in an essential way.  For, if above we put for `y` not `C x` but an
> arbitrary function `φ x`, we obtain in a similar way
>
>     I = S C φ
>
> where any function can then be substituted for `φ`.

这里有一个注释说明 Schönfinkel 总是在考虑函数的类型：

> [2]: Only such a function, of course, as has meaning for every `x`.

> (2) According to the definition of `Z`
>
>     Z f g x = f (g x)
>
> Furthermore, by virtue of the transformations already employed,
>
>     f (g x)
>     = (C f x) (g x)
>     = S (C f) g x
>     = (C S f) (C f) g x
>
> Fusion over `f` yields
>
>     S (C S) C f g x
>
> therefore
>
>     Z = S (C S) C

这里解方程的过程，前瞻了消除表达式中变量的算法，
与其说是消除变量，不如说是把变量都整理到末尾的参数位置。

> (3) In an entirely analogous way
>
>     T f y x = f x y
>
> can be further transformed thus:
>
>     f x y
>     = f x (C y x)
>     = (f x) (C y x)
>     = S f (C y) x
>     = (S f) (C y) x
>     = Z (S f) C y x
>     = Z Z S f C y x
>     = (Z Z S f) C y x
>     = (Z Z S f) (C C f) y x
>     = S (Z Z S) (C C) f y x
>
> Therefore we have
>
>     T = S (Z Z S) (C C)
>
> If we here substitute for `Z` the expression found above, `T` too
> will have been reduced to `C` and `S`.

不得不说，在一个具体的代数结构内解方程的感觉很好，
尽管这里的具体代数结构只是 free magma，
外加一些 presentation 意义上的生成元和生成元所满足的等式。

TODO 如果 group 和 groupoid 的 presentation 理论，
有对应的 topological 解释，
那么 magma 的 presentation 理论是否也有 topological 解释？

# 5

用 lisp 的为 lambda 引入 bound variable 的语法
-- `(lambda (x) ...)`，
可以推广到 forall -- `(forall (x) (f x))`
和 exists -- `(exists (x) (f x))`。

在这种语法之下，第一章所描述的，
命题演算中的 Sheffer's stroke 到谓词演算的推广，
可以表示为：

    (forall (x) (not (and (f x) (g x))))

或者写作：

    (forall (x) (nand (f x) (g x)))


> Let us now apply our results to a special case, that of the calculus
> of logic in which the basic elements are individuals and the
> functions are propositional functions [predicates].
>
> First we require an additional particular function, which is
> peculiar to this calculus.  The expression
>
>    (forall (x) (not (and (f x) (g x))))
>
> where `f` and `g` are propositional functions of one argument -- in
> view of an earlier remark we may confine ourselves to these -- is
> obviously a definite function of the two functions `f` and `g`; thus
> it is of the form `U'(f, g)`, or, by our principle of
> transformation, `U f g`. Thus we have
>
>    U f g = (forall (x) (not (and (f x) (g x))))
>
> where `f` and `g`, of course, now are propositional functions,
> as the defining equation of the _incompatibility function_ `U`.

> It is a remarkable fact, now, that every formula of logic can be
> expressed by means of our particular functions `I`, `C`, `T`, `Z`,
> `S`, and `U` alone, hence, in particular, by means solely of `C`,
> `S`, and `U`.

> First of all, every formula of logic can be expressed by means of
> the generalized stroke symbol, with the bound variables (apparent
> variables) at the upper ends of the strokes. This holds without
> restriction; hence it holds for arbitrary orders of propositions
> [Aussagenordnungen] and also if relations occur. Furthermore, we can
> introduce the function `U` step by step in place of the stroke
> symbol by suitable use of the remaining constant functions.

> We will not give the complete demonstration here but only explain
> the role of the different particular functions in this reduction.

如果 Schönfinkel 有编程相关的经体，
即用程序表达过程式知识的经验，
那么他就不用省略对这里算法的完整描述了。

The role of functions:

- C -- 引入变元，以增加平凡的依赖关系。
- T -- 交换参数位置。
- Z -- 变换括号的位置。
- S -- 将一个变元的两次出现融合（fuze）成一次出现。

> Thus, for example,
>
>     (forall (x) (nand (f x) (g x y)))
>     = (forall (x) (nand (f x) (T g y x)))
>     = U f (T g y)

> Or, to take a somewhat more involved example,

```scheme
(same-as-chart
 (forall (x)
   (nand (forall (y)
           (nand (f x y)
                 (g x y)))
         (forall (z)
           (nand (h x z)
                 (k x z)))))
 (forall (x)
   (nand (U (f x) (g x))
         (U (h x) (k x))))
 (forall (x)
   (nand (Z U f x (g x))
         (Z U h x (k x))))
 (forall (x)
   (nand (S (Z U f) g x)
         (S (Z U h) k x)))
 (U (S (Z U f) g)
    (S (Z U h) k)))
```

再考虑由上面的例子所引起的例子：

    S (Z U f) f

如何 fuze 这里的 `f`？

```scheme
(same-as-chart
  (S (Z U f) f)
  (S (Z U f) (I f))
  (S ((Z U) f) (I f))
  (Z S (Z U) f (I f))
  ((Z S (Z U)) f (I f))
  (S (Z S (Z U)) I f))
```

> To give a practical example of the claim of this section we shall
> deal with the following proposition:
>
>     For every predicate there exists a predicate
>     incompatible with it.
>
> that is
>
>     For every predicate `f` there exists a predicate `g` such that
>     the propositional function `(f x) & (g x)` is not true of any
>     object `x`.

```scheme
(same-as-chart
 (forall (f) (exists (g) (forall (x) (not (and (f x) (g x))))))
 (forall (f) (exists (g) (U f g)))
 (forall (f) (not (forall (g) (not (U f g)))))
 (forall (f) (not (forall (g) (not (and (U f g) (U f g))))))
 (forall (f) (not (U (U f) (U f))))
 (forall (f) (not (Z U U f (U f))))
 (forall (f) (not (S (Z U U) U f)))
 (forall (f) (not (and (S (Z U U) U f) (S (Z U U) U f))))
 (U (S (Z U U) U) (S (Z U U) U)))
```

# 6

> So far as we can see, we cannot carry the reduction to anything
> beyond the symbols `C`, `S`, and `U` without doing violence to it.

> Purely schematically, to be sure, we could replace even `C`, `S`,
> and `U` by a single function if we were to introduce the new
> function `J` through the definitions
>
>     J C = U
>     J S = C
>     J x = S
>
> where `x` is any object distinct from `C` and `S`. We ascertain,
> first, that `J` is itself distinct from `C` and `S`, since `J` takes
> on only three function values whereas `C` and `S` take on infinitely
> many.

注意，这里用来定义 `J` 的方程，
与之前用来定义 `S`，`C`，`U` 的方程有本质不同，
即后者用到的只有自由变元，而前者用到了常量。

> Consequently we have
>
>     JJ = S
>     J(JJ) = JS = C
>     J[J(JJ)] = JC = U
>
> which in fact accomplishes the reduction. But on account of its
> obvious arbitrariness it is probably without any real significance.

下面的最后三段是德文版编者 Heinrich Behmann 写的。

> However, we can in a certain sense free ourselves at least from the
> sign U in another, more natural way. Every formula of logic [when
> transformed as indicated] certainly contains the sign U and -- quite
> in accordance with our earlier conclusion about an arbitrary symbol
> -- can be written in terms of the particular functions of the
> general function calculus (hence, in particular, by means of C and
> 8) in such a way that U occurs as the argument of the entire
> expression; the expression thus assumes the form FU, where F itself
> no longer contains U. If in writing the expression down we omit the
> U, regarding it as understood, we can in fact manage with C and S.

就是对所有表达式中的 U 再做一次 abstraction，
然后把所有表达式都理解为 U 的函数
（正如有人把所有 froth word 都理解为 stack 的函数）。

但是，其实获得的表达式之间不再 composable。

> On the other hand we could, while relinquishing the most extreme
> reduction of the basic function symbols, demand that parentheses be
> entirely avoided. If now we take the form FU as a point of
> departure, then, by means of Z alone, F can be transformed in such a
> way that all parentheses disappear. By means of C, Z, and S,
> therefore, every formula of logic can be written without parentheses
> as a simple sequence of these signs and can therefore be
> characterized completely by a number written to the base 3.

这里指出可以用 `Z`（函数复合）来消除所有的括号。
代数意义上，可以从 magma 转化到 semigroup，
或者说 monoid（因为函数符合显然有单位元）。

> ... The reduction here considered of the formulas of logic has the
> remarkable peculiarity, therefore, of being independent of the
> axioms of logic.

Schönfinkel 理论的通用性，也让人震惊。
