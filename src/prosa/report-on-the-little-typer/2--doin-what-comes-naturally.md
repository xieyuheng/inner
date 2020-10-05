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

## Definition of equivalent between lambda-expressions

Two lambda-expressions that expect the same number of arguments
are the same if their bodies are the same.

Note that
  (x) => cons(x, x)
is the same
  Nat -> (Pair(Nat, Nat))
as
  (y) => cons(y, y)

Because two lambda-expressions are also the same
if there is a way to consistently rename the arguments to be the same
that makes their bodies the same.

Consistently renaming variables can't change the meaning of anything.

Renaming variables in a consistent way is often called alpha-conversion.
Thank you, Alonzo Church.

The Initial First Commandment of lambda：

Two lambda-expressions that expect the same number of arguments
are the same if their bodies are the same
after consistently renaming their variables.

这里没有说 "rename the arguments to be the same"，
它其实是第一步，比如要比较
  (x) => (y) => cons(x, y)
与
  (y) => (x) => cons(x, y)
后者的参数先要与前者一致，即 "rename the arguments to be the same"，
然后才可以 "consistently renaming" 得到
  (x) => (y) => cons(y, x)
然后可以比较出二者 body 的不同。

The Initial Second Commandment of lambda：

If `f` is an
  (X) -> Y
then `f` is the same
  (X) -> Y
as
  (x) => f(x)
as long as x dose not occur in `f`.

注意这里的规则体现着 "Everything Is an Expression" 的原则。

## Definition of Neutral

2020-10-05

Expressions that are not values
and cannot yet be evaluated due to a variable
are called neutral.

## The Commandment of Neutral Expressions

Neutral expressions make it necessary to
expand our view on what it means to be the same.

Each variable is the same as itself, no matter what type it has.
This is because variables are only replaced consistently,
so two occurrences of a variable cannot be
replaced by values that are not the same.

If two expressions have identical eliminators at the top
and all arguments to the eliminators are the same,
then the expressions are the same.

Neutral expressions that are written identically
are the same, no matter their type.

功能 (feature) "each variable is the same as itself" 来自
约束 (constraint) "variables are only replaced consistently"。
