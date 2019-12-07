---
title: Language implementation tricks
---

# Language implementation tricks

## Closure and lexical scope

We need lexical scope for lambda.

The closure trick says,

> Closure = Lambda expression + Environment

this is the most basic trick for writing simple call-by-value eval function.

Whenever lexical scope is need, the trick of closure must be used.

## Keep the model of expression and value simple

The expression is already simple.

Value is also simple, if the only difference
between expression and value is the closure of environment.

The "keep it simple trick" says,

> Keep it simple.

## Implementation of inference rules

Type system is specified by logic inference rules.

Such specification make it possible to prove `e: T`,
but might not provide algorithm for checking `e: T`,
that is, it might not generate a proof of `e: T` for you.

Why not?
Just because we are not providing enough information in the syntax.

For example,
Curry's typed lambda calculus has less information in syntax
than de Bruijn's typed lambda calculus.

The "bidirectional trick" says,

> We can turn a set of inference rules to a type checking algorithm,
>   by sometimes view a rule as infer sometimes as check.

For example, the rule of function application,

``` js
ctx |- f: A -> B
ctx |- x: A
------------
ctx |- f(x): B
```

We need infer to check function application,
and if we have infer, we can also infer the type of function application.
Thus for rule of function application we use infer.

``` js
[infer] ctx |- f: A -> B
[check] ctx |- x: A
------------
[infer] ctx |- f(x): B
```

We examine our rules recursively and do infer as far as possible.
For those left we do check.

Take the rule of (Curry's) function abstraction for example,

``` js
ctx, x: A |- e: B
------------
ctx |- (x) => e: A -> B
```

we can not possibly infer the type of a bound variable `x`.
thus we do check for the rule of function abstraction.

``` js
[check] ctx, x: A |- e: B
------------
[check] ctx |- (x) => e: A -> B
```

Function abstraction is the constructor of function type.
Function application is the eliminator of function type.
Since the rules about function are core of type systems,
we propagate the property "we need to check instead of infer"
to all rules about constructor.

Why we have this propagation?

Why we sometimes need infer, when we only want to implementation check?
because we need to infer the type of target of elimination rules.

For elimination rules the pattern is,

``` js
[infer] premise about target
[check] premise
[check] premise
[check] ...
----------
[infer] conclusion
```

for construction rules the pattern is,

``` js
[check] premise
[check] premise
[check] ...
----------
[check] conclusion
```

Can we just provide enough information in the syntax.
for example, use de Bruijn's typed lambda calculus instead of Curry's typed lambda calculus?
In de Bruijn's typed lambda calculus, can we infer all the way down?
Is this true for all the rules about constructor?

``` js
[infer] ctx, x: A |- e: B
------------
[infer] ctx |- (x: A) => e: A -> B
```

Note that, we do not need to annotate the return type of function.

A sub-trick is that, the argument type of type check function
should be `(e: Exp, T: Val)` instead of `(e: Exp, T: Exp)`.

The dependent version of the rule of function application

``` js
ctx |- f: A -> B
ctx |- x: A
val_eq(val_apply(B, x), T)
// B is a value that can apply to x
//   thus it must be value (with closure)
//   to maintain lexical scope
------------
ctx |- f(x): T

[infer] ctx |- f: A -> B
[check] ctx |- x: A
[assert] val_eq(val_apply(B, x), T)
// B is a value that can apply to x
//   thus it must be value (with closure)
//   to maintain lexical scope
------------
[infer] ctx |- f(x): T
```

About the duality (or variance) between premise and conclusion in inference rule

``` js
premise
----------
conclusion
```

is like function of type `premise -> conclusion`

``` js
[check] premise
----------
[infer] conclusion
```

which can be read as,
if we can implement check for premise,
we can implement infer for conclusion.

And if we only need to implement check for premise to implement infer for conclusion,
the rule will be useful in more places,
for it is usable even if we can not implement infer premise.

## Comparing equivalence between expressions

We can comparing equivalence between expressions, if we can normalize expressions.

The "normalization by evaluation trick" says

> Evaluate the expressions to values, then read them back to normal form.

That is to say, `exp_eq` can be factored into `normalize` and `alpha_eq`,
and `normalize` can be factored into `eval` and `readback`.

Because there will be undefined free variables during the evaluation,
we need to define **neutral form** to handle this,
- Undefined free variable is neutral,
- Each application of eliminator to neutral is neutral.

We can also make `exp_eq` faster,
by comparing weak head normal form step by step,
and know that two expressions are not equal
as soon as they start to appear to be not equal.
