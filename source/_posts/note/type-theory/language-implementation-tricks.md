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

If the only difference between expression and value is the use of closure,
value is also simple,

The "keep it simple trick" says,

> Keep it simple.

## Implementation of inference rules

Type system is specified by logic inference rules.

The "bidirectional trick" says,

> We can turn a set of inference rules to a type checking algorithm,
>   by sometimes view a rule as infer sometimes as check.

For example, the rule of function application,

``` js
[infer] ctx |- f : { A -> B }
[check] ctx |- x : A
------------
[infer] ctx |- f(x) : B
```

We wish to check `f(x)`,
we need to infer `f` to check `f(x)`,
and if we can infer `f`, we can also infer `f(x)`.
Thus we can do infro for the rule of function application.

We examine our rules recursively and do infer as far as possible.
For those left we do check.

Take the rule of (Curry's) function abstraction for example,

``` js
[check] ctx + (x : A) |- e : B
------------
[check] ctx |- { x => e } : { A -> B }
```

we can not possibly infer the type of a bound variable `x`.
Thus we do check for the rule of function abstraction.

Function abstraction is the constructor of function type.
Function application is the eliminator of function type.
Since the rules about function are core of type systems,
we might propagate the property "we need to check instead of infer"
to all rules about constructor.

We only want to implementation check,
but we sometimes need infer,
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

For construction rules the pattern is,

``` js
[check] premise
[check] premise
[check] ...
----------
[check] conclusion
```

We can also just provide enough information in the syntax.
for example, using typed bound variable,
we can infer function abstraction.

``` js
[infer] ctx + (x : A) |- e: B
------------
[infer] ctx |- { x : A => e } : { A -> B }
```

Note that, we do not need to annotate the return type of function.

The rule of dependent function application

``` js
[infer] ctx |- f : { x : A -> B }, env
[check] ctx |- a : A
[equal] eval(env + (x = a), B) = T
------------
[infer] ctx |- f(a) : T
```

About the variance between premise and conclusion in inference rule

``` js
premise
----------
conclusion
```

is like function type `premise -> conclusion`.

``` js
[check] premise
----------
[infer] conclusion
```

which can be read as,
if we can implement check for premise,
we can implement infer for conclusion.

is like dependent function type `check_t(premise) -> infer_t(conclusion)`,
where `infer_t` is a subtype of `check_t`.

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
