# 1. The More Things Change, the More They Stay the Same

2020-09-15

## Didactics 1: Forwarding engaging examples

To introduce a new concept, first give many engaging examples.

## Didactics 2: Familiar concept

More formal concept should be introduced
after the introduction of the equivalent more familiar concept.

For example, explaining judgment as Laws in English
is more familiar than direct introduction of formal judgment.

"The Law of Tick Marks" is an instance of the first form of judgment.
Just like this, we should also introduce instance first, then introduce class.

## Definition of Judgment

A judgment is an attitude that a person takes towards expressions.
When we come to know something, we are making a judgment.

这里的叙述就是故事性的，
其中凸显出了很多核心名词，
可以用来组织代码与思想。

具体名词如下：
- person
- expression
- judgment -- attitude person take towards expression
  - `we have reason to believe ____.`
  - `we have no reason to believe ____.`
- evidence -- person's reason to believe judgment

这里也定义了 form of judgment：

A form of judgment is an observation
with blank spaces in it, such as

`____ is a ____.`

## Didactics 3: What not obvious should be further explained

We want to ask the question:
"Is (cons 'ratatouille 'baguette) a (Pair Atom Atom)?",

But we have not explained the meaning of Pair,
thus we ask "Is it obvious that ...",
and give a reason to explain the meaning of Pair.

Just like at the beginning of this chapter,
the first question is:

"Is it obvious that this is an Atom? -- 'atom"

And the answer is "Not at all. What does Atom mean?"

用这种方法可以逐渐将结构化的知识解构成更加线性的形式。
类似于用 stack 去实现 tree 的递归处理，
stack 相较于 tree 是更加线性的。

## Definition of Type

Expressions that describe other expressions are called types.

This definition of type is very simple,
with a leading question, it is even more natural.

## The Four Forms of Judgment

2020-09-16

```
(1)                   ____ is a ____.
(2)    ____ is the same ____ as ____.
(3)                   ____ is a type.
(4)  ____ and ____ are the same type.
```

Judgments are acts of knowing, and believing is part of knowing.

Judgments are sentence.

But, Sentences get their meaning from those who understand them.
The sentences capture thoughts that we have,
and thoughts are more important than the words we use to express them.

## Definition of Normal Forms

2020-09-17

Expressions that are written differently
may nevertheless be the same,
as seen in frames 39-41.
One way of writing these expressions
is more direct than the others.

The normal form of an expression is the most direct way of writing it.

Any two expressions that are the same have identical normal forms,
and any two expressions with identical normal forms are the same.

Sameness is always according to a type,
so normal forms are also determined by a type.

- When `equivalent` is implemented by `readback` and `alpha_equivalent`,
  it is the `readback` that "is always according to a type".

Given a type, every expression described by that type has a normal form,
which is the most direct way of writing it.
If two expressions are the same,
then they have identical normal forms,
and if they have identical normal forms,
then they are the same.

- Each type can specify its own notion of equality,
  and thus the syntax of its normal forms.
  -- "NbE Tutorial", David.

但是，如果不考虑实现中 `equivalent` 与 `readback` 之类的函数，如何以 type 为参数。
为什么 `equivalent` 总是需要给定 type 才能判断的？
是否有这种情况，`x: A` 且 `x: B`，
而 `x` 以 `A` 为类型时的 normal form，
与 `x` 以 `B` 为类型时的 normal form 不同。

在 `readback` 中，根据不同的 type，为不同的 value 实现了不同的 eta-expansion。

## Value, Neutral & Normal

当 `Neutral` 想要递归引用到 `Value` 时，
应该通过带有类型的 `Value`，即 `Normal` 来引用，
因为 `Value.readback` 必须是带有类型的 readback，
其类型不应是 `(ctx: Ctx, value: Value) => Exp`，
而应该是 `(ctx: Ctx, t: Ty, value: Value) => Exp`。

## Normal Form in jojo

也许在不同类型的语义中考虑 Normal Form，对于理解 Normal Form 有帮助。
也许在 jojo 里考虑 Normal Form，对于理解 Normal Form 有帮助。

我之前认为，其中的难点在于多返回值。

也许我应该模仿 lambda 演算的定义序列，
先处理 untyped 版本的 partial evaluation。
- 定义有 reduction step 定义。
- 用 reduction step 定义有方向的 (multi-step) reduction。
- 用有方向的 reduction 定义无方向的 equivalent。

``` cicada
@datatype Jo {
  v(name: String)
  let(name: String)
  jojo(list: Array(Jo))
  exe()
}

@datatype Value {
  jojo(list: Array(Jo), env: Env)
}

@datatype Neutral {
  jojo(list: Array(Jo), env: Env)
}
```

也许 equivalent relation 应该被定义于 jojo 而不是 jo。

reduction step:
```
... [ f g h ] ! ... => ... f g h ...
```

partial evaluation:
- 假设没有递归定义。
- inline all definitions.
- do all the reductions (including in-closure reductions).

example of in in-closure reductions:
```
[ (f) [f! swap f! swap] ]
```

the only pattern of reduction is `[  ]!`.

```
@define swap [ (x) (y) x y ]

[1 2] [3 4] swap
[1 2] [3 4] [ (x) (y) x y ]!
[1 2] [3 4] (x) (y) x y
[1 2] (y) x y
x y
[3 4] y
[3 4] [1 2]

// variable bound by @define, is different from
// variable bound by (x) and (y).
```

## Definition of Value

An expression with a constructor at the top is called a value.

这种对 Value 的定义，好像很适合 lazy language。
也就是说 Dan 的理解，可能比 David 的实现要深刻一些。

## Definition of Neutral

Expressions that are not values
and cannot yet be evaluated due to a variable
are called neutral.

这个定义是出现在下一章的，
我先写在这里做对比。

## Definition of constructor

Some expressions, such as Nat or (Pair Nat Atom), are types.

Part of explaining a new type
is to say what its constructors are.
The constructor expressions are the direct ways
of building expressions with the new type.

## Value, Neutral & Normal as predicates or subsets of Exp

注意，当定义这些术语时，Dan 是通过给 Exp 的集合分类来作出定义的。

首先定义的是 Normal。
"The normal form of an expression is the most direct way of writing it."
与实现中不同，定义用的不是 "带有 Type 的 Value"。

其次是 Value。
"An expression with a constructor at the top is called a value."
现在我们有：Normal <: Value <: Exp
然后我们可以定义：Neutral = Exp - Value
或者说：Neutral = Non Value
并且有：Neutral + Value = Exp

最后是 Neutral。
"Expressions that are not values
and cannot yet be evaluated due to a variable
are called neutral."
也就是说，上面的 Neutral + Value = Exp 是错误的，
要加上 "尝试 evaluate，但因 var 而失败" 才正确。

## Definition of evaluation

Finding a value that is the same as
some starting expression is called evaluation.

What about the type? Sameness, after all, requires types.

From time to time, when talking about sameness,
we do not explicitly mention a type.
Nevertheless, a type is always intended,
and can be discovered by reading carefully.

Doesn't evaluation refer to
finding the meaning of an expression,
not just some simpler expression?

Not here. Expressions do not refer to some external notion of meaning
-- in Pie, there is nothing but expressions
and what we judge about them.

- In Lisp, values are distinct from expressions,
  and the result of evaluation is a value.

Everything Is an Expression

In Pie, values are also expressions.
Evaluation in Pie finds an expression,
not some other kind of thing.

A normal expression has no remaining opportunities for evaluation.
Usually, expressions that are normal are easier to understand.
Finding a value is often enough, however,
because the top constructor can be used to determine what must happen next.

可以说 David 所实现的 Pie 践行了这里的描述吗？
也许是 Dan 不对，因为当我们将 lambda evaluate 成 closure 时，
没法避免不去区分 Exp 与 Value。
