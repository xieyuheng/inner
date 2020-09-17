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

## Definition of Value and Neutral

Non Value is Neutral.

In the implementation the concept of Value seems not used,
but it is used, because Non Value is Neutral.

This definition of Value is different from the implementation.

Not only this definition can be used to define Neutral later,
it can also provide us a handle to talk and think about Exp.

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

# Definition of Normal Forms

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
