---
title: on understanding types data abstraction and polymorphism
authors: [luca cardelli, peter wegner]
year: 1985
---

# My Motive

[2025-07-16] 想要同时支持 polymorphism 和 subtyping，
可能就不可避免地要处理 type variable 被 subtype 限制的情况，
比如 typescript 中带有 `extends` 限制的类型参数：

```typescript
interface Lengthwise {
  length: number;
}

function loggingIdentity<Type extends Lengthwise>(arg: Type): Type {
  console.log(arg.length)
  return arg
}
```

就是这篇论文中的 bounded quantification。

[2025-07-23] 在实现 lattice-lisp 的过程中，
遇到了 union 的难点。
读这篇论文休息一下。

# Abstract

> Our objective is to understand the notion of type in programming
> languages, present a model of typed, polymorphic programming
> languages that reflects recent research in type theory, and examine
> the relevance of recent research to the design of practical
> programming languages.

> Object-oriented languages provide both a framework and a motivation
> for exploring the interaction among the concepts of type, data
> abstraction, and polymorphism, since they extend the notion of type
> to data abstraction and since type inheritance is an important form
> of polymorphism. We develop a λ-calculus-based model for type
> systems that allows us to explore these interactions in a simple
> setting, unencumbered by complexities of production programming
> languages.

# 1. From Untyped to Typed Universes

## 1.1. Organizing Untyped Universes

考虑 untyped lisp，
人们自然会去考虑数据组成的集合，
以及函数的 domain 和 codomain 作为集合的属性。

这样就会自然产生一种起源于集合论直觉的类型系统。

但是考虑 lambda calculus 中 identity 函数 `(lambda (x) x)`，
它可以被同时赋以类型 `(-> int-t int-t)` 或 `(-> string-t string-t)`。
从集合论的角度看，这两个函数的集合是没有交集的，
因为集合论意义上的函数是特殊的关系，
而关系是笛卡尔积的子集，
`int-t` 和 `string-t` 既然没有交集，
那么 `int-t x int-t` 和 `string-t x string-t` 也没有交集。

这种类型系统可以说是起源于函数作为算法的具体结构。
我们可以去考虑满足某个类型的元素所组成的集合，

但是类型之间的操作与集合之间的操作已经有分歧了。
比如，如果我们的类型系统中有类型的 inter 操作，
并且我们希望把 `(-> int-t int-t)` 和 `(-> string-t string-t)`
的 inter 理解为空，那么就不能把类型的 inter
理解为类型所对应的元素的集合的 inter。

除非，我们说 `(lambda (x) x)` 不算元素（value），
只有带有类型的 `(the (-> int-t int-t) (lambda (x) x))` 才算是元素，
或者可以推导出类型的表达式才是元素。

这就是 curry 和 church 对类型系统的理解方式的差异。
curry 不要求类型可以推导，church 要求类型总是可以推导。

> As soon as we start working in an untyped universe, we begin to
> organize it in different ways for different purposes. Types arise
> informally in any domain to categorize objects according to their
> usage and behavior. The classification of objects in terms of the
> purposes for which they are used eventually results in a more or
> less well-defined type system. Types arise naturally, even starting
> from untyped universes.

> Untyped universes of computational objects decompose naturally into
> subsets with uniform behavior.  Sets of objects with uniform
> behavior may be named and are referred to as types. For example, all
> integers exhibit uniform behavior by having the same set of
> applicable operations. Functions from integers to integers behave
> uniformly in that they apply to objects of a given type and produce
> values of a given type.

> After a valiant organization effort, then, we may start thinking of
> untyped universes as if they were typed. But this is just an
> illusion, because it is very easy to violate the type distinctions
> we have just created.

> In set theory, what is the set-union of the function successor and
> the function predecessor?

在集合论中，人们也不会考虑函数作为集合的 union，
因为结果已经不是函数了，倒是会考虑 relation 的 union。

## 1.2. Static and Strong Typing

TODO

## 1.3. Kinds of Polymorphism
## 1.4. The Evolution of Types in Programming Languages
## 1.5. Type Expression Sublanguages
## 1.6. Preview of Fun

# 2. The λ-Calculus

## 2.1. The Untyped λ-Calculus
## 2.2. The Typed λ-Calculus
## 2.3. Basic Types, Structured Types and Recursion

# 3. Types are Sets of Values

# 4. Universal Quantification

## 4.1. Universal Quantification and Generic Functions
## 4.2. Parametric Types

# 5. Existential Quantification

## 5.1. Existential Quantification and Information Hiding
## 5.2. Packages and Abstract Data Types
## 5.3. Combining Universal and Existential Quantification
## 5.4. Quantification and Modules
## 5.5. Modules are First-Class Values

# 6. Bounded Quantification

## 6.1. Type Inclusion, Subranges, and Inheritance
## 6.2. Bounded Universal Quantification and Subtyping
## 6.3. Comparison with Other Subtyping Mechanisms
## 6.4. Bounded Existential Quantification and Partial Abstraction

# 7. Type Checking and Type Inference

# 8. Hierarchical Classification of Type Systems

# 9. Conclusions
