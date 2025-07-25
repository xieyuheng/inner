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

可能还是要以 untyped 语言为基础。
否则都算是太复杂了。

> In set theory, what is the set-union of the function successor and
> the function predecessor?

在集合论中，人们也不会考虑函数作为集合的 union，
因为结果已经不是函数了，倒是会考虑 relation 的 union。

## 1.2. Static and Strong Typing

> A type may be viewed as a set of clothes (or a suit of armor) that
> protects an underlying untyped representation from arbitrary or
> unintended use. It provides a protective covering that hides the
> underlying representation and constrains the way objects may
> interact with other objects. In an untyped system untyped objects
> are _naked_ in that the underlying representation is exposed for all
> to see. Violating the type system involves removing the protective
> set of clothing and operating directly on the naked representation.
> Objects of a given type have a representation that respects the
> expected properties of the data type.

但是我想实现的 structural type 确实就是描述
underlying representation 的结构而已，
不会隐藏 untyped representation。

作者这种理解类型的方式，
对于为实用的语言设计类型系统而言是适用的。

但是这并非全貌，
比如辅助证明系统是以 type 为主，
而把函数理解为证明的。

这一节尝试定义
"statically typed" 和 "strongly typed"，
这些定义是没意义的。

## 1.3. Kinds of Polymorphism

> Conventional typed languages, such as Pascal, are based on the idea
> that functions and procedures, and hence their operands, have a
> unique type. Such languages are said to be _monomorphic_, in the
> sense that every value and variable can be interpreted to be of one
> and only one type.

> Monomorphic programming languages may be contrasted with
> _polymorphic_ languages in which some values and variables may have
> more than one type. Polymorphic functions are functions whose
> operands (actual parameters) can have more than one type.

> Strachey [Strachey 67] distinguished, informally, between two major
> kinds of polymorphism.  _Parametric polymorphism_ is obtained when a
> function works uniformly on a range of types: these types normally
> exhibit some common structure. _Ad-hoc polymorphism_ is obtained
> when a function works, or appears to work, on several different
> types (which may not exhibit a common structure) and may behave in
> unrelated ways for each type.

> In terms of implementation, a universally polymorphic function will
> execute the same code for arguments of any admissible type, while an
> ad-hoc polymorphic function may execute different code for each type
> of argument.

> If we view a type as partially specifying the behavior, or intended
> usage, of associated values, then monomorphic type systems constrain
> objects to have just one behavior, while polymorphic type systems
> allow values to be associated with more than one behavior.

> Strictly monomorphic languages are too restrictive in their
> expressive power because they do not allow values, or even syntactic
> symbols that denote values, to exhibit different behavior in
> different contexts of use.

C 就是实用的 monomorphic language 的例子。
比那些设计的不好的 polymorphic language 好多了。
可见，如果不知道如何把一个功能设计好，还不如不设计。
可见 untyped language 的重要。

## 1.4. The Evolution of Types in Programming Languages

很枯燥的一节。

## 1.5. Type Expression Sublanguages

> The type expression sublanguage should be sufficiently rich to
> support types for all values with which we wish to compute, but
> sufficiently tractable to permit decidable and efficient type
> checking. One of the purposes of this paper is to examine tradeoffs
> between richness and tractability for type expression sublanguages
> of strongly typed languages.

## 1.6. Preview of Fun

> Fun is a λ-calculus-based language that enriches the first-order
> typed λ-calculus with second-order features designed to model
> polymorphism and object-oriented languages.

> Section 2 reviews the untyped and typed λ-calculus and develops
> first-order features of the Fun type expression sublanguage. Fun has
> the basic types Bool, Int, Real, String and constructed types for
> record, variant, function, and recursive types. This set of
> first-order types is used as a base for introducing parametric
> types, abstract data types, and type inheritance by means of
> second-order language features in subsequent sections.

> Section 3 briefly reviews theoretical models of types related to
> features of Fun, especially models which view types as sets of
> values. Viewing types as sets allows us to define parametric
> polymorphism in terms of set intersection of associated types and
> inheritance polymorphism in terms of subsets of associated
> types. Data abstraction may also be defined in terms of set
> operations (in this case unions) on associated types.

这一章关于集合论的，正是我现在想要了解的。

> Sections 4, 5, and 6 respectively augment the first-order
> λ-calculus with universal quantification for realizing
> parameterized types, existential quantification for realizing data
> abstraction, and bounded quantification for realizing type
> inheritance.

> Section 7 briefly reviews type checking and type inheritance for
> Fun. It is supplemented by an appendix listing type inference rules.

> Section 8 provides a hierarchical classification of object-oriented
> type systems. Fun represents the topmost (most general) type system
> of this classification. The relation of Fun to less general systems
> associated with ML, Galileo, Amber, and other languages with
> interesting type systems is reviewed.

# 2. The λ-Calculus

## 2.1. The Untyped λ-Calculus

这里描述的语言很像 scala，可能是 scala 的起源。

## 2.2. The Typed λ-Calculus
## 2.3. Basic Types, Structured Types and Recursion

这里在描述 intersection type 的时候，
限制了 intersection 只能作用在 record type 上。

record type = labeled product type
variant type = labeled sum type

这里用与 record 对称的语法来描述 variant 并不好，
因为 record 的语法所表示的是 key-value collection。
而 variant value 只能有一个 key。

是否可以直接用 list 构造 variant type？
把 list 的 head symbol 当作 label。
好像完全可以：

```scheme
(define-variant int-list-t
  (null)
  (cons int-t int-list-t))

(define-variant (list-t A)
  (null)
  (cons A (list-t A)))
```

这也许是正确的设计，因为绝大多数 union 都是为了表示 variant。

```scheme
(define-type int-list-t
  (union (tau 'null)
         (tau 'cons int-t int-list-t)))

(define-type (list-t A)
  (union (tau 'null)
         (tau 'cons A (list-t A))))
```

注意，`define-variant` 与 `define-datatype` 不同，
variant 是完全 structural 的。

# 3. Types are Sets of Values

TODO

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
