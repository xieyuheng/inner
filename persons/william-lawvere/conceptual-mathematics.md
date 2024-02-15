---
title: Conceptual Mathematics
subtitle: A first introduction to categories
authors: [William Lawvere, Stephen Schanuel]
year: 1991
---

# Review

首先这本书很适合入门，
因为它的形式模仿了一个学习氛围很好的班级，
即老师在课堂上对范畴论中概念的讲解 (Article)，
再加上很多学生的讨论（Session）。

Session 6.3 讨论范畴论的哲学，
解释了对偶的两个视角之间的差异，
是如何体现在实际应用中的。
如果没有这种对应用的讨论，
两个视角的差异就很肤浅了。

Session 10.5 有对 Brouwer 不动点定理的范畴论处理，
指出了如何在范畴论的指导下发明新的数学。
在看这一节之前，我只知道数学结构是去发现一个理论中证明的模式，
并为这个理论抽象出来一个数学结构，
而范畴论是以数学结构为对象的更高一层抽象；
看了这一节之后，我才明白范畴论不只有这种宏观的意义，
而是在每个微观的证明中，都可以给以范畴论的抽象。
范畴论是用于发现证明中的模式的（不只是理论中的模式），
这种方法的有效性在于函数即证明。
这种方法的有效性正如 interface 在编程中的有效性。

# Part I The category of sets

> A _map_ of sets is a process for getting from one set to another. We
> investigate the _composition_ of maps (following one processby a
> second process), and find that the algebra of composition ofmaps
> resembles the algebra of multiplication of numbers, but its
> interpretation is much richer.

## Article I Sets, maps, composition

用 `A -g-> A -f-> B`
这样的的后缀表达式描述的函数复合，
类似英文的 -s 所属格；
用 `f(g(x))` 和 `f°g(x)`
这样的前缀表达式描述的函数复合，
类似英文的 of 介词。

当我们设计 sexp 的 OOP 语法时，
可以同时支持这两种表达方式。

也许我们应该把语法设计成这样：

```scheme
(define User
  (class
    (claim name String)
    (claim age Number)))

(define xyh
  (object
    (define name "Xie Yuheng")
    (define age 32)))

;; use syntax sugar:

(define xyh
  {.name "Xie Yuheng"
   .age 32})

(.name xyh) ;; name of xyh
(xyh .name) ;; xyh's name
xyh.name    ;; xyh's name
```

感觉还是 JS 语法更好：

```typescript
class User {
  name: String
  age: Number
}

let xyh = {
  name: "Xie Yuheng",
  age: 32,
}

xyh.name
```

# Part II The algebra of composition

> We investigate the analogy: If composition of maps is like
> multiplication of numbers, what is like division of numbers?  The
> answers shed light on a great variety of problems, including (in
> Session 10) 'continuous' problems.

## Article II Isomorphisms

如果把函数理解为属性，
在等式 `h = g ° f` 中，
已知 `h` 和 `f` 求 `g` 的 determination 问题有解，
就是说 `h` 这个属性，可以被 `f` 这个属性所决定。
比如在 OOP 中，它可以被实现为一个 getter 函数。

映射 `f: A -> B` 可以被视为对集合 `A` 的分类/划分。
尝试从这种角度去理解这一章的概念：

- division problem analogy

  就像数字的 division problem 给出分数一样，
  集合的 division problem 考虑划分之间的粗细。

- determination/extension

  在 `h = g ° f` 中，`f` 是比 `h` 更细的划分，
  进一步用 `g` 做划分，可以得到 `h`。
  注意，划分会把值域中的某些元素视为等价，
  因此进一步的划分一定会越来越粗（最多只能做到粗细不变）。
  即 `g ° f` 比 `f` 粗。

- retraction

  在 `id(A) = g ° f` 中，`A` 被 `f` 划分后，
  可以进一步经过 `g` 的再次划分返回到 `A` 而不失去信息。
  这代表 `f` 是单射 injective map。

- choice/lifting

  此时 `g` 是已知的而 `f` 是未知的，
  因此不能再考虑对 `A` 的相继划分了。

  在 `h = g ° f` 中，已知其他而未知 `f`，被称作是 choice 问题，
  是因为此时对于每个 `a: A` 要找到一个 `b: B` 使得 `g(b) = h(a)`，
  找到了之后就可以定义 `f(a) = b`。
  对于每个 `a` 都能找到这样的 `b` 就是可以找到 `f` 的前提。
  对于集合之间的映射来说，这就是说 `g` 的值域能覆盖 `h` 的值域，
  如果 `g` 是满射，一定是如此。

- section

  在 `id(A) = g ° f` 中，要覆盖 `id(A)` 的值域，`g` 必须是满射。

  > Another word that is used to describe this point of view about a
  > map is fibering, by the agricultural analogy in which a bunch is
  > imagined in the shape of a line or fiber. We say that X is divided
  > into B fibers. If one fiber is empty, the map has no sections.

  section 一词来自 fibering 这种观点。

- idempotent

  `e = e ° e`，就是说 `a` 依然在其所代表的划分 `e[-1](a)` 中。

  reduction 就是典型的 idempotent，
  利用 reduction 的 normal term，
  我们可以定义所有 term 之间的一个等价关系。
  等价关系就定义一个商集。
  用整数对儿的集合定义分数的集合，就是这样一个商集的例子。

## Session 10 Brouwer's theorems

### 5. How to understand a proof: The objectification and 'mapification' of concepts

> You may have felt that none of our reasoning about Brouwer's
> theorems was valid, since we still have no precise notion of
> 'continuous map.'  What we wish to do next is to extract those
> properties which are needed for our reasoning, and see that our
> conclusions are valid in any category in which these properties
> (which we will call Axiom 1 and Axiom 2) hold.

这大概就是新数学如何在范畴论的指导下被发明出来的一般过程。

> The main thing to study, though, is the way in which by objectifying
> certain concepts as maps in a category, the combining of concepts
> becomes composition of maps! Then we can condense a complicated
> argument into simple calculations using the associative law. Several
> hundred years ago, Hooke, Leibniz, and other great scientists
> foresaw the possibility of a 'philosophical algebra' which would
> have such features. This section has been quite condensed, and it
> may take effort to master it. You will need to go back to our
> previous discussion of Brouwer's proof, and carefully compare it
> with this version. Such a study will be helpful because this example
> is a model for the method of 'thinking categorically.'

# Part III Categories of structured sets

> We use maps to express extra 'structure' on sets, leading to graphs,
> dynamical systems, and other examples of 'types of structure.' We
> then investigate 'structure-preserving' maps.

## Article III Examples of categories -- Directed graphs and other structures

Set + endomap 的范畴，考虑其 internal map，就是 directed graphs 的范畴。

对于很多不适合被画出来的 directed graphs，
比如 lambda 演算等等 rewrite system，
用范畴论的代数语言描述更方便。

# Part IV Elementary universal mapping properties

> We find there is a single definition of multiplication of objects,
> and a single definition of addition of objects, in all categories.
> The relations between addition and multiplication are found to be
> surprisingly different in various categories.

# Part V Higher universal mapping properties

> We find that the algebra of exponents comes from the notion of 'map
> object', and we explore other universal mapping properties including
> that of 'truth-value' objects.
