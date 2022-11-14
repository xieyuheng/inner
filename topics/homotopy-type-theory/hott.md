---
title: Homotopy Type Theory
subtitle: Univalent Foundations of Mathematics
author: The Univalent Foundations Program
---

# Xie: Before Notes

My research problem is to design a formalized foundation of mathematics,
to manipulate theorems and proofs like polynomial equations.

The Aim of my study of HoTT is to learn about
how to extend Martin Löf's basic type theory in general.

# Introduction

**Xie:** Homotopy equivalent and homotopy types
are as intuitive to human mind as natural numbers,
thus we should design language to formalize them directly.

## Type theory

The principle of type theory is:

> We should study **terms** and **types** together.

One problem in understanding type theory
from a mathematical point of view, however,
has always been that the basic concept of _type_
is unlike that of _set_ in ways that have been hard to make precise.
We believe that the new idea of regarding types,
not as strange sets (perhaps constructed without using classical logic),
but as spaces, viewed from the perspective of homotopy theory,
is a significant step forward.
In particular, it solves the problem of
understanding how the notion of equality of elements of a type
differs from that of elements of a set.

- **Xie:** In but in type theory, there can also be

  - structural subtyping;
  - union type;
  - intersection type;

  which can be view as the same operations on sets in set theory,

In homotopy theory one is concerned with spaces
and continuous mappings between them, up to homotopy.

A _homotopy_ between a pair of continuous maps `f: X → Y` and `g: X → Y`
is a continuous map `H: X × [0, 1] → Y`
satisfying `H(x, 0) = f (x)` and `H(x, 1) = g(x)`.
The homotopy `H` may be thought of as
a "continuous deformation" of `f` into `g`.

- **Xie:** A geometric object embedded in another space,
  is often study by a map `f: X -> Y`,
  where `X` is the space of parameters,
  and `Y` the the another space,
  and the object is the image of the map.

  - For example, [parametric equation](https://en.wikipedia.org/wiki/Parametric_equation)

The spaces `X` and `Y` are said to be _homotopy equivalent_, `X ≃ Y`,
if there are continuous maps going back and forth,
the composites of which are homotopical
to the respective identity mappings,
i.e., if they are isomorphic "up to homotopy".

- **Xie:** In formal definition,
  we use one map and `Fiber` and `Singleton` to define this equivalence,
  instead of using two maps and their composites.

Homotopy equivalent spaces have the same algebraic invariants
(e.g., homology, or the fundamental group),
and are said to have the same _homotopy type_.

## Homotopy type theory

We use the word "spaces" only in the pure homotopical sense,
not in the topological sense, remember that
any other topological concepts are NOT applicable.

The idea of interpreting types as structured objects,
rather than sets, has a long pedigree,
and is known to clarify various mysterious aspects of type theory.
For instance,

- interpreting types as sheaves helps explain
  the intuitionistic nature of type-theoretic logic,
- while interpreting them as partial equivalence relations or “domains”
  helps explain its computational aspects.
- The homotopical interpretation fits this same pattern:
  it clarifies the nature of identity (or equality) in type theory,
  and allows us to use type-theoretic reasoning
  in the study of homotopy theory.

The key new idea of the homotopy interpretation is that
the logical notion of identity `a = b`
of two objects `a, b: A` of the same type `A`
can be understood as the existence of a path `p: a ~> b`
from point `a` to point `b` in the space `A`.
This also means that two functions `f, g: A → B`
can be identified if they are homotopic,
since a homotopy is just a (continuous) family of paths
`p(x): f(x) ~> g(x)` in `B`, one for each `x: A`.
In type theory, for every type `A`
there is a (formerly somewhat mysterious) type `Id(A)`
of identifications of two objects of `A`;
in homotopy type theory, this is just the _path space_
of all continuous maps `I → A` from the unit interval.
In this way, a term `p: Id(A)(a, b)`
represents a path `p: a ~> b` in `A`.

- **Xie:** Homotopy types can be viewed as
  encoding of homotopy equivalent classes.

- **Xie:** TODO Univalence axiom is true
  when we view Kan simplicial sets as the model of types, how?

  And how is Church’s principle of extensionality for propositions
  a special case of it? what is the principle?

## Univalent foundations

> Isomorphic things can be identified!

Mathematicians are of course used to
identifying isomorphic structures in practice,
but they generally do so by “abuse of notation”,
or some other informal device, knowing that
the objects involved are not “really” identical.
But in this new foundational scheme,
such structures can be formally identified,
in the logical sense that every property or construction
involving one also applies to the other.
Indeed, the identification is now made explicit,
and properties and constructions
can be systematically transported along it.
Moreover, the different ways in which
such identifications may be made,
themselves form a structure
that one can (and should!) take into account.

- **Xie:** TODO The above should be explained by formal definitions
  `Fiber`, `Singleton`, `Equivalence` ...
  specially the last sentence about
  "different ways in which such identifications may be made",
  is this only about higher inductive types? or NOT only?

  - Inlining the definitions of `Fiber` and `Singleton` to `Equivalence`
    is interesting even with out higher inductive types?

## Higher inductive type

One of the classical advantages of type theory is
its simple and effective techniques
for working with inductively defined structures.

- **Xie:** Cell complexes are also inductively defined.

- **Xie:** TODO Does the type theory syntax describe general cell complexes,
  or only a special subset of cell complexes?

  If a subset, which subset?

- **Xie:** TODO For inductive types, we have inductive principle
  (eliminators and their lambda encoding).

  What is the inductive principle for higher inductive types?

Moreover, this is only the tip of the iceberg,
many abstract constructions from homotopy theory, such as:

- homotopy colimits,
- suspensions,
- Postnikov towers,
- localization,
- completion,
- spectrification,

can alsobe expressed as higher inductive types.
Many of these are classically constructed
using Quillen’s “small object argument”,
which can be regarded as a finite way of algorithmically
describing an infinite CW complex presentation of a space,
just as “zero and successor” is a finite algorithmic
description of the infinite set of natural numbers.

Spaces produced by the small object argument
are infamously complicated and difficult to understand;
the type-theoretic approach is potentially much simpler,
bypassing the need for any explicit construction
by giving direct access to the appropriate “induction principle”.
Thus, the combination of univalence and higher inductive types
suggests the possibility of a revolution,
of sorts, in the practice of homotopy theory.

```cicada
datatype Sphere1 {
  base: Sphere1
  loop: endpoints [ base, -base ]
  check loop: Equal(Sphere1, base, base)
}

datatype Sphere2 {
  south: Sphere2
  north: Sphere2
  meridian: endpoints [ south, -north ]
  disk: polygon [ meridian, -meridian ]
}

datatype Sphere2 {
  base: Sphere2
  check refl(base): endpoints [ base, -base ]
  loop2: polygon [ refl(base), -refl(base) ]
  check loop2: Equal(
    endpoints [ base, -base ],
    refl(base),
    refl(base),
  )
}

datatype Torus2 {
  base: Torus2
  toro: endpoints [ base, -base ]
  polo: endpoints [ base, -base ]
  spoke: polygon [ toro, polo, -toro, -polo ]
}

datatype Torus2 {
  base: Torus2
  warp: endpoints [ base, -base ]
  weft: endpoints [ base, -base ]
  loop2: polygon [ weft, warp, -weft, -warp ]
  check loop2: Equal(
    endpoints [ base, -base ],
    path [ weft, warp ],
    path [ warp, weft ],
  )
}
```

- **Xie:** See [warp and weft](https://en.wikipedia.org/wiki/Warp_and_weft)
  for the naming of edges in `Torus2`.

- **Xie:** 关于算數的分級。

  注意，`Nat` 就是以 inductive type 的方式定義的。

  而且，由 `Nat` 擴展而來的 `Integer` 與 `Sphere1` 等價，
  所以說 _算數的分級_ 也許可以在這種遞歸定義中考慮，
  也就是說，這種遞歸定義給出有趣的代數結構
  也許可以用以給出新的「算數」從而用以研究 _算數的分級_，
  而他們給出的幾何直覺可以幫助我們理解 _算數的分級_。

## Sets in univalent foundations

We have claimed that univalent foundations can eventually serve as a
foundation for “all” of mathematics, but so far we have discussed
only homotopy theory. Of course, there are many specific examples of
the use of type theory without the new homotopy type theory features
to formalize mathematics.

But the traditional view is that mathematics is founded on set the-
ory, in the sense that all mathematical objects and constructions can be
coded into a theory such as Zermelo–Fraenkel set theory (ZF). However,
it is well-established by now that for most mathematics outside of set
theory proper, the intricate hierarchical membership structure of sets in
ZF is really unnecessary: a more “structural” theory, such as Lawvere’s
_Elementary Theory of the Category of Sets_ [Law05], suffices.

- **Xie:** TODO Learn about Lawvere’s _Elementary Theory of the Category of Sets_.

In univalent foundations, the basic objects are “homotopy types”
rather than sets, but we can define a class of types which behave like
sets. Ho- motopically, these can be thought of as spaces in which
every connected component is contractible, i.e. those which are
homotopy equivalent to a discrete space. It is a theorem that the
category of such “sets” satisfies Lawvere’s axioms (or related
ones, depending on the details of the the- ory). Thus, any sort of
mathematics that can be represented in an theory like _Elementary
Theory of the Category of Sets_ (which, experience suggests, is
essentially all of mathematics) can equally well be represented in
univalent foundations.

## Informal type theory

One objective of the present work is to develop an informal style of
doing mathematics in univalent foundations that is at once rigorous
and precise, but is also closer to the language and style of
presentation of everyday mathematics.

## Constructivity

One of the most striking differences between classical foundations and
type theory is the idea of proof relevance, according to which
mathematical statements, and even their proofs, become first-class
mathematical objects.

In type theory, we represent mathematical statements by types, which
can be regarded simultaneously as both mathematical constructions and
mathematical assertions, a conception also known as propositions as
types. Accordingly, we can regard a term `a: A` as both an element of
the type `A` (or in homotopy type theory, a point of the space `A`),
and at the same time, a proof of the proposition `A`.

The logic of propositions-as-types is flexible and supports many
variations, such as using only a subclass of types to represent
propositions. In homotopy type theory, there are natural such
subclasses arising from the fact that the system of all types, just
like spaces in classical homotopy theory, is “stratified” according
to the dimensions in which their higher homotopy structure exists or
collapses. In particular, Voevodsky has found a purely type-theoretic
definition of homotopy n-types, corresponding to spaces with no
nontrivial homotopy information above dimension n. (The 0-types are
the “sets” mentioned previously as satisfying Lawvere’s axioms.)
Moreover, with higher inductive types, we can universally “truncate”
a type into an n-type; in classical homotopy theory this would be its
nth Postnikov section. Particularly important for logic is the case of
homotopy (−1)-types, which we call _mere propositions_. Classically,
every (−1)-type is empty or contractible; we interpret these
possibilities as the truth values “false” and “true” respectively.

- **Xie:** Is it true that
  the _mere propositions_ will be
  defined as `Singleton` in formalization?

TODO Discussion of _axiom of choice_ and the _law of excluded middle_.

## Open problems

TODO

## How to read this book

TODO

# 1 type theory

## type theory versus set theory

- 一個集合必須有等詞
  聲明兩個元素相等時 必須給出證明
  對兩個元素相等 可以有很多證明
  就像是兩個點之間可以有多條路
  對兩元素相等的證明 就是對路的構造
- 等詞的分類
  propositionally equal
  judgmental equality [definitional equality]
  後者其實是說 有基本等詞
  這種基本等詞應該消除
  所有的等詞之間的關係都是相對的
  基本等詞的產生
  是因爲有基本數據結構 在 agda 中 即 tree of symbol
- definitional equality
  爲了解決全局命名所帶來的
  不同名字所構造的結構之間的相等
  全局命名只是在給所構造出來的結構貼標籤做引用而已

## function types

- 在我的類型系統中 只有一種類型
  即 則式
  (... -> ...)
  這種語法更方便函數複合
  但是
  在古典的類型系統中
  卻可以把 各種邏輯連詞做爲類型構造子引入
  其實 在蟬語中
  則式 也可以被類型構造子引入
  只不過 所有的語法都是圍繞 則式 優化的
  所不容易發現這一點
  就像是
  面向對象的語言 用 associate list 來實現對象時
  所有的 語法都是圍繞 關於 associate list 的操作 而優化的
  所以 看不出 associate list 了 一樣
- 可以看出 definitional equality
  就是 agda 所選取的相當任意的一個基本等詞
  爲所有 term 所共用
- function type
  - formation
    (... -> ...)
    (A -> B)
  - introduction [constructors]
    []
    [(A -> B) ...]
    or
    pattern matching
  - elimination [eliminators]
    apply
    (A (A -> B) -> B)
  - computation [reduction]
    a [(A -> B) ...] apply
  - uniqueness principle [expansion]
    f
    [f]

## 記 如何定義集合

- 我之前說
  定義一個新集合的時候
  需要指定如何判定其中二元素相等
  而引入類型的方式有二
  一是
  先構造然後再命名
  二是
  給出名字之後做遞歸定義
  遞歸定義 就是指明這個數據類型的 構造子[指定其接口函數]
  那麼
  這兩種方式之所以能用以引入新的集合
  就是因爲它們滿足定義集合的兩個條件
  如何滿足呢
  說 則式 是特殊的類型構造子 又如何如此呢
- 先構造然後再命名
  就涉及到類型構造子[返回類型的函數]
  既然這個函數代表一族類型
  那麼它也要給出一族等詞
  比如 有函數的類型爲 (set set -> set)
  它所構造的集合的等詞
  就一定是用它的兩個參數集合的等詞構造的
- 遞歸定義又如何呢
  我可以讓觀察具體一點
  遞歸定義 給出了構造屬於這個類型的元素的方式
  [或者說 規定了這個類型的元素的表達式的語法]
  也就是給出一個遞歸定義的謂詞
  來判斷 一般表達式是否表達這個類型的元素
  那麼
  等詞也一樣是這樣一種遞歸函數
- 我覺得最重要等詞相關的概念被忽略了
  或者說被以隱含方式處理了
  設想一下如果要給語言加入 商類型 這種新的類型構造子的話 將如何
  根本沒法良好定義這個新的引入類型的方式
  因爲每次定義類定的時候 [比如 使用歸納定義的時候]
  給出集合的等詞的方式太平凡了
  就是 使用基本的數據結構的等詞而已
  對於 商類型 來說 這是不充分的
  比如
  quotient (? set -> set)
  如果這樣聲明類型發現根本就沒法定義 quotient 的函數體
  ```cicada
  set
  ({(term -> bool) #predicate}
   {(term #term1 term #term2
     {:term1 :predicate apply true?}
     {:term2 :predicate apply true?} -> bool) #equality}
   -> :predicate :equality)
  ```
  如若如此
  quotient (predicate equality quotient-function -> predicate equality)
  看起來也不是很正確
  因爲
  在現有的語言中 實現 set 的方式都不是如此
  而且
  {} 成了一個對真假的判斷
  這正是基本原理所反對的
  如果 用兩個 term 空間的函數來定義 set
  那麼 類型構造子[返回類型的函數]
  就成了返回函數的函數
  這正是我的基本原理所反對的
- 我發現 要求給出等詞 可能太強了
  比如 在 lambda term 的空間中
  考慮 由 reduction 所生成的等價關係所義的等詞
  有了等詞之後
  相當於是給出了一個判別任何連個元素是否相等的算法
  也就是自動生成證明的算法
  所生成的證明 其實就是把計算過程記錄下來
  這個計算過程代表了如何從一點走到另一點
  因此就是一條路
- 定義等詞的不應該是一個返回布爾值的謂詞
  而應該是 一個對返回類型的函數的遞歸定義
  每個這種遞歸定義都能生成一個判別函數
  來判別 term 是否滿足這個遞歸定義
  滿足這個遞歸定義的 term 就是對相等的證明
  也就是說
  我們並沒有給出 對兩個[某類]元素是否相等的判別
  我們給出的是 對一個證明 是否是 對相等的證明的判別
  這樣要求就弱多了
  [所有的謂詞 都以這種方式處理]
  cicada:equal? (cicada cicada -> proposition)
- 但是 此時 cicada:equal? 又是一個類型構造子了
  [因爲它是一個返回類型的函數]
  如果要求所有的集合都帶有等詞
  我們又需要給出這個高階集合的等詞
  那將是

  ```cicada
  cicada:equal2?
  ({cicada #cicada1 #cicada2}
   :cicada1 :cicada2 cicada:equal?
   :cicada1 :cicada2 cicada:equal? -> proposition)

  cicada:equal3?
  ({cicada #cicada1 #cicada2}
   {:cicada1 :cicada2 cicada:equal? #cicada11 #cicada12}
   :cicada11 :cicada12 cicada:equal2?
   :cicada11 :cicada12 cicada:equal2? -> proposition)
  ```

  這是沒完沒了的
  也就是說
  如果想要把等詞處理爲 path 的集合
  而又要求 對每個集合都要給以等詞
  那麼對等詞的定義將是沒完沒了的

- 也就是說 連個基本原理之間發生衝突了
  其一是
  應該把所有的謂詞都實現爲返回類型[命題][集合]的函數
  而不是返回真假值的函數
  其二是
  定義每個集合的時候都要給以等詞
  其矛盾在於
  等詞是謂詞
  如果把等詞實現爲返回集合的函數
  那麼又要定義新的等詞了
- 如何調和這兩個基本原理之間的矛盾
  只有一種方法
  那就是 在定義了第一個層次的等詞之後
  其他層次的等詞 應該做爲一個潛在無窮的集合
  而被自動生成
  也就是說要給出生成這個潛在無窮集合的方式
  也許有不同的方式呢
  對於 lambda term 的空間來說 確實如此
  但是 定義高階路徑的時候 有多種方式
  也許我們每次給出等詞時都要給出這個潛在無窮等詞列
  但是 通常只有第一項是非平凡的
  所以 當之給出這個無窮列的前幾項[比如 第一項]時
  就假設其其他項是由前幾項 以默認方式生成的
- 也就是說
  這兩個看似矛盾的基本原則合在一起
  使得我們在定義集合時
  要聲明的東西更多
  所聲明的信息 甚至可以是潛在無窮多
- 注意
  還有一種解決矛盾的方式
  就是發現基本原則之一是錯誤的
  在 bishop 的基本原則下
  等詞是唯一特殊的謂詞
  其他的謂詞都不必如此
  正是這種特殊性導致了衝突
  我想
  可以通過消除這種特殊性來化解衝突
  可以稍稍改變一下 bishop 的原則
  定義一個集合時
  需要指明構造這個集合的元素的方式
  還需要至少給出一個基本謂詞
  給出基本謂詞的方式是
  給出一個涉及所定義的集合的返回類型[集合]的函數
  這樣
  關於函數的定義也要調整
  函數[證明]是一個能夠在有限步驟內完成的操作
  函數所保持的可以不是等詞
  而是那個集合的基本謂詞
- 注意
  典型的難以定義等詞的集合就是 函數的集合
  p : f1 f2 (A -> A) id
  p 是一個證明
  它證明了 兩個以 (A -> A) 爲類型的函數 f1 與 f2 相等
  其實在 typed lambda-calculus 裏是可以有函數的等詞的
- 這種默認生成
  可以說是 對等詞的繼承
  hott 中所有的地方都使用了這種默認的對等詞的繼承
  只有一個地方沒有使用就是 ua 的地方
  不知道這樣的說法對不對
  > <><><
- 如果是繼承
  那麼 就是對接口的繼承
  那麼 就是子類型的概念了
  如果是不要求等詞
  那麼 有什麼數據類型是沒有等詞的呢
  數值分析邪 概率論邪
  > <><><

## universes and families

- cumulative
  sub-type [sub-set]
- families of types [dependent types]
  B (A -> U)
  or
  B (A -> set)

## dependent function types

- formation
  (... -> ...)
  (A #a -> :a B)
- introduction [constructors]
  []
  [(A #a -> :a B) ...]
  or
  dependent pattern matching
- elimination [eliminators]
  apply
  (A (A #a -> :a B) -> :a B)
- computation [reduction]
  a [(A #a -> :a B) ...] apply
- uniqueness principle [expansion]
  f
  [f]

## >< product types

- product in stack
  - formation
    (... -> ...)
    (-> A B)
  - introduction [constructors]
    f (-> A)
    g (-> B)
    f g (-> A B)
    or
    function composition
  - elimination [eliminators]
    f (-> A B)
    f drop (-> A)
    f swap drop (-> B)
  - computation [reduction]
  - uniqueness principle [expansion]
- 爲了使得依賴性可以被表達
  product type 是用 lambda abstraction 定義的
  這是因爲 沒有多值函數 也沒有返回多值的函數
  參數之間的依賴性 和 返回值之間的依賴性
  就必須用 curry 來處理
- product in memory
  - formation
  - introduction
  - elimination
  - computation
  - uniqueness

## dependent pair types

- in stack
- in memory

## coproduct types

## the type of booleans

## the natural numbers

## pattern matching and recursion

## propositions as types

## 記 商空間

- 假設 集合都配以等詞爲基本接口性質
  那麼 做商空間的方式就是
  以一個更強的 等詞代替 原有等詞
- 然而 對於原集合
  定義與其上的變換分兩種
  保持等詞者稱函數
  不保持等詞者稱操作
  當做 商空間 的時候
  所有函數的性質都改變了
  所以需要重新定義接口函數
  或者重新證明接口函數保持新的等詞
- 如果 對等詞可以有如此繼承
  那麼 對別的接口也可以
  商空間 和 子類型 還有 類型類
  說的都是這種對接口函數的繼承與修改
  注意 商空間修改等詞之後 集合的元素就不同了
  所以 商空間與子空間是很不同的

## identity types

- 怎麼可能對任何型都有一致的方式引入等詞呢
  如果這樣的話 根本就沒有 商類型可言了
  這確實做到了 對於每個類型 都有一個等詞
  但是放起了對這個等詞的操作

# 2 homotopy type theory

# 3 sets and logic

# 4 equivalences

# 5 induction

# 6 higher inductive types

# 7 homotopy n-types

# 8 homotopy theory

# 9 category theory

# 10 set theory

# 11 real numbers
