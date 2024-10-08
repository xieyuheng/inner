---
title: algebraic topology note
---

# [todo-list]

- 現在有了
  1. 構造幾何對象的形式語法
  2. 判斷(低階)幾何對象之間的同倫等價的方法
  而 引入同倫等價這個概念 最初就是爲了在計算同倫群時
  給出 判斷群中元素是否等價的方式

# a reading guide of hott

- always concern yourself about the geometry aspects
  of the formal language,
  because the language is to be used as a formalization
  of the geometry intuition of human.

- a type is a geometry object.
  a definition of a geometry object is just like
  a description of how you could draw a picture of the geometry object.

- constructors are used to construct geometry objects.
  | level | geometry object |
  |-------+-----------------|
  |     0 | point           |
  |     1 | path            |
  |     2 | surface         |
  |   ... | ...             |

- cell complex
  注意 不能跨階粘貼邊界
  boundary
  ><><><
  note that the '=' based syntax is not 對稱
  因爲 (例如就二階元素而言) p = q 等價於 p q^{-1} = 1
  在高階的情形更是如此

- a dependent type is a type family,
  a type family is a fibration,
  a function of such type is a section of the fibration.

- all functions are considered continuous.
  when you want to find a function of certain type,
  such as (A -> B), (x:A -> B(x)),
  it must satisfy some constrains to be continuous.
  what are such constrains?

  - a fibration is a type family,
    a type family can be used as a function.
    to be able to describe the constraint of continuous function,
    what constrains a type family must satisfy?

- a type is a proposition,
  a function body of such type is its proof.
  a proof is a record of deduction steps
  that finally would arrive the proposition.

- what is the meaning of being constructive?

# the meaning of being constructive

- deduction system 與 rewrite system 之間的關係
  首先以 deduction system 爲本
  設計用來記錄 record of deduction steps 的語法
  此時的語法並沒有 rewrite 的意義

- rewrite 的意義來原於
  1. constructor 作爲平凡的函數體
  2. 定義函數體的 pattern match 語法使得新定義的函數體描述 rewrite

- 注意
  type class 的接口函數名字 與 constructor 不同
  不能被當作平凡函數體
  我想這就是 logic harmony
  constructor 構造
  而 pattern match 解構

# constraint of fibration

- to be able to describe the constraint of continuous function,
  a fibration must also has a function body for its transport.
  because constraints of continuous function of fibration
  are described by transport.

- 如果 一個 dependent type 不足以確定一個 fibration
  即 還需要明確指定其 transport
  那麼 就不可能有一般的 獲得 transport 的函數體 的方式

# 單值性 與 連續性

- 當把 path 理解爲 geometry object 時
  函數定義的基本限制是 連續性

- 當把 path 理解爲 引入的等詞時
  函數定義的基本限制是 也可以被理解爲 單值性

# higher inductive definition

- ~ 是遞歸展開
  = 是遞歸基礎

# >< continuous

- (A -> B)

- (x:A -> B(x))

# (Σ2 ~> S^1) 與 (ΣΣ2 ~> S^2)

- 函數之間的 同倫 記 '~'
  空間之間的 同倫等價 記 '~>'

- 目前所有的計算都來源於想要證明
  Σ2 ~> S^1 與 ΣΣ2 ~> S^2
  證明 '~>'
  即 構造兩個作爲 fibration 的特殊 path space 中的元素
  f : (A ~> B)
  g : (B ~> A)
  k : ((a : A) -> a f g = a)
  z : ((b : B) -> b g f = b)

# f ~ g [curry vs uncurry]

```scheme
(def f
  (lambda (-> [A] [B])
    ...))
(def g
  (lambda (-> [A] [B])
    ...))

;;;; f ~ g

;; curry
(def k
  (lambda (-> [A %:a] [(1 :a f :a g)])
    ...))

;; uncurry
(def h
  (lambda (-> [A I] [B])
    {(-> [:a #0] [:a f])
     (-> [:a #1] [:a g])}
    ;; an extension problem indeed
    ...))
```

- note that
  the equality of function is defined by equality of value
  [no matter curry or uncurry]

# (bool-suspend ~> sphere-1)

```scheme
(def sphere-1
  (type (-> [] [type])
    b (-> [] [sphere-1])
    loop (-> [] [(== b b)])))

(+ bool : type
   0 : bool
   1 : bool)

(+ bool-suspend : type
   n : bool-suspend
   s : bool-suspend
   m : (bool -> (n = s)))

(~ f : (bool-suspend -> sphere-1)
   (n -> b)
   (s -> b)
   f : ((n = s) -> (b = b))
   (0 m -> loop)
   (1 m -> b rf))

(~ g : (sphere-1 -> bool-suspend)
   (b -> n)
   g : ((b = b) -> (n = n))
   (loop -> {0 m 1 m rev}))

(~ g f : (sphere-1 -> sphere-1)
   (b -> b)
   g f : ((b = b) -> (b = b))
   (loop -> loop))

(~ f g : (bool-suspend -> bool-suspend)
   (n -> n)
   (s -> n)
   f g : ((n = s) -> (n = n))
   (0 m -> {0 m 1 m rev})
   (1 m -> n rf))

(~ :k : ((:x : bool-suspend) -> (:x f g = :x))
   {n :k : (n = n)}
   {s :k : (n = s)}
   :k : ((:p : (n = s)) -> (n :k (:p :k tp) = s :k))
   {0 m :k : ((0 m f g) rev n :k 0 m = s :k)}
   {1 m :k : ((1 m f g) rev n :k 1 m = s :k)}
   (<solve>
    n :k == n rf
    s :k == 1 m
    0 m :k == 1 m rf
    1 m :k == 1 m rf))
```

# (bool-suspend-suspend ~> sphere-2)

```scheme
(+ sphere-1 : type
   b : sphere-1
   loop : (b = b))

(+ sphere-2 : type
   b2 : sphere-2
   surf : (b2 rf = b2 rf))

(+ bool : type
   0 : bool
   1 : bool)

(+ bool-suspend : type
   n : bool-suspend
   s : bool-suspend
   m : (bool -> (n = s)))

(+ bool-suspend-suspend : type
   n2 : bool-suspend-suspend
   s2 : bool-suspend-suspend
   m2 : (bool-suspend -> (n2 = s2)))

(~ f : (bool-suspend-suspend -> sphere-2)
   (n2 -> b2)
   (s2 -> b2)
   f : ((n2 = s2) -> (b2 = b2))
   (n m2 -> b2 rf)
   (s m2 -> b2 rf)
   f : ((n m2 = s m2) -> (b2 rf = b2 rf))
   (0 m m2 -> surf)
   (1 m m2 -> b2 rf rf))

(~ g : (sphere-2 -> bool-suspend-suspend)
   (b2 -> n2)
   g : ((b2 rf = b2 rf) -> (n2 rf = n2 rf))
   (surf -> {0 m m2 1 m m2 {n m2 s m2 rev}}))

(~ f g : (bool-suspend-suspend -> bool-suspend-suspend)
   (n2 -> n2)
   (s2 -> n2)
   f g : ((n2 = s2) -> (n2 = n2))
   (n m2 -> n2 rf)
   (s m2 -> n2 rf)
   f g : ((n m2 = s m2) -> (n2 rf = n2 rf))
   (0 m m2 -> {0 m m2 1 m m2 {n m2 s m2 rev}})
   (1 m m2 -> n rf rf))

(~ g f : (sphere-2 -> sphere-2)
   (b2 -> b2)
   g f : ((b2 rf = b2 rf) -> (b2 rf = b2 rf))
   (surf -> surf))

(~ :k : ((:x : bool-suspend-suspend) -> (:x f g = :x))
   {n2 :k : (n2 = n2)}
   {s2 :k : (n2 = s2)}
   :k : ((:p : (n2 = s2)) -> (n2 :k (:p :k tp) = s2 :k))
   {n m2 :k : (n2 :k n m2 = s2 :k)}
   {s m2 :k : (n2 :k s m2 = s2 :k)}
   :k : ((:h : (n m2 = s m2)) -> (n m2 :k (:h :k tp2) = s m2 :k))
   ;; 這裏的 tp2 使用比 hott 更高階的類型
   {0 m m2 :k : (><><><)}
   {1 m m2 :k : (><><><)}
   (solve-by
    ><><><))
```

# at1 之用

## 引

- x ::
  我知道我不必找到一個終極的目的之後
  才能爲了這個目的而行動
  我可以用別的方式來理解行動
  或者來說服自己去行動
  但是
  亞里士多德的書給了我們一些希望
  不是嗎

- k ::
  at1 如果能促進 at
  那麼 at 之用就是 at1 之用

- x ::
  但是我想找更直接的用

# [note]

- pattern match is about reverse and reversible function
  we need to learn more about reverse and dual

- ><><><
  in a practical langauge
  beside algebraic datatype
  there is also record
  which is used as the named product type
  some interface functions can be generated by these names
  - I need to designed syntax for this too
    maybe {} should be used for them

- if type can be generated
  then we have many ways to define a type
  - thus how a type is defined
    should be part of the metadata of a type
  - thus how a function is defined
    should also be part of the metadata of a function

- meta programming is about macro
  when we are able to write macro
  how should we type macro

- to be constructive
  a quotient space should be defined by a natural-projection
  maybe fiber bundle should also be defined by projection

- ><><><
  the gluing of adjunction is the same as
  that of the gluing of fibers?

# the extension problem

```scheme
(def inclusion
  (lambda (-> [(: :a type) (: :x type)
               (< :a :x) drop]
              (-> :a :x))
    (-> [:a :x]
        (lambda (-> :a :x)
          (-> :v :v)))))

;; in the view of sze-tsen-hu
;; extension problem is the main kind of general problem of topology

;; to solve the extension problem
;; is to solve an equation in the continuous function space
;; is to find x for given condition

(def g
  (lambda (-> [(< A X) drop A]
              [Y])
    ...))

(def f
  (lambda (-> [X]
              [Y (== [g]
                     [A X inclusion @ f])])
    ...))
```

# the method of algebraic topology

- to induce algebraic equation from continuous equation
  is the method of algebraic topology

```scheme
(def H/induce
  (lambda (-> []
              [])
    (-> []
        [])))

;; should act like the following
;;   this means the language must be powerful enough
;;   to handle function have type (-> [...] [...])
;;   it is serious meta programming

(def g
  (lambda (-> [{< A X} A]
              Y)
    ...))

(def f
  (lambda (-> X
              [Y (== [g]
                     [A X inclusion @ f])])
    ...))

(def [g H/induce]
  (lambda (-> [{< A X} A empty-space :m H]
              [Y empty-space :m H])
    ...))

(def [f H/induce]
  (lambda (-> [X empty-space :m H]
              [Y empty-space :m H
                 (== [g H/induce]
                     [A X inclusion @ H/induce f H/induce])]))
  ...)

;; many styles pf homology theory
;;   which is the best for implementation

;; without serious meta programming power
;;   we can try the following limited definition of homology theory

(def H
  (lambda (-> [(: :X space) (< :A :X) int]
              abelian-group)
    ...))

(def H/induce
  (lambda (-> [(-> [:X :A] [:Y :B]) (: :q int)]
              (-> [:X :A :q H] [:Y :B :q H])))
  (note H 作用於空間本身
        還需要一個 H* 作用於空間中的元素)
  (note 可能需要一個選擇函數
        來從 由 H* 得到的 abelian-group 中的元素
        選擇一個原來 space 中的元素
        這樣 實現 H/induce 就簡單了))

(def boundary
  (lambda (-> [:X :A :q H]
              [:X empty-space :q 1 sub H])
    ...))
```

# retraction

```scheme
(def r
  (lambda (-> X
              [A (== [A id] [A X inclusion @ r])])
    ...))

(def [r H/induce]
  (lambda (-> [X empty-space :m H]
              [A empty-space :m H
                 (== [A empty-space :m H id]
                     [A X inclusion @ H/induce r H/induce])])
    ...))
```

# [note] quotient space

- to define a quotient space
  is to lessen the equality

- the construction of quotient space
  is also called topological identification

- there are many patterns by which we can re-implement equality of a type

- when one is trying to formalize a concept in math
  he should try to use all the implementation tech
  and all the language paradigms

- but it seems we have a basic uniformed equality in the term-lattice
  re-implement of equality is to be built on top of it

# quotient/natural-projection

- which can always be done by natural-projection

```scheme
(def quotient/natural-projection
  (lambda (-> [(: :x type) (-> :x :y)]
              (: :z type))
    (-> [:x :p]
        [{= :z (derive :x)}
         {= :z.eq (-> [:a :b]
                      [:a :p @ :b :p @ :y.eq @])}
         :z])))

(def npj
  (lambda (-> [(: :x type) (: :z type) {/ :x :z}]
              (-> :x :z))
    (-> [:x :z]
        (lambda (-> :x :z)
          (-> :v :v)))))
```

# quotient/acting-group

```scheme
(def quotient/acting-group
  (lambda (-> [(: :x type) (< :g (-> :x :x))]
              (: :z type))
    (-> [:x :g]
        [{= :z (derive :x)}
         {= :z.eq (-> [:a :b]
                      [{= :e (search :g)} :a :e @ :b :x.eq @])}
         :z])))
```

# quotient/identity-element

- by enlarging zero (or one) in algebraic structure

```scheme
(def quotient/identity-set
  (lambda (-> [(: :x type) {with-interface sub :x} (< :x0 :x)]
              (: :z type))
    (-> [:x :x0]
        [{= :z (derive :x)}
         {= :z.eq (-> [:a :b]
                      {: [:a :b sub] :x0})}
         :z])))
```

# ><><>< adjunction space

```scheme
(def adjoin
  (lambda (-> [(: :x type) (: :y type) (-> [:a {< :a :x}] :y)]
              (: :z type))
    (-> [:x :y :g]
        [{= :z (derive (+ :x :y))}
         {= :z.eq (lambda (-> [:z :z] bool)
                    (-> [:a :b]
                        [{} ><><><]))}
         :z])))
```

# >< extension equal to retraction of adjunction

```scheme
(~ g : ((A < X) -> Y))

(~ r : ((X Y g adjoin) -> Y
        (Y id = Y (X Y g adjoin) inclusion @ r)))
(~ f : (X -> Y
          (g = A X inclusion @ f))
   (:x -> :x p r))

(~ f : (X -> Y
          (g = A X inclusion @ f)))
(~ r : ((X Y g adjoin) -> Y
        (Y id = Y (X Y g adjoin) inclusion @ r))
   ><><><)
```

# >< mapping cylinder

```scheme
(def f (lambda (-> X Y) ...))

(def mapping-cylinder/p
  (lambda (-> (+ (X I) Y) [f mapping-cylinder])
    (-> [:x 1] [:x f])
    (-> [:x :i] [:x :i])
    (-> :y :y)))
```

# >< from chain-complex to homology

# >< the cat of top

- for the cat of top
  a top constructor is defined by
  specifying set-level construction
  and specifying the open set or closed set

# >< cell-complex

- CW-complex
  C for closure-finite
  W for weak-topology

# [note] fiber space

# covering homotopy property

```scheme
p : E -> B
project : total-space -> base-space

(def f (lambda (-> X B) ...))

(~ f/homotopy : (X -> B))

;; cover is defined by abstract interface
(~ cover )
(~ f p cover : (X -> E
                  (p f p cover = f)))
```

# [note] continuous

- to say a map is continuous
  is to allow it to be used in the language

# set

## [note] set theory vs type theory

- ><
  what is this vs?

- class (in oo) is encoded by a list of interface functions
  type-class (in haskell) is encoded by a list of abstract interface functions
  which all seem like the dual of the poset structure of set as cat

- the methods of set theory have no fault
  the fault is to not to view them with implementation in mind

## the encoding of type

```scheme
(def-type T
  T.C = [(c1 : (* -> T))
         (c2 : (* -> T))]
  T.G = [(g1 : (term -> bool))
         (g2 : (term -> bool))]
  T.I = [(i1 : (T -> *))
         (i2 : (T -> *))
         (note
           interface functions are implemented by term rewriting)
         (note
           interface can be limited by type-cless)])

(T1 < T2 :=
    T1.C < T2.C
    T1.G > T2.G)

(note
  T1.I < T2.I
  T1 is a quotient space of T2)

(note
  comparing can only be done by comparing list of names
  or declared relation between names
  (naming itself is also a declaration))

(note
  names are made as unique as possible
  this labor can be easied by a module-system or infer-system)

(note
  constructor can be shared by different types
  thus can not infer a unique type from a constructor
  thus cut needs to do search and backtracing)

;; in the view of
;; function as proof and type as theorem and space

(note
  union of two types as a list of glue operations
  intersection in the two types will intro new connection
  continuous is all about levels of glues
  maybe the main formal law of continuous condition is hidden in the union)

(def-function union)
(def-function intersection)

(note
  the formalization of gluing
  must be with detailed info about the gluing is done
  if i view union as gluing
  the info will be denoted by the global-naming-as-mark
  two types with the same constructor can still be seprated or not?)

(note
  to define a named type
  is to intro a named level structure over the space of terms)
```

## image and inverse-image of function

```scheme
(~ f : (X -> Y))
(+ A < X)
(+ B < Y)
(+ B inverse f = ((:x : X) {:x f : B}))
(+ A f = ({:x : A} (:x f : Y)))
```

## >< cartesian product

- cartesian product of two type is a special case of
  general cartesian product
  which is the space of section of fiber bundle
  (i.e. dependent function space)
  (not the total space)

- actually we can see
  the dependent function space is not about fiber bundle
  but only about general cartesian product
  to get fiber bundle we need more info

# >< topology

## [note]

- ><
  how this would improve my understanding of
  the formalization of continuous function in my language

- how the concept of continuous is formalized by abstract axioms of topology
  not only by open set but by the union and intersection functions

## >< abstract axioms of topology structure

# homology

## definition

```scheme
(def H
  (lambda (-> [(: :X space) (< :A :X) int]
              abelian-group)
    ...))
(def H/induce
  (lambda (-> [(-> [:X :A] [:Y :B]) (: :q int)]
              (-> [:X :A :q H] [:Y :B :q H])))
  ...)
(def boundary
  (lambda (-> [:X :A :q H]
              [:X empty-space :q 1 sub H])
    ...))

(def co-H
  (lambda (-> [(: :X space) (< :A :X) int]
              abelian-group)
    ...))
(def co-H/induce
  (lambda (-> [(-> [:X :A] [:Y :B]) (: :q int)]
              (-> [:Y :B q co-H] [:X :A q co-H]))
    ...))
(def co-boundary
  (lambda (-> [:X empty-space :q 1 sub co-H]
              [:X :A :q co-H])
    ...))
```

## >< axioms

```scheme
(def H/identity
  (lambda (-> [{: :id (-> [:X :A] [:X :A])} :id space/iso {: :q int}]
              [:id :q H/induce abelian-group/iso])
    ...))

(def H/compose
  (note
    this is always true
    for H/induce is recursively defined over function composition))
```

# [note]

## term

| at | algebraic-topology |
| dc | data-constructor   |
| tc | type-constructor   |

# [todo-list]

- why design at1?
  what is the use of at?
  why lisp was designed?
  why riemman did his study?
  can we use n-level to model things
  which are not in the kingdom of at theory?
  what I want to do with at1?

- eq must be able to be used as a rewriter
  and not like arrow
  it can rewrite data in both ways
  it uses unify instead of cover

- homology and homotopy uses different type-check for dc and for function

# at

- at -{formalization or algebraiclization}-> infi-groupoid

# chiso

- type as proposition
  function-body as proof

- 在 sequent0 中 可以定義數據
  還可以定義變化這些數據的函數
  之後我們就能 '證明' add/commute 這種函數的性質
  這些性質也是藉助 eq 和 has-length 之類的 tc 來表達的

- 而在 at1 中
  也是定義數據
  但是所允許定義的數據不同了
  然後定義變化這些數據的函數
  但是檢查這些函數合法性的方式也不同了

- 同樣是結構
  但是用以檢查結構合法性的方式不同了
  這樣就形成了不同的理論和不同的語言

# tc & dc

- to define a type by higher-inductive-type
  is like to describe how to draw it level-by-level

- rules for production-by-gluing

  1. production in many ways
     not as classical algebra
     product along common border

  2. position of element in border is important
     same-position-self-production will be canceled
     this is the concept of the reverse-of-element generalized

  3. n+1-level elements as relation of n-level elements
     thinking about a combinatorial infi-groupoid theory

- to intro a new n+1-level dc for a type
  we need to form a closed orientable n-level element
  by producting many n-level elements
  this closed orientable n-level element will be viewed as
  the border of the newly intro-ed dc

# function

- to capture the concept of continuity
  we define function level-by-level
  for all the levels of the input type

- type-check for elements of level above 0
  is like continuity-check

- continuity-check of 1-level elements
  uses the rewrite-rule of 0-level elements

  - ><><><
    a group of specific principles are needed
    as a general way to generate the border-condition
    (A B -> C)
    (:a :b -> :c {:c : border-condition})
    the border-condition must be generated from
    the value
    on  ((border :a) :b ->?)
    and (:a (border :b) ->?)

  - ><><><
    this is where the dependent-type come into play

  - ><><><
    (border (:a * :b)) = (((border :a) * :b) + (:a * (border :b)))

# >< elim diff-level-map as product-type-map

- uncurry level to number of input elements

# the extension problem

- not the extension of a function
  of which the input type is a subtype of another
  but the extension of a partial function
  which is defined only for part of the dc of the input type

# 關於生成無窮多樣數據的方式

- 在 at1 中定義一個 type
  其 0 階元可能只有有限多個
  而其 1 階元可能有無限多個
  相反在 sequent0 中
  一個 type 中只有 0 階構造子
  而其無限多元素只能通過遞歸定義來生成

- 這是因爲 at1 中的一階或更高階元素可以做乘法
  而這種乘法可以生成無限多元素

# 從代數角度考察連續性

- at1 中的一個函數可以作用於屬這某 type 的所有階元素
  包括這些在乘法下生成的元素
  而函數的定義是有限的
  定義這個函數時
  只要對這個 type 的有限個 dc 描述函數所施的變化就行了
  對於相乘而來的元素
  函數所施的變化由函數與乘法的交換性來完成
  即 所有的函數都是 type 之間的同態

- sequent0 中也可以定義能處理無限多數據的函數
  不同點在於
  此時 type 只有 0-level 元素
  元素之間沒有乘法
  每個 dc 代表了 type 的一類數據
  函數定義只要覆蓋了所有這些 dc
  就能處理屬這個 type 的所有數據了
  此時並沒有什麼同態發生

# 具體的連續性檢查

- 如此說來 連續性檢查 就是 同態性檢查
  但是 我們知道 連續性檢查 的具體形式是
  (f a) : (G (f (border a)))
  爲什麼它就算是連續性檢查呢
  因爲假設我們有
  a : 0 == 1
  b : 1 == 2
  如過想要定義 f 使得
  (f (a + b)) = ((f a) + (f b))
  如果 (f a) 和 (f b) 還想要相 glue
  就必須保證 (f a) 的末點和 (f b) 的始點相同
  而 continuity-check 是用
  (f a) : (f 0) == (f 1)
  (f b) : (f 1) == (f 2)
  此時 (f a) 的末點和 (f b) 的始點 都是 (f 1)

# at1 之於 at

- 首先 at 作爲一個數學結構
  有其對象 即拓撲空間
  at 給出了空間之間的基本等詞的定義 即同胚
  這個等詞難以計算於是就有了 at 中的 a 與 同倫
  [畢竟有了等詞之後自然就有分類問題]
  我們在 at1 中定義 type
  就是給出 at 這個數學結構的研究對象
  而我們能就我們所定義的對象 來定義 at 中的基本等詞
  這樣我們就把一個數學理論形式化了

# 被檢查的其他條件

- 這包括 closeness 和 orientable
  由 (border (:a * :b)) = (((border :a) * :b) + (:a * (border :b)))
  看來 (border (border :x)) = 0 如果對於所有 dc 成立
  那麼對於所有 dc 相 + 而得的數據也成立

- 同樣 orientable 可能也有這個性質
  即 被 * 與 + 保持

- 類似的性質可能不只是 closeness 和 orientable
  這些性質都可能被作爲檢查的條件

# 何謂計算同倫羣

- 我之前認爲這在於
  找出一組形式簡單且易於比較的已知其不同倫的空間
  然後證明所要求同倫羣的空間與其同倫
  並且還要發展出系統的方法來這樣做
  這才稱得上是計算

- 但是
  一個空間有多階同倫羣
  如果是與基本的已知不同倫的一組空間比較
  那麼這 多階 又是如何體現出來的呢

# 同倫羣是函數空間 [arrow-type] 而不是空間本身 [type]

- 如何處理函數空間之間的等價

- 注意
  在 hott 中 同倫羣並非 arrow-type 而是 path-type
  我想 這就是我所謂的 uncurry 與 curry 的處理方式不同了吧

- 之前的想法是用 與典型的空間之等價 來說明同倫羣的計算
  其實 用典型的 arrow-type 也可以
  用典型的 path-type 也可以
  計算的本質如此

- 函數空間 之等價 就是 arrow-type 之等價

- 注意
  對一般 type 而言
  證明空間之間同倫等價
  是被劃歸到證明函數之間同倫的

# higher-inductive-type <2016-12-20 Tue>

## 引

- x ::
  對於一個類型 可以引入高階 data-constructor
  與 構造 0 階數據的 data-constructor 不同
  高階的 data-constructor 之引入不是自由的
  它們的類型需要滿足各種條件
  homology 與 homotopy 所需要的條件可能不同
  等等

- k ::
  其實
  構造 0 階數據的 data-constructor 的類型也是有限制的
  它的返回值必須屬這個類型本身

- x ::
  定義類型[space]的過程就是描述一個幾何體[拓撲體]構造的過程
  類型系統的特點就是兩層數據
  在 sequent0 中我們已經能看出來
  有 main-rewrite 也有檢驗部分
  通常是 body-arrow 爲 main-rewrite
  但也可能是 type-arrow 爲 main-rewrite
  在 higher-inductive-type 中我們也能看出來
  此時的 body 是平凡的
  而 type 上的檢驗可以多種多樣
  homology 的檢驗可能較爲寬鬆
  homotopy 的檢驗可能更嚴格 並且還要檢驗可定向性 等等
  檢驗的方式不同
  所允許的構造就不同
  用 '圖像' 來理解這些構造的方式也不同
  homology 可能是 non-determinate
  homotopy 可能是 determinate
  等等
  甚至形式化之後的 homotopy 已經與古典不同
  都有可能

# at1 的目的

- 設計 at1 還有類似語言的目的在於
  我發現 當形式化某個領域之後
  就可能用計算機來實現這個形式系統
  如此我的目的有二
  1. 把數學知識變具體 這關乎教學
  2. 把對數學知識之 '價值' 之評價變得客觀
     消除主管評價數學成果的價值之必要
