---
title: object -- old notes
---

# type of type system

- dynamic vs. static

- different subtype semantic

# classify functions according to different information

- classify functions according to their output arguments

  (: f1 (* -> t1))
  (: f2 (* -> t1))

  in algebraic data structure
  all data-constructors of a type returns this data of this type

- classify functions according to their input arguments

  (: m1 (c1 -> *))
  (: m2 (c1 -> *))

  when pass a message to an object
  the object is the main input argument
  of the function denoted by the message

- classify functions according to both their input and output arguments

  for example, type class of haskell

# data-constructors in ocaml

- ocaml 裏面
  同一個數據構造子
  可以接收各種類型的參數
  接收不同類型的參數的時候 可以返回各種不同類型數據
  並且
  接收相同類型參數而構造出的數據 也可以被認爲是屬於多種類型

# inheritance in oo

- (: m1 (c1 -> *))
  (: m2 (c1 -> *))

  (c2 < c1)

  (: m1 (c2 -> *))
  (: m2 (c2 -> *))

  add-method :
  (: m3 (c2 -> *))
  (: m4 (c2 -> *))

# subtype

## encoding of type and subtype relation by poset structure of set

- poset -- partially ordered set

- encoding type by its data-constructor list
  the more its data-constructors,
  the bigger the type is.

- encoding type[class] by its interface-function list
  the more its interface-functions,
  the smaller the type[class] is.

- note that,
  when encoding by data-constructor list or interface-function list,
  we not only need their name,
  but also need their types.

## implied subtype relation

- sub-type relation implied by sum-type :
  t0 := t1 | t2
  t1 < t0
  t2 < t0

- sub-type relation implied by multiple-inheritance :
  c0 (inherit c1 c2)
  c0 < c1
  c0 < c2

- 也就是說 multiple-inheritance 與 sum-type 恰好相反

# pattern match in functional language

- pattern match is possible because of :

  1. data-constructor separate the type to disjoint parts
     or say, separate the type into disjoint subtypes
     [this is for totality-check]

  2. data-constructor as function is trivially reversible

# pattern match in oo

- a class in oo also has constructor
  for example, sending message 'new' to class

  (: new ([* c1-it-self] -> c1))

- such functions might not be reversible
  thus no pattern match in oo

  even they are reversible
  it will still be hard to do totality-check

  - might be done by explicitly assert subtype relations
    syntaxly, this would hardly be an acceptable solution

# inheritance in functional language

- can be done, for example, in type class of haskell

  thus, 'classify functions according to their input arguments'
  is not what makes inheritance possible

  what makes it possible
  is encoding class by interface-function list

# 在 oo 中實現 linked-list 體現 oo 缺少 sum-type 語義

- 如果定義 <list> := <null> | (cons <object> <list>)
  從 sum-type 的角度看 <null> 是 <list> 的 sum-type 中的一項
  這樣就有子類型關係 :: <null> < <list>
  因此 <null> 就應該繼承 <list>

  但是 <list> 有兩個數據域 一個是 head 一個是 rest
  而 <null> 沒有數據域
  它只有一個值 是一個比 <bool> 還簡單的 class
  這就與繼承的語義相矛盾了

- 也就是說 oo 缺少 sum-type 語義

- 能夠想到的解法有 :
  1. 聲明 null 是 linked-list 這個 class 的 special value
  2. 在 list 這個 class 中添加一個 flag field
     所有以 list 爲參數的函數都要檢查這個 flag

# pattern matching 與 sum type 的缺點

- pattern matching 是針對 sum-type 的
  每個 以 sum-type 爲參數的函數
  必須 cover sum-type 的所有項

  因此當想要增加 sum-type 的一項的時候
  就要修改所有的函數

- 在這種情況下 如何保持動態性?
  做動態的 dispatching?

  所有需要就類型來做 branch 的地方
  都可以用 dynamic dispatching

  但是 在 <list> 與 null 的例子中
  需要做 dispatching 的是值

- 用添加 flag field 的方式來解決這個問題時
  也有 '需要修改所有函數' 的問題

- 也就是說
  sum-type 的缺點是本質的

# 動態性 與 generic function

- 就 dynamic type tag 做 multiple-dispatch 的定義函數之方式
  是把 一個大函數中的很多 branch 處理成了 dynamic-dispatching

  如此一來
  需要增加一個 branch 的時候
  不必修改這個大函數本身
  [因爲這個大函數已經被拆分成許多小函數了]
  而只要增加一個小函數就可以了

- 這個性質使得 代碼易於修改

- 如果想要把這種 '代碼易於修改'性 推向極致
  函數就不單單要 multiple-dispatch by type-tag
  同時還要 multiple-dispatch by value

  - 如何實現這中性狀呢?

- 函數式的 pattern-matching
  如果能把分支拆開來寫
  就算是做到了這一點
  [但是函數式的類型之間沒有繼承關係 [不考慮 type class 的條件下]]

- 考慮 type class 的條件下 又如何呢?

# forgetful functor and subtype

- <vector> is subtype of <list>
  because functions return <list>
  are more then functions return <vector>
