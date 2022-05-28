---
title: Old Notes
---

# 新記

## 引
    1. 所有 lambda-term 所構成的有向圖中
       等詞 =b= 是一個無向路
       對等詞的肯定是對一條具體的路的展示[一段[或多段]計算]
    2. 每個路的性質是不同的
       並且其實其不同的性質是需要被注意的
       因爲每一條路都代表計算
    3. M =b= N 是無向路的集合[一個類型]
       所以
       對這個等詞的證明就是
       去找到這個類型中的一個元素
    4. 自然數 是一個集合[一個類型]
       所以
       對自然數的證明就是
       去找到這個類型中的一個元素
## 類型
    1. 帶有類型的 lambda-calculus 能夠形成層次
       而無類型的 lambda-calculus 在沒有層次結構的條件下
       也能編碼自然數和自然數上的基本運算
## as type system
    - with functor builtin
    - ua
      給出兩個 類型之間的 雙射 -> 給出兩個類型相等的證明
      不同的雙射 可能給出同樣類型的證明[對同一個命題的證明]
      雙射就是兩個方向的函數
      它是有計算語義的
      也就是說 對等詞的證明是有計算語義的
    - transport
      兩個類型相等的證明 -> 兩個類型的元素 可以在任何地方相互代換
      但是具體的代換必須用具體的函數來完成
      如何從對相等的證明中選取出函數來實行代換
## 關於等詞
    - ua 成了等詞的引入
      但是其實應該可以有不同的等詞
      每個 type 都必須有等詞做爲其基本接口函數
      注意
      等詞並不是一個函數 而是一個類型
      也就是說 等詞返回的不是 bool 而是 type
## 同構
    - 證明 兩個數據類型某種意義上同構
      其中一種數據類型 可能適合證明
      而另一種 可能適合計算
      這樣就能在不同的場合使用同構不同數據類型了
## quotient
    - 以 bishop 的方式定義集合
      然後再加上 quotient 之後
      是否就達到 hott 的效果了呢
      畢竟
      在 bishop 的概念下
      集合的意義已經深刻改變了
      但是
      如果沒有 帶到 hott 的效果
      那還差什麼呢
## 等詞與則式之間的關係是什麼
    - 有了則是 是否就不需要等詞了
      有 (A -> B)
      且有 (B -> A)
      就是 (A == B)
## >< 則式 的 幾何解釋 是什麼
    - 如果想要用 則式 來處理等詞
      那麼 則式 的意義有該如何
# notes on homotopy λ-calculus [vladimir voevodsky]
## 引
    - 數學基礎的相對性
      只要理論本身的複雜性
      還沒有發展到 讓直覺性的[半直覺性的]論證進行不下去
      那麼人們通常根本就不考慮數學基礎這個問題
    - 然而
      當考慮到同行對證明的驗證
      而意識到 在技術細節上 需要機器輔助證明[驗證]的時候
      徹底的形式化就勢在必行了
# 動機
  - 去以構建一個機器輔助證明系統爲目的
    也許能幫助人理解這裏的工作
  - 想要提供一個更好的對數學基礎的形式化的動機是
    希望能夠設計出可用性更強的機器輔助證明系統
# 關於邏輯
  - 在構造性的數學中
    如果我有一個數學結構
    - 按經典的集合論語義來理解
      我所使用的基本集合是我用歸納定義來得到的
    然後如果我定義等價關係
    作爲歸納定義有向樹中的無向路
    對於基本集中的兩個具體元素
    我已經有一種方法來判斷它們之間是否具有某個等價關係了
  - 在舊的筆記中 形式理論 是一個重要的名詞
    但是其實也許我應該完全廢棄這個名詞
    而在 curry-howard-correspondence 的幫助下
    用 lambda-calculus 來理解邏輯
- ><><<>< [舊筆記]
    我再引入一些推理規則是什麼意思?
    首先
    當引入一些推理規則的時候
    我就得到形式理論
    這時在這個形式理論和我的數學結構之間
    可以問
    - 一致性[協調性 相容性]
    - 完備性
    這兩個主要問題
  - 形式理論與數學結構之間的關係
    就是 形式理論的推理規則
    與 數學結構的基本集中的基本等詞之間的關係
    - 但是它們的關係好像都是虛的
      爲了從 基本等詞
      形成各種關於理論的命題
      我只需要用基本等詞定義謂詞[到0和1的映射]而已
    - 但是
      有些謂詞 雖然存在 但是 不可計算?
      所以需要高階理論?
  - 當我把形式理論與數學結構之間的一般關係明確了
    我就可以
    - 自由地引入推理規則對某個數學結構形成形式理論
    - 把所能形成的各種形式理論
      作爲描述我的數學結構中的那些一般性質的語言
  - 要知道
    能形成什麼樣的命題都是和形式理論有關的
  - 甚至
    如果我說
    "形式理論爲我提供了證明的工具"
    那都是不恰當的
    因爲
    - "證明" 的意義包含於形式理論本身
      因爲是推理規則在構建以命題爲節點的有向圖
    - "去證明什麼樣的東西" 也包含於形式理論本身
      因爲是推理規則在決定以基本命題爲基礎
      形式理論中的其它命題長什麼樣
      即 如何由基礎命題引入複合命題
# 等詞的意義
  - 說兩個集合等勢時
    它們之間的雙射可以是多種多樣的
  - 說兩個拓撲空間對同倫等價時
    它們之間的同倫變換可能是多種多樣的
  - 當我說等詞 M =b= N 成立的時候
    在有向圖中
    我可能能以很多的方式找到
    來對這個等詞形成判定的無向路
  - 除了基本等詞的判定方式可能是單一的之外
    對其它的等詞的判定都是不單一的
  - 重要的是要理解到
    對非基本等詞的判定是要找一條路
# type theory [the book]
## 動機
    類型論內 每個變元都被指定類型
    作下面的考慮就知道這是自然的:
    集合論構建在一階邏輯的形式理論的基礎上
    而在實際的數學事件中
    人們卻直接使用集合論和一階邏輯所構成的
    一種混雜形式語言
    也就是在用量詞引入約束變元的同時規定約束變元所在的集合
    也就是說量詞不是被單獨使用的 而總是與集合一同使用的
    這種擴展了的量詞的使用可以被看成是
    之使用單純量詞的一階邏輯語言的"語法糖"
    - > 類型論處理了這個問題嗎?
      也就是要給這種混雜語言一個理論基礎?
## 類型有兩種語義:
    1. 集合
    2. 命題
       (a:A是a對A所代表的命題的可證性的見證)

    "一個變元對一個類型的屬於"
    與"一個元素對一個集合的屬於不同"
    後者是一個一階邏輯中的命題
    前者是一個證明論層次上的元命題
## as languages
    一階邏輯與集合論
    類型論
    它們都作爲數學基礎的兩種形式語言
    它們之間的關係是什麼?
    - 就像德語與中文之間的關係一樣
      一種語言可以用來介紹另一種語言嗎?
## functions not are as relations
    but are a primary concept in type-theory
## 推理規則 v.s. 公理
    - 類型論:
      動態的推理規則
    - 一階邏輯 + 集合論:
      一階邏輯的推理規則 + 集合論的靜態公理
## polymorphic identity function:
    id :== λ(A:U).λ(x:A).x

    也就是說表達式中類型所在位置也可以用來作符號代入
    但是問題也跟着來了:
    後面的λ(x:A)對前面代入的A有依賴性
    即只有代入A之後才知道後面的東西的類型是什麼
    使得沒法用正常的記號寫出這個λ-abstraction的類型

    只能引入記號∏:
    id : ∏(A:U).A> - A

    ∏(A:U).A is just like λ(A:U).A
    it is ∏-abstraction,
    the type of a ∏-abstraction is not important,
    ∏-abstraction is for to help people to describe
    the type of λ-terms like λ(A:U).λ(x:A).x

    所作出來的函數的 前面所需要帶入的類型可以被看做是
    對後面所輸入的函數的類型的要求
## universes and families
    同集合論中一樣
    這裏需要用類型的universes的層次結構來避免
    U∞:U∞所能引起的悖論
    - 每一層次的universes對於cartesian-product封閉
      observing that:
      ordered pairs are a primitive concept,
      as are functions.
    - 每一層次的universes包含前一層次
      這樣規定的不好之處在於
      一個變元所屬的類型不再是唯一的了

    同樣也有families的概念
    但是既然families是函數那就也應該可以用
    類似λ-abstraction的東西來把它們寫出來
    這樣就產生了∏-abstraction和上面的
    對λ(A:U).λ(x:A).x的類型的記法
## >< 語言
    對比 人類交流語言 程序語言 數學語言 的基本功能

    要創造一個人造人類交流語言
    我需更要實現的核心功能有那些?

    要設計一個新的(一般目的的)程序語言
    我需要實現的核心語義有那些?

    要給數學基礎設計一個新的形式語言
    我需要獲得的核心語義有那些?

    這三種語言之間有什麼區別?
    首先原料不同
    比如語音的需要不同
    普通的人類交流語言需要語音
    而數學語言完全不需要語音
    一個數學家在家安靜地看書 然後給朋友寫信就行了
    程序語言也不需要語音

    數學語言的基本語義在於能夠聲明我證明了某個東西是真理
    也就是說其核心語義在於證明
    在於讓將思想概念之間的關係完全形式化
    不管是
    一階邏輯+集合論
    範疇論
    類型論
    都有推理規則來作證明

    發明一種新的推理規則之後
    這種推理規則所產生的理論的整體性質是什麼?
    那種有向圖的結構所能形成的幾何的幾何性質是什麼?
    - >< 這是我感興趣的
      也許第四級運算的不可能性就是一個整體性質呢?!!!

    與類型論相比
    一階邏輯與集合論所構成的數學的基礎語言就像一種混雜語
    因爲此時公理是在集合論中的
    而推演規則是在一階邏輯中的
## dependent pair types
    ∑(x:A).B(x)
    這個式子作爲類似λ-abstraction的東西
    帶入a:A後 在類型公式中的得到的類型是:
    A×B(a)

    而∏(x:A).B(x)
    被帶入a:A後 在類型公式中的得到的類型是:
    B(a)
## how to define functions
    to define a function
    is to construct elements of A->B

    to define a function
    is to show the rewrite-rule of it
    by some equations
## natural numbers
    the essential property of the natural numbers
    is that we can
    define functions by recursion
    and perform proofs by induction
## propositions as types
    translation of logical connectives into
    type-forming operations

    The basic principle of the logic of type theory
    is that a proposition is not merely true or false
    but rather can be seen as the collection of
    all possible witnesses of its truth

    since types classify the available mathematical objects
    and govern how they interact
    propositions are nothing but special types
    namely, types whose elements are proofs

    這裏反證法的語義是"直覺主義"的 或 "構造性的"
    ¬¬A == (A->0)->0
    =/= A

    the propositions-as-types versions of "or" and "there exists"
    can include more information than
    just the fact that the proposition is true
## >< 類型之間的依賴性爲什麼是重要的?
    據說這還是各種形式理論中一直以來所確實的
## >< 關於應用
    機器證明被用來作爲對代碼進行靜態分析的工具
    並且已經形成了相關的產業
# formalization [觀點來自俄國人VV的演講]

- 好的形式體化
  應該使得各種層次的 "等價" 都成爲可能
- 用同倫理論來編碼數學對象就可以實現這一點
  這在於證明
  formalism of higher equivalences
  (theory of higher groupoids)(範疇論)
  ==
  homoptopy theory
  但是這種編碼是不可用的
  因爲同倫理論本身就是複雜的數學理論
- 類型論可以在這裏起到作用
  以幫助同倫理論 對其它數學對象的編碼
- 因爲類型論提供了直接面向同倫理論的形式語言
- 關於 "不接受"
  用編程界的術語來打比方
  數學家的社區不接受某種東西
  可能是因爲
  這種東西的 syntax 沒有良好對應的 semantics
  - 比如類型論剛產生時候的處境

# syntax

  t ::= x | c | f | λx.t | t(t')

  f as defined constant
  each defined constant has zero, one or more *defining equations*

  f(x1,...,xn) :== t
  where t does not involve f

  f就是rewrite-rule
  或者說f用來微觀地定義一個代數結構
  + 比如SKI就是f的代表
# contexts
  A context is a list
  x1:A1, x2:A2, ..., xn:An
  which indicates that the distinct variables
  x1, ..., xn are assumed to have types
  A1, ..., An, respectively

  the context holds assumptions

  (x1:A1, ..., xn:An) ctx
  ------------------------------------Vble
  x1:A1 , ..., xn:An ͱ xi:Ai
# methodology
## note
    每個基本的東西:
    笛卡爾積,等詞,不交併 等等
    都是通過給出一個類型而給出的
    + propositions as types是什麼?
      是兩個形式語言之間的關係嗎?
      一階邏輯與類型論??
      兩個形式語言之間的關係是通過模型法而被探索出的嗎??
      當同時爲同一個模型構造兩種形式語言的時候就會出現這種問題了

    >< 每次補充定義類型都會增加新的推演規則??
    這使得這種語言更加靈活
## formation rule
 stating when the type former can be applied

 Γ ͱ A:Ui    Γ, x:A ͱ B:Ui
 ---------------------------Π-FORM
 Γ ͱ ∏(x:A).B:Ui

 每個證明論意義下的論斷
 都必須用"ͱ"來明確其語境(條件)
 因此推演規則就是在"ͱ"語句之間的作推演

 ∏(x:A).B
 是這種語言提供的描述類型之間依賴關係的方法之一
 比如Γ, x:A ͱ B:Ui
 就是包含了對一種對類似的依賴性的描述
 也可以理解爲B:A->U
## introduction rules
 stating how to inhabit the type

 Γ, x:A ͱ b:B
 ----------------------Π-INTRO
 Γ ͱ λ(x:A).b:∏(x:A).B
## elimination rules
 or an induction principle
 stating how to use an element of the type

 Γ ͱ f:∏(x:A).B    Γ ͱ a:A
 ---------------------------Π-ELIM
 Γ ͱ f(a):B[a/x]
## computation rules
 which are judgmental equalities
 explaining what happens
 when elimination rules are applied to results of introduction rules

 Γ, x:A ͱ b:B    Γ ͱ a:A
 -----------------------------------Π-COMP
 Γ ͱ (λ(x:A).b)(a) == b[a/x] : B[a/x]
## uniqueness principles
 (optional)
 which are judgmental equalities
 explaining how every element of the type
 is uniquely determined by the results of
 elimination rules applied to it

 Γ ͱ f:∏(x:A).B
 ------------------------------Π-UNIQ
 Γ ͱ f == (λx.f(x)) : ∏(x:A).B
# from-video
## note
    1. types are ∞-groupoids
       ∞-groupoid is a algebra-structure of category theory
    2. workflow
       數學給類型論提供新想法
       類型論給數學提供新形式證明方式
    3. type的兩個基本語義
       - spaces as types
       - propositions as types
    4. 同倫不變性對這個形式語言來說是內蘊的
       空間的同倫類就是這個語言的基本元素
## π...1(S^1) = Z(Zahl)
 Circle is inductively generated by:
 (point) base : Circle.
 (path) loop : base = base.

 we get free ∞-groupoid with these generators
 id
 loop^[-1]
 loop o loop
 inv : loop o loop^[-1] = id
 ...
### Circle recursion
  function:
  f : Circle ->  X
  is determined by:
  base' : X
  loop' : base' = base'
### Circle induction
  to prove ∀x:Circle,P(x)
  suffices to prove
  1. prove P(base)
  2. the proof you give is continuously in the loop
### π_1(S^1)
  π_1(S^1) == 0-truncation of Ω(S^1)
  == set of connected componets of Ω(S^1)

  to prove:
  Ω(S^1) = Z(Zahl)

  is to define:
  + 即找同構映射
  winding : Ω(S^1) -> Z(Zahl)

  is to represent the universal cover in type theory
  the universal cover is fibration
  in type theory fibration is familiy of types
  對fibration的經典定義是保持道路的連續映射
  + path-lifting
    proj : E -> B
    B中的path:
    path-of-B : p(e) =B= y
    的逆像是E中的path:
    proj^[-1](path-of-B) : e =E= p^[-1](y)
    主意這裏通過固定一個E中的e點來簡化說明

  語義上映射的像集被映射的定義域纖維化
  實際上是一個空間被令一個空間參數化
  這就自然得到了fibration在type-theory中的表示

  fibration = familiy of types
  + 也就是說fibration是familiy of types的語義之一
    familiy of types還有邏輯學上的語義
  notation:
  (E(x))_x:B
  + 語義上 即B對空間E的參數化
    給出一個參數b:B後E(b)是E的子空間
    因此E(x)所描述的依賴關係就是上面的proj^[-1]
  Π x:B . E(x)
  ((Π x:B . E(x)) b) --> E(b) == proj^[-1](b)
  where E(b) is a type (a fiber)

  語義中對path的保持性由下面的式子捕捉:(transport)
  ∀ path : b1 =B= b2
  gives equivalence E(b1) == E(b2)
  什麼意思?
  B中的道路給出高維度的道路嗎?

  so here we have the universal cover:
  (Cover(x))_x:S1
  DEFINE:
  Cover(base) :== Z(Zahl)
  transport_Cover(loop) :== successor
  即定義纖維化就是去
  定義纖維
  + 這裏是:Cover(base) :== Z(Zahl)
  然後定義lifting the path的時候所給出的纖維上的變換是什麼
  + 這裏是:transport_Cover(loop) :== successor
    transport_Cover(loop o loop) :== successor o successor
    等等
  DEFINE:
  winding : Ω(S^1) -> Z(Zahl)
  (winding path) :== ((transport_Cover path) 0)
  + 我用lisp的語法了要不然歧義太大

  https://video.ias.edu/sites/video/files/ams/2012.restore/2012/MembersSeminar/Licata-2012-11-26.hi.mp4
  and about group
  https://video.ias.edu/members/rivin

## >< the hopf fibration
## constructive-type-theory-and-homotopy
### about equivalence
  在我對λ-cal的理解中
  t:Λ這樣一個類型聲明甚至都是構造性的
  它說明t是無窮有向圖graph(Λ;-sβ->)中的一個節點
  而p:Id_Λ(a,b)說明
  p是graph(Λ;-sβ->)中的兩點a,b間的一條有向路
  + 或者寫成p:a =β= b這樣寫的話就更明確了"Id_Λ"的意義
    因爲對每個類型(比如這裏的Λ)可能可以定義不同的等詞
    比如我可以寫α:Id_(Id_Λ)(p,q)
    但是這裏我需要知道類型(或空間)Id_Λ中的等詞是什麼
    當Λ是一個拓撲空間時α:Id_(Id_Λ)(p,q)就是
    道路p,q之間的homotopy
    但是當Λ是λ-term的集合時上面的類型(Id_Λ)(p,q)中的等詞又是什麼呢?
    考慮這樣一個有向圖:N
    它的節點是二維平面上的所有整數點
    有向邊是橫座標或者縱座標上的後繼關係
    這樣的圖中顯然(Id_N)(p,q)中的等詞是有自然定義的
    因爲我可以相像一條無向邊在這個圖中的"連續移動"
    對於圖graph(Λ;-sβ->)來說當然也可以有這樣的理解
    太棒了

  但是問題是在類型論中對t:Λ這樣的聲明是如何理解的?
  是先驗的嗎?
  是隨意引入的嗎?
  是構造性的嗎?
  來形式化Id概唸的推理規則是下面這樣的:

  A:type
  ----------------------- Id formation
  x,y:A ͱ Id_A(x,y):type
  + 那麼對應於Id_A的等詞只能是單一的了???
    這樣的情況是可以接受的嗎??

  a:A
  ---------------- Id introduction
  r(a):Id_A(a,a)
  + r denotes reflexivity

  x,y:A, z:Id_A(x,y) ͱ B(x,y,z):type
  x:A ͱ b(x):B(x,x,r(x))
  ---------------------------------------- Id elimination
  x,y:A, z:Id_A(x,y) ͱ J(b,x,y,z):B(x,y,z)
  + heuristic:
    x = y
    B(x,x)
    -------
    B(x,y)

  a:A
  ----------------------------------- Id computation
  J(b,a,a,r(a)) = b(a) : B(a,a,r(a))
  + "bookkeeping of witness-terms"
    什麼意思???

### about dependent
  dependent types are fivrations
  so x:A ͱ B(x) has the following lifting-property

  x:A ͱ B(x)
  ---------------------
  x:A y:B(x) ͱ y:B(x)
  -------------------------------
  x:A ͱ (λ y.y) : (B(x) -> B(x))

  p:Id_A(a,b), x:A ͱ (λ y.y) : (B(x) -> B(x))
  ----------------------------------------------??用到Id-elim嗎??
  p*:B(a)->B(b)

  A中的路p:Id_A(a,b)
  被舉到B空間族裏
  就成了兩個纖維B(a),B(b)之間的映射

  p*:B(a)->B(b)
  a^:B(a)
  -----------------
  p*(a^):B(b)

### homotopy interpretation of type theory
  concrete:
  |-------------------+------+-----------------------------|
  | type              | <==> | space (homotopy type)       |
  |-------------------+------+-----------------------------|
  | term              | <==> | map                         |
  |-------------------+------+-----------------------------|
  | a:A               | <==> | point a:1->A (a map)        |
  |-------------------+------+-----------------------------|
  | p:Id_A(a,b)       | <==> | path p from a to b in A     |
  |-------------------+------+-----------------------------|
  | h:Id_(Id_A)(p,q)  | <==> | homotopy h from p to q in A |
  |-------------------+------+-----------------------------|
  | dependent type    | <==> | fibration                   |
  | x:A ͱ B(x)        |      | map:B -> A                  |
  |-------------------+------+-----------------------------|
  | identity type     | <==> | fibration                   |
  | x,y:A ͱ Id_A(x,y) |      | map:Id_A -> (A x A)         |
  |-------------------+------+-----------------------------|

  >< abstract:
  even better
  we have abstract axiomatic description
  via Quillen model categories
  only need weak factorization system of it

  沒有範疇論的基礎weak factorization system我還沒法理解
  只知道weak factorization system與上面的四個推理規則完全契合
