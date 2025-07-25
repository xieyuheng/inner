---
title: Lambda Calculus
---

# [note] 用图论来想象表达式所形成的空间

在考虑 exp 与 reduction 所定义的 exp 之间的 equivalence relation 时
把 exp 所形成的空间想想成了 graph
confluent (Church-Rosser theorem) 之类的定理很容易在这种 graph 中想象

# [note] 语法

```
(x: arg_t) => body
(x: arg_t) -> ret_t
```

# the lambda calculus, its syntax and semantics

好的講授方式是先在非形式化的語言下試着描述
以發現對理論的哪部分作嚴格的形式化是有必要的
這正是數學研究的典型過程
數學家正是要在觀察中發現結構
然後通過形式化的描述來增進對問題的認識

scott 限制函數與某個拓撲空間上的連續函數集
然後 λ-cal 的理論就有了拓撲[幾何]解釋
那麼根遞歸函數有關的不動點概唸的幾何意義是什麼呢?

pure λ-calculus
models of λ-calculus are objects in category [of course]
and these categories are cartesian closed
[then non-unary functions can reduced to unary functions]

λ-calculus studies functions as rules
[but not geometry object]

no matter use λ-exp as expression defined by grammars
or λ-post-exp as directed graph
one can always captures the concept of
"exps modulo convertibility"
which is the principle object of the study
the following questions need to be studied
1. λ-definability
   λ-可定義的函數的集合等於遞歸函數的集合
2. the relations between non-convertible exps
   定義一個λ-可定義函數的exp要麼是一個要麼有無窮多個
3. the limit of λ-calculus to captures the notion of function
   exp 的集合上有一個自然的拓撲結構 :tree-topology
   exp -> Bohm-tree -> scott-topology
   所有的λ-可定義函數都是就這個拓撲而言的連續函數

conversion
all about "exps modulo convertibility"
use λ-exps to introduce "exps modulo convertibility"
the set of λ-exps is a set of strings
defined inductively by grammars
the theories on this string-level is called syntax
≡ denotes the syntactic equality

[x:=N] is an function act on λ-exps
which is used with rules to define =b=
[or just =][b-convertibility]
this equivalence relation define the theory λ [or λ-calculus]
[when there is rules there is theory]
provability in the theory λ of equation is denoted by λ⊦(M=b=N)
- 其實說 =b= 是一個等詞 而 M =b= N 爲真就行了
  這是兩種形成理論的風格嗎?
  一種來源於證明論?
  證明論 中 層次結構好像格外的多
one can say M and N is convertible when λ⊦(M=b=N)
this seems a loop-define
bacause the notion: provability of proof-theory is used
to define the equivalence relation
rules give a partial-order on the set of λ-exps

connectives and quantifiers which can describe propositions
will be used in the discussion about λ-cal
- 這裏的敘述有問題

what is equation?
equation in λ-cal is proposition with many quantifiers

# hott enlightenment

關於類型論
因爲每引入一個新的類型
就可以引入一些相應的 referece-rules

所以一次啓蒙在於
類型論讓人認識到
referece-rules 並不必以一階邏輯爲中心
而是可以相當任意的創造的

比如
我可以以字符串或者有向圖爲基本數據結構
來實現一個形式理論
那麼對於字符串或者有向圖我都自然有一些基本的 "等詞"
對推理規則的引入的自由性就在於:
推理規則中對這些等詞的引入不能違背
這些等詞在基本數據結構中本來的語義
此限之外別無它限

更常見的是
推理規則 需要引入
以基本等詞爲基礎在基本數據結構中實現的
其它關係
對其它關係的實現本身並不會違背基本等詞
但是引入這些關係的推理規則可能會違背基本等詞
所以需要加上上面的限制

TODO 是不是
有了基本等詞之後就沒有必要使用 推理規則了?

因此首先我要一個直覺性的並且具有可構造性的數學結構
當我用計算機來實現一個數據結構時
很自然地我就得到上面的良好性質
然後我引入一些針對這種數學結構的推理規則
之後我就得到了一個形式語言
用這個語言我可以推導出一些描述這個數學結構的一般性質的命題
這些命題的正確性由推理規則和數學結構的相容性保證
- 推理規則 和 數學結構 之間的關係就是證明論??
這些命題是具有實在意義的因爲我是一個純粹的形式主義者

關於術語
這裏 爲了描述上面的形式語言於數學結構之間的關係
我通過創造新的術語
並且把對這些術語的使用只侷限在我在上面所描述的這種關係
來避免歧義
稱上面的關係爲
形式語言 捕捉到 數學結構
既然我是在創造一個語言來描述我的幻想
那麼這個動詞是很生動的
一組推理規則可能能於一個數學結構相容
而又於另一個數學結構不容
即這組推理規則所形成的形式語言
能夠捕捉到某些數學結構
而捕捉不到其它數學結構

TODO 考慮完備性時
我是不是應該更改我的術語
- 上面只考慮了相容性
當形式語言就某個數學結構而言具有完備性的時候
應該怎麼說??

# (henk barendregt) type free lambda calculus

## symbol

1. 給出兩個 symbol
   人們可以區分它們是相等還是不相等
   這是一個先驗的假設
2. 在我看來
   這也就是
   形式邏輯作爲一個數學結構的基本等詞
3. 我想形式主義沒有認識到的一點是
   基本等詞的任意性

## alphabet

1. 一個 symbol 的有限集合

## word

1. 只與 alphabet 有關

## language

1. 歸納定義一個 word 組成的集合
   然後稱這個集閤中的元素爲 formula 或 expression
   formula 于歸納定義的方式有關
   常用的進行歸納定義的方法是 grammar
2. 而我的 expression 的集合可以是有向圖

## theory

1. language的子集
   根語義有關

## combinatory logic [CL] 作爲一個形式理論

### alphabet

```
ΣCL = {I, K, S, x, ', ), (, =}
```

### language

constant := I | K | S
variable := x | variable'
exp := constant | variable | (exp exp)
formula := exp =b= exp

+ 上面這個應該不出現在這裏
  因爲就我的理解而言它們應該屬於第二層次的exp

### 模式匹配對=b=的引入

#### note

沒有(λx.M)N =b= M[x:= N]
就是說這個形式理論中
有λ-application
但是沒有λ-abstraction

#### 於ISK有關的

即把第二層次的exp的作爲節點引入一個有向圖(一個推理場景)
- TODO 這個課程想把所有λ-exp都編譯到ISK嗎???

in the following P,Q,R are pattern-vars

```
----------(I-axiom)
IP =b= P

-----------(K-axiom)
KPQ =b= P

----------------(S-axiom)
SPQR =b= PR(QR)
```

#### 其它引入=b=的推理規則

in the following P,Q,R are pattern-vars

```
--------------(axiom)
P =b= P

P =b= Q
--------(交換性)
Q =b= P

P =b= Q
Q =b= R
------------(傳遞性)
P =b= R
```

下面的兩個在一起就等價於
=b=與集合language的遞歸定義的相容性

```
P =b= Q
-----------
PR =b= QR

P =b= Q
-----------
RP =b= RQ
```

### TODO cool examples

#### TODO doubling

下面的等號代表命名

```
D :== SII

------------
Dx =b= xx
```

#### TODO composition

#### TODO self-doubling, life!

## TODO representing algebraic functions in CL

## λ-cal作爲一個形式理論

### alphabet

```
Σ = {x, ', (, ), λ, =}
```

### language

```
variable := x | variable'
exp := variable | (exp exp) | (λ variable exp)
formula := exp =b= exp
```

#### 主要的引入=b=的模式匹配

模式匹配用下面的等式來表達

```
DEFINE:
(λx.M)N =b= M[x:= N]

如果M,N在上面的公式所表達的模式匹配下匹配成功
---------------------------------------------
M =b= N
```

#### 其它引入=b=的推理規則

in the following M,N,L are pattern-vars

```
--------------(axiom)
M =b= M

M =b= N
--------(交換性)
N =b= M

M =b= N
N =b= L
------------(傳遞性)
M =b= L
```

下面的兩個在一起就等價於
=b=與集合language的遞歸定義的相容性

```
M =b= N
-----------
ML =b= NL
LM =b= LN

M =b= N
--------------
λx.M =b= λx.N
```

### bureaucracy

#### substitution

| M       | M[x:=N]            |
|---------|--------------------|
| x       | N                  |
| y(=/=x) | y                  |
| PQ      | (P[x:=N])(Q[x:=N]) |
| λx.P   | λx.P              |
| λy.P   | λy.(P[x:=N])      |

#### variable convention

assume that the bound and free variables in a situation differ

在用字符串來實現λ-cal這個形式語言時
在一個exp中如果需要的話總是重命名約束變元
使得它們不與exp中出現的其它任何自由變元相同
這樣在進行substitution的時候就不用考慮那麼多了

#### The set of free (bound) variables of M , notation FV(M ) (resp. BV(M ))

##### FV

```
FV(x) = {x}
FV(MN) = FV(M)∪FV(N)
FV(λx.M) = FV(M)\{x}
```

##### BV

```
BV(x) = ∅
BV(MN) = BV(M)∪BV(N)
BV(λx.M) = BV(M)∪{x}
```

### =η=

```
λx.Mx -sη-> M
```

## exercises

### in-class problems

#### PROBLEM3:

##### (a)

π :== λxyf.fxy
<M, N> :== π MN
+ it "packages" two λ-exps in one single λ-exp
  這是一個笛卡爾積的模型
show that there are π1, π2 ∈ λ such that:
π1 <M, N> -b-> M
π2 <M, N> -b-> N

SHOW:

+ 下面出現的那些外在定義的的東西
  應該和理論中的exp用=a=相連嗎?
<M, N> =a= λxyf.fxy M N -ssb-> λf.fMN
有:
(λf.fMN)L -sb-> LMN
所以:
L :== λxy.x
LMN -ssb-> M

L :== λxy.y
LMN -ssb-> N

所以:
π1 :== (λpl.pl)(λxy.x)
π1 <M, N> -sb-> (λl.(λf.fMN)l)(λxy.x)
-sb-> (λf.fMN)(λxy.x) -sb-> (λxy.x)MN -ssb-> M

π2 :== λpl.lp(λxy.y)
π2 <M, N> -sb-> (λl.(λf.fMN)l)(λxy.y)
-sb-> (λf.fMN)(λxy.y) -sb-> (λxy.y)MN -ssb-> N

END-OF-THE-SHOW.

##### (b)

show that for F,G∈Λ there exists F^,G*∈Λ such that
F^ <x, y> -b-> F xy
G* xy -b-> G <x, y>

show that there are T-curry,T-uncurry∈Λ such that
+ 即求一個同構變換
T-uncurry F -b-> F^
T-curry G -b-> G*

SHOW:

F xy
<-sb- (λf.fxy)F
=a= <x, y> F
<-sb- (λp.pF) <x, y>
so:
F^ :== (λp.pF)
so:
T-uncurry :== (λf.(λp.pf)) =sugar= (λfp.pf)

G <x, y>
=a= G(λf.fxy)
<-sb- (λxy.G(λf.fxy)) xy
so:
G* :== (λxy.G(λf.fxy))
so:
T-curry :== (λg.(λxy.g(λf.fxy))) =sugar= (λgxy.g(λf.fxy))

END-OF-THE-SHOW.

##### (c)

check whether
+ 即驗證同構變換的性質
T-uncurry (T-curry f) -b-> f
T-curry (T-uncurry f) -b-> f

SHOW:

T-curry (T-uncurry f)
=a= (λgxy.g(λf.fxy)) ((λfp.pf) f)
-sb-> (λgxy.g(λf.fxy)) (λp.pf)
-sb-> λxy.(λp.pf)(λf'.f'xy)
-sb-> λxy.(λf'.f'xy)f
-sb-> λxy.fxy
就作用於MN而言f與λxy.fxy相同
=η= f
但是T-uncurry (T-curry f) -b-> f其實是不成立的

T-uncurry (T-curry f)
=a= (λfp.pf) ((λg.(λxy.g(λf.fxy))) f)
-sb-> λp.p((λgxy.g(λf.fxy)) f)
-sb-> λp.p(λxy.f(λf'.f'xy))
就作用與<M,N>而言f與λp.p(λxy.f(λf'.f'xy))相同
因爲:
λp.p(λxy.f(λf'.f'xy)) <M,N>
-sb-> (λc.cMN) (λxy.f(λf'.f'xy))
-sb-> (λxy.f(λf'.f'xy))MN
-sb-> f(λf'.f'MN)
=a= f <M,N>
但是T-curry (T-uncurry f) -b-> f其實是不成立的

END-OF-THE-SHOW.

從這個習題可以看出類型系統的必要性
沒有類型系統就不能良好地去描述同構於同態

# minimal logic

## note

- implicational logic
  i.e. only connective is ->

- intuitionistic
  not classical
  即 推導不出下面的節點
  ͱ ((a->b)->a)->a
  + 它的語義是什麼?
    爲什麼推到不出來它就是非經典邏輯

## grammar of formulas

A ::= a | (A -> A)

- 與"類型"的集合的歸納定義完全相同

## rewrite-rules

### implication introduction

```
Γ, A ͱ B
----------
Γ ͱ A -> B
```

### implication elimination (modus ponens)

```
Γ ͱ A
Γ ͱ A -> B
----------
Γ ͱ B
```

### example


"if a then it holds that if b then a"
"a implies that b implies a"
a -> b -> a
== a -> (b -> a)

```
a, b ͱ a
------------
a ͱ b -> a
-----------------
ͱ a -> (b -> a)
```

in type theory:

```
x:a, y:b ͱ x:a
----------------------------
x:a ͱ (λ y:b . x) : b -> a
-----------------------------------------
ͱ (λ x:a . (λ y:b . x)) : a -> b -> a
```

# styles of logic

## note

所有這些所謂邏輯風格都應該能簡潔地在grap中實現
因爲它們都是不過是對無窮有向圖的惰性求值而已

## logic style 1: Hilbert system

### just one proof rule: modus ponens (MP)

```
Γ ͱ A
Γ ͱ A -> B
----------
Γ ͱ B
```

### axiom schemes

K : A -> B -> A
S : (A -> B -> C) -> (A -> B) -> A -> C

example: proof of a -> a

|---|-------------------------------------------------|--------|
| 1 | (a -> (b -> a) -> a) -> (a -> b -> a) -> a -> a | S      |
| 2 | a -> (b -> a) -> a                              | K      |
| 3 | (a -> b -> a) -> a -> a                         | MP 1,2 |
| 4 | a->b->a                                         | K      |
| 5 | a->a                                            | MP 3,4 |
|---|-------------------------------------------------|--------|

所以I也是可以被推到出來的:SKK =b= I

### Curry-Howard for Hilbert system

|-------------------|------|---------------------------|
| logic             | <==> | type theory               |
|-------------------|------|---------------------------|
| *Hilbert system*  | <==> | *typed combinatory logic* |
|-------------------|------|---------------------------|
| proof of a -> a   | <==> | SKK =b= I                 |
|-------------------|------|---------------------------|
| deduction theorem | <==> | converting lambda exps    |
|                   |      | to combinatory logic      |
|-------------------|------|---------------------------|

## logic style 2: sequent calculus

### notations

sequents:
A1, ..., An ͱ B1, ..., Bm

to be read as:
A1 ∧ ... ∧ An ͱ B1 ∨ ... ∨ Bm

A1, ..., An and B1, ..., Bn are sets, not lists

### intro/elim versus left/right

for each logical connective *:
- natural deduction:
  intro rules *I (introduction)
  elim rules  *E (elimination)
- sequent calculus:
  left rules  *L
  right rules *R

### rewrite-rules

assumption rule

```
------------- ass
Γ, A ͱ A, Δ
```

left rule for implication

```
Γ ͱ A, Δ
Γ, B ͱ Δ
---------------- ->L
Γ, A -> B ͱ Δ
```

right rule for implication

```
Γ, A ͱ B, Δ
---------------- ->R
Γ ͱ A -> B, Δ
```

### example: proof of a -> b -> a

```
---------- ass
a, b ͱ a
---------- ->R
a ͱ b -> a
---------- ->R
ͱ a -> b -> a
```

### cuts

cut rule

```
Γ ͱ Δ, A    A, Γ ͱ Δ
--------------------- cut
Γ ͱ Δ
```

但是語義上不是Γ ͱ Δ, A要強過Γ ͱ Δ嗎
這是因爲
如果沒有cut就沒有消去規則了
所以這樣的話
在證明過程中就只能是讓節點的大小遞增
也就是一種不能回頭的證明
而下面的定理說明這種不能回頭的證明總存在

cut elimination theorem:
all provable statements can also be proved with a cut-free proof

## logic style 3a: natural deduction, Gentzen-style

### rewrite-rules

assumption rule

```
A ∈ Γ
--------- ass
Γ ͱ A
```

implication introduction

```
Γ, A ͱ B
---------------- ->I
Γ ͱ A -> B
```

implication elimination

```
Γ ͱ A -> B
Γ ͱ A
----------------- ->E
Γ ͱ B
```

### example: proof of a -> b -> a

```
---------- ass
a, b ͱ a
------------- ->I
a ͱ b -> a
--------------- ->I
ͱ a -> b -> a
```

與前面的minimal logic完全相同

### intro/elim versus left/right, revisited

natural deduction: introduction and elimination rules

```
... ͱ ...
----------------- *I
... ͱ  ... * ...

... ͱ ... * ...
----------------- *E
... ͱ ...
```

sequent calculus: left and right rules

```
... ͱ ...
--------------- *L
... * ... ͱ ...

... ͱ ...
---------------- *R
... ͱ ... * ...
```

反正都是要把東西弄到右下角
"ͱ" 與 "------------"
其實在兩個不同的語法層次
提供着相似的語義
所以在那種語法層次中推進證明都是沒有關係的

TODO 這帶來了什麼啓示呢?
我可不可以設計形式語言把不同的語法層次對稱化???

### TODO 是否可以列舉出分別易於在intro/elim和left/right中理解的推理的例子

畢竟這些風格被某些人採用正是因爲它們可以方便的用來表達出易於理解的推理過程

### Curry-Howard for natural deduction

在例子中體會:

```
---------- ass
a, b ͱ a
------------- ->I
a ͱ b -> a
--------------- ->I
ͱ a -> b -> a
```

```
----------------
x:a, y:b ͱ x:a
----------------------------
x:a ͱ (λ y:b . x) : b -> a
----------------------------------------
ͱ (λ x:a . (λ y:b . x)) : a -> b -> a
```

與前面的minimal logic中所作的對比完全相同

## TODO logic style 3b: natural deduction, Jaskowsky/Fitch-style

這種推理風格的發明
是爲了探索"ͱ"的語義

TODO 即當以以"ͱ"爲核心的表達式爲有向圖的節點時
對那些推理規則所形成的節點之間的關係的直觀理解是什麼???

這裏使用了更高維數的模型來理解這一切

# 無類型的 λ

## 術語

1. formal-language ==
   形式語言 == 形式理論
2. exp == expression == formula ==
   表達式
3. grammar == grammar-formula == BNF ==
   形式語法 == 形式語法公式
   + 用來歸納定義一個字符串的子集
4. 等詞
   每個等詞都應該與某個類型的數據相聯繫
   否則會引起很多歧義
   + 這裏可以看出類型論的一些合理性
     因爲在類型論中對等號的使用正是如此
5. 等號
   等號是被濫用最多的數學符號
   在定義形式語言的時候一定要小心使用
   等號的基本語義
   1. 賦值
   2. 基本等詞
   3. 等價關係
6. priori == 先驗的
   在每個理論中 基本等詞 將是唯一的先驗的概念

## 對表達式的集合的遞歸定義

1. inductive definition of the set of exp
   或者說是 歸納定義
   這是爲了獲得形式邏輯的基本研究對象
2. 首先
   入我所述
   集合 和 基本等詞 之間的關係是密切的
   這在於
   樸素的集合的唯一特性就是
   你能夠區分其中的元素
   [因而你可以約定集合中沒有重複的元素]
   而這種區分就在於基本等詞
   而形式邏輯中
   我必須先有一些能夠被我操作的符號
   這些符號
   所具有的唯一重要的特性就是
   你能夠區分兩個符號的不同
   而這種區分就在於基本等詞
   總上所述
   基本等詞纔是重要的
   並不必執着於 用形式邏輯來構造集合論
   也不必執着於 用集合論來構造形式邏輯
   所以
   在這裏我弱化 集合論 還有 形式邏輯
   而強化 基本等詞 和 可操作性[可計算性][[用程序的]可實現性]
3. 然而弱化這些概念的同時
   我有自由地使用這兩個概念
4. 我用圖來形成基本數據結構的方式
   與這裏用字符串的方式有根本的不同
   而這裏的方法還是有意義的
   因爲代碼[目前]必須是線性的
5. var ::= v | var'
   exp ::= var | (exp exp) | (λ var . exp)
   the set of all exps is denoted by Λ
6. 其中括號的使用是爲了形成樹的語義
   由遞歸定義
   一個exp被理解爲一個有根的樹
   + 樹可因根的選取而獲得一個定向
   這個有向樹中只有葉節點是被標記了的
   用來標記葉節點的是 var 或者是字符 "λ" 或 "."
7. 用字符串來實現一個形式語言時
   字符串之間的相等就是基本等詞
   即 給出兩個字符串時
   人們可以區分它們是相同還是不同
   + 這是一個先驗的假設
     但是在計算機上的可實現性增強了這個假設的合理性
     先驗假設的合理性 == 假設中的概念在機器上的可實現性
   這樣就得到了就字符串而言的基本的等詞
   這個等詞記爲 "=="
8. 等詞永遠根所使用的模型有關

## 推理規則與推理樹

1. 我忘了這一節的筆記來源於哪裏了
   可能是我自己想要用推理樹來理解上一節的歸納定義
2. exp 於 exp 之間有引入關係
   比如 M, N 引入 (M N)
   - 這可以被理解爲 數據結構 的 構造子
   所以也許可以用推理規則來描述這個歸納定義
   用推理規則來描述歸納定義也許具有更大的普遍性
3. 也就是說
   我統一把這種 "類後繼關係" 用 inference-rules 來描述
   這樣在用圖論對整個理論的想像之中
   因爲形成了統一的理解方式
   所以一切都變得簡單了
4.
   ----- (∈var axiom)
   v∈var
   這裏v是真正的字符
5.
   x∈var
   ------------ (∈var introduction)
   x'∈var
   這裏x是字符串的模式匹配中的pattern-variable
   '是真正的字符
6.
   x∈var
   ----------- (∈exp axiom)
   x∈exp
7.
   M∈exp
   N∈exp
   ------------- (∈exp introduction1)
   (M N) ∈exp
8.
   x∈var
   M∈exp
   ------------------- (∈exp introduction2)
   (λ x . M) ∈exp
9. 此時
   顯然可以看出 exp 和 exp 之間的引入關係
   可以形成一個以 Λ 中的元素爲節點
   以 (∈exp introduction1) 於 (∈exp introduction2) 爲有向邊
   的無窮有向樹
10. 可以記爲
    inductive-definition-tree(Λ;(∈exp introduction1),(∈exp introduction2))
    ID-Tree(Λ; ∈exp-intro1, ∈exp-intro2)
    indude-tree(Λ; ∈exp-intro1, ∈exp-intro2)

## TODO 不迂腐的描述

1. 上面兩節的描述現在看來都顯得迂腐
   考慮如何在一個程序語言中實現 lambda-calculus
   那麼上面的討論就都能夠很容易地被具體化了
2. 在使用歸納定義的時候
   就能形成一個集合
   並且這些集合中的點能夠組成一個有向圖
   在這個有向圖中有向邊就對應與歸納定義中的各個規則
3. 在程序語言中有兩方面
   1. 讀入的字符串
   2. 語法解析字符串而形成內存中的鏈表
4. 比如
   ``` cicada
   define-type <lambda-exp>
     * <symbol>
     * λ <symbol> : <lambda-exp>
     * <lambda-exp> . <lambda-exp>
   ```
5. 而上面的定義還是不過是初始化一個 <lambda-exp> 的時候
   所使用的表達式而已
   還沒有設計到機器接受到這個表達式的時候
   如何把這個表達式內化爲內存中的一個數據結構
6. 或者
   ``` cicada
   define-type <lambda-function>
     * λ <symbol> : <lambda-body>

   define-type <lambda-body>
     * <symbol>
     * <lambda-function>
     * <lambda-body> . <lambda-body> (* 函數作用 *)
     * <lambda-body> <lambda-body>   (* 函數複合 *)
   ```

## 代入

1. substitution
   這是一個基本的對表達式的集合 Λ
   中的元素的處理
   | M       | M[x:=N]            |
   |---------|--------------------|
   | x       | N                  |
   | y(=/=x) | y                  |
   | PQ      | (P[x:=N])(Q[x:=N]) |
   | λx.P   | λx.P              |
   | λy.P   | λy.(P[x:=N])      |

2. 最後一個操作其實就是
   在程序語言中所實現的 lambda-calculus 中的 apply

3. 迂腐之處在於
   其實這個 substitution 就是單純的爲了說明
   lambda-抽象 與 lambda-作用 而定義的

## 對約束變元與自由變元的計數

1. the set of free (resp. bound) variables of M
   notation FV(M) (resp. BV(M))
2. FV 與 BV 都是基本的表達式的集合 Λ 上的函數
3. FV
   FV(x) := {x}
   FV(MN) := FV(M)∪FV(N)
   FV(λx.M) := FV(M)\{x}
4. BV
   BV(x) := ∅
   BV(MN) := BV(M)∪BV(N)
   BV(λx.M) := BV(M)∪{x}
5. 把上面的兩個函數理解爲程序中的函數就好了
   這個函數的類型是 Λ --> number

## =a= [從線性表達式到有向圖]

### =a= introduction

1. a-conversion
2. 公式 λx.M =a= λy.M[y:=x]
   - y 不在 M 中自由出現
   其實是在描述一個 pattern-matching
   而我用下面的對實現而言更直接的定義
3.
   N == M[y:=x]
   M == N[x:=y]
   -------------------- =a= introduction
   λx.M =a= λy.N
4. 我把這個等價關係理解爲
   當把線性的對 lambda-function 的表達處理爲有向圖之後
   約束變元的名字就不重要了
   這個等詞可以被理解爲
   <lambda-function> 這個數據類型中的基本等詞
5. 可以發現兩種形成理論的風格
   我傾向於把 =a= 理解爲一個具體的用程序實現的謂詞
   而證明論者 可能傾向於把
   對 A =a= B 的肯定
   [我理解爲這個謂詞的值是 true]
   理解爲對一個新的表達式的引入
6. 後者徒增層次 無甚必要

### 例子

1. 這個早期的例子是我還不怎麼會編程的時候引入的
   當我在蟬語中實現過一次 lambda-calculus 了之後
   這個例子就非常乏味了
2. 可以看出
   在以具體的方式 實現某個形式理論的過程中
   所獲得的對這個理論的認識是非常具體而清晰的
3. an affirmation[allegation][assertion]
   of the following proposition
   λt.tzt =a= λs.szs
4. λx.M pattern-matching λt.tzt
   1. λ == λ, so
      the pattern is matched successfully
   2. pattern-vars binding:
      x -- t
      M -- tzt
5. λy.N pattern-matching λs.szs
   1. λ == λ, so
      the pattern is matched successfully
   2. pattern-vars binding:
      y -- s
      N -- szs
6. performing M[x:=y]
   where:
   M -- tzt
   x -- t
   y -- s
   so performing tzt[t:=s]
   we get: szs
   so N == M[y:=x]
7. performing N[x:=y]
   where:
   N -- szs
   x -- t
   y -- s
   so performing szs[s:=t]
   we get: tzt
   so M == N[x:=y]
8. so λt.tzt =a= λs.szs

### 相容性

1. compatibility
2. 就與 Λ 的歸納定義的相容性擴展
3.
   M =a= N
   ------------
   ML =a= NL
   LM =a= LN
   λv.M =a= λv.N
4. 這是說 <lambda-function> 上的基本等詞
   可以被 推廣到 <lambda-body> 上

### 記

1. 這一小節也是早期的不成熟的筆記
2. 變元只是語法對象 它們的意義只在於
   人們想要利用這些語法對象來說明一些語義
3. 語言是爲了表達
4. =a= 這個等價關係使得表達式的集合 Λ 獲得了有向圖的語義
   也就是說[線性結構上的]等價關係的引入
   可以被看成是[非線性結構的]對語義的引入
   - 就像一個點之間的等價關係
     可以把 樹 編程 有圈的圖 一樣
   或者說
   在這裏我可以進行一次模型的轉換
   去直接用有向圖來實現我想要定義的形式語言
   這時 Λ 就是有向圖的集合而不是字符串的集合
   逃離 "線性的字符串" 這種概唸的限制而使用圖論
5. 然而要明白
   "線性的字符串" 的概唸的重要價值是
   作爲輸入需要被解釋的表達式的工具
   即 作爲人控制機器的方式
   "線性的字符串" 與 機器對形式理論的實現 之間是有區別的
6. 用有向圖來實現一個形式語言時
   有向圖之間的某種相等就是基本等詞
   - 可以想象有向圖之間可以定義各種的相等
   給出兩個有向圖時
   人們可以區分它們是相同還是不同
   - 同樣這也是一個先驗的假設
     但是在計算機上的可實現性增強了這個假設的合理性
     先驗假設的合理性 == 假設中的概念在機器上的可實現性
   這樣就得到了就有向圖而言的基本的等詞
   這個等詞記爲 "==" 或 "=a="
   但是要知道此時 "=a=" 的意義已經完全變了
   它不再作爲一個等價關係而被別的基本等詞來引入
   而是它本身就是一個基本等詞
7. 也就是說
   通過轉換模型
   等價關係 可以變爲 基本等詞
8. 對於所有在我希望定義的形式語言裏要使用到的 exp
   我也可以形成歸納定義
   需要更改的是上面的 exp ::= (λ var . exp) 這一項
   但是因爲所定義的表達式的幾何性質
   此時已經不能用一般的形式語法公式來作歸納定義了
   因爲這些定義之中還設計到對有向圖的操作
   而不是隻涉及到對字符串的簡單操作
9. 也就是說
   傳統的 BNF
   對於遞歸定義數據類型來說
   是不充分的
   比如上面對 <lambda-function> 這個數據類型的定義中
   λ <symbol> : <lambda-body>
   這個 字符串 或者說 symbol 的列表
   並不是最終的數據類型
   它還必須被解析並處理而內化於內存之後
   [變成有向圖之後]
   才能被認爲是一個屬於這個數據類型的數據
10. 如果還用老方式來試着描述構造子的話
    x∈var
    M∈exp
    ------------------- (∈exp introduction2)
    (λ . M[free:x := (* --> λ)]) ∈exp
    這裏M已經是有向圖了
    而它後面的方括號中描述了一個對它的操作
    把x在M中的所有自由出現換成無名節點
    然後把這些無名節點全部都連接到最前面的λ點

## 變元約定 [無奈的技術處理]

1. variable convention
2. assume that the bound and free variables
   in a situation differ
3. 即 假設所有的變元名字都不同
   這樣就避免了對 約束變元 有效範圍的計算
4. 當我把有向圖作爲公式來實現我的形式語言的時候
   我其實根本不需要這個技術性處理
5. 在用字符串來實現 λ-cal 這個形式語言時
   在一個 exp 中如果需要的話總是重命名約束變元
   使得它們不與 exp 中出現的其它任何自由變元相同
   這樣在進行 substitution 的時候就不用考慮那麼多了
6. 缺點是一個 lambda-function 被機器處理之後
   就面目全非了 [可讀性非常弱了]

## 不含有自由變元的 lambda-function [combinator]

1. closed λ-exp
2. M is called closed λ-exp (combinator)
   if FV(M) == ∅
   the set of closed λ-exps is denoted by Λ°

## "λ-cal as a formal theory of equations between λ-exps"

1. "λ-cal as a formal theory of equations between λ-exps"
   "λ-cal 是一個關於 λ-exps 所組成的方程的形式理論"
   這句話是什麼意思
2. 解方程的重點就是熟悉恆等變換
   而 恆等變換 就在於 運算律
3. "λ-cal as algebra-structure"
   除非就 函數複合 優化語法
   而把 函數作用 視爲一個次要的東西
   因爲 函數作用 被視爲二元運算的時候
   甚至沒有結合律
   - 把 函數作用 當成運算的時候
     幾乎沒有任何運算律可言
4. 有趣的是
   當運算升級的時候
   這裏根本就不必擴充基本集
   所有的函數方程還有函數方程的解都可以在其內表示
5. 不必 就 函數方程的節來擴充集合 [積極的]
   不能 就 函數方程的節來擴充集合 [消極的]
6. 最簡單是是 不動點方程
   FX =b= X
   這個方程描述了
   X 是 F 的不動點
   這種二元關係
7. 我想正式 不動點方程 的有趣性質
   使得 作者說出
   "λ-cal as a formal theory of equations between λ-exps"
   這句話的
8. 但是
   其實稍微把這句話變一下
   對於一般的數學結構就也適用了

## -sb-> [有向邊]

1. 就是單步的 apply
   這還不同於一個 lambda-function 的作用
   因爲 後者不是單步的
2. -sb-> introduction
   b-step-reduction
   - 共軛的有 <-sb-
3. 也就是說公式 (λx.M)N -sb-> M[x:=N]
   其實是在描述一個 pattern-matching
   而我用下面的對實現而言更直接的定義
4. M* =a= M[x:=N]
   -------------------- -sb-> introduction
   (λx.M)N -sb-> M*
5. 青澀時期的例子
   an affirmation(allegation)(assertion)
   of the following proposition
   λx.F(xx) λx.F(xx) -sb-> F(λx.F(xx) λx.F(xx))
6. (λx.M)N pattern-matching λx.F(xx) λx.F(xx)
   1. λ == λ, so
      the pattern is matched successfully
   2. pattern-vars binding:
      x -- x
      M -- F(xx)
      N -- λx.F(xx)
7. performing M[x:=N]
   where:
   x -- x
   M -- F(xx)
   N -- λx.F(xx)
   we get F(λx.F(xx) λx.F(xx))
8. F(λx.F(xx) λx.F(xx)) =a= F(λx.F(xx) λx.F(xx))
   so
   λx.F(xx) λx.F(xx) =b= F(λx.F(xx) λx.F(xx))

### compatibility

就與Λ的歸納定義的相容性擴展

```
M -sb-> N
------------
ML -sb-> NL
LM -sb-> LN
λv.M -sb-> λv.N
```

## graph of exps and -sb->

1. 如果使用字符串來是實現形式語言的話
   一切都是字符串
   "(λx.M)N -sb-> M*" 其實也只不過是字符串
   只不過比表達式的集合作爲字符串的集合高了一個層次
   並且對這個更高層次的字符串的集合的歸納定義也變得複雜了
2. 然而更好的理解方式是於形成更高層次的語義
   - 比如上面對 =a= 所作的
   -sb-> 是一個類似於後繼關係的二元關係
   這裏以 -sb-> 爲有向邊
   顯然能形成以 Λ 中的元素爲節點的有向圖
   記爲 graph (Λ, -sb->)
3. 在計算機中實現一個能作 λ-cal 的語言
   其實就是實現 graph (Λ, -sb->) 的 lazy-eval
   - graph (Λ, -sb->) 是一個無窮圖所以需要 lazy-eval
   每次計算其實就是給出求出局部的 graph (Λ, -sb->)
   只不過這裏的局部是極端的
   即 只有圖中的一個點
   可以說 λ-cal 的理論
   就是對這個無窮有向圖的局部 lazy-eval
4. 而證明 λ-cal 中的定理
   就是去證明這個無窮有向圖的性質
   比如下面的 church-rosser theorem 所作的
5. 另外
   在機器輔助證明系統中
   比如在 coq 中
   tactics 就可以被看作是對無窮有向圖的惰性求值

## 新記

1. 用有向圖來表示 lambda-exp [或 lambda-function]
   就[幾乎]不用 =a= 了
2. 而 這個數據類型中的點
   在 -sb-> 這個後記關係下
   可以生成一個
   以 其點爲點
   而以 -sb-> 爲有向邊的有向圖
3. -b-> 是 -sb-> 生成的有向路
4. =b= 是 有向路所對應的無向路
5. 注意這種理解方式中
   有向圖有兩個層次
   大的有向圖中的每個點也是一個小的有向圖
6. 並且 -sb-> 這個關係[謂詞]是很容易實現的
7. 而 -b-> 和 =b= 卻不容易
   其前者 是在有向圖中尋找兩點之間的有向路
   其後者 是在有向圖中尋找兩點之間的無向路
8. 也許有好的方法存在的
   因爲竟這個有向圖是非常特殊的

## -b-> [有向路]

1. b-reduction
2. -sb-> 類似於後繼關係
   -b-> 是一個偏序關係
3. -sb-> 就傳遞性生成 -b->
   這種生成即是用有向路來代替有向邊
   因此 -b-> 這個二元關係定義爲:
   graph(Λ;-sb->)中的兩個節點之間是存在有向路
4. 以-sb->爲基礎
   M -sb-> N
   ----------
   M -b-> N
5. 添加自反性
   M -b-> M
6. 添加傳遞性
   M -b-> N
   N -b-> L
   ----------
   M -b-> L

## =b= [無向路]

1. b-conversion
2. -b-> 是一個偏序關係
   =b= 是等價關係
3. -b-> 就對稱性生成 =b=
   這種生成即是用無向路來代替有向路
   因此 =b= 這個二元關係定義爲:
   graph(Λ;-sb->)中的兩個節點之間是存在無向路
4. path is equality
   -sb->: 有向邊
   -b->: 有向路
   =b=: 無向路
   構造性的數學中
   數學結構都是歸納定義的
   等價關係自然地被定義爲歸納定義有向樹中的無向路
5. TODO 在 hott 中有的數學結構被稱爲是高階歸納定義的
   比如同倫類
   這是什麼意思???
6. 以-b->爲基礎:
   M -b-> N
   -----------
   M =b= N
7. 添加對稱性
   M =b= N
   ---------
   N =b= M
8. 還要再添加一次傳遞性
   M =b= N
   N =b= L
   ----------
   M =b= L
9. 爲什麼還要多添加一次傳遞性???
   下面錯誤的定義2說明了
   再次添加傳遞性的必要
10. DEFINE2 (錯誤的定義):
    - 1
      M -b-> N
      -----------
      M =b= N
    - 2
      M <-b- N
      -----------
      M =b= N
11. 上面這兩個推理規則代替了"或"這個詞
    這樣會有些不好嗎?
    這樣的定義能成嗎?
    能證明對稱性與傳遞性嗎?
    試驗如下:
    因爲
    首先
    M <-b- N
    -----------
    M =b= N
    等價於
    M -b-> N
    -----------
    N =b= M
    所以對稱性的事實是顯然的
    其次
    M =b= N
    N =b= L
    ----------
12. 有四種引入上面兩個節點的可能性
    我必須說明每種都能推出M =b= L
    1. M -b-> N
       N -b-> L
       ---------
       M -b-> L
       ---------
       M =b= L
    2. N -b-> M
       N -b-> L
       ---------
       此時已經不能推出
       M -b-> L或L -b-> M了
       因此不能推出M =b= L了
       所以這種定義是錯誤的
13. 直觀地看這也是很顯然的
    新的傳遞性來自於一個約定
    而不完全來自於原來的傳遞性
14. 在做形式定義的時候需要注意這一點

## 遲到的定義 [belated definitions]

1. compatible
2. 首先[用構造子]遞歸定義[不一定限於BNF]一個集合的時候
   這些構造子也成爲這個集合上的[特殊的]函數
   [正如ml這類語言所實現地那樣]
   而所謂的相容性就是 定義於這個集合的其他的函數
   和這些構造子所代表的函數之間的關係
3. 其實是兩種向圖之間的和諧關係
4. 集合 Λ 上的關係 -R- 被稱爲是與對集合 Λ 的歸納定義相容的
   如果:
   * 1
     M1 -R- N1    M2 -R- N2
     -----------------------
     (M1 M2) -R- (N1 N2)
      或
     M -R- N
     -----------------
     (M Z) -R- (N Z)
     (Z M) -R- (Z N)
   * 2
     M -R- N
     -------------------
     (λv.M) -R- (λv.N)
5. 如果用字符串來實現形式理論
   那麼 -R- 可以被實現爲對第二層次的字符串的集合的歸納定義
   也就是說對於形式理論來說
   引入一個關係其實是在歸納定義一個集合
6. 下面又是一些堂皇的術語
7. congruence relation
   Λ 上與 Λ 的歸納定義相容的關係 -R-
   如果是等價關係
   - 即 對稱 自反 傳遞
   則稱其爲全等關係 :congruence
   記爲 =R=
8. reduction relation
   Λ 上與 Λ 的歸納定義相容的關係
   如果是偏序關係
   - 即 自反 傳遞
   則稱其爲約化關係 :reduction
   記爲 <-R-
   因爲沒有對稱性
   所以共軛的有 -R->
   - 儘管 <-sb- 使用了類似的記號
     但是它並不是reduction relation
     但是考慮有向圖就知道使用<-sb-這種記號是合理的
     - TODO 或者我應該設計更好的記號來區分傳遞性???
9. b-redex
   (λx.M)N
   就是表達式中模式匹配到的 λ-abstraction 的 application
10. b-normal form (b-nf)
    即 在有向圖中的 某種 邊界點
    λ-exp which dose not have any b-redex
    as subexpression
    這顯然可以通過用模式匹配來寫一個
    表達式的集合上的謂詞來實現
11. M has a b-normal form
    if ∃N : M =b= N ∧ N is b-normal form

## TODO church-rosser theorem

### 目標

1. 實現了 <-b- =b= =a= x:A x∈A 這些關係以後
   就可以用謂詞演算所提供連接詞與量詞來形成
   關於形式理論的一般性質的命題
   比如這裏的這個命題就是如此
2. TODO 這個命題所依賴的推理規則屬於什麼形式語言??
   不論如何這個命題都不像某些命題那樣迂腐
   而是真正給了我們一些關於形式理論的知識
3. THEOREM:
   ∀M,N∈Λ s.t. M =b= N
   ∃L∈Λ : L <-b- M ∧ L <-b- N
4. 這個定理給出了否定 M =b= N 的一種算法
   即 把 M, N 都化爲 圖中的邊界點
   如果這兩個點不相等 那麼 就否定了上面的謂詞
5. 這個定理並沒有它看起來的那麼不平凡
   因爲
   用公式 (λx.M)N :== M[x:=N] 去定義 -sb->
   =b= 其實被定義爲 <-b- 的交換化
   即 <-b- 然後忽略方向
   即 <-b- 添加對稱性
   而 <-b- 被定義爲 <-sb- 從有向邊到有向路的生成
6. 對這個定理的證明就是對
   關係 -sb-> 所形成的有向圖 graph(Λ;-sb->) 的性質的觀察

### lemmas

1. if: M is b-nf
   then:
   M -b-> N
   ---------
   N =a= M
   b-nf 作爲有向圖的葉節點當然有這種性質
2. M -sb-> M'
   ------------
   M[x:=N] -sb-> M'[x:=N]
   就引入-sb->的推理規則作歸納證明即可

### 回到church-rosser theorem

∀M,N∈Λ s.t. M =b= N
∃L∈Λ : L <-b- M ∧ L <-b- N

即

M =b= N
----------
∃L :
L <-b- M
L <-b- N

這樣寫還是沒能脫離謂詞演算
不過好多了
因爲更接近實現方式了

加強其條件如下:
M <-b- Z
N <-b- Z
----------
∃L :
L <-b- M
L <-b- N

加強的條件中排除了=b=
+ 可以看出這是
  爲了證明命題
  而去在等價變換的前提下加強命題的條件
+ 因爲
  M <-b- Z
  N <-b- Z
  ----------
  M =b= N
  所以
  等價變換只需要考慮一個方向
  即是需要在假設:
  M <-b- Z
  N <-b- Z
  ----------
  ∃L :
  L <-b- M
  L <-b- N
  的正確性的前提下
  按引入=b=的推理規則來分類用歸納法證明:
  M =b= N
  ----------
  ∃L :
  L <-b- M
  L <-b- N

  PROOF:
  1. M -b-> N
     -----------
     M =b= N

     此時引入節點M =b= N的父節點是M -b-> N
     而M -b-> N中不包含"=b="
     所以這個推理規則被認爲是一個"基礎步驟"
     此時沒有歸納假設
     需要證明:
     ∃L :
     L <-b- M
     L <-b- N
     並且已經有假設成立的定理:
     M <-b- Z
     N <-b- Z
     ----------
     ∃L :
     L <-b- M
     L <-b- N
     了
     顯然此時只要取Z爲M就行了

  2. N =b= M
     ---------
     M =b= N

     此時引入節點M =b= N的父節點是N =b= M
     而N =b= M中包含了"=b="
     所以這個推理規則被認爲是一個"歸納推步"
     此時有歸納假設:
     N =b= M
     ----------
     ∃L :
     L <-b- N
     L <-b- M
     需要證明:
     ∃L :
     L <-b- M
     L <-b- N
     然後由量詞下面那兩項的交換性這是顯然的
     這並沒有用到已經假設成立了的定理:
     M <-b- Z
     N <-b- Z
     ----------
     ∃L :
     L <-b- M
     L <-b- N

  3. M =b= L
     L =b= N
     ----------
     M =b= N

     此時引入節點M =b= N的父節點是
     M =b= L和L =b= N
     而它們中都包含了"=b="
     所以這個推理規則被認爲是一個"歸納推步"
     此時有歸納假設:
     1. M =b= L
        ----------
        ∃P1 :
        P1 <-b- M
        P1 <-b- L

     2. L =b= N
        ----------
        ∃P2 :
        P2 <-b- L
        P2 <-b- N

        這兩個約束變元被下面當成
        對所取的一個元素的命名了
     需要證明:
     ∃P :
     P <-b- M
     P <-b- N
     使用已經假設成立了的定理:
     P1 <-b- L
     P2 <-b- L
     ----------
     ∃P :
     P <-b- P1
     P <-b- P2
     由<-b-的性質就知道此時存在的P
     就是
     ∃P :
     P <-b- M
     P <-b- N
     中所需要找的P

     EDN-OF-PROOF.

### 系

1. if: M has N a b-normal form
   i.e. M =b= N ∧ N is b-normal form
   then:
   M -b-> N

   這是非常好的性質
   它說明如果一個λ-exp有b-nf
   按一定的方式rewrite下去就一定能得到這個b-nf
   + 按壞的方式rewrite下去可能無限循環

   PROOF:

   M =b= N
   ---------
   ∃L :
   L <-b- M
   L <-b- N

   N is b-nf
   L <-b- N
   -----------
   L =a= N

   L <-b- M
   L =a= N
   ------------
   N <-b- M

   END-OF-PROOF.
2. a λ-exp has at most one b-nf

   PROOF:

   N1 <-b- M
   N2 <-b- M
   -------------------------
   ∃L :
   L <-b- N1
   L <-b- N2

   L <-b- N1
   L <-b- N2
   N1 is b-nf
   N2 is b-nf
   --------------
   N1 =a= N2

   END-OF-PROOF.

   這也是很好的性質
   它說明b-nf的唯一性

### 再次回到church-rosser theorem

現在就可以集中精力證下面的東西了

```
M <-b- Z
N <-b- Z
----------
∃L :
L <-b- M
L <-b- N
```

這是關係<-sb-所形成的有向圖的良好性質
它說分開的東西一定有可能被重新融合

### main lemma

這其實是想要表達一種對稱性
但是沒有表達好

M <-sb- Z
N <-b- Z
----------
∃L :
L <-b- M
L <-b- N

the way to proof this is similar to
"radioactive tracing isotopes"
in experimental biology

PROOF:

已知 M <-sb- Z 具體 rewrite 了那個 b-redex
又已知 N <-b- Z
通過 N <-b- Z 求的 其對邊 L <-b- M
+ 如果在模仿 N <-b- Z 求的 其對邊 L <-b- M 的過程中
  那個b-redex被消除了
  那麼我作模仿的時候就省略這步消除繼續模仿後面的
然後就發現能自然得到
M <-sb- Z 的對邊: L <-b- N

END-OF-PROOF.

這個證明其實是給出了尋找
L <-b- M
L <-b- N
的算法

### remark

1. church-rosser theorem
   所歸結到的那個有向圖的性質很像 如下的敘述
2. 我先定義自由的代數結構
   然後給出代數結構的圖論表示
   然後說明這個代數結構中的運算律如何對應於
   它的圖論表示的圖的性質
   也就是說這個性質很像是運算律

## fixed point theorem

1. ∀F∃X::FX==X
   對每一個函數F都可以構造出它的不動點X
   這當然可以被理解成一個方程了
   這樣的對這個方程的可解性的敘述
   形成了傳統數學形式語言中的一個命題
2. HEURSITIC
   X == FFFFF...(無窮個F作用於自身)
   這種東西顯然滿足所給出的方程
   F(X) == F(FFFFF...)
   == FFFFF... == X
   但是這種東西在語法上是不合法的
   因此我們定義 F*
   而 F* 的性質是
   作用於自身一次就能額外的在外面獲得一個 F
   也就是說: F* F* == F (F* F*)
   可以說F*對自身的作用就是在形成潛在的FFFFF...
3. DEFINE
   F* :== λx.F(xx)
   X :== F* F*
   然後驗證 X 就是所求的 exp
   X == F*F* == λx.F(xx) λx.F(xx)
   == F(λx.F(xx) λx.F(xx))
   == F(F*F*)
   == FX
4. Y combinator
   對上面的定理的解法的抽象
   就得到 Y combinator
   ∀F :: F(YF) == YF
   只要把exp: λx.F(xx) λx.F(xx)中的F
   用λ-abstraction抽象出來就行了:
   Y == λf.(λx.f(xx) λx.f(xx))
   == λfx.f(xx)(λx.f(xx))

## corollary

∀C == C[f,x] ∈Λ
+ 這個等式是對可能存在的依賴關係的明確聲明
  可見等號這個東西是被相當可怕地濫用了的
∃F∈Λ ∀X∈Λ : FX == C[F,X]
+ where C[F,X] == C[f:=F][x:=X]
現在爲了證明這裏給出的方程的可解性
需要構造的是F了 下面給出典型的解法:
+ 根據F所需要滿足的性質來向後計算
+ 熟練的對等式作恆等形變
+ 首先這個解法是以不動點方程的求解公式爲基礎的
  就像解某些代數方程時
  人們可以以二次方程的求根公式爲基礎
  將需要求解的方程轉化爲二次方程
∀X∈Λ : FX == C[F,X]
<== Fx == C[F,x]
<== F == λx.C[F,x]
<== F == (λf.λx.C[f,x])F
+ 上面這步已經化歸成功了
<== F == Y(λf.λx.C[f,x])

APPLICATION:
1. 求F 使得對任意X 有:
   FX == XF
   + 存在和任意X都交換的exp T
   此時: C[F,X] == XF
   再寫一遍求解過程作爲練習:
   ∀X : FX == XF
   <== Fx == xF
   <== F == λx.xF
   <== F == (λf.λx.xf)F
   <== F == Y(λf.λx.xf)
2. 求G 使得對任意X,Z 有:
   GXZ == ZG(ZXG)
   + 類似於前面那個一元的交換性
   此時: C[G,X,Z] == ZG(ZXG)
   再寫一遍求解過程作爲練習:
   ∀X,Z : (GX)Z == ZG(ZXG)
   <== (Gx)z == zG(zxG)
   <== G == λx.λz.zG(zxG)
   <== G == (λx.λz.zg(zxg))G
   <== G == Y(λx.λz.zg(zxg))

## 一個 λ-exp 所生成的子圖

一個 帶有很多λ作用的 [有待被計算的] λ-exp
可以形成這樣一個邊標記的有向圖
用來標記有向邊的是 一次λ作用
當然這個圖是 所有 λ-exp 所形成的有向圖的子圖

## strongly normalizing

1. 如果
   一個 λ-exp 所生成的子圖 是有窮的
   那麼這個 λ-exp 就被稱爲是 strongly normalizing
2. 一個 λ-exp 所生成的子圖 中的兩條路
   就是對 M 的兩種 rewrite 方式
   如 church-rosser theorem 的證明過程所展示的那樣
   兩條路可以用來交織成一個網
   所以每條路就像是有限圖的一個 "維度"
   但是兩條路所交織成的一個網
   的幾何結構
   又比平面上整數形成的晶格複雜的多
3. 有可能沿一個維度
   一個 λ-exp 所生成的子圖 是有限的
   [有限就代表 沿着這個方向 rewrite 可以找到邊界[b-nf]]
   而沿另一個維度它是無窮的
   所以 strongly normalizing
   就一定有 b-nf
   但是就算有 b-nf
   也不一定是 strongly normalizing
   必須 沿着各個方向
   都能走到一個邊界
   才是 strongly normalizing
4. leftmost reduction strategy is normalizing
   先消去函數位置的作用
   就能找到
   生成子圖有邊界的 exp 的
   生成子圖的邊界

## TODO bohm trees and approximation

1. bohm trees  is a kind of infinite normal form
2. lemma
   every exp matchs one of the following two patterns
   or another [可以就歸納定義證明]
   1. λ x1 ... xn . y N1 ... Nm
      + where n,m>=0
        pattern-var y matchs [free or non-free] var
        pattern-var Ni matchs exp
   2. λx1...xn.(λy.N0)N1...Nm
      + where n>=0, m>=1
3. 上面這個引理也是廢屁話
4. head normal form (hnf)
   M is called a head normal form
   if M matchs
   λx1...xn.yN1...Nm
   + where n,m>=0
   例:
   ac(bc) is a hnf
   bc is a hnf
   λabc.ac(bc) is a hnf
   and the variable matched by y
   is called the head variable of M
5. M has a hnf
   if M =b= N
   ∧ N is a hnf
6. head redex
   if M matchs
   λx1...xn.(λy.N0)N1...Nm
   + where n>=0, m>=1
   then (λy.N0)N1 is called the head redex of M
7. some property of tree
   - as partially ordered set
     即 可以定向
   - there is a root
   - each node(point,joint) has
     finitely many direct successors
   - the set of predecessors of a node
     is finite and is linearly ordered
8. bohm tree of exp M
   notation BT(M)
   BT(M) is well defined and
   M =b= N
   --------
   BT(M) = BT(N)

# TODO the system λ->curry (systems of type assigment)

## note about "ͱ"

推理規則定義表達式的集合
並且給出以這些表達式爲節點
以推理規則爲有向邊的無窮有向圖

上面的type-free λ-cal中的推理規則並不包含"ͱ"
而下面的推理規則包含"ͱ"

此時爲了好的理解還是要去形成圖論的語義
首先要知道
在逐層地構建一個形式語言時
圖的節點一次次地在變化
每次圖的節點都是上次的圖

下面的推理規則所給出的圖的節點是包含"ͱ"的表達式
type-free λ-cal中的推理規則
可以用來處理下面的表達式的"ͱ"的前半部分或後半部分
但是不觸及"ͱ"本身

設計一個以"ͱ"爲核心的語法層次
是爲了做局部的(臨時的)假設
+ 語法上
  假設寫在"ͱ"的前面
  作爲context的內容
也就是爲了在證明的過程中使用"反證法"和"歸納法"等技術

有一些推理規則只不過是在給命題作"恆等變形"
也許是在爲另一些推理規則的使用做準本
+ type-free λ-cal中不觸及"ͱ"的推理規則都是如此
  除此之外還有一些觸及"ͱ"的推理規則也是如此
而另一些推理規則才真正是在"推理"
這裏"恆等變形"值得深入討論
首先這個概念在代數化的看待問題上很有幫助
因爲恆等變形是解任何方程的手段
並且在這裏這個概念就更有意思
因爲有時恆等變形類的推理規則提供的是
同一個語義在不同語法層面上的轉化
明顯地提出這種轉化是否能爲理解這一切提供一個代數的視角??

增加假設 減少結論 會讓ͱ節點變弱
減少假設 增加結論 會讓ͱ節點變強
把ͱ寫成橫線"--------"或者"========"時
ͱ節點就像是分數一樣
可以形成一個正比例函數和一個反比例函數
從代數的角度看
數學論證就是在對這種ͱ節點作恆等變形
如果統一用有向圖來理解代數
那麼這種相似性就是自然的了

## inductive definition of the set of exp

tvar ::= α | tvar'
type ::= tvar | (tvar -> tvar)
the set of all types is denoted by T

var ::= v | var'
exp ::= var | (exp exp) | (λ var . exp)
the set of all exps is denoted by Λ

## notations

one writes
ͱ M:σ
to assign type σ∈T to exp M∈Λ

1. M is called a *subject*
2. M:σ is called a *statement*
3. as a special statement
   x:σ is called a *declaration*
4. a *basis* is a set of declaration
   with distinct variables as subject

"ͱ" pronounced "yields"
the thing on the left-side of ͱ
is called *context*
the thing on the right-side of ͱ
is called *conclusion*

## type assigment

### inference-rules

a statement M:σ is derivable from a basis Γ
denoted: Γ ͱ M:σ
if Γ ͱ M:σ can be inferenced by the following inference-rules:

+ where the basis Γ is locally presupposed

(x:σ)∈Γ
--------- (axiom)
Γ ͱ x:σ
+ 這個引入表明瞭類型指定可以在局部任意引進
  而下面的"->introduction"
  會把context中臨時引入的類型指定消除

Γ,x:σ ͱ M:τ
------------------- (->introduction)
Γ ͱ (λx.M):(σ->τ)
+ 假設(context)中的類型指定x:σ
  會在這個引入規則中被消除

Γ ͱ M:(σ->τ)
Γ ͱ N:σ
--------------- (->elimination)
Γ ͱ (MN):τ

### 如果從圖論的角度來理解這種類型系統

那麼節點是帶有"ͱ"的表達式更高一層次的表達式
+ 前面的type-free λ-cal中圖graph(Λ;-sb->)的節點
  可以看做是省略了"ͱ"的表達式
  因爲在那裏對推理規則的描述還不需要"ͱ"這個記號
基本的"類後繼關係"就來源於上面的兩個推理規則
這就像是在indude-tree(Λ; ∈exp-intro1, ∈exp-intro2)中的
所有變元節點上釋放一些相同或不同小精靈
沿着有向樹的有向邊旅行時
這些小精靈會按它們路過的有向邊的類型
而在道路的交匯之處於其它的小精靈按規則結成夥伴
+ 上面的推理規則並不能給所有的exp附上類型
  比如x:σ,y:τ並不能給出xy的類型
  這是因爲作者簡化了推理規則嗎?

### 如果我強調λ-abstraction中的約束變元的非本質性

即如果我用有向圖來描述λ-abstraction
那麼
Γ,x:σ ͱ M:τ
------------------- (->introduction)
Γ ͱ (λx.M):(σ->τ)

就需要被改寫爲:
Γ ͱ M:τ
x∈var
σ∈tvar
------------------------------------------ (->introduction)
Γ ͱ (λ:σ.M[free:x := (* --> λ)]) : (σ->τ)
+ 也就是說釋放小精靈的過程也是非本質的
  或者說type assigment的性質基本上就是
  indude-tree(Λ;(∈exp introduction1),(∈exp introduction2))
  的性質
上面的改寫其實是不對的
因爲它丟失了ͱ的語義

### examples

1. ͱ (λx.x):(σ->σ)

   ----------- (axiom)
   x:σ ͱ x:σ
   ---------------- (->introduction)
   ͱ (λx.x):(σ->σ)

2. y:σ ͱ (λx.x)y:σ

   ----------- (axiom)
   x:σ ͱ x:σ
   ---------------- (->introduction)
   ͱ (λx.x):(σ->σ)


   --------------------------------- (axiom)
   y:σ ͱ y:σ

   y:σ ͱ (λx.x):(σ->σ)
   y:σ ͱ y:σ
   --------------------------------- (->elimination)
   y:σ ͱ (λx.x)y:σ

### 這裏我就是在手動實現一個機器輔助證明系統了

都是關於推導規則的
推導規則使用就相當於是在惰性求值一個有向圖
而且 特殊地 這裏的有向圖是一棵有向樹(推理樹)

在實現一個機器輔助證明系統時
在需要的時候一個人可以用show函數之類的東西
打印出易於閱讀的現在的樹是什麼
人機的交流過程 就是單純的對這個樹的操作
推理樹以及其它的底層東西的存儲就是實現者需要設計的了

### 用對又向圖的操作來理解我使用這個輔助證明系統的方式過程

#### 首先要知道節點是什麼

節點是被"ͱ"連接的東西
"ͱ"的前面是第二層次的若干個表達式的列表
"ͱ"的後面是一個第二層次的表達式

因此節點是第三層次的表達式

要知道"ͱ"的使用是爲了使用複雜的證明技術

#### 基本操作有兩個

1. 引入新節點

2. 根據推理規則i
   從已有的若干個節點:x1,x2,...(在任何位置都行)
   構造一個新的節點:y
   在圖中增添有向邊:
   x1-->y, x2-->y, ...
   也許所有這些有向邊還應該用
   推理規則i的這次使用標記
   + 標記 即是 邊到推理規則的某此使用的映射
   + 注意:
     同一個推理規則的每次使用都是需要被區分的

3. 其實是
   一個無窮的有向圖已經在哪裏了
   我只需要去惰性求值出它的一部分

#### 這樣對每個形式理論的學習就是

1. 對每個具體的推理規則的實現方式的學習
2. 對每個具體的引入節點的規則的實現方式的學習
   比如按規則對
   λx.F(xx) λx.F(xx) =b= F(λx.F(xx) λx.F(xx))
   的肯定
   就是一次對節點的引入

3. 還有對更底層的東西學習
   比如是用:
   + 線性的字符串
   + 有限有向圖
   + 其它的更複雜的數據結構
   來作爲最基本的(或者是某個層次的)表達式

#### 一些哲學話題

1. 什麼數據結構是被形式主義所允許的呢?
   如果只是"線性的字符串"那就太狹隘了
   我想"有限有向圖"是不錯的基礎
   就用機器來實現形式理論而言
   它們二者的基本性是相當的
   對它們的使用
   都可以被理解爲是一個小孩在拿一些積木
   自顧自地做遊戲

2. 應該如何認識一個人可以在這裏所作的選擇呢?
   對於人機的互動而言
   "線性的字符串"是不可避免的
   或者說只有"線性的字符串"纔是方便的
   但是就人類的理解而言它們又是不方便的

3. 可能對於人了理解來說三維的連續模型是最容易理解的
   對於四維模型人類就無法形成恰當的想像了
   而在一二三維中維數越高越好
   而對於機器來說一維的離散模型是最容易實現的

## TODO pragmatics of constants

這個標題讓人想到關於命名系統的問題
除了單純地用λ-abstraction寫出函數然後進行作用之外
一個良好的命名系統對於實現具有可用性的
機器輔助證明系統來說是非常重要的
因此對形式理論來說也是重要的
+ 這裏可以發現一個要點
  那就是
  對於一個邏輯學的研究者或者學習者而言
  時刻把實現一個機器輔助證明系統
  當做目標
  是非常有宜的
  這個機器輔助證明系統也許並不是研究的終極目的
  但是對"實現"的熱切渴望
  使得人對理論的理解上容不得半點兒的馬虎
  這當然是好的
  一個明確的目標把人變勤奮了
但是作者想要在這個標題下介紹的是這個問題嗎?

其實
constants的使用在於在不違背形式主義精神的前提下
增加第一層次表達式的可讀性
否則的話
var只能是:
v
v'
v"
v"'
v""
這些東西被當做基本的"字符"了
但是顯然用這些字符構造起來的表達式沒有什麼可讀性
constants的使用就是說
人可以隨時以具有較強可讀性的方式引入新的基本"字符"
比如:
+ type-constant:
  nat
+ exp-constant:
  0:nat
  suc:(nat->nat)
這些常量不能作爲約束變元在推理規則中使用
使用了其實也沒關係
因爲反正它們都變成約束變元了 只有局部意義
但是這樣會減弱可讀性 這就與引入常量的初衷相悖了

"ML is essentially λ->curry
extended with a constant Y
and type-assignment Y:((σ->σ)->σ) for all σ"

## TODO λ->curry的性質

### 首先定義一個作用於basis的函數:dom

let basis Γ :== {x1:σ1,x2:σ2,...,xn:σn}
+ 也就是說使用記號:==來做命名
  關於命名系統還需要更細緻地討論
DEFINE:
dom(Γ) == {x1,x2,...,xn}
+ 上面這個定義被理解爲對字符串
  (更好地應該說 字符串的列表)的操作
  就像substitution這種東西差不多
  但是這裏顯然是在濫用等號了
DEFINE:
Γ(xi) == σi
Γ as a partial function
這裏類型已經亂了
應該定義
proj(Γ xi) == σi
而把Γ(xi)當做proj(Γ xi)的語法糖
+ 但是不理論如何
  這裏都和上面的函數一樣涉及到了濫用等號的問題
let V0 be a set of variables
DEFINE:
Γ↾V0 == {x:σ | x∈V0 ∧ σ == Γ(x)}
這個東西類似於proj(Γ xi)在
第二個自變元的集合上的重載
但是又有區別
+ 如何形式化地理解這裏這些亂七八雜的定義
DEFINE:
σ[α:=τ]
也就是把文本中出現的類型也定義substitution
這就又真真正正是一個字符串的操作了

### basis lemma for λ->curry

let Γ be a basis
1. if:
   Γ'⊃Γ
   then:
   Γ ͱ M:σ
   --------
   Γ' ͱ M:σ

   這是推理規則嗎?
   如果是 那麼推理規則就不光可以被先驗地引進
   並且還可以被證明 !!!
   所以這一定不是推理規則吧 !!!
   一定不是
   因爲這裏涉及到了集合論裏的東西
   如何排除這些東西呢?
   排除了它們之後還能作證明嗎?
   不排除它們的時候證明是如何被完成的呢?
   我想純粹的形式主義所構建的形式體系
   至少一定是在集合論的語法之外的
   因爲集合論的語法是
   一階邏輯這種形式語言中的一個形式理論
   既然那裏(構建集合論的形式理論時)可以排除集合論的語法
   那麼這裏爲什麼不行呢?
   畢竟如果不排除它們的話類型就又亂了
   即 我所使用的各種形式語言就交織在一起了
   這也許是合法的
   但是這對於理解而言是不宜的

   也許排除集合論的概唸的方式就是把這裏的集合
   理解爲表達式的有序列表(或許應該遺忘列表的有序性)
   這個列表被維持成其中的表達式不相互重複的狀態

2. Γ ͱ M:σ
   --------
   FV(M)⊂dom(Γ)

   這個引理是關於
   對自由變元的類型指定的引入的

3. Γ ͱ M:σ
   --------
   Γ↾FV(M) ͱ M:σ

   這個引理是關於
   在context中消除對
   引入自由變元的類型聲明沒有貢獻的項的

PROOF:
by induction on the derivation of M:σ
也就是就推理規則對Γ ͱ x:σ之類的斷言的引入方式
+ 即 引入這第三層次表達式的方式
  或推理規則對這第三層次的表達式的歸納定義

(x:σ)∈Γ
--------- (axiom)
Γ ͱ x:σ

Γ,x:σ ͱ M:τ
------------------- (->introduction)
Γ ͱ (λx.M):(σ->τ)

Γ ͱ M:(σ->τ)    Γ ͱ N:σ
-------------------------- (->elimination)
Γ ͱ (MN):τ

1. 下面的分類是按Γ ͱ M:σ被推理規則生成的方式來分的
   Γ ͱ M:σ作爲有向圖中的一個節點
   它被添加入有向圖中的方式只有三種
   1. (axiom)
      這是顯然的
      具體的實現設計到對列表Γ的處理
   2. (->introduction)
      已知Γ ͱ M:σ形如Γ ͱ (λx.M):(σ->τ)
      + 這個"已知"顯然是通過一個模式匹配來實現的
      是被Γ,x:σ ͱ M:τ引入的
      在Γ'⊃Γ的前提下
      引入節點Γ',x:σ ͱ M:τ
      + 這裏需要Γ',x:σ還是一個basis
        即它還表示着一個集合
        即作爲列表的表達式其中的表達式不重複
        即需要x:σ不在Γ'中
        (或者是x不在dom(Γ')中???)
      然後就可以引入節點Γ' ͱ (λx.M):(σ->τ)
   3. (->elimination)
      已知Γ ͱ M:σ形如Γ ͱ (MN):τ
      是被Γ ͱ M:(σ->τ)    Γ ͱ N:σ引入的
      在Γ'⊃Γ的前提下
      用集合論中的推理規則(在這裏被實現爲列表處理)
      引入節點Γ' ͱ M:(σ->τ)    Γ' ͱ N:σ
      然後就可以引入節點Γ' ͱ (MN):τ

      現在總結一下這上面的證明是在作什麼
      首先術語上這些命題被某些作者稱爲是"元語言"中的命題
      "元語言"是就人的理解方式而言
      具有根本性的東西
      我想這裏的:
      "字符串處理"
      "列表處理"
      "有窮有向圖處理"
      其根本性就在於它們能夠用機器來方便的實現
      或者說其中的基本關係能夠用機器來方便的實現
      比如說對"字符串處理"而言的等詞"=="
      對"列表處理"而言的"∈"
      TODO 對"有窮有向圖處理"而言有什麼呢???
2. 類似於1.
3. 類似於1.

我需要這些元語言中的命題
這在於
數學的特點就在於
我去用"證明"的形式對我所觀察與思考的東西
形成一些"一般性的"論斷
+ 這裏就用機器實現而言
  論斷的一般性就在於模式匹配
即 數學是一種想要儘可能一般地描述
人們通過觀察所總結到的規律的語言(或行爲)
而 這種對一般性的追求
可能是人們爲了使人們自己能對所觀察到的東西
形成更"經濟的"理解
或者說"更好的"理解

### generation lemma for λ->curry

1. Γ ͱ x:σ
   ----------
   (x:σ)∈Γ

2. Γ ͱ MN:τ
   ---------------
   ∃σ(Γ ͱ M:(σ->τ) ∧ Γ ͱ N:σ)

   這個命題其實是說
   有向圖中的每個可以模式匹配到Γ ͱ MN:τ的節點
   在圖中都有模式匹配到Γ ͱ M:(σ->τ)與Γ ͱ N:σ的
   直接父節點
   這就像我說:"我坐在地上玩積木
   我一個一個地把積木摞起來
   那麼要麼最上面的積木的下面一定存在一個積木
   要麼最上面的積木的下面就是地面"
   難道我需要形成一個一階邏輯中的命題來描述這個事實嗎?
   用謂詞演算中的命題來描述這個事實
   反而模糊了這個事實
   這就是有窮有向圖的"本源性"

   這裏用到了謂詞演算
   而且連詞是寫在"ͱ"類的表達式之間的
   這下又不清楚了
   TODO 可能我必須先重新構造起來一階邏輯的形式體系
   然後才能作我希望作的討論???
   這是不可能的!!!
   因爲這些討論的目的就是爲了脫離一階邏輯的形式體系
   而建立新的形式體系

3. Γ ͱ λx.M:ρ
   ---------------
   ∃σ,τ(Γ,x:σ ͱ M:τ ∧ ρ == (σ->τ))

   這個迂腐的命題和上面的命題一樣值得被批判

### TODO typability of subexps in λ->curry

### TODO substitution lemma for λ->curry

### TODO subject reduction theorem for λ->curry

the subset of Λ that
having a certain type in λ→
is closed under reduction

# TODO the system λ->church

## inductive definition of the set of exp

tvar ::= α | tvar'
type ::= tvar | (tvar -> tvar)
var ::= v | var'
exp ::= var | (exp exp) | (λ var:type . exp)

church system與curry system的區別就是
對於church system來說所有的exp的集合
的歸納定義變了

## inference-rules

(x:σ)∈Γ
--------- axiom
Γ ͱ x:σ
+ 集合可以看成是有序列表

Γ,x:σ ͱ M:τ
---------------------- ->introduction
Γ ͱ (λx:σ.M):(σ->τ)
+ 這裏是唯一與curry system不同的地方

Γ ͱ M:(σ->τ)
Γ ͱ N:σ
-------------------------- ->elimination
Γ ͱ (MN):τ

## TODO 從圖論的角度理解上面的推理規則所形成的形式理論

## TODO 類型的加入

把類型作爲exp的組成部分之後
理論如何變得複雜了?
首先
約束變元是帶類型的
這樣變元的數量就成了一個笛卡爾積嗎?

## TODO -sb->

DEFINE:
(λx:σ.M)N -sb-> M[x:=N]
如果這裏對N:σ沒有要求
那麼對於-sb->來說
"(λx:σ.M)"中的":σ"就成了虛置的部分
因爲在實際用對字符串的操作實現-sb->時
根本用不到":σ"
如果":σ"不是用來限制N:σ的
那麼它還有什麼用呢?

## TODO basis lemma for λ->church

let Γ be a basis
+ 即一些先驗的變元對類型的屬於關係的列表

1. if:
   Γ'⊃Γ
   then:
   Γ ͱ M:σ
   --------
   Γ' ͱ M:σ

2. Γ ͱ M:σ
   --------
   FV(M)⊂dom(Γ)

3. TODO Γ ͱ M:σ
   --------

## TODO original version of λ->

如果寫成:
λx:σ.x:τ
λx:σ.x:τ : σ->τ
那麼:
(λx:σ.x:τ)N:σ -sb->???
N:σ???
N:τ???
都不對

實際上所使用的是:
x:σ ͱ x:σ
----------
ͱ (λx:σ.x):(σ->σ)

(λx:σ.x:τ)這種項是不會被引入的

## TODO λ-cube

# TODO classical logic formal language

## note

在學習了這麼多形式語言之後
也許改回顧最初所學習的形式語言
既然我已經有了去對比各種語言之間的差異的能力
那麼我對classical logic formal language的理解一定也可以變得更深刻了

而且對於理解propositions as types來說
經典數理邏輯的知識是必要的

## 回到<<元數學導論>>

模型的方法起初是爲了證明公理的無矛盾性
而且模型的概念具有相對性
這在我想要使用有向圖爲基礎來建立其它形式語言的過程中也可以看出來
因爲我必須還要有一個語言能夠描述有向圖的一般性質
這種對一般性質的描述必然又用到一個形式語言

## 在傳統的以集合論爲中心的數學的形式化基礎中

人們把某些命題當做證明的目的 即結論
來探索對這些結論的論證
人們希望論證的作爲結論的命題有什麼樣的形式?
我想共同點在於它們都是對一些關係的判斷
而關係是定義於集合的
這樣整個形式體系就是以集合論爲中心的

## TODO logic of statements recovered via curry-howard

傳統的謂詞演算由curry-howard
在類型論中重新實現???

# TODO

`if ... then ...'
is different form
`thus' `therefore' `so' `hence' `since'
in mathematical reasoning (about the rules of inference)

converse and contraposition

bi-tree of proposition
=connectives=>
proposition

open proposition
=quantifiers=>
less-open proposition

quantifiers:
if is there is no set theory at first
how can there be quantifiers?
by inference-rules of course
and by the following sugar
∃x∈A(...) :=sugar= ∃x(Ax∧...)
∀x∈A(...) :=sugar= ∀x(Ax∧...)
and there are matter of scope
just as in the case of λ-abstraction

function:
f: X -> Y
∀x∈X∃!y∈Y(xfy)
here comes another kind of scope problem
about dependence
∀x∈X∃!y=y(x)∈Y(xfy(x))

scope problem should always solved by graph theory

inference-rules:
(A A→B) ==> B
C→A(x) ==> C→∀xA(x)
A(x)→C ==> ∃xA(x)→C


rewrite-rules are about computation
but inference-rules are not
TODO do not have a computational nature??
TODO yes???

"the quantiers usually range over
an infinite set and therefore one loses decidability"

"Aristotle had already remarked that it is often hard to find proofs
but the verification of a putative one can be done in a relatively easy way"

so i should write something to help to show a math proof

A more eficient way to represent proofs employs typed lambda exps
and is called the propositions-as-types
which maps propositions into types
and proofs into the corresponding inhabitants
TODO how about axioms?

# curry-howard-correspondence

## 動機

1. 想要完全用 lambda-calculus 來理解邏輯推理
   這樣對我來說數學基礎就齊活了
2. 其實 需要仔細回顧一下邏輯理論
   因爲在進行證明的時候
   某些步驟使用地太自然了
   都忘記推理規則的存在了

## 記

1. 命題演算 可以被趣稱爲 "謂詞使用方法總結"
   也就是說我還是不想引入新的理論
   而想直接把它理解爲布爾代數的性質
2. 對邏輯演算[推理規則]的需要
   是因爲無窮集的出現嗎?

## formulae as types

1. 對一個形式命題[formulae]的證明
   就[將]是去尋找一個屬於所對應類型的 λ-exp

## 推理規則

1. 所謂 推理規則 就是[樹狀]數據結構的構造子
