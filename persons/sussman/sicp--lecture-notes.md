---
title: Structure and Interpretation of Computer Programs -- Video Lecture
authors: [Harold Abelson, Gerald Jay Sussman, Julie Sussman]
date: timeless
---

# 1A

- 層次結構是爲了控制複雜性
  把複雜性控制在人類所能理解的範圍內
- 其實
  程序設計中的一大部分知識
  是關於如何控制這種複雜性的
  [關於表達的 而不是算法的時空消耗]
- 一個基本的控制表達的複雜性的方法是
  設計更特殊化的新編程語言
- 有趣的是 在純數學中
  總想要把函數看成是點集
  而在計算機科學中 發展了一套形式語言去描述過程
  因此函數總被看成是對數據的操作
  顯然 後者更具體 它的語義更豐富
  [例如 在 lambda-cal 中函數被理解爲 代入轉寫]
- 如何描述對與一個數學結構族而言的抽象函數[generic operation]
  使得它可以使用於結構族中各個具體的結構
  [例如以線性空間族 與線性空間]
  需要動態地檢測變量的類型
  即 變量無類型而值有類型
- 函數式編程是把編程比爲建築的最恰當範式
- to show a language is to show
  1. primitive elements
  2. means of combination
  3. means of abstraction
- in lisp
  1. primitive data & primitive procedures
  2. using ( ) cond cons if
  3. using define
- >< 反思上面兩段對語言的本質的總結
  [也許從數學與語言學的角度]

# 1B

- the key to understanding complicated things
  is to know what not to look at
  and what not to compute
  and what not to think
- using Substitution Rule to explain evaluation (temporarily)
- 去學習這些術語
  之後我們就可以用這些術語進行討論了
  one fo the things that every sorserer will tell you
  is if you have the name of a spirit, you have power over it
- 培養直覺
  一個特殊形狀的 procedure or expression
  如何給出一個特殊形狀的 process
- 關於 迭代 遞歸 尾部遞歸 算法複雜度

# 2A

- 用一級一級的抽象來使程序變得容易理解
  這就是所說的
  用層級結構來控制[就表達而言的]複雜性
- 這種複雜性關乎於
  一個人如何表達一個東西[比如對算法的表達]
  而使得所作出的表達
  易於被另一個人[或者將來的自己]所理解
- 很明顯
  要從結構主義的角度定義所謂 關於表達的 複雜性
  那麼就必須指明
  表達者 與 理解者
  他們將給出用於定義複雜性的基本元素 等等
- 這顯然還沒有把 "什麼是複雜性" 這個問題研究清楚
  這些陳述所未表達出來的還有很多
- 比如 這樣的層次結構還使得
  寫程序的人可以從 總體的性質 入手
  從一個大的觀念入手
  而不必過早的去考慮 大的過程中的細節過程 是如何實現的
  [即所謂的 wishful thinking]
  可以說當使用這樣的函數式編程範式時
  就像是 lisp 語言在教你如何解決問題
  即 去構造層次

# 2B

about compound data
just like compound procedure

構建數據結構的 同對過程的抽象一樣 也是一種構建層次結構的手段
理由是相同的:
1. 表達清晰 易於理解
2. 解構之後的各個部分更易於修改
3. 使人們可以從大觀念入手來構造程序 並且保持靈活性
在程序極端複雜龐大的情形 這些不僅僅是優勢 而處理複雜問題的必要條件
並且某種程度上 在使用這樣的範式時
你會更進一步學習到關於所處理的問題的知識
因此在解決問題的過程中獲得解決問題的能力

而劣勢在於:(反對意見相當單薄而易於反駁)
1. 降低了機器那方面的效率
   提高了人類的效率,並且這個劣勢可以用好的編譯器來彌補
2. 解構所帶來的分離性
   + 這樣的分離性必須是絕對的
     即使得低一層次的改變(同時低一層必須保證自己完成了高一層次所部署下來的任務)
     完全不影響高一層次的工作
   可能使不同的工作這之間不能觸及彼此的工作
   或者可能造成對程序的權利的保護 這時無政府主義者所希望避免的
   + See, in general, as systems designers,
     you're forced with the necessity to make decisions about how you're going to do things,
     and in general, the way you'd like to retain flexibility is to
     never make up your mind about anything until you're forced to do it.
   + 對內存處理的封鎖有時候帶來麻煩
3. deferring decisions 有可能變成 outright procrastination
   推遲抉擇 有可能變成 徹底的拖延
   + 這也許是在那lisp的語法開玩笑呢

the whole name of this scheme is that
we'd like the programming language to express the concepts
that we have in our heads

當一個存儲單元存儲另一個存儲單元的地址
這樣所形成的關係滿足圖論中的有向圖的定義
所以就用箭頭來表示,所以就有指針這個術語

術語Closure在數學結構中很常見,指一種完備性
例如,通過cons得到的元素對本身也可以作爲cons的對象來再次形成元素組
即cons操作(運算)在lisp的所有數據集合中的封閉性
+ 數學中的經驗是:
  有些時候去判斷所定義出的一個過程
  或者形成新數據結構的組合方式
  是否具有某種程度的封閉性是本質重要的
  因爲這種就不封閉性的完備化可以作爲一個非常普使的思路來理解很多東西
+ 這纔是閉包這個術語的意義
  而"把環境包起來"只是表象
  即"把環境包起來"是爲了使得λ-term能夠在某些運算下完備

# 3A

list in lisp is essentially just a conventional way for representing a sequence

meta-linguistic abstraction

如之前已經申明的,給出一個語言就在於:
1. primitives
2. meams of combination
3. means of abstraction
又一次這些東西讓人想起數學結構,只不過與靜態的數學結構相比,這裏的東西被想成時動態的過程,而第3條(其實第2條也類似)是獨特與數學結構的,它指明如何用 基本的元素 與 基本的操作 來定義複雜的元素與操作.
聯想一直困擾我的數學結構的層次問題!
我有一個 一般的規則 來從 (起初只有基本後繼關係的)自然數結構(或者其他任何具體的結構)衍生出包含 自然數結構 的更復雜結構,這個規則是:
1. 去需找所有可能的運算(多元的或一元的),此時一種對於運算的有意義的分類是它們的對稱性,即它們所能滿足什麼樣的運算律
2. 同態概念(等等類似的概念)是基本的 而且是在所給出的基本結構之外的
3. 從 自然數 的 後繼關係 能夠 構造出 加法 在於 後繼關係的自同態的集合(以結構中的 某些已有運算 爲基礎的 滿足某些運算律(在自然數的例子中 運算律由 同態 指出)的 結構的基礎集合上的 函數的集合) 能夠在某種意義上與 自然數集等同;從 自然數 加法運算 能夠構造出 乘法運算 在於 加法運算的自同態的集合 能夠在某種意義上與自然數集等同;從乘法運算能夠構造出加法運算 在於乘法運算的自同態的一部分 能夠與自然數集等同.
4. 如果某些(n元)運算的集合能夠在某種意義下與結構的基本集本身等同,那麼就可以形成一種(n+1元)運算,如果這些運算的集合滿足某些運算律 那麼這樣形成的新運算就是有意以的或者有趣的.
5. 以上都是在某個具體的數學結構的運算方面(更廣泛的關係方面)作補充定義,同樣重要的是在結構的基本集方面(結構的元素方面)作補充定義,這在於 對所引入的 新運算 以及它們的逆運算 作完備化,即補充定義新的元素使得運算完備.
6. 從某種意義上來說 實數以及幾乎全部數學 都 以這種方式 內蘊於 自然數集.

(至少從形式上看來)這與所謂的 meta-linguistic abstraction 有很大的共通之處!
現在問題來了,我能在某種程度上,利用 lisp 實現一個有趣的東西來介紹這些概念嗎?
這樣的實現必須新穎並且意義豐富.

這裏所介紹的 meta-linguistic abstraction 是 Henderson-Escher example.

here because the operations are closed,we could built up complexity so fast.(這是 embedded 所要求的性質嗎?)

a language embedded in lisp.
lisp 作爲強大的工具來處理和擴展 這個埋入 lisp 的語言.
課程裏想要展示的就是如何把一個語言埋入 lisp 中,
這比在 lisp 中實現一個語言要好得多,因爲它使得你不失 lisp 的全部原始力量(比如定義高階過程的能力).
也就是說,用形成了一個多層次結構的埋入 lisp 語言鏈 來 完成任務(解決問題),比用 把任務分解成任務樹再來分部解決要好的多(兩者都是用來控制複雜性的方法).
比如 這樣能獲得 健壯性(insensitive to small changes: a small change in the problem should lead to only a small change in the solution.There ought to be a continuity. The space of solutions ought to be continuous in this space of problems.),對某個層次的語言中元素的改變可以被 高一層次捕獲 而不影響整體.
同時構建語言鏈,使得你用大量的詞彙來描述一些細節性質,這使你對問題的理解更透徹,並且這帶來了解決問題的靈活性.

# >< 3B

# 4A

關於模式匹配 與 在指定規則下的替換.
當想要描述一個替換規則時所使用的
尤其是在符號計算中經常需要使用(用於對符號表達式的化簡)

方法就是
1 匹配(有固定的規則 並且 用到 通配符(構造特殊的通配符來匹配類型 (??c?v)))
2 替換

# 4B

Generic operator means what it sort of precisely does depends on the kind of data that it's looking at.

以複數的運算爲例.(注意:與我的問題相聯繫的是,每當結構擴張,都需要補充定義運算 以實現這種運算符重載.)

typed data comes now!
dispatch on type.

首先,利用添加標籤來實現,給不同類型的數據順便貼上標籤.
之後,爲了使新的數據結構容易被增加進來,把那個查表的管理者踢了,而直接用那張表.

data-directed programming.
這時會作出標籤鏈的.
層次結構又出現了!

decentralized control

練習1:
去發現Galois中可以用於符號計算 或一般計算的 題目,然後用 lisp 來實現,注意 要用4B中的方法.

練習2:
計算數論中的計算題目.

練習3:一階語言.

練習4:公理集合論.

# 5A Assignment, State, and Side-effects

- 問題 1 -
  一個人
  對 描述性(普遍性)知識
  與 過程性(計算性)知識 的理解是統一的
  那麼機器如何做到這一點?
  - 比如 機器可以在計算一個表達式之前 先審視這個表達式
    用形式規則沿某一方向 找出一些等價的表達式
    即它們的計算結果將是相同的
    但是這些形式規則是人告訴機器的
    並不是機器通過它所又能力執行的那個計算本身來獲得的
    而計算本身理應包含這些形式規則
    人既知道自然數有加法 又知道加法有交換律
    而如何讓機器把 就同一個具體的數學結構的
    數值計算與符號計算相結合?
  - 可計算性是什麼意思?
    它限制機器使得它不能獲得這種能力嗎?

- 問題 2 -
  機器可不可以看着一個具體的數學結構
  然後用 提高運算 級別的方法去擴展這個具體的數學結構?
  機器如何理解數學結構?
  - 考慮 lisp 作爲形式語言本身而形成的數學結構試試!
    此時結構的基本集合爲所有的 S-表達式
    具有潛在的無窮性
    而且 lisp 本身並沒有儲存所有的結構的基本集合中的元素
    之後還有一些對這些 S-表達式 的基本操作
    可是關於這些操作的一般性知識是在形式語言之外證明的
    - 如何理解 lisp 可以在 lisp 之內實現?
    還有 lambda 與 cond 它們使得形式語言能用來表達過程

- 練習 1 -
  去用列表實現自然數結構

- 事實1 -
  描述性知識描述一些具有普遍性的定理 例如 加法交換律
  而計算時 我們發現 以兩種方式計算兩個具體的數的加法
  它們的結果是相等的

- 事實 2 -
  運算律 可以很好的用形式化的置換規則描述
  - 甚至我們可以構造一個 更一般的
    可以任意指明某種目的 對錶達式的化簡方向
    - 這可以作爲一個練習 -- 練習 2

- 觀點 1 -
  以後繼關係爲基本關係的自然數集 和其中的加法交換律
  都可以作爲統計性知識 (在實際的計算實踐中) 而習得
  而形式的邏輯規則 是在我們考慮這些(普遍性)知識之間的關係時
  作爲統計性知識被習得
  邏輯指明命題之間的序關係
  加法交換律可以作爲結論由自然數集的基本後繼關係而推出

回到課程本身 ~

set! comes now!

用這個 詞 之後 表達式的求值結果就與時間有關了!
side-effect!
這樣就 出離 函數式編程範式了
函數的行爲不再一致了
- 不再與時間無關
- 不再像一個數學函數了

明白什麼時候自己的代碼在函數式編程範式之內
而什麼時候在函數式編程範式之外是很重要的

then comes the environment model here
- 爲了引入對自由變元的求值
since the sbubstitution model fail
- 它只適用於約束變元的情形

- 老師的觀點 1 -
  object 這個術語在於
  人們的爲了思維的經濟性
  而把在細緻地描述某個集合的性質時所觀察到的
  集合的 (就所描述的性質而言) 基本上相互獨立的兩個子集分離開
  把它們作爲兩個整體稱爲兩個對象
  使得在之後的討論中不必再深入細節

- 老師的觀點 2 -
  這樣的分離有時並不恰當
  比如在量子力學中
  有時實際上被我們爲了經濟性而分離了的
  所謂兩個對象之間的聯繫比表面上的更多
  有時我們甚至爲了思維的經濟性而拒絕承認這一點
  而我們認爲量子力學很難就在與我們這樣的思維習慣
  因爲我們正是被訓練得去這樣思維的
  這使我們不得要領 (比如愛因斯坦對量子力學的觀點)
  思維的經濟性 很值得思考的一點

- 老師的觀點 3 -
  about actions and identity
  物體 (identity) 的相等與不等
  是就某些可以所用於他們的作用 (actions) 而言的
  (類比 克萊因 埃爾朗根綱領)
  但是有趣的
  例如
  考慮一個自然數軸上的映射
  它把第三個點移動到第四個點
  或者由指向第三個點變成指向第四個點
  但是不論如何總有一個客體好像是前後不變的 -- 點或者箭頭
  它們只不過是被移動而已
  如果它把數字 3 變成 4
  3 只不過是變成了 4 的 3
  就像把粉筆掰斷了之後得到的是掰斷了的之前的那個粉筆

  雖然 Assignment statement 讓我們覺得
  那裏好像有一個物體的存在被聲明了
  但是當我們越深入細節
  這一點就可能看起來越不真實

  object 是如此
  function 是如此
  relation 和 type 也是如此

# 5B Computational Objects

以數字電路爲例子
來在 scheme 中實現 OO

inverter (not-gate)
and-gate
or-gate

可以把下面的西線想像成小球
然後那些門上的線連接到小球上

這樣每個做出來的電路就是一個以某些小球爲接口的東西

``` scheme
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)
```

一個語言中的複合物看起來要像基本物一樣
以同樣的方式使用和處理 等等
儘管複合物與基本物之本質不同

# 6A Streams-Part 1

引入 assignment 之後
一切變得複雜多了
很多概念都進入討論了 比如 狀態 時間 和 id

It's a technically harder way of looking at things
because we have to think more mechanistically
about our programming language
We can't just think about it as mathematics
It's philosophically harder, because suddenly
there are all these funny issues
about what does it mean that something changes
or that two things are the same
And also, it's programming harder,
because as Gerry showed last time
there are all these bugs
having to do with bad sequencing and aliasing
that just don't exist in a language where
we don't worry about objects

但是
之所以要引入這些概念是因爲
We wanted to build systems that
fall apart into chunks that seem natural

又但是
See, maybe the real reason that
we pay such a price to write programs
that mirror our view of reality is that
we have the wrong view of reality
See, maybe time is just an illusion,
and nothing ever changes

又但是
我們畢竟得到了一種來把模塊分得更細的能力
只要不隨意的把這種能力用到沒有必要的地方就行了

here comes stream processing: (as conventional interfaces)
another way to decompose systems that's
more like the signal processing engineer's view of the world
than it is like thinking about objects
that communicate sending messages

- 當你有興趣學的東西
  和老師有興趣講的東西完全一致時
  奇蹟就發生了

# [todo] 6B Streams-Part 2

# [todo] 7A

把程序視爲機器
將要展示的是 universal machine
(考慮圖靈 和 他的 通用圖靈機)

# 9A 一個可以作爲編譯器的中間語言的低級語言

1. 寄存器機的特點就是
   函數的輸入值與輸出 都明依賴於以顯地方式聲明寄存器而完成
2. 與forth這種棧機器相比
   可以說sicp寄存器機是針對對寄存器的操作來優化自己的語法的
   而forth是針對對棧的操作來優化自己的語法的
3. 另外
   不同語言對函數語義的實現方式不一樣
   也就是對函數的參數傳遞的實現方式不一樣
   而在scheme這種更高級的語言中 根本就感覺不到對函數調用的約定
   調用一個函數的時候 就是需要在被調用位置用到函數的返回值的時候
   所以對參數傳遞方式的約定被隱藏了
   而在一個函數返回的值可以被留在棧裏之後在用
   而不是需要被立即使用
   在scheme中是通過局部變量來實現這種效果的
4. 關於smalltalk中的協議和信息傳遞:
   在寄存器機裏也有對函數參數的約定等等
   但是有什麼區別呢???
   wordy-lisp如何呢???
5. 這節反覆說明 機器很笨
   + 類似於圖靈的計算員隱喻 但是略有區別
   但是正是機器的這種笨的但是能夠被重複並且被通過積累而增加性能設計
   使得現代電子計算機這種機器非常成功
   ``` scheme
   (define gcd
     (lambda (a b)
       (if (zero? b)
         a
         (gcd b (remainder a b)))))
   ;; (gcd 3 6)
   ;; (gcd 3 7)

   (define remainder
     (lambda (n d)
       (if (< n d)
         n
         (remainder (- n d) d))))
   ```
6. 極簡主義的金玉良言:
   one of the important things for designing a computer,
   which i think most designers don't do,
   is you study the problem you want to solve
   and then use what you learn from studying the problem you want to solve
   to put in the mechanisms needed to solve it in the computer you're building,
   no more no less.
7. Now it may be that the problem you're trying to solve is everybody's problem,
   in which case you have to build in a universal interpreter of some language.
   But you shouldn't put any more in
   than required to build the universal interpreter of some language.
8. 也就是說,如果你對你所想要解決的問題有充分而深入的研究,並且透徹理解了那個問題,
   那麼,在實現一個解決那個問題的方案的時候給出一個極簡主義的設計就是水到渠成的了


每個函數就像一個機器,大機器裏可能有小機器
而這一節的語言是一種機器描述語言
每個機器由兩部分組成:
1. 電路(data path)
   一個data path對應於彙編語言中的一個指令
   + 但是顯然這是兩種計算模型之間的類比
     這裏的每個小機器都是特殊的計算機
     而 比如說 x86的機器是一個通用的計算機
     彙編命令是這個計算機用來模擬特殊的小計算機的方式
   + 注意通用計算機所模擬的每個小機器都可以直接作爲硬件被造出來
2. 控制器(controller)
   控制器對應於流程圖
   它把小機器以某種方式鏈接起來變成大機器
   一些彙編指令的按順序排列就是controller
   按順序排列之外也可利用mark language形成流程圖中的圈
   而時間可以看成是在流程圖中運動的一個點
參數在兩個機器是之間的傳遞在於它們都讀寫某個共同的存儲空間:寄存器,或者棧

機器被理解爲這樣的東西(一個有向圖):
1. 寄存器
   一種可以存放值的節點
2. 計算元件
   一個原子計算元件 或者是 一個被抽象起來的同類機器(歸納定義產生於這裏)
   一種節點
   有一些入邊鏈接到某些寄存器,可以從這些寄存器裏fetch(並不刪除舊的值)出值來
   有一些出邊鏈接到某些寄存器,可以把計算的結果保存到這些寄存器中
   就像一些電流被過濾成了另一些電流
   這個節點上有一個開關來控制計算的進行
3. 單向信息流導線(可以被理解爲 特殊的計算元件)
   一種特殊的有向邊
   兩邊都連到寄存器
   導線上有開關
   當按下開關時會把一個寄存器中的值複製到另一箇中
4. 指示燈
   一種節點
   與某個寄存器相連
   指示燈可以作爲謂詞對這個寄存器中的值形成一個判斷
   也就是對寄存器中的值我們能夠形成我們所能想像到的任何謂詞
   控制器可以讀指示燈
5. 控制器
   來控制按那些開關的先後順序
machine == data path + controller

``` scheme
(define-machine gcd
  (register <a> <b> <t>)
  (controller;; 就像彙編語言 或者流程圖
   ;; 程序運行過程中的某一時刻 可以看成是流程圖中的一個點
   ;; 而流程圖中的一些操作可以看成是與機器中的開關的按鈕相對應
   MAIN (assign <a> (read))
        (assign <b> (read))
   LOOP (branch (zero? (fetch <b>)) DONE)
        (assign <t> (remainder (fetch <a>) (fetch <b>)))
        ;; fetch指出了那些寄存器節點鏈接到remainder的入邊
        (assign <a> (fetch <b>))
        (assign <b> (fetch <t>))
        (goto LOOP)
   DONE (print (fetch <a>))
        (goto MAIN)
        ))
;; 在上面assign與fetch就代表了帶有開關的有向邊
;; + 這裏計算元件也被分解了
;;   因爲其實不需要那麼多的開關 所以可以更精簡一點


;; 參數在兩個機器是之間的傳遞在於它們都讀寫某個共同的存儲空間:寄存器(或者棧)
;; 注意這裏機器被理解爲函數的方式
;; 注意約定參數傳遞的方式

(define-machine gcd
  (register <a> <b> <t>)
  (controller
   ;; 1. 是controller在給出按鈕 並進行控制
   ;;    一個mod可以被controller分配多個按鈕而運用多次
   ;;    controller描述了機器如何被搭建 同時也描述了機器如何被控制
   ;; 2. 謂詞是返回bool值的機器 它返回的值能夠被branch處理
   ;;    branch專門就是用來處理bool值的裝置
   MAIN (<a> <-- (read))
        (<b> <-- (read))
   LOOP (branch <-- zero? <-- <b>
                DONE)
        (:remainder <t> <-- mod <-- :dividend <a> :divisor <b>)
        ;; fetch指出了那些寄存器節點鏈接到remainder的入邊
        (<a> <-- <b>)
        (<b> <-- <t>)
        (goto LOOP)
   DONE (print <-- <a>)
        (goto MAIN)
        ))
```

上面是iterative(尾遞歸的)的函數所對應的機器
下面就是看遞歸函數對應與什麼樣的機器
在這裏就需要用棧來模擬無窮多個小機器的嵌套了
語義上 棧中保存的是外面的大機器的狀態
當裏面的小機器工作完了之後
利用棧中所保存的信息可以恢復大機器額工作
``` scheme
(define factorial
  (lambda (n)
    (if (= n 1)
      n
      (* n (factorial (- n 1))))))
```
這不是尾遞歸的函數了
因爲爲了計算返回值我們不只需要調用factorial本身
還需要把這個調用的返回值拿來和n乘
以得到最後的返回值
即 對*的調用需要等待對factorial的調用的返回值
而在尾遞歸的情況下不用等待

這是就需要無窮的嵌套了
但是無窮的嵌套在物理的對機器的實現中並不存在
我們把這個問題的有窮部分和無窮部分分開來解決
有窮部分就跟之前一樣
而無窮部分用棧這個非常簡單的數據結構來解決
棧並不是無窮的 只是非常大而已

這時候機器作爲一個有向圖的樣子也變了
但是爲了以更簡潔的方式理解這個圖
我不去考慮棧的實現方式
而像在joy中一樣 把操作棧的primitives理解成以棧爲參數的一元函數
``` scheme
(define-machine factorial
  (register <return> <arg> <continue>)
  (controller
        (assign <continue> DONE)
   LOOP (branch (= 1 (fetch <arg>)) BASE)
        (save <continue>)
        ;; 下面把<continue>指定爲factorial的遞歸調用返回後所必須經過的處理
        (assign <continue> AFTER)
        (save <arg>)
        (assign <arg> (sub1 (fetch <arg>)))
        (goto LOOP)
   BASE (assign <return> (fetch <arg>))
        (goto (fetch <continue>))
  AFTER (restore <arg>)
        (assign <return> (* (fetch <arg>) (fetch <return>)))
        (restore <continue>)
        (goto (fetch <continue>))
   DONE
        ))

;; 大寫的word是地址的值

(define-machine factorial
  (register <arg> <result> <next>)
  (stack <<ReturnStack>>)
  (controller
        (<next> <-- DONE)
   LOOP ;; 這段計算是爲了把遞歸的扇子展開
        (branch <-- :bool one? <-- <arg>
                    :address BASE)
        (<<ReturnStack>> <-- <next>)
        (<next> <-- AFTER)
        (<<ReturnStack>> <-- <arg>)
        (<arg> <-- sub1 <-- <arg>)
        (goto <-- :address LOOP)
   ;; 下面兩段計算是爲了把展開的遞歸的扇子合起來
   BASE
        (<result> <-- <arg>)
        (goto <-- :address <next>)
   AFTER
        (<<ReturnStack>> --> <arg>)
        (<result> <-- * <-- <arg> <result>)
        (<<ReturnStack>> --> <next>)
        (goto <-- :address <next>)
   DONE
        ))
```
足夠大的棧給你一個幻覺
認爲遞歸過程可以是無窮的

在練習一個例子 以熟悉棧的用法
戒律:
1. 不要在棧裏保存以後用不到的值
2. 之所以有一個有用的值需要被保存
   是因爲保存這個值的寄存器馬上就有別的用處
3. 取出來一個值就趕快用這個值
4. 覆蓋一個寄存器的時候一定要確定裏面的值已經不需要了
5. ><>< 是不是可以借鑑CPS???
``` scheme
(define fib
  (lambda (n)
    (if (<= n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2))))))
(fib 20)
==> ...

(define fib
  (lambda (n p)
    (if (zero? n)
      (car p)
      (fib (sub1 n)
           (cons (cdr p)
                 (+ (car p)
                    (cdr p)))))))
(fib 20 (cons 1 1))
==> ...


(define-machine fib
  (register <result> <arg> <continue>)
  (controller
        (assign <continue> DONE)
   LOOP (branch (< 2 (fetch <arg>)) BASE)
    #0= (save <continue>)
        (assign <continue> AFTER-fib:n-1)
    #1= (save <arg>)
        (assign <arg> (- (fetch <arg>) 1))
        (goto LOOP)
   BASE (assign <result> (fetch <arg>))
        (goto (fetch <continue>))
AFTER-fib:n-1
    #1# (restore <arg>)
        (assign <arg> (- (fetch <arg>) 2))
        ;; (restore <continue>)
        ;; (save <continue>)
        ;; peephole optimization:
        ;; 當對一個寄存器的restore save assign三連,而中間無其他操作時
        ;; 就可以作這樣的優化
        (assign <continue> AFTER-fib:n-2)
    #2= (save <result>)
        (goto LOOP)
AFTER-fib:n-2;; 有幾個遞歸調用就有幾個AFTER
        (assign <arg> (fetch <result>));; fib:n-2
    #2# (restore <result>)
        (assign <result> (+ (fetch <result>) (fetch <arg>)));; 只有在最後一次遞歸調用的之後才能算出一個返回值
    #0# (restore <continue>)
        (goto (fetch <continue>))
   DONE
        ))

(define-machine fib
  (register <arg> <result> <next>)
  (stack <<ReturnStack>>)
  (controller
        (<next> <-- DONE)
   LOOP
        (branch <-- :bool < <-- :a 2 :b <arg>
                    :address BASE)
        (<<ReturnStack>> <-- <next>)
        (<next> <-- AFTER-fib:n-1)
        (<<ReturnStack>> <-- <arg>)
        (<arg> <-- sub1 <-- <arg>)
        (goto <-- :address LOOP)
   BASE
        (<result> <-- <arg>)
        (goto <-- :address <next>)
   AFTER-fib:n-1
        (<<ReturnStack>> --> <arg>)
        (<arg> <-- sub2 <-- <arg>)
        (<next> <-- AFTER-fib:n-2)
        (<<ReturnStack>> <-- <result>)
        (goto <-- :address LOOP)
   AFTER-fib:n-2
        (<arg> <-- <result>)
        (<<ReturnStack>> --> <result>)
        (<result> <-- + <-- <result> <arg>)
        (<<ReturnStack>> --> <next>)
        (goto <-- :address <next>)
   DONE
        ))
```

# 9B 用低級語言實現的解釋器

在這節中可以發現
當仔細分析用低級語言實現的解釋器時指令的順序
那麼就自然而然得到尾遞歸優化
並不是什麼神奇的預處理機制在作尾遞歸優化

So we built all of these languages, they're all based on LISP.
A lot of people ask what particular problems is LISP good for solving for?
The answer is LISP is not good for solving any particular problems.
What LISP is good for is constructing within it
the right language to solve the problems you want to solve,
and that's how you should think about it.

我想強調scheme的上面的這個性質
並且改進它 以使它更適合完成這類任務

對於初學者來說,用元lisp解釋器寫一個lisp解釋器會帶來驚奇
而寫個lisp到某個機器的彙編的編譯器就能消除這種驚奇
一種愉快的理解被代替爲另一種愉快的理解

這裏是在用上節課所介紹的低級語言來寫lisp的解釋器

注意展開者把值(保存後面的計算的指令的地址)入棧
合起來者把值(保存後面的計算的指令的地址)出棧

尾遞歸優化其實不是針對尾遞歸的
而是針對所有尾部調用的
``` scheme
(define-machine eval
  (register
   ;; contract that eval-dispatch fulfills
   <sexp>        ;; eval的第一個參數
   <env>         ;; eval的第二個參數
   <continue>    ;; 保存下一步將要去的地址
   <return>      ;; eval的返回值
   ;; 當返回值時其他的寄存器中的值就可以都不要了

   ;; contract that apply-dispatch fulfills
   <fun>         ;; apply的第一個參數
   <arg-lis>     ;; apply的第二個參數
   ;; 要求棧的頂端保存着下一步要去的地址
   ;; apply的返回值也保存在<return>寄存器中
   ;; 之後pop stack
   ;; 之後其他的寄存器中的值就可以都不要了

   <temp>
   )
  (controller

        ))
```

# 10A 編譯器優化

解釋器是一個可以計算某個語言的所有的表達式的機器
而編譯器是一個把一個語言的表達式轉化到另一個語言的機器
當目標語言是彙編時 編譯器就像是製造機器的機器

+ 只要統一用define定義的函數的參數所用的寄存器
  編譯器和解釋器所定義的函數就能相互調用
  這就需要把解釋器中的(至少是)define用編譯器的目標語言來實現
+ 非全局優化的漸進編譯器也能解決相互調用的問題
  因爲此時解釋器只不過是一個編譯器的包裝

關於編譯器的優化:
最極端的生成低效率的代碼的方式是
先寫一個單純地把一個(用低級語言實現的)解釋器的解釋過程存儲起來的編譯器
然後在用分析函數來過濾這個生成的目標代碼中沒必要出現的部分

關於中間語言:
1. 應該以這樣的方式來實現中間語言
   使得中間語言的每一個指令必須都相互獨立
2. 使用scheme中的中間語言就可以把對目標代碼的處理維持在scheme中
   而儘量晚生成真正的會編碼或機器碼

函數的複合體現在彙編級的低級語言中
就是把一段一段相互獨立的指令接起來
但是在把指令段接起來的同時要利用棧來保護某些寄存器中的值
即 如果後面的代碼段需要某個寄存器 前面的代碼段更改了這個寄存器
那麼就需要用一對進棧與出棧來爲這次連接保存這個寄存器中的值
所以對於編譯器來說代碼段作爲數據結構的組成部分是:
1. 代碼段本身
2. 代碼段需要的寄存器(一個小機器讀取的寄存器)
   代碼段修改的寄存器(一個小機器寫入的寄存器)
3. 注意連接兩個代碼段而形成一個大的代碼段時
   數據結構中的這些值的變化

# 10B pair的實現 與 垃圾回收 與 尾聲

首先pair的實現是非本質的問題
比如低效地
我們可以用哥德爾配數法來編碼pair
這將是極端低效的
低效到這種實現只是在理論分析中有用

直觀的從幾何上看pair是非常簡單的
但是並沒有電子設備能直接實現這種幾何直觀
我們能利用的電子設備只是線性的內存而已

所以用來實現pair的機制是內存中的一個數組
每一項包含car與cdr兩部分
+ 實際上這個數組的每一項保存更豐富的信息
  比如垃圾回收機制就用到了每一項中的mark信息
+ 如果讓數組的每一項都保存自己這個位置的地址(或者數組的索引)
  儘管效率很低
  但是這樣我就能實現一種更好的pair了
  即 從每一個pair我能顯式地得到它的地址
  從而自由並且安全並且方便地在別的地方引用這個pair
+ 這樣就也阻止了用戶去直接處理地址
  同時又提供給我方便的引用機制
+ 但是這就給垃圾回收帶來了困難
  因爲比如說如果被這樣明顯引用的pair不允許被回收的話
  那麼就需要free的幫助來明顯的回收它們

這樣的實現方式就需要分配內存空間
笨辦法是用一個表格來記錄哪些空間是自由的
另一個辦法是使用一個free-list
預先初始化所有pair數組使得:
1. 有一個指針指向第一個自由的pair項
2. 每一個自由的pair項的cdr位置保存這另一個自由的pair項的地址
   + 發現 每個自由的pair在被聲明使用並被覆蓋之前
     它的car和cdr位置可以用來保存其他信息
     利用這一點嘗試實現內存分配機制
     >< 但是帶有loop的list是個問題
     當我失去對p = (1 2 b)的引用的時候
     我可能還需要對p中的其他部分的引用
     free-list:
     '(() () () () () () () ())
     或者
     '(1 2 3 8 4 2 3 4)
     因爲free-list的中每一個cons的car並不重要 是cdr讓它們鏈接起來的

     注意列表中的元素必須有類型
     因爲否則列表中保存一個地址的時候 我就沒法區分它是地址還是數字了
     也就是說如果想要實現類lisp的list這種數據結構 我就必須要設計類型系統
     而這只是簡單的給不同的數據類型設計編碼而已 而不是寫一個類型推導器

     內存的分配:
     (隨着構造子的出現而自動分配)
     然後每遇到一個構造子cons的時候
     free-list的第一個cons就會被拿來使用
     而free-list向後移動
     '(1 2 3 8 4 2 3 4) ==> '(2 3 8 4 2 3 4)
     出現cons的地方就是需要分配新的內存的地方

     內存的回收:
     需要計算有向圖的(有向)聯通性
     而且是先計算有用的 然後就知道沒用的
     利用一個mark實現這一切
     但是如果我不先完全地計算好哪些是有用的
     我就沒法知道那些是沒用的
     marking & sweeping
     如果marking作爲遞歸函數是利用棧來實現的
     那麼當有很多的cons被用到的時候 就很可能讓棧溢出
     >< 我知道數據結構上的豐富性可以式新的性狀和更快的算法成爲可能
     如何豐富list的數據結構才能實現一個更好的gc呢???

     >< 爲什麼我告訴自己我不能用那個swap算法來實現gc??
     因爲我想給list實現更豐富的性狀
     但是這真的形成衝突嗎???
     我想實現的新性狀是
     1) 當(cons sexp-a sexp-b)被求值的時候
        sexp-a和sexp-b中要能夠引用這個cons的地址本身
        但是 如果cons嵌套了怎麼辦??
        嵌套也是可以解決的只要用對地址的明顯的命名來使用它
        比如可能的語法是:
        ``` scheme
        (cons {kkk}
               sexp-a sexp-b)

        (cons :address kkk
              :car sexp-a
              :cdr sexp-b)
        ```
        然後在sexp-a中對kkk的引用就是對這個cons的地址的引用
     2) 我需要能夠以明顯的方式處理每個cons的地址
        上面的這種機制就足夠了嗎???
     3) 我可不可以原生地直接實現對wordy-list的支持呢???
        可能不行 因爲沒有基本的列表數據結構 我就沒法用列表來實現字符串
        而字符串是需要被作爲wordy-list中的那些symbol的
     4) 如果我用sicp中的方式來實現gc與列表結構
        那麼字符串怎麼辦呢???
        一個字符串將有8 bytes而不是1 byte
        這甚至都足夠用來編碼字符的顏色和字體了
        >< 但是如果每個字符都需要用64 bit來編碼
        用戶空間能承受的了嗎???

關於垃圾回收:
1. 原理是每一個計算機的"意識"就是它的寄存器中的值而已
   + 或者說只有幾個固定的變量是一個機器能夠意識到的
     比如 讓機器意識到用戶內存空間的
     可以是一個指向用戶空間中的某個位置的指針
   + 而對於我的forth系統來說
     字典中保存的東西決定了那些內存空間是在機器的意識之內
     而那些內存空間是在機器的意識之外的
   計算機訪問內存的方式是在寄存器中保存內存中pair數據結構(或其他數據結構)的地址
   然後pair數據結構之間的指針決定着那些內存是可以訪問到的
   其他的內存就是自由的
2. 在pair數組的項中添加mark信息就能用一個遍歷二叉樹的算法來考察使用情況
3. 標記好了信息之後
   就可以在再跑一遍整個pair數組(很費時間)
   然後把自由的項聯繫起來以形成一個free-list
4. 另一個算法是把pair數組分成兩部分
   在需要的時候利用swap把一半弄到硬盤中然後壓縮然後再傳回來

尾聲 關於不可計算性
