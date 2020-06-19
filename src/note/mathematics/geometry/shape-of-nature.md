---
title: 自然之形 / Shape of Nature
---

# 自然之形 / Shape of Nature

- 青涩时期的课程笔记

# 1 understanding nature

對自然中的圖形與空間的理解可以有多種方式
數學只是其中一種
但是是最有力量的一種
經典的數學方法提供語言使我們能夠描述空間與圖形或者其他某些東西的性質
正如計算機科學提供語言使我們能夠描述對某些東西的操作過程
- 因此
  正因爲我所創造的那些數學是在以一種新的方式描述一些未曾被很好描述的性質
  所以這些一定是有意義的
- 作數學 就在於 設計形式語言來捕捉人們的有趣幻想
  因此對每個數學理論的學習 就是對由別人所設計出來的一個形式語言的學習
  像學習德語一樣 一個人只有多讀多寫才能流利地使用這種語言來表達自己的想法

就易於被我們觀察和感知的對象
我們形成一個相當完備的抽象語言系統來描述這些對象的性質
然後在逐步的改進中我們把這個抽象的語言系統的描述對象擴充爲
不易被我們觀察和感知的對象 這就是數學抽象的力量

shapes particularly of nature對於我們來說是本質重要的
shapes particularly in nature對於我們來說是相當有趣的
而自然之力之外全無它物?
數學世界中有一些造物因其純然人爲性而形成精巧的反例
比如一些病態的拓撲空間

形式與功能的內在聯繫:
容易想到的 形式在每個時刻決定功能
同時 功能與能力在長久的時間中經選擇原理而決定形狀

偉大的數學在於其抽象的力量
而計算機科學相反要深入計算的細節

等價關係的定義依我們的興趣而定

# 2 the language of shapes

語言的分支的一種很差的分類方法:
*analysis* 描述 變化(的直覺概念)
*algebra* 描述 結構 指數學結構 即按證明方法分類(範疇論)(布爾巴基)
*geometry* 描述 空間與形狀(的直覺概念)
- 與這些模糊的表述不同
  用範疇論的語言可以把一個數學理論的領域描述的很清晰明瞭
- 但是沒有一個分類方法總是好的

拓撲方法適於對宏觀與微觀世界的描述
而幾何方法適於對人類理解層次的一般世界的描述
- 那些病態的拓撲空間就是出離了人類所理解的一般世界之外的
- 幾何方法的特點在於量的系統更爲豐富
  幾何方法考慮微分流形
  其實現在人們用的基本就是實數與複數連續統

# 3 knots and strings

- 拓撲流形作爲嵌入高維歐氏空間的子流形
  由於在自己的結構之外還繼承高維歐氏空間的結構
  所以有更豐富的性質

unkont
trefoil

- isotopy:
  兩個嵌入的子流形之間的同胚
  並且在保持嵌入性的條件下與恆同映射同倫則稱爲isotopy?

knot在平面上的投影(附帶相交位置的上下關係)描述了knot的全部性質
當knot簡單時我們能從投影形成很好的對knot的直覺感受
但knot複雜時就需要羣論的抽象理論來幫忙了:

Kurt Reidemeister (1893-1971)
給出保持knot不變的 對knot的投影表示的 三個操作
並且證明
這三種對knot的投影表示的變換
可以把一個knot的某個投影表示
變成同一個knot的任意另外一個投影表示
(即這個變換羣形成了knot的一個(通過投影表示得到)的完整的不變量)
(這也是局部操作決定整體性質的例子)
(這也是如何從一些樸素的直觀想像形成數學理論的例子)
就對兩個投影表示是否代表相同的knot而言 這給出了一個判別方法
- 如果能找到變換羣中的一個變換 把一個投影表示變爲令一個
  這兩個投影表示就代表相同的knot
但是對兩個投影表示是否代表不同的knot而言 這並不能形成判別法

Ralph Fox (1913-1973)
考慮coloring(of strand) of knot的投影表示
(對簡單例子 比如trefoil 的考慮很容易讓人想到做色理論)
並定義3-colorable爲滿足如下三條件：
1. knot投影表示的每一段用三色之一着色
2. 所有的三色必須都用到
3. 相交處做色數爲奇數 即1或3
3-colorable是kurt變換羣的不變量
因此這也是knot的一個(通過投影表示得到)的不變量
這個不變量 給出一個判別法 區分分別以unkont與trefoil爲代表元的兩類knot

# 4 creating new knots from old

Lord Kelvin (1824-1907)
以knot爲原子的模型

Niels Bohr (1885-1962)
給出了電子質子中子的模型取代了之前的理論
而string theory把研究的方向扭轉了回來

- knot理論的困難在於人們能夠直觀的其實只是很簡單的knot
- knot理論的重要性除了string theory
  更在於它與3維流形之間的關係 它們的分類問題是等價的
  - 2維流形的簡單而直觀的不變量的存在
    所激發出來的3維流形的分類問題竟然如此困難
    這是值得思考的一點
- 另外knot的補空間的基本羣使得knot與代數相聯繫
- 關於knot的很多性質與操作的定義都與knot的平面投影表示有關
  因爲這具有很強的計算意義
  還因爲這樣的投影揭示着嵌入3維空間中的1維子流形

knots的運算:
相加(composite knot):
在投影表示中不引進新相交位置地將兩個knot融合爲一個只有一種方法
這個運算形成以unknot爲零元的交換半羣
?通過補充定義元素總可以把半羣擴充爲羣嗎?
但是不是羣 因爲並不存在減法(逆運算)這樣的相加只能增加knot的複雜性(用2維輔助流形來證明)
但是可以用不可分解性定義prime knot

knot的不變量(即knot的分類問題):
!!未解問題:knot的良好完整不變量 或knot的分類問題
這裏"良好"通常指算法的性質

crossing number:所有投影表示中最少相交位置的個數
unknotting number:所有投影表示中最少穿越次數使得knot變爲unknot
同時也是把一個knot變成unknot的最少穿越次數 它如何不依賴於投影表示是顯然的
可以說二者從不同的角度度量knot的複雜性
據說二者都很難計算
而其上界都容易由具有某個個數相交位置的投影表示的存在性得出

相關的有概念:
alternating knot:存在投影表示使相交位置的上與下隨弧長而交替變化
reduced 投影表示:不能使相交位置減少的投影表示
定理:
reduced alternating 投影表示 中相交位置的個數就是knot的crossing number

*未解問題*
knot加法下crossing number或unknotting number的值的變化
(同樣這裏上界是顯然的)
未證明的猜想是crossing number或unknotting number也相加!

# 5 DNA entanglement

DNA的雙螺旋結構決定了它作爲分子的性質 與它與其他分子的交互方式

link:knot經``加法''所作成的結構
例:unlink Hopf-link Whitehead-link
Borromean rings:3環去除任何一個另兩個都unlink

同樣有Kurt的對投影表示的3個變換作爲完整的不變量

link的不變量:

linking number:用來度量兩個knot的相link方式(忽略knot各自的結構)的複雜性
需要利用orientation來定義
改變定向將改變符號 因此加絕對值纔得到link的不變量
定義稍微複雜(但是容易計算)
只要證明它在Kurt的3個變換下不變就說明了它與投影表示無關因而是link的不變量

由此引出一個關於knot的量:
writhe:因爲linking number的定義方式同樣適用於單個的knot
在定向的改變下不變 這一點很重要
在Kurt 2 3下不變 在1下增減1
因此這個量度量了Kurt變換1
直觀地即紙帶的扭轉 因爲Kurt變換1將引起紙帶的扭轉
因此這個量是 帶狀knot(例如DNA) 的不變量!

# 6 the Jones revolution

Vaughan Jones (1984):
Jones polynomial:
由下面的X polynomial經簡單的代換而得到

- 考慮一般的環上的代數 那麼一元多項式的向量表示就不足爲奇了

這裏的多項式包含負數次冪者

Louis Kauffman:
bracket polynomial:
由rule(1)(2)(3)來定義於knot的投影表示
1. 圓的bp爲1
2. 考慮一個相交位置
   投影表示的bp被分解爲
   兩個消除這一相交位置的投影表示的bp的按特定係數A B的線性組合
   - 如果多項式能捕捉knot的投影表示之間的關係 那麼knot就可以對應於多項式
3. 投影表示中一個分離的圓項可以作爲特定的係數C而提出
(unlink的unknot項)
接着只要證明證明
1. bp的計算與順序無關
2. 投影表示bp在kurt的對投影表示的3變換下不變 bp就是knot的不變量了
但是其實bp並不是knot的不變量 而與writhe相同是帶狀knot的不變量
其實2不是被證明的而是要求A B C的滿足所需要的方程後得到的
(即要求相容性之後得到的)
首先由kurt2的方程可把B C用A解出
A將是多項式的不定元
kurt3 將自然被滿足
之後與kurt1的相容性將給出兩個不相容的新的方程
因此bp不能與kurt1相容 它是帶狀knot的不變量

X polynomial:這是一個knot不變量
發現writhe(需要定向)與bp這兩個不變量與kurt1的不相容性是可以相互抵消的
- 很實用的技巧 cool!!!

*未解問題*
Jp是否可以區分unknot與knot(類似於龐加萊定理!)

# 7 symmetries of molecules

定理:
X polynomial是knot加法到多項式乘法的同態!!!
因此只要對素knot計算Xp就行了
/?是否是同構?有沒有兩個knot的X polynomial不同的?
//因此knot加法不成羣

考慮分子的手性異構 即空間鏡面對稱
與knot的存在類似 手性異構也在於分子嵌入三維空間的方式不同

knot或link被稱作amphicheiral
如果它能形變爲自己的投影表示在平面上的平面鏡面對稱 即改變相交位置的上下
(這種意義下"手性異構"存在)
命題:
X polynomial作爲knot不變量 當鏡面對稱不同是它可以作出區分
從而形成對amphicheiral的不完全判別
因爲考慮rule(2)就知道 這只不過是置換多項式中的A與1/A
但是存在並非amphicheiral的knot 其Xp在這種置換下不變
這裏的例子要利用signature invariant
!!未解問題:尋找amphicheiral的良好的完整不變量
(可以看作對amphicheirality 即鏡面對稱性的度量)
這裏就體現了Xp作爲不變量的力量
因爲平面上的平面鏡面對稱的其他樸素knot不變量都是相同的

# 8 the messy business of tangles and mutations

想要討論生物學中突變的概念

John Horton Conway 1960+:
tangle:(也是一個局部概念)
knot或link的一部分與一個過這部分4次的圓稱爲tangle
這將帶來兩段在圓內互相纏繞的線
將線頭兩兩相連可以形成knot或link
限制在圓內也可以做kurt的3個變換
抓住兩個相鄰線頭可以作扭轉 這是一個基本操作
右邊兩線頭的扭轉 接着 平面旋轉 接着 平面鏡面反射 接着右邊兩線頭的扭轉
如此循環 這樣的操作序列可以作出的tangle叫做rational tangle
以zero tangle爲起始 對這樣作出的rantional tangle就有簡單的編碼
(有限整數序列 但這種編碼不是rational tangle的完整不變量 即編碼是有重複的
即存在兩個不同的編碼2 1 1 與-2 2 1表示同一個rational tangle)
當有奇數次平面旋轉時 以zero tangle爲起始 等價的有
對右邊兩線頭的扭轉 接着 對下邊兩線頭的扭轉 接着 對右邊兩線頭的扭轉 並循環
當有偶數次平面旋轉時 以infinite tangle爲起始 等價的有
對下邊兩線頭的扭轉 接着 對右邊兩線頭的扭轉 接着 對下邊兩線頭的扭轉 並循環
//bp的rule(2)就是對一個相交位置這種最簡單的tangle而言的
//注意:扭轉的正負由相交位置的上層線段的斜率確定
/而斜率的概唸的給出需要在投影表示中引入直角座標系

定理:(另一個強有力的不變量的給出)
編碼是重複的當且僅當編碼所形成的連分數相等(rational tangle之名如此而來)
這樣就形成了rational tangle的完整不變量
//連分數就像是對數字的交錯的扭轉!!!
因此對於通過將rational tangle的線頭兩兩相連可以形成的knot或link就有了完整的不變量
但是並不能這樣形成所有的knot
因爲儘管所有的knot都可以看成是一個tangle
但是並非所有的tangle都是rational tangle
rational tangle形成的rational knot or link一定是alternating knot or link
//用這種編碼對knot(儘管是一部分knot)的描述正是人們所追求的
/它展示了數學如此語言明顯的是創造語言的學問

tangle的運算:
加法:左項的右邊兩線頭連接到右項的左邊兩個對應的線頭
乘法:平面旋轉與平面鏡面反射之後再相加 因此編碼本身就是乘法
這里加法是交換的結合的 而乘法是不交換的不結合的 且沒有分配律
由rational tangle經這些運算所形成的成爲algebraic tangle
對應的有algebraic knot or link
zero tangle就是這里加法的零元
infinite tangle就是這裏乘法的左單位元但是不是右單位元
////對於knot與link與tangle的運算
///我的用通過解函數方程來構造運算
///或者說明可能存在的運算的方法有什麼用嗎?

回到生物學中突變的概念這個動機
mutants of tangle:
指一個tangle的就平面直角座標系的兩個座標軸所形成的三個平面鏡面反射圖形
knot的一次突變就是其中一個tangle的突變
例如:Kinoshita-Terasaka mutants
?未解問題:如何描述knot在突變這種局部變化下的整體變化
生物學中類似的突變也有這樣的難點

# 9 braids and the language of groups

Chen-Ning Yang 1968:
quantum field theory 亞原子級別
Rodney Baxter 1971:
statistical mechanics 分子級別
得相同的方程
Yang-Baxter equations
而這個方程可以用braid來展示

topological quantum cryptography中braid用來捕捉計算機的邏輯結構

braid是很多條線繩(strands)在兩個木條間連接兩個木條 所形成的幾何構造
(如果不是木條而是木板 那麼幾何構造就樸素了)
約定它的投影表示中(如果以上面的木條爲起始)線繩只向下走不會轉回向上
與tangle類似 在投影表示中也以作kurt的3變換 只要限制在兩木條之間就行
可以通過把兩木條上對應連接點的線頭相連而得到knot或link 美其名曰closure
定理:(James Alexander)
所有的link都可以這樣通過braid來構造

//嵌入3維空間中的1維子流形還能構成什麼樣的有趣結構呢?

現在爲braid來創造一種適合與描述它們的語言 正如對rational tangle所做的一樣
這在於找可以把簡單的braid轉化成複雜的操作 然後找到基本簡單的braid
這樣我們就遇到了 組合羣論 或者說有限生成羣論
即有n條strands的所有braid是用有限生成羣來編碼的
用羣的字來編碼顯然是有重複的
而有限生成羣的三組關係將消除這種重複性
兩組對應於kurt的後兩個變換(對應與kurt2的是逆元的定義)
另一組是圖形上不相互干擾的兩生成元的交換性
(由上面的定理知 這也形成對所有knot與link的編碼
但是這並不形成knot的良好的不變量
因爲想要把兩個knot或link作爲braid來相加
必須先指明它們是作爲哪類braid來相加
即必須先明確想要把它們視爲是共同在哪個羣中)

# 10 Platonic solids and Euler's masterpiece

tetrahedron (3 3) [4 6 4]
cube (4 3) [8 12 6]
octahedron (3 4) [6 12 8]
dodecahedron (5 3) [20 30 12]
icosahedron (3 5) [12 30 20]

技巧:多面體的三角剖分
//多面體是對曲面的近似表示

fullerenes:
一類分子
每個碳元素都有三個化學鍵與其他三個碳元素相連
而形成的只有五邊形pentagon和六邊形hexagon組成的多面體
定理:
它們每一個都一定只有十二個五邊形(例如正12面體)
這很容易由Euler公式所形成的方程中解出
(例如 足球 高爾夫球)

# 11 surfaces and a new notion of equivalence

這裏曲面指嵌入3維歐氏空間的有界2維流形(2維緊流形)
直觀的說 它有界 且其中任意點的局部是開平面片

clothesline trick (the power of isotopy)

這裏也是 嵌入子流形
且在homeomorphism下(而不在isotopy下 那據說很複雜)
(子流行的homeomorphism變換可以更具體地看作是先任意切開 並記住切開的方式
再對每部分作isotopy變換 即橡皮變換
然後一定再沿原切口粘回去)
分類問題很簡單(更粗略的分類)
而且與knot相比其度量性質更加豐富
(homeomorphism與嵌入無關因此是intrinsic而isotopy是extrinsic)


下面考慮曲面在homeomorphism下的不變量
(每次提到不變量時當然需要強調這個不變量是在什麼變換下不變的
之前是isotopy而現在是homeomorphism)

定理:

Euler characteristic 可定向性 邊界個數 這三者
在同胚下形成曲面的良好的完整不變量 良好的分類
技巧:通過曲面的三角剖分來計算Ec(這是局部性質概念)
需要證明對同一個曲面的計算結果與所選取的它的不同的三角剖分無關

# 12 reaching boundaries and losing orientations

所有曲面可以通過將偶多邊形的某些邊等同而構造出一類模型
(可定向曲面用4的倍數多邊形)

這裏引入帶邊流形的概念 因爲如上所示它們是很有用的

然後是曲面的可定向性的概念(一個整體概念)
(帶邊界的或不帶邊界的都可一致地定義此性質)
//考慮格拉斯曼的方法就知道
/可定向性對於得到用來計算面積的良好公式是很必要的
/因此對於積分 可定向性也是重要的
(注意無邊界的不可定向2維流形不能嵌入3維歐氏空間)

# 13 knots and surfaces

//3維歐氏空間的1維和2維嵌入子流形 通過一個是另一個的邊界而聯繫起來

利用對曲面的完全分類來研究knot

已給knot求一個可定向曲面 使其邊界爲給定的knot
同胚的曲面在空間中的安置方式不同就可以有不同的knot爲邊界
Herbert Seifert (1907-1996)
Seifert algorithm完全解決上面所說的問題
很簡單且直觀的 關鍵是要保持着色的一致性
算法的輸入是knot的任意一個投影表示
先解除所有的相交位置(這需要先給knot定向)
將獲得分離的與圓盤同胚的區域
(每個區域的不同着色由它們繼承knot的定向的環繞方向不同而區分)
(當它們在投影表示中嵌套 它們在空間中的安置就重疊)
然後在用扭轉的帶子來把相交位置加進來
因爲一個相交位置扭轉一次所以獲得可定向性

現在把所求得的可定向曲面的邊界用一個蓋子消除
這樣的到的無邊界的可定向曲面的虧格是否是knot的一個不變量?
對於由S算法到的曲面可用knot的投影表示的相交位置的個數和圓盤狀區域的個數來計算虧格(即用一個蓋子消除邊界後的曲面的虧格)
同時這也出了對證明它是一個knot不變量的指示
即證明這個量在kurt的3個變換下的不變性 但是發現在kurt變換下虧格是改變的
所以它不是一個knot不變量
而knot的虧格可以定義爲以它爲邊界的所有曲面的最小虧格數
只有unknot的虧格是0 但存在不同的knot有相同的虧格 虧格不是knot的完整不變量

定理:(Yoav Moriah 1987)
可給出一個knot 它的最小虧格曲面不能由S算法作用於任何knot的投影表示而得到

定理:
knot的虧格是knot的加法到自然數加法的同態
這就展示了knot的加法只能使knot的複雜性增加 沒有knot減法

knot的虧格有時可以用來探測knot的突變
因此這是一個很強的不變量

# 14 wind flows and currents

這樣就遇到了向量場 因爲描述變化 所以按這裏的分類 我們進入了分析的領域
analytic topology
當然經典的結果是
無邊界有限曲面上向量場的奇點指標和等於歐拉示性數
介紹了William Thurston的證法
首先利用多邊形給出了向量場奇點指標的一個離散性計算方法
然後利用這個計算方法並考慮曲面的一個有很多三角形的剖分
就能給出一個能很好的揭示示性數與奇點指標和關係的證明
即給剖分所得的 點 邊 面 分別標上 +1 -1 +1 它們的和當然是歐拉示性數
考慮向量場所能形成的流對這些標記的推動
按向量場奇點指標的離散計算方法就知道它們的和是奇點指標和

# 15 curvature and Gauss's geometric gem

Gauss-Bonnet theorem
多面體頂點處的曲率是周角減去面上的角的和

# 16 playing with scissors and polygons

現在討論discrete geometry
這裏的看法是 離散 可以給出對 連續 的近似
比如面體對曲面的近似
這對於計算機來說是重要的因爲 離散 的數據正是計算機的操作對象
//我想 離散 的重要性一定超出着這裏的看法
/考慮p-進制數域 它們是離散通過近似所構造的另一個數域
/也就是說離散可以近似不同的連續

scissors congruent:一種多邊形之間的有趣的等價關係
可以用來樸素地定義多邊形的面積
對於在計算機中實現多邊形的模型而言這也是很有意義的
定理:(Bolyai-Gerwien theorem)
兩個多邊形有相等的面積當且僅當它們scissors congruent
這個有趣的定理有很多有趣而簡單的證明
(所有的證明中都要用到多邊形的三角剖分
證明三角剖分的存在 只要證明每個多邊形都存在一條對角線
這也需要一個有趣的證明 其實是用一個算法找到的)
它說明 多邊形的面積是多邊形在scissors congruent下的完整不變量
但是多面體卻沒有類似的定理
(這是一個希爾伯特問題由他的學生Max Dehn解決
Dehn-Hadwiger theorem:
因爲多面體有兩種角face angle and dihedral angle
如果一個多面體的dihedral angle除以pi之後都是有理數
而另一個多面體的dihedral angle除以pi之後得到一個超越數
則它們不可能是scissors congruent的
(這是這裏的教程中所展示的定理的一小部分 而定理的良好的完整敘述如何呢?)

# 17 bending chains and folding origami

knot和link的離散版本是
嵌入3維歐氏空間中的1維子空間的bending chains
同樣也可給出嵌入3維歐氏空間中的2維子空間的folding origami

# 18 Cauchy's rigidity and Connelly's flexibity

# 19 mountain terrains and surface reconstruction

# 20 Voronoi's regions of influence

# 21 convex hulls and computational complexity

# 22 patterns and colors

複雜性無疆界 但是數學上的簡單性卻是有自然的極限的

平面上的區域 和 圖的點 的着色問題
這是一個拓撲問題 因爲它之考慮relative position
/?局部上的平面上的區域的4色問題是很簡單的
/但是卻沒好有辦法把這個局部性質過度到整體

命題:
每個地圖上至少有一個國家 有5個或更少的鄰國
這單純的是平面的一個拓撲性質 很容易經反證發證明由Euler公式
由這個命題經反證法就得到6色定理

# 23 Orange Stackings and Bubble Partitions

# 24 The Topology of the Universe

Universe 有界但是沒有邊 如球面

# 25 Tetrahedra and Mathematical Surgery

- p q curve on torus
  and how to glue two tori

- p q lens-space
  glue maridian to of one torus
  to the p q curve of the other torus
  - note that
    there are different ways to construct lens-space

- glue general handlebody
  heegaard splitting

- Dehn surgery

# 26 The Fundamental Group
# 27 Poincaré's Question and Perelman's Answer
# 28 The Geometry of the Universe
# 29 Visualizing in Higher Dimensions
# 30 Polyhedra in Higher Dimensions
# 31 Particle Motions
# 32 Particle Collisions
# 33 Evolutionary Trees
# 34 Chaos and Fractals
# 35 Reclaiming Leonardo da Vinci
# 36 Pushing the Forefront
