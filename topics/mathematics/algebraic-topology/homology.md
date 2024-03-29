---
title: Homology
---

# >< homology

- 如果能形成一個高階代數理論的話
  universal-covering-space 推廣到 branching-cover
  可能能夠讓我們解決高階代數的基本問題

  這就需要明白 universal-covering-space 是如何解決曲面的基本羣的基本問題的

- homology 與反物質
  是否能夠就非 simplicial-complex 形成 homology theory

  不可定向的閉曲面是有邊界的
  只是這個邊界是反物質
  我們看不出來

  所謂 反物質 是不自然的
  因爲在一階情形 並沒有這種現象
  在 一階 情形 定向很容易表示
  定向與逆是相同的概念

- 如果想要使得 homology 的 chain 是 at1 的 chain 忘記 glue 細節
  那麼 at1 的 chain 就必須能夠區分 (1 a b c) 與 (1 c b a) 之間的差別
  我想這與 '直接就 polyhedron-complex 形成 homology theory' 這個問題是一樣的
  因爲如果能的話
  我們就一定能給 polyhedron-complex 以定向

  也就是說 給以定向
  就是 '直接就 polyhedron-complex 形成 homology theory' 的關鍵
  [對於 polygon 形成定向的方式很簡單]

  polygon glue 的時候 要通過代入來消除公共邊界
  代入的時候 可以變換某個邊界表達式
  比如 (1 a b c) 可以變爲 (1 b c a)
  但是變換方式是要受到限制的
  比如 (1 a b c) 不能變爲 (1 c b a)

  我們必須給以明確的規則來描述 代入以消去的方式
  並且 還要描述 所允許的[代入之間的]變換

  一階邊界的等式理論 對應與 二階元素的 glue
  二階邊界的等式理論 對應與 三階元素的 glue
  如何描述 二階邊界的等式?

  是否也可以說
  零階邊界的等式理論 對應與 一階元素的 glue
  零階邊界是點對 (0 a b)
  定向之相反 (0 a b) 到 (0 b a)

- x -
  當使用 simplicial-complex 的時候
  是以一致的方式來描述各個階元素的性質的

  但是當考慮我們所謂的高階代數的時候
  零階元素和一階元素是特殊的

- k -
  如此一來一切都說的通了
  但是可能要重新設計語法了

- x -
  並且我們現在的目標很明確
  那就是 要發展 polyhedron-complex 的 homology 理論
  並且說明 homology 是我們的高階代數忘記 glue 信息而得到的交換代數

# homology

- x -
  homology is to ignore how gluings are down in homotopy,
  higher homology is abelianization of higher algebraic structure.
  而 我所要定义的高阶代数
  将给出 三维拓扑空间在同伦等价意义下的完整分类

  就高維代數結構而言
  homology 忽略了類型信息
  主要問題就是
  如何在信息不減的同時
  簡化代數結構
  但是 homology 忽略了太多信息

  homology theory 像是我們高階代數的類型系統
  它可以用來檢查我們的運算中所可能出現的簡單錯誤

- k -
  注意
  同調羣中 每個元素只有兩種定向
  相加的方式只有兩種
  但是在我們所設想的高階代數中並非如此
  因此所謂的 abelianization
  其實忽略了很多結構

- x -
  當把一個 n-disk 的邊界誒 (n-1)-sphere attach 到一個 (n-1)-sphere 上時
  n-disk 可以出現在這個 (n-1)-sphere 的內部或者外部
  我想這種定向所區分的就是這兩種情況

  這樣在我們的語言中每個元素也要有一個逆

- k -
  但是在我們的語言中 已經有一種逆的概念了
  那就是 同位自乘

- x -
  等等
  敘述 attach 的時候
  我們真的需要定向嗎?

  可能根本不需要
  而 homology 中所使用的定向也不是爲了 attach
  而是爲了說明邊界

  [問題]
  給出一個按照古典方式定義的 simplicial-complex
  找出它的帶有命名的表示
  以明瞭邊界

- k -
  homology 中的定向 與整個圖形的可定向性有關
  比如就曲面而言
  偶置換所形成的等價類
  就代表了一個定向的 circle

- x -
  homology 中的定向使得我們能夠定義邊界
  並且邊界的邊界爲空

  而在我們的語言中
  邊界的邊界爲空是在定義空間的時候
  利用一個檢查器來保證的
  - 檢查器還檢驗了
    n 階生成子的邊界是 (n-1)-sphere

- k -
  引入 chain 的概念 即 (+ ...)
  試試取 boundary of chain

- x -
  在構造一個 chain 的時候
  每增加一個新的元素 都要能明確指出其邊界
  甚至我想的是 明確指出邊界
  然後讓解釋器自己尋找 增加新元素的方式

- k -
  那麼 boundary of boundary of chain 是否總是空呢?

- x -
  形成 chain (+ A B) 的過程可以看成是 connected-sum
  並且當 A B 有的所有的公共邊界都 connected 了
  [而 connected-sum 只是 connect 一個邊界]

  - 其實不能看成是 connected-sum
    而應該說是 amalgamate

  當 A 不等於 B 時這是無所謂的
  但是當形成 chain (+ A A) 的時候
  就需要指明位置了

- k -
  但是 A 經過適當的細分之後 就不會有這種情況了

- x -
  A 經過適當的劃分之後
  不同位置的 sum 的差異 就被明顯的體現出來了
  因而不用指出了

  經過充分的細分
  可以簡化 complex 所形成的代數
  使得我們不用關心 sum 的方向
  但是 這將使得 group 變成 groupoid

  這種所謂的簡化
  與 normalization 正好相反
  因爲 normalization 是把 groupoid 變成 group

- k -
  我想現在我們需要確信
  我們對空間的定義確實能夠使得我們計算同調羣
  並且所得到的結果與 simplicial-complex 一致

-----

- x -
  simplicial-complex 考慮的是 simplex 嵌入某空間
  因此不會出現自交的情況
  因此邊界不會有重複的命名

  但是重複的命名 正是 normalization 所需要的

  當邊界出現重複的命名時
  就有可能在形成 chain 的時候 自我相加
  此時 需要考慮 邊界的位置信息

  正是這種自我相加
  使得我們的高階代數中有 逆 的概念

  但是我猜測這種 逆 的概念
  與空間的可定向性無關

  空間的可定向性
  需要計算同調羣才能知道
  在我們的語言中也能計算同調羣

  如果同調羣是忘記我們的高階代數中的結構而得到的
  那麼同調羣中元素的符號還在
  這些符號是高階代數中的什麼信息?
  這些信息看來並沒有被忘掉

- k -
  一階同調羣是 fundamental-group 的 abelianization
  但是並不是 fundamental-groupoid 的 abelianization
  我們要計算一些沒有經過 normalization 的曲面的 一階同調羣 試試

------

- x -
  同調有很多解釋方式
  比如 用剪刀和鑽頭
  比如 嵌入曲面中的 circle 的同調類

------

- x -
  在高階代數中
  只有當兩個元素有兩個以上的公共邊界時
  它們才能夠以不同的方式相稱

  並且
  看來只有當考慮元素自乘的時候
  這種不同的方式之間的差異才有意義

  但是在 homology group 中
  形成 chain 的時候一個元素可以以兩種不同的定向出現

  這種東西看來不可能是由高階代數忘記某些信息而得來的
  因爲它包含了高階代數所不具有的信息

- k -
  除非我們把定向信息加到高階代數中

- x -
  我的感覺是 高階代數本身的有效性是值得質疑的

- k -
  可以說當有了充分的細分的時候
  就不需要位置信息了
  但是高階代數本身的有效性是沒有問題的

- x -
  爲什麼說形成一個高階代數的理論
  就可以幫助我們理解 underlying-space?

  由曲面得到了 group 之後
  我們需要證明這些 group 不能同構
  我們有代數的方法來做這樣的證明
  [問題 具體是如何做的?]

  - 是通過交換化而形成 homology group 來證明基本羣不同構的

  但是當有了高階代數的展示之後
  如何證明不同的代數結構是不同的呢?

  我想這就是我們需要發展一個代數結構的原因

  - 可以用類似交換化的方式找到簡單的不變量
    方法沒必要侷限於交換化
    只要能形成簡單的不變量就行

# simplicial-complex

- x -
  simplicial-complex 的很多 simplex 在劃分某個空間的同時
  還要求每個 simplex 都嵌入在空間中
  因此 考慮命名的話 一個 simplex 的邊界就不能有重複命名
  這就需要 空間被充分細分
  此時考慮高階代數中的 chain 的話
  就不需要考慮某個 simplex 自我疊加時的複雜情況
  此時 homology theory 中的 chain
  與高階代數中的 chain 的區別就在於
  後者要求順序信息 而前者忘記了順序信息
  這樣 abelianization 這個詞就是有意義的了

  使用 simplex 而不使用一般的 polyhedron
  可能是因爲要給 chain 中的元素以定向

- k -
  對於 polyhedron 也可以定義定向

- x -
  我們是否需要給高階代數中 chain 的元素加以定向信息?

- k -
  可以看看定向信息在 simplicial-complex 有什麼用

- x -
  定向信息 使得在求 chain 的邊界時
  內部的邊界能夠被相互消除

  但是在高階代數中
  形成 chain 的時候 內部的邊界就已經被相互消除了
  因此看來就不需要定向的概念了

- k -
  如此看來高階代數中的 chain 就帶有很多信息
  chain 中元素的順序是固定的
  其順序就是形成 chain 的時候 逐次添加元素的順序
  並且 除了這個順序之外
  每次添加元素還要記錄當時的邊界
  每一步都記錄邊界信息
  那麼就能夠直接給出最終的 chain 的邊界了
  - 從計算機實現的角度看
    高階代數的數據結構更豐富 計算 chain 的邊界的時間複雜度是常數
    而 homology theory 中的數據結構信息較少
    計算 chain 的邊界的時間複雜度不是常數

  [問題]
  我們是否能夠從這些信息中恢復定向信息呢?
  如果能的話
  homology group 就可以看成是高階代數的信息弱化[交換化]

- x -
  我們看不出應該如何恢復定向信息
  因爲 homology group 中的 chain 可以是不聯通的
  但是我們所想象的 高階代數的 chain 中每一步所得到的 中間 chain 都是聯通的
  因此我們就不能想象 A 可能同時與 帶有相反定向的 +B 和 -B 相加而構成 chain
  或者說
  我們只能想象邊界確實被相互消除了的 相加
  而不能想象同一條邊界被累積起來的 相加

- k -
  如果是爲了把 homology group 看成是 高階代數結構的 abelianization 的話
  還有一種方法
  那就是 限制 homology theory 中的 chain 的形式

- x -
  但是這顯然是不合理的
  這是違背 homology theory 中 chain 的無序本質的

- k -
  那麼我們就必須給 高階代數中的元素加上定向信息

- x -
  如果這麼加的話
  好像每個元素都有一個反元素了?

------

- x -
  或者說
  就算是爲了說明 abelianization
  我們沒有必要給高階代數中的元素加上額外的定向信息
  因爲 那些在 homology group 中
  由於定向相反而相互消除的元素
  在形成高階代數的 chain 的時候都已經被逐步消除了

- k -
  對於高階代數的 chain
  如何說明其邊界的邊界是空呢?

  好像這個性質可以作爲一個定理被證明
  而不需要一個檢查器來保證這個性質

- x -
  反物質 也許是正確的解法
  考慮不可定向曲面的邊界試試

- k -
  但是如果使用反物質這個概念的話
  如何解釋一階的情形?
  如何解釋 first homology group 是 fundamental-group 的 abelianization?
