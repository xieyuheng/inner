# Neuroscience Note / 神經學記

## Stomatogastric nervous system

### 題解

- 螃蟹的咀嚼系統的神經系統
  lectures by eve marder
- understanding circuit dynamics
- lessons from small rhythmic circuits : variability
- compensation modulation homeostasis
- 顯然的以小見大之研究
  問題是
  螃蟹的咀嚼系統的神經系統中數量極少的神經細胞
  何以產生複雜的咀嚼行爲

### Introduction to central pattern generators (CPGs)

- 中央模式生成子 中成子
  就像一個高階函數(組合子)一樣
- rhythmic movements 對應 rhythmic circuits
  比如 呼吸 行走(螃蠏之橫行)
  產生這些行爲之神經電路
  稱爲 中央模式生成子
- 最理想的用來做觀察的對象是
  crustacean stomatogastric nervous system
  甲殼動物 口胃的 神經系統
- 就像人之心電圖之節奏一樣
  簡單的動物也有類似的 具有節奏的
  生物電信號可以被記錄
  這樣真正記錄下來的數據稱爲 vivo
- 所測量的是多種神經細胞
  所得到的是多組數據
- 一種實現方式是
  在不破壞(不嚴重破壞)細胞的前提下
  把 甲殼動物 口胃的 的 神經細胞
  分離出來 到培養皿裏
  然後通電觀察
  (分離出來的神經細胞就容易觀察很多)
  結果發現也能記錄到類似的數據
  這樣虛擬條件下記錄的數據稱爲 vitro
- 這種在與生物體分離之後
  神經細胞還繼續產生的就被成爲
  central pattern generators
  (上面所述的並不是一個嚴格的定義)
- 通過 對逐對地 兩類神經細胞一組地 測量其電信號
  可以畫出不同類型神經細胞之間的鏈接方式
  (注意 甲殼動物 口胃的 神經系統 之神經細胞總數非常少)
  即畫出一個電路圖來(connectivity diagram)(connectome)
- 但是這個 電路圖 所包含的信息
  不足以讓我們推測出
  (必要 但是不充分)
  所測量到的電信號之模式(circuit dynamics)
- 回到 stomatogastric nervous
  螃蟹的不同的咀嚼狀態之間的變化
  映射到電路上來
  就是給基本的節奏上加上其他的節奏
  * 也就是說a電驢的帶寬被復用了
- pyloric pylorus neurons
  (控制幽門的開閉)(幽門通向下體)
  gastric mill neurons
  (控制胃之研磨)
  其二者之間有複雜的聯通關係(聯通度很高)
- 震盪的電信號的產生
  1. 單個細胞獨自的膜點位的變化
  2. 多個細胞形成的回路
     這種回路產生的電信號
     通過把兩個培養皿鏈接起來
     就能模擬出來
- 也就是說基本的信息是通過節奏來編碼的
  不是沒有節奏的 bit 流
- 關於所觀測到的電信號之中的 不變量
  發現 週期不穩定(通過統計數據的概率分佈來得出結論)
  而峯值(激發態)所持續的時間(phase delay)
  與週期的比值是穩定的
  這個不變量稱爲是(phase relationship)
- 解釋
  phase constancy is not an automatic property
  of networks with complex active conductances
  it is so only for
  network activity with perfect compensation
- 如果是信道的復用
  那麼接受信息的細胞就一定還需要濾波?

### 記

- 在什麼方式與意義下
  簡單者可以得以複合而形成複雜者
- 如果只有三十多個神經細胞在相互作用以完成其功能而已
  我就很容易在 erlang 之類的語言之中模擬它們
- 難點在於每個細胞都應該被視爲一個小計算機
  有相當多的計算機在並行運算
- 一個隨時間不斷演化的電路
  爲了觀察這個模型
  可以在模擬中添加機制
  使電路的狀態能夠被輸出
  以被觀察
- 與一個等待輸入的靜止的機器不同
  這個機器羣總是在運動
  只不過其運動方式會被輸入改變而已
- 我現在沒有時間設計這些
  但是在最開始我就應該給蟬語的語義以能力
  去把每個函數都變成一個機器
  或者把一組相對獨立的函數當成一個機器的能力

### Neuromodulation

- neuropeptides
- circuit reconfiguration
  the temporal dynamics
  and modulatoty environment
  construct the functional circuit
  by tuning functional synaptic strength
  and modulating neuronal excitability
- conundrum

### Homeostasis

### Intra-animal variability

## The crustacean stomatogastric system
