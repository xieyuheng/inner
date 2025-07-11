---
title: foundations of mathematical logic
author: haskell curry
year: 1963
---

# 1 introduction

## 題解

* 討論什麼是數理邏輯
  這裏所遇到的難點是之後使用形式化方法的原因

## the natural of mathematical logic

* 數理邏輯
  即從數學角度去研究的邏輯
  或爲數學的目的而去研究的邏輯

* 這就要求我們回答
  什麼是邏輯
  什麼是數學的角度

* 所謂 邏輯是對思維的分析與批判
  這在於
  我們在觀察中總結關於自然的一般規律
  用以預判未來之事
  我們以命題的方式做這種總結
  我們總說 在什麼條件下有什麼結論
  然而我們發現
  有些命題正確 它們是可靠的知識可以幫助我們預判未來之事
  然而
  有些命題錯誤
  我們還發現
  依照某些範式所推演出的命題是可靠的正確的命題
  對這些推演範式的研究 被認爲是哲學一個分支
  我們稱之爲 哲理邏輯 以與其他類型的邏輯研究相區別

* 在 哲理邏輯 的研究中我們發現數學方法是很有用的
  即 去構建數學結構[mathematical systems]
  這些數學結構是與我們所感興趣的研究對象相關的
  這種研究自然是數學的分支
  我們稱之爲 數理邏輯

* 邏輯一詞 還有其他廣泛的意義
  我們可以用 幾何一詞 作以類比
  研究詞源 我們發現 幾何是關於空間的科學 它可以被爲物理學的分支
  同時做爲數學的分支 我們又建立數學結構來研究幾何學
  這樣 數學中 幾何學 之於空間
  正如 數學中 邏輯學 之於思想
  是爲類比

* 儘管我們做此定義
  但是 對學科的邊界的劃定是沒有必要
  與其如此 不如敘述學科的中心思想與目的
  就我們的邏輯學而言
  其主要目的 即研究思想本身

* 數理邏輯還包括了對數學基礎的研究
  這在於
  因此數學是推演的科學
  什麼是嚴格的證明
  什麼是數學真理
  以及數學的意義本身
  都是數理邏輯研究中
  所需要回答的問題

## the logical antinomies

## the nature of mathematics

* 以絕對確然爲數學之特徵與目標
  不盡然是關於數學的良好態度
  理論之用才重要
  在物理學中
  一切理論皆依假設而存
  當它可用於做預測時就使用
  當發現它的錯誤時就更改
  當有更好的理論替代它時就拋棄
  同樣
  形式系統之用才重要
  發現其不一致性就修改
  Gödel 的定理表明 這是我們最多只能做到如此
  經驗主義哲學則建議 我們應該持有如此態度

* 一致性固然重要
  然而在不知道一致性之缺失之前
  我們仍然可以使用不具一致性的理論推演有用的結果
  正如歷史做展示的

## mathematics and logic

# 2 formal systems

## 寫作風格

* 少用圖示而多用文字描述
  有時用簡單圖示很容易說明的東西
  作者也用文字來描述
  正如用文字來描述如何折紙鶴
  是非常不好的風格

* 正文中缺少例子
  也許意圖是在習題中給出例子
  在我看來這是不好的風格
  原因在於
  把材料推入習題中
  就是把組織材料的責任拋棄了

## language

* 考慮程序語言時很多所謂技術處理都沒有必要了

* 關於形式系統的術語
  明顯是爲謂詞演算而設計的

* U language
  the language being using
  這顯然是一個沒有一致性的語言
  但是
  既然使用一個語言之前 已經不再追求它的一致性
  那麼 對這個語言的使用 就得到了辯護
  作者認爲
  既然所有的形式語言都必須用自然語言來描述
  那麼 U language 的特殊性就在於
  所有其他語言都嵌入其中
  但是
  考慮程序語言的實現
  我可以說作者的思維是受歷史條件限制的
  因爲做爲形式語言的程序語言
  其描述並不依賴與自然語言

* 形式語言的構成
  * alphabet 取冪集
    而得 expression [word]
  * 語法所生成的 expression 的子集
    稱爲 phrase
  * phrase 分類
    1. noun -- object
    2. sentence -- statement of theory
    3. functor -- combining phrases to form other phrases
       進而有分類
       1. operators -- which combine nouns to form other nouns
       2. verbs [predicators] -- which combine nouns to form sentences
       3. connectors -- which combine sentences to form other sentences
       4. subnectors -- which form nouns out of sentences

## effective process & definite question & conceptual class

* effective process
  就是在有限步內終止的計算 [sequence of transformation]
  admissible element
  就是計算可以適用的數據類型

* definite question
  可以用 effective process 來回答的問題 [計算的結果是 bool 值]
  admissible statement
  問題適用的 statement 類型
  進而有分類
  1. definite question
     if there is such an effective process
     which is applicable to every admissible statement
  2. semidefinite question
     if there is an effective process which is applicable
     whenever the admissible statement is true

* conceptual class
  conceptual 這個定語只爲區分之後對類似概念的定義
  分類
  1. definite class
     元素的所屬問題爲 definite question
  2. semidefinite class
     元素的所屬問題爲 semidefinite question
  3. inductive class
     這將用來定義 deductive theory
     即 由推演規則所形成的 對元素的所屬問題的判別
     作者嘗試給以定義 但是在我看來沒有必要
     需要注意的是 蘊含規則的引入 [對應於函數作用] 使得構造中的信息減少了
     注意
     inductive class 之 definite 與否
     就是 對命題的真僞的計算 是否可以在有限步驟內終結

## construction of element of inductive class

* 一個 construction 是就某個 inductive class 中的元素而言的
  這就是說
  判別元素是否屬於 inductive class 的問題[即 謂詞] 具有生成性

* 進而 inductive class 有分類
  1. monotectonic
     若其中每個元素有唯一 construction
  2. polytectonic
     有某些元素有多個 construction

* 每個構造對應一個 tree diagram
  proof tree 即爲此類
  用 term 記錄 proof 就得到 curry-howard iso

* construction sequence
  可能是爲 sequence 演算而準備的概念

## theory

* 設一 definite class
  其元素稱爲 elementary statement
  elementary 這個限定詞 只爲區分之後對類似概念的定義
  比如在 U language 中的 epistatement

* 一個 theory 是某個 elementary statement 的集合的子集
  其元素稱爲 elementary theorem
  一個 elementary statement 就某 theory 而言爲真
  它即爲這個 theory 的 elementary theorem
  注意
  elementary statement 之真僞
  只是就某 theory 而言的
  進而有重要的分類
  1. decidable theory [關於計算]
     如果 theory 是 definite class
  2. deductive theory [關於計算]
     如果 theory 是 inductive class
  3. consistent theory [關於語義]
     如果 theory 不包含全部的 elementary statement
     即 不是所有 elementary statement 都爲真
     就 謂詞演算[predicate calculus] 而言
     由其特殊的推理規則知 contradiction 蘊含 inconsistent
     另外顯然有 inconsistent 蘊含 contradiction
     但是
     contradiction 這個概念 只有在有 否定[negation] 語義時才能使用
     而如上對 consistent 的定義則更一般

## deductive theory

* axiomatic statement [axiom] -- initial element
  deductive rule [rules of inference] -- mode of combination
  premise -- deductive rule 的前提 [即 參數]
  postulates -- rules and axioms together

* 就 deductive theory 而言的一個 construction
  稱爲 formal demonstration [demonstration] [deduction] [proof] [derivation]

* completeness
  一個 deductive theory
  不變 推演規則的條件下
  若 增加一個不是其 elementary theorem 的 elementary statement
  這個 theory 就變得 inconsistent
  則稱
  這個 deductive theory 爲 complete
  即 不能在保持一致性的條件下對公理進行擴充的理論 就被認爲是完備的
  這是個很強的件
  命題演算[propositional calculus] 滿足這個條件
  但是其他很多重要的形式理論都不滿足這個條件

## consequence relation
## interpretation of theories

* interpretation 是形式體系與語義的對應關係
  比如某個形式的物理理論中的某些 statement
  可以被解釋爲 物理現象 而被實驗檢驗
  而某些 statement
  可能並不對應有意義的物理現象
  或者並不能被實驗檢驗

* 假設所對應的 contensive statement [內容陳述] [語義陳述]
  有額外的規則可以判斷其真僞
  那麼
  可以定義 interpretation of theory 的如下性質
  1. valid
     每個 elementary theorem 的解釋 [即 所對應的 contensive statement]
     都爲真
  2. adequate [relatively complete]
     一個 elementary statement 的解釋爲真
     那麼這個 elementary statement 就是 elementary theorem
     即 它就 theory 而言也爲真
     即 它就在 theory 中
  上面這兩個定義對應於沒有被解釋的 theory 的
  consistent 和 complete 這兩個性質

* 注意
  形式體系與模型之中 判斷真僞的標準不同

* 需要強調的是
  理論之用最爲重要
  而用性是就某一目的而言的
  我們對形式理論的取捨又有美學的考量
  這些討論已經出離數學之外了

## system

* 用集論把這一節的 system 理解爲 我所謂的 [具體]數學結構 即可
  在我的理解方式下
  形式系統是做爲具體的數學結構的
  而一個謂詞
  可以在 形式系統中處理
  也可以在 數學結構中處理
  當做這種處理方式的轉變時
  形式系統 和 數學結構 都將變化
  對整個結構的理解方式都變化了

* 例如 把等號在形式系統中處理

* 例如 用 term 來記錄 proof 時
  此時所生成的表達式已經包含了對謂詞的判定了
  因爲
  如果一個 term 術語某個 命題 這個命題就是真的

# 3 epitheory
# 4 relational logical algebra
# 5 the theory of implication
# 6 negation
# 7 quantification
# 8 modality
