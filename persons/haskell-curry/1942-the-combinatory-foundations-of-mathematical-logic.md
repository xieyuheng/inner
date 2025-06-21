---
title: the combinatory foundations of mathematical logic
author: haskell curry
year: 1942
---

# introduction

* 在研究數學基礎方面
  區分兩種趨勢
  1. 徹底的形式化
     即 儘可能精確地描述
  2. 簡化
     儘可能化簡所獲得的形式體系
  第一種趨勢下所獲得的形式體系可能多種多樣
  而簡化 可以幫助理解各種形式體系之間的差異與關係

* curry 以 數學分析的一致性 爲例子
  形式體系越簡單 其表達能力就越弱
  就越容易處理與分析 數學分析的一致性 這類問題

# variables

* 使用有名局部變元就能描述很精緻的結構
  但是所獲得的形式系統很複雜
  這在於
  有名局部變元 其實是一種標記語言
  利用 局部變元值名字 做標記
  就可以描述有向圖
  也就是說
  在 有名局部變元 這種技術的幫助下
  用相對線性的文本
  就可以描述極端非線性的複雜結構

* 如果不允許對把變元代入嵌套在裏層的函數體或其他結構話數據
  那麼蟬語就反對濫用有名局部變元

# some preliminary explanations

* 本文的目的是構建一個沒有變元的形式系統
  它能夠表達 變元能表達的一切東西

* 符號約定
  這裏是以集合論爲討論的基礎的
  現在常稱的 judgement 被稱爲 assertion
  並且是基本集合上的唯一基本謂詞
  其他謂詞都以基本謂詞爲基礎定義
  比如等詞就是如此

* 我的以圖爲基礎的理解方式是
  | point           | 抽象的點         |
  | point generator | 語法             |
  | path            | 等詞             |
  | path generator  | 用等式表達的公理 |

* point 分各個種類
  尤其是 當形式語言涉及到變元時
  point 將包含變元

* 等詞 是特殊的 二元謂詞
  | 傳遞 對稱 | 無向 path |
  | 傳遞      | 有向 path |
  [而自反性是相對平凡的性質]
  其他的謂詞當然不是 path
  然而可以有其他的幾何意義

* 從邏輯語義上看 judgement 是爲了引入假設的前提條件
  把它視爲 謂詞 時
  其性質是特殊的
  左邊可以有任意多個參數 而右邊只能有一個參數
  形式邏輯中的推演規則 就是這個謂詞的生成子
  另外一種理解方式是
  把它理解爲一個 一元謂詞
  也就是一個子集
  這在於
  利用謂詞生成子的性質
  謂詞左邊的多個參數總能被消除
  把它當作一元謂詞時 也可以描述所有谓词生成子
  但是個別的谓词生成子除外

# the theory of combinators

* 作者说 combinatory logic 是以研究对象命名的
  而 combinator 是研究方法
  比如 combinator 在程序语言中的应用 与逻辑无关

# combinatcny arithmetic
# quantification
# the kleene-rosser paradox
# consistency questions
