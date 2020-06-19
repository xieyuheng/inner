---
title: Diagonalization and self reference
---

# Diagonalization and self reference

------
- Authors: Raymond Smullyan
- Date: 1994
------

# 引

- 可以想象 smullyan 在最開始是被 Gödel 的理論的難度和有趣性所吸引
  然後在學習其理論的過程中
  把自己對相關問題的理解方式做了總結

# 標記語言 與 indexical

- 一般的人類語言就以下觀點來看
  可以被稱爲是標記語言
  "john is reading this sentence."
  其中 this 就是一個標記 它代表了這句話本身

- smullyan 稱
  "this sentence" "I" "now" 這類
  其語義跟語境相關的詞
  爲 indexical

- 但是 傳統的 數理邏輯 中使用的形式語言
  並不提供這種 類似標記語言的 語義

# use and mention

- 從下面的有趣例子
  可以看出 lisp 中 symbol 的語義問題
  1. ice is frozen water.
  2. ice has three letters.
     [useing a word]
  3. "ice" has three letters.
     [mentioning a word]
  4. "ice" is the name of ice.
     [mentioning and useing a word]
  5. it takes longer to read the bible than to read "the bible".
  6. this sentence is longer than "this sentence".
  7. ""ice"" has two pairs of quotation marks.
  8. ""ice"" has one pair of quotation marks.

- 也就是說
  可以看出
  lisp 之所以對 symbol 的語義有如此的設計
  是因爲其設計是受人類的自然語言的影響的
  自然語言中的符號
  在沒有被聲明是引用的情況下
  被視爲是 symbol 的所指

- 然而上面的句子在不同的語境下都可以被看作是正確的
  因爲自然語言並沒有嚴格性

# >< self-reference using diagonalization

- 这其實就是 Y combinator

# >< normalization

# one-side quotation

- 也許在蟬語中我包含兩種引用方式
  它們只是形態上不同而已
  其本質都是邊綴表達式

- 其實在這裏可以看出
  smullyan 對函數作用的語義的理解並不透徹
  這裏 quotation 被作爲了 first-class-object
