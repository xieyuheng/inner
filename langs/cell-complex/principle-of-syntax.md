---
title: principle of syntax
---

# my personal principle about syntax design

- when there is a problem to be solved by designing new syntax,
  first I capture all the information by a info-sufficient language,
  then try to simplify the language,
  and try to make it info-necessary.
  but absolutely info-necessary is not the aim,
  the aim is easy to read.

- to make it easier to modify the design,
  I do not early-sugarize the syntax,
  but only use prefix symbol-expression and postfix notation.

- when need to initiate some data in the stack,
  I use prefix symbol-expression to describ the literal-data,
  like '(keyword ...)'.
  when need to do an operation on such data,
  I use postfix notation.

- the above rules must not be followed strictly.
  because for some kinds of simple literal-data,
  there is no need to use symbol-expression.
  while for some kinds of complicated operation,
  symbol-expression is needed.

- there are many ways to sugarize a syntax,
  1. symbel-pattern
  2. local macro

# principle of syntax

## 引

- x ::
  即便是使用 sexp 我也根本沒法找到語法設計的準則

- k ::
  也許只是另外一個設計決策而已
  我們先試着解決具體的問題吧

- x ::
  處理基本的函數定義時
  有兩種設計方式

  ```scheme
  (def <function-name> (lambda ...))
  (def <tc-name> (type ...))
  ```

  或

  ```scheme
  (: <name> type)
  (= <name> value)
  ```

- k ::
  如果用後一種方法的話
  tc 的定義將要變成什麼樣子
  我們知道 這後一種方式是同 agda 與 haskell 學習的
  還知道 它們都用了 where 來處理 tc 的 dc

- x ::
  ```scheme
  (def sphere-1
    (type (-> [] [space])
      b (-> [] [(0 <>)])
      loop (-> [] [(1 b b)])))

  (: sphere-1 (-> [] [space]))
  (= sphere-1
     (list
      (: b (-> [] [(0 <>)]))
      (: loop (-> [] [(1 b b)]))))
  ```

- k ::
  就實現來講
  一個名字 本身就會命名到兩個東西

- x ::
  我在想這裏的兩種方式是否都能達到信息充分
  我是否能證明這一點
  要知道 dependent-type language 的 類型檢查器 是一個特殊的解釋器
  它能以解釋運行的方式運行語言中的函數
  同時運行中的數據都是帶有自己的類型的
  我現在可能還沒法判斷如何才算是信息充分
  因爲我還不知道 at1 的語義是什麼
  並且 sequent0 的語義我也忘了大半了

- k ::
  我建議繼續使用當前的語法
  而不考慮任何大的變動了
  在這種限制下
  努力嘗試解決當前形式化 at 的過程中所遇到的問題
