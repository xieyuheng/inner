---
title: Microkanren -- The algebra of stream of states
---

- x -
  這裏的 disj & conj 類似 bool 代數中的 or 和 and
  其實二者是代數結構
  [goal-t = (-> state-t -- state-t stream-u)]
  中的運算

- k -
  運算律是什麼?
  或者說此代數結構的公理是什麼?

- x -
  我們先來看 forgetful functor 作用於這個代數結構的效果
  如果 考慮 goal-t 所返回的 state-t stream-u 的長度
  那麼 disj 如 add 而 conj 如 mul
  再次遺忘 而考慮 state-t stream-u 的長度是否爲 0
  那麼 disj 如 or 而 conj 如 and

- k -
  但是注意 只給出 goal-t 的話
  並不能得到一個 state-t stream-u
  還需要給出 goal-t 的參數 state-t
  當參數不同時 state-t stream-u 的長度是不同的
  並且 state-t stream-u 的長度還可能是無窮的

- x -
  只有當給出了某個固定的參數的時候
  才能作出所說的遺忘

  並且 當考慮到 call/fresh 之類的算子的時候
  call/fresh : (-> (-> var-t -- goal-t) -- goal-t)
  就知道所處理的空間是很豐富而複雜的
  也許在範疇論裏 這些算子都有所對應吧
