---
title: tutorial of prolog
---

# [note] interface

- fact and rule in file
- query in interpreter

# k1

```prolog
loves(vincent, mia).
loves(marsellus, mia).
loves(pumpkin, honey_bunny).
loves(honey_bunny, pumpkin).

jealous(X, Y):-
  loves(X, Z),
  loves(Y, Z).
```

# unification

## 簡要敘述

- two terms unify if :

  1. they are the same term

  2. or if they contain variables
     that can be uniformly instantiated with terms
     in such a way that the resulting terms are equal

     - 這裏所說的 'uniformly instantiated with terms'
       就是可能給兩個變元共同鏈接到一個新的變元
       這樣就能保持有向圖無圈

## 具體算法描述

- If term1 and term2 are constants,
  then term1 and term2 unify
  if and only if
  they are the same atom,
  or the same number.

- If term1 is a variable and term2 is any type of term,
  then term1 and term2 unify,
  and term1 is instantiated to term2 .
  Similarly, if term2 is a variable
  and term1 is any type of term,
  then term1 and term2 unify, and term2 is instantiated to term1 .
  - So if they are both variables,
    they’re both instantiated to each other,
    and we say that they share values.
    這裏就是困難的地方
    或者不如說是實現方式非平凡的地方

- If term1 and term2 are complex terms,
  then they unify if and only if:
  - They have the same functor and arity,
  - and all their corresponding arguments unify,
  - and the variable instantiations are compatible.
    - For example,
      it is not possible to instantiate variable X to mia
      when unifying one pair of arguments,
      and to instantiate X to vincent
      when unifying another pair of arguments .

- Two terms unify
  if and only if
  it follows from the previous three clauses that they unify.

## 實現方式非平凡的地方

- X = Y demands that
  we unify two finite representations of infinite terms
  ```prolog
  X = father(X),
  Y = father(Y),
  X = Y.
  ```

- here we need the function 'equal?'
  to be able to check two looped lists

- standard unification algorithms are pessimistic.
  They first carry out the occurs check,
  and only when they are sure that the situation is safe
  they do go ahead and actually try to unify the terms.

# proof search as computation

- prolog reads the knowledge base,
  and tries to unify k(Y) with
  either a fact, or the head of a rule.

- 雖說運行方式是 search
  但是只要按照 functor 的名字 來查找 hash-table
  就也是線性時間

- 所以每次 通過 functor 查找到的是函數體
  所謂的 在查找中試錯
  其實就是用 return-stack 來記錄之前執行到的函數體中的位置
  這樣 與我目前所使用的實現方式就非常相似了
  注意
  函數體中都是 一次一次得 unification
  在一次 unification 中 X 可能匹配到 a
  在下一次 unification 中 X 可能匹配到 b
  可以想像如何使用 scope-stack
  和 編譯時在 clauses 之間增加一些特殊的 primitive-function
  來實現這種效果
  - 也就是需要 undo binding

# [note] 以 謂詞的處理方式 分類語言

- x -
  之前說過 謂詞的處理方式 可用以 分類語言
  prolog 就是一個極端的例子
  因爲裏面所有的函數都是謂詞
  根本沒有正常意義上的函數

# recursive definition

## [note]

- recursive definition is different from recursive unification
  how?

# ffi and instantiation_error

- 默認的語義是
  =(X,+(1,2)).
  需要做算數時使用
  is(X,+(1,2)).
  也就是說
  參數都被當作 term 了
  但是函數頭不是

- But there is a deeper point that is worth reflecting on:
  bolting on the extra capability
  to do arithmetic in this way
  has further widened the gap
  between the procedural and declarative meanings
  of Prolog programs.

# 多出一個命名

- 有些東西可以表達地很簡單
  因爲於等式相比
  謂詞多了一個命名
  比如
  ```prolog
  add(0, N, N).
  add(succ(M), N, succ(O)):-
    add(M, N, O).

  %% add(0, N) = N.
  %% add(succ(M), N)
  %% = succ(add(M, M)).
  %% with
  %% add(M, M) = O.

  append([], List2, List2).
  append([Head1 | Tail1], List2, [Head1 | Tail2]):-
    append(Tail1, List2, Tail2).

  %% append([], List2) = List2.
  %% append([Head1 | Tail1], List2)
  %% = [Head1 | append(Tail1, List2)].
  %% with
  %% append(Tail1, List2) = Tail2.
  ```

- 函數 -> 謂詞
  把等號右邊命名
  然後重新組織模式匹配對算法的表達方式
  - 很可能就更簡單了
    因爲多了一個名字

- 謂詞 -> 函數
  當某個變元做爲 結果變元 在某個謂詞式子末尾的位置單獨出現時
  把這個謂詞式子轉化爲等式

- 可以發現 logic 編程的特點是能夠模式匹配運算結果

# 實現

- 做爲 functor 的名字的同時
  每個 atom 本身也數據
  - 被 quote 的時候 lisp 中的 symbol 是數據
    而 prolog 中默認所有的 atom 在 參數位置的出現是 quote 的
    只有在頂層的

# 語義

- 特點是
  沒有 argument-stack
  只有 scope-stack
  我的 unification 是
  函數體中 一個 sequent 的 antecedent 去匹配 argument-stack 中的值
  但是這裏已經沒有 argument-stack 了
  要知道
  邏輯範式下 定義的不是函數 而是謂詞
  只有函數才需要使用參數棧來 傳遞參數 和返回值

- 函數體 jojo 中的每個 jo
  都代表 一個等待 unify 的 term
  而不是一個等待作用的 function
  每個函數體中項所帶有的變元 需要在 scope-stack 中查找來知道其意思
  每個函數退出時 忘掉它自己的 scope-stack 就能維護 lexical-scope

- 好像還是需要一個類似 argument-stack 的棧
  感覺上是 argument-stack 來處理 '且'
  而 return-stack 來處理 '或'

- 但是 argument-stack 和 return-stack 看起來是連動的
  所以
  還是設想單獨的 return-stack
  此時每個 return-point 是
  return-point = [querying term, index to functor body]
  需要兩個指針
  一個是 return-stack-top
  一個是 return-stack-current
  但是這樣 可能使用的就不是深度優先的搜索了
  但是這種語義是正確的 並不應該使用深度優先
  注意 [term, index]
  在遞增 index 的時候 會伴隨特定的副作用
  比如
  - 若前一個 jo 給 X 賦值了
    那麼在遞增 index 的時候 就應該 undo 這個賦值
  - 若前一個 jo 入返回棧了新的元素
    那麼在遞增 index 的時候 就應該 undo 這些入棧的元素

- 注意在每次 :- 成功時 都要 reset scope-stack
  所有的 local-variable 都應該被匹配到了 [因爲這樣謂詞才算是成功]
  所以 在返回的 term 的時候 取出這些匹配到的元素就行了
  注意
  如果匹配的時候兩個都是變元 那麼就會生成新的變元
  新的變元所佔用的內存 在返回之後也是不能回收的
  [上面的 '返回' 和函數返回意義不同]

- 比如
  ```prolog
  append([], List2, List2).
  append([Head1|Tail1], List2, [Head1|Tail2]):-
    append(Tail1, List2, Tail2).
  ```
  問 append([a,b,c],[1,2,3],X).
  X 匹配到了 [Head1|Tail2]
  在返回 append(Tail1, List2, Tail2) 的時候
  就不能把 所有的 Tail1, List2, Tail2 都代換成值
  其中 Tail2 不能代換成值
  因爲 雖然 匹配成功了
  但是它所匹配到的是不定元
  匹配的方式是
  X = [a|Tail2]

- 所以
  不應該在每次 :- 成功時 都要 reset variable-stack
  因爲
  有些 local-variable 會匹配到不定元
  也許
  在 undo 的時候 或者遇到 fact 的時候
  可以 reset variable-stack
  直接使用一個 round-buffer 不是長久之計

- 我們再來考慮一下 undo 和 fact

- undo [或者說 back]
  就在於
  一般的函數結束一個分支之後 就能出 return-stack
  但是邏輯式中 要遍歷所有的分支
  所以一個分支結束之後 也不能出 return-stack
  只有當所有的分支都結束之後 才能出 return-stack
  每個分支就是一個 jo
  當 k(X):-f(X),g(X). 匹配成功
  而把 f(X),g(X) 作爲 goal 入 goal-stack 之前
  要先入 goal-stack 一個收尾 jojo
  當作爲 goal 的兩個 jojo [ f(X) 和 g(X) ] 結束之後

# k 的例子

## define

```scheme
(define
  (f a)
  (f b))
(define
  (g a)
  (g b))
(define
  (h b))
(define
  (k :x <-
     (f :x)
     (g :x)
     (h :x)))
```

## ><

```scheme
(goal: (k :y))

(variable-stack:
  :y?)
(unify: #1#
  (k :x <-
     (f :x)
     (g :x)
     (h :x)))
(variable-stack:
  :y :y:x
  ------
  #1#
  :y :y:x
  :y:x?)

(goal: #1#
  (f :x)
  (g :x)
  (h :x))
(unify: (f a))
(variable-stack:
  :y :y:x
  ------
  #1#
  :y :y:x
  :y:x a)
```

# back

- back 時
  sub-goal 的撤銷是用過 pop goal-stack 實現的
  但是
  back 時
  unify 的撤銷 卻不可以用 pop 實現

# k 的例子 更嵌套

## defind

```scheme
(define
  (f a)
  (f b)
  (f c))

(define
  (g a)
  (g b))

(define
  (h :x <-
     (f :x)
     (g :x)))

(define
  (k :x <-
     (f :x)
     (g :x)
     (h :x)))
```

## #0#

```scheme
(goal: #0#
  (<and>
   [(k :y)
    (<or> #1#
     (k :x <-
        (f :x)
        (g :x)
        (h :x)))]))
(variable-stack:
  ------
  #0#
  :y :y:x
  ------
  #1#
  :x :y:x
  :y:x?)
```

## #1#

```scheme
(goal: #1#
  (<and>
   [(f :x)
    (<or>
     (f a)
     (f b)
     (f c))]
   [(g :x)
    (<or>
     (g a)
     (g b))]
   [(h :x)
    (<or>
     (h :x <-
        (f :x)
        (g :x)))]))
(variable-stack:
  ------
  #0#
  :y :y:x
  ------
  #1#
  :x :y:x
  :y:x a)
```

## #2#

```scheme
(goal: #2#
  ([(f :x)
    ((f a)
     <or>
     (f b)
     (f c))]
   <and>
   [(g :x)
    (<or>
     (g a)
     (g b))]
   [(h :x)
    (<or>
     (h :x <-
        (f :x)
        (g :x)))]))
(variable-stack:
  ------
  #0#
  :y :y:x
  ------
  #1#
  :x :y:x
  :y:x a)
```

## #3#

```scheme
(goal: #3#
  ([(f :x)
    ((f a)
     <or>
     (f b)
     (f c))]
   [(g :x)
    ((g a)
     <or>
     (g b))]
   <and>
   [(h :x)
    (<or> #3#
     (h :x <-
        (f :x)
        (g :x)))]))
(variable-stack:
  ------
  #0#
  :y :y:x
  ------
  #1#
  :x :y:x
  :y:x a
  ------
  #3#
  :x :x#1)
```

## #4#

```scheme
(goal: #4#
  ([(f :x)
    ((f a)
     <or>
     (f b)
     (f c))]
   [(g :x)
    ((g a)
     <or>
     (g b))]
   <and>
   [(h :x)
    ((h :x <-
        (f :x)
        (g :x))
     <or>)])
  ------
  (<and>
   [(f :x)
    <or>
    ...]
   [(g :x)
    <or>
    ...]))

(variable-stack:
  ------
  #0#
  :y :y:x
  ------
  #1#
  :x :y:x
  :y:x a
  ------
  #3#
  :x :x#1)
```

## [note]

- 只有當所有的指針都到盡頭了都沒有匹配到
  才算是失敗了

- 注意
  <and> 和 <or> 這兩個指針
  所指向都 都是編譯好的 jo 內的數組
  <and> 顯然是如此
  <or> 也顯然是如此
  如果全都使用 jo 內部靜態的數據 就可以不返回結構化的數據了
  所有 結構化的數據 都是靜態的

- 這樣說是不對的
  遇到 ':-' 或 '<-' 的時候
  還是需要返回新的值的
  比如 開始匹配的時候所使用的 k(Y)
  就不同於謂詞體中所出現的 or 分支 k(X) 等等

- 已經可以着手實現了

# lispy

- play
  ```scheme
  (append () :list2 :list2)
  (append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
          (append :tail1 :list2 :tail2))

  (goal:
    (append (list a b c) (list 1 2 3) :x))
  (variable-stack:
    :x --?)

  (unify:
    (append (cons :head1 :tail1) :list2 (cons :head1 :tail2)))
  (variable-stack:
    :x -- (cons a :tail2)
    :head1 -- a
    :tail1 -- (list b c)
    :list2 -- (list 1 2 3)
    :tail2 --?)

  (note:
    此時需要複製壓縮了
    但是 複製壓縮 的依據 看來有兩個
    一個是 :x
    一個是 新返回的
    (goal:
      (append (list b c) (list 1 2 3) :tail2)))

  (goal:
    (append (list b c) (list 1 2 3) :tail2))
  (variable-stack:
    :x -- (cons a :tail2)
    :tail2 --?)

  (unify:
    (append (cons :head1 :tail1) :list2 (cons :head1 :tail2)))
  (variable-stack:
    :x -- (cons a :tail2)
    :tail2 -- (cons b :tail2)
    ------
    :head1 -- b
    :tail1 -- (list c)
    :list2 -- (list 1 2 3)
    :tail2 --?)

  (goal:
    (append (list c) (list 1 2 3) :tail2))
  (variable-stack:
    :x -- (cons a :tail2)
    :tail2 -- (cons b :tail2)
    ------
    :tail2 --?)
  ```

# 討論 <2015-12-24>

- x -
  上面的 :tail2 不能代換成值 的原因是
  unify 的時候 它沒有匹配到確定的值
  匹配的方式是
  :x = (cons a :tail2)
  :tail2 的不確定性來源於 :x
  但是 完整的信息卻是保存在 :x 中的
  當看 :tail2 的時候 是不能知道爲什麼這個元素是不確定的

- k -
  那麼能不能反過來編碼
  把 unify 的方式
  :x = (cons a :tail2)
  轉化爲
  :tail2 = (equal :x (cons a -))
  這樣當 :tail2 確定了的時候
  就能順次去確定 :x

- x -
  總之在編碼這些信息的時候
  如果使用一些技巧 看來就是可能有利於垃圾回收器的
  又比如 我們可以通過生成共同引用
  使得 從 :x 開始 也能獲得獲得全部的信息
  需要 編碼的 unify 是
  :x = (cons a :tail2)
  此時可以生成公共引用點 (= :x (cons a :tail2))
  然後
  (:x (= :x (cons a :tail2)))
  (:tail2 (= :x (cons a :tail2)))
  比如
  :x = :y
  (:x (= :x :y))
  (:y (= :x :y))
  注意
  只有在做 unify 的時候
  才有機會生成這種相互引用的編碼

# lispy

```scheme
(append () :list2 :list2)
(append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
        (append :tail1 :list2 :tail2))

------

(goal:
  (append (list a b c) (list 1 2 3) :x))
(variable-stack:
  :x?)
(unify:
  (append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
          (append :tail1 :list2 :tail2)))
(variable-stack:
  :x (= :x (cons a :tail2))
  :head1 a
  :tail1 (list b c)
  :list2 (list 1 2 3)
  :tail2 (= :x (cons a :tail2)))
(new-goal:
  (append (list b c) (list 1 2 3) :tail2))
(variable-stack:
  ;; copy to conjugate
  :x (= :x (cons a :tail2))
  :tail2 (= :x (cons a :tail2)))
(unify:
  (append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
          (append :tail1 :list2 :tail2)))
(variable-stack:
  :x:1 (= :x:1 (cons a :tail2:1))
  :tail2:1 (= :x:1 (cons a :tail2:1)) (= :tail2:1 (cons b :tail2:2))
  ------
  :head1 b
  :tail1 (list c)
  :list2 (list 1 2 3)
  :tail2:2 (= :tail2:1 (cons b :tail2:2)))
(new-goal:
  (append (list c) (list 1 2 3) :tail2))
(variable-stack:
  ;; copy to conjugate
  :x:1 (= :x:1 (cons a :tail2:1))
  :tail2:1 (= :x:1 (cons a :tail2:1)) (= :tail2:1 (cons b :tail2:2))
  :tail2:2 (= :tail2:1 (cons b :tail2:2)))
```

# 回收內存的時機

## [note]

- 起初我想 回收內存應該激進 每次函數退除的時候都應該回收

- 之後我想 回收內存的時機應該延遲

- 最後我發現 根本不需要回收 local-variable 的內存
  只要使用一個很大的 round-buffer 就可以了
  假設沒有一個單獨的循環或者遞歸會使用到這麼多的內存就行了

- 注意
  在 cicada-language 的類型檢查中也有同樣的問題
  也可以使用同樣的處理方式

## define

```scheme
(define
  (append () :list2 :list2)
  (append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
          (append :tail1 :list2 :tail2)))
```

## #0#

```scheme
(goal: #0#
  (<and>
   [(append (list a b c) (list 1 2 3) :x)
    ((append () :list2 :list2)
     <or> #1#
     (append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
             (append :tail1 :list2 :tail2)))]))
(variable-stack:
  ------
  #0#
  :x (cons a :tail2)
  ------
  #1#
  :head1 a
  :tail1 (list b c)
  :list2 (list 1 2 3)
  :tail2?)
```

## #1#

```scheme
(goal: #1#
  (<and>
   [(append (list b c) (list 1 2 3) :tail2)
    ((append () :list2 :list2)
     <or> #2#
     (append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
             (append :tail1 :list2 :tail2)))]))
(variable-stack:
  ------
  #0#
  :x (cons a :tail2#1)
  ------
  #1#
  :head1 a
  :tail1 (list b c)
  :list2 (list 1 2 3)
  :tail2 (cons b :tail2#2)
  ------
  #2#
  :head1 b
  :tail1 (list c)
  :list2 (list 1 2 3)
  :tail2?)
```

## #2#

```scheme
(goal: #2#
  (<and>
   [(append (list c) (list 1 2 3) :tail2)
    ((append () :list2 :list2)
     <or> #3#
     (append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
             (append :tail1 :list2 :tail2)))]))
(variable-stack:
  ------
  #0#
  :x (cons a :tail2#1)
  ------
  #1#
  :head1 a
  :tail1 (list b c)
  :list2 (list 1 2 3)
  :tail2 (cons b :tail2#2)
  ------
  #2#
  :head1 b
  :tail1 (list c)
  :list2 (list 1 2 3)
  :tail2 (cons c :tail2#3)
  ------
  #3#
  :head1 c
  :tail1 ()
  :list2 (list 1 2 3)
  :tail2?)
```

## #3#

```scheme
(goal: #3#
  (<and>
   [(append () (list 1 2 3) :tail2)
    (<or> #4#
     (append () :list2 :list2)
     (append (cons :head1 :tail1) :list2 (cons :head1 :tail2) <-
             (append :tail1 :list2 :tail2)))]))
(variable-stack:
  ------
  #0#
  :x (cons a :tail2#1)
  ------
  #1#
  :head1 a
  :tail1 (list b c)
  :list2 (list 1 2 3)
  :tail2 (cons b :tail2#2)
  ------
  #2#
  :head1 b
  :tail1 (list c)
  :list2 (list 1 2 3)
  :tail2 (cons c :tail2#3)
  ------
  #3#
  :head1 c
  :tail1 ()
  :list2 (list 1 2 3)
  :tail2 (list 1 2 3)
  ------
  #4#
  :list2 (list 1 2 3))
```

# 實現 <2015-12-25>

- x -
  感覺最大的題已經解決了
  我們再來細化一下實現方式

- k -
  我們回到我們以前的 jojo 術語吧
  因爲這裏已經不是 函數體 而是 謂詞體 了

- x -
  那麼 jojo 中的每個 jo 都是一個等待 unify 的謂詞 term
  在 prolog 教程的指導下 已經知道如何實現 unify 了
  看來難點在於 這裏的運算流程需要 undo
  此次的優勢是 我們直接使用 scheme 中的 vector
  而不使用底層的 byte-vector [byte-array] 了

# unification

- x -
  其實
  不光是在 back 的時候
  需要記錄信息以 undo variable-instantiation
  [variable-substitution]
  用 variable-stack 來實現 unification 算法的時候
  在失敗一般的 unification 失敗的時候
  都要做 undo
  以使得某個 term 之後還能被用於下次的 unification
  我還發現
  在分類 unification 的時候所說的
  higher-order unification
  syntactical unification
  semantical unification
  其中 higher-order unification
  是 semantical unification 的特殊情況
  此時 關於 semantical 的就是 函數作用

# predicate-term & predicate-formal-term

- x -
  注意 predicate-formal-term 是純粹靜態的 [純粹語法的]
  計算的時候 goal-stack 的一個 and-list 中
  所保存的不是這種 predicate-formal-term
  而是 predicate-term
  需要把其中的 formal-variable [比如 :formal-variable]
  變成 variable-stack 中的地址 [就是 真正的 variable]

- k -
  如果 undo 是 predicate-term by predicate-term
  那麼只要在 predicate-term 中記錄信息
  就知道如何擦除之前的 variable-instantiation 了

- x -
  不過看來不是 predicate-term by predicate-term
  undo 信息應該保存在 or-list 裏面
  每個分支之間 應該又 undo-info
  或者說
  只是在每次 or-pointer 的位置保存 undo-info

# 模塊系統

- x -
  當我們之前爲一個純粹的解釋器做模塊系統的時候
  我們當時的設計是
  不斷修改 hash-table 中 無前綴的名字所對應的函數體
  這些沒有前綴的名字 代表了當前我們能夠引到的東西
  那時 使用的是靜態函數體
  然而現在使用動態函數體了

- k -
  並且
  因爲是在很高級的動態語言中實現這些
  所以我們可以利用編譯時期的轉換了
  在轉換的時候
  我們需要一個全局變量來告訴我們
  應該如何補全 不帶前綴的名字的模塊前綴
  補全 的時候有兩個位置
  一個是被定義的位置
  一個是作爲定義內容的函數體

- x -
  我們嘗試在這裏實驗一下我們之前設計的 tag-group
  並且這次就直接用 org-mode
  畢竟這是一個非純粹縮進的語言

- k -
  也就是說在一個文件之內
  又很多默認的 import

- x -
  沒錯
  並且用明顯的語法來聲明 helper-function
  使得它們不被 export
  其他的不帶明顯聲明的都是默認的 export

- k -
  隱藏的搜索 tag-group 機制可能不易於推理程序行爲
  也不利於調整代碼結構順序

- x -
  import
  import all with prefix
  use
  import local with prefix
  import other without prefix
  同一個文件中
  默認 import 之前的所有 module

# 結構化的數據中的命名

- x -
  在設計一個數據結構時
  發現有時可以命名
  有時可以匿名
  如何取捨

- k -
  在 racket 中
  我們可以約定一下
  名的結構化數據
  [需要用 'keyword:' 處理的]
  就用 list 的 pattern 來實現
  無的結構化數據
  就用單純的 vector 來實現
  這還沒有解決具體設計數據結構的時候應該如何選擇的問題
  但是 可以維持我們代碼的一致性

# query 的回答 <2016-01-03>

- x -
  所收集到的信息可能還需要整理之後
  才能 report

# backtracking

- chronological backtracking
  the latest choice at the moment of failure is reexamined first

# goal tree

- node
  and-vector

- possible sub-node
  sequent in and-element
  only for the and-element that is rule
  but not fact

- change of sub-node
  change the or-curser in the and-element

- a stack is uesd to help the tree

- the state is always ready to go next step

- goal-stack only stores and-vector
  whose and-cursor is not at the end

- if or-curser is at the end
  and you still wish to step
  it moves the and-cursor back and loop
