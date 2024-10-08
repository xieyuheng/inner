---
title: syntax -- design and notes
---

# note

- 上同調與微分形式有關

- 古典的數學語言之所以晦澀
  是因爲 爲了 書寫的速度 而 放棄了 易讀性
  寫者在當時的思考環境下 可以很容易讀懂自己的簡記
  但是另外一個讀者 或者寫着本身脫離了當時的思考環境之後
  所寫的東西就需要解釋許久才能恢復意義
  有時甚至根本不可能再恢復意義

# design the language with the UI in mind

- should I design the language with the UI in mind,
  or separate the language and the UI?

- what kind of UI do I want?

# on the type of syntax

- infix
  prefix
  postfix
  borderfix

- the asymmetry of writing method

# syntax to describe syntax

| global | <name> |
| local  | :name  |

```scheme
(:= <name> (or ...))
(:= :name (or ...))
```

# general linguistics

- compare to natural language,
  the syntaxes of programming languages are very simple.
  this single fact makes it very important to learn general linguistics.

# what is good syntax?

- good syntax make meaning explicit to reader.
  meaning is use.

# the psychology of reading code

- by reading code in some language,
  you imagine how the machine would run.
  in your mind, you are always actively interpreting the code.
  the understanding of code happens in many levels.

- this observation amost can guide a language designer.
  language should help the imagination.

# about modularity

- it is such a burden
  to have to come up with unique name for every dc.
  it is also not satisfactory
  to prefix every dc with their space name.

- a module-system must be design to solve this.
  a syntax for 'prefixing every dc with their space name'
  must be designed.
  but locally, in every source code file,
  the prefix can be omitted.

- I will use symbel-pattern '<space-name>/<dc-name>' here.
  or '<module-name>/<space-name>/<dc-name>'
  or '<module-name>/<function-name>'

# about simple macro system

- in (key1 ... (key2 ...))
  key2 might be a marker for key1 which follows the rule of key1
  or itself a key which follows its own rule

# the syntax of 'belong to'

- x -
  'belong to' = 'boundary ='
  目前 (lambda ...) 與 (type ...) 這兩個關鍵詞
  使得 'belong to' 的語義被隱藏起來了

- k -
  之前之所以有這種 (lambda ...) 與 (type ...)
  是因爲想要統一 (def ...)
  並且使得可以形成沒有命名的數據
  但是這種對 '無名' 的追求是不切實際的

- x -
  那麼我們就拋棄這種設計
  新設計的要求是
  1. make 'belong to' explicit
  2. [maybe] make the mark-lang richer

- k -
  首先我們不能使用中綴表達式
  這必須當作是一個重要的原則
  否則語法就喪失就設計者而言的簡潔性了

------

- x -
  我們不能取消 (lambda ...) 與 (type ...) 這種 '無名' 語法關鍵詞
  因爲 我發現 '有名' 的傾向 會使得名字氾濫

- k -
  那我們增加 (: ...) 與 (= ...) 這兩個前綴表達式的語法關鍵詞
  用來做 assertion

- x -
  中綴表達式的傾向體現於如下的例子中
  ```scheme
  (def h ;; to proof (~ [f g] [bool-suspend-suspend id])
    (lambda (-> (* bool-suspend-suspend I) bool-suspend-suspend)
      (extend-from
        (lambda (-> (* bool-suspend-suspend (-1 I)) bool-suspend-suspend)
          (-> (* :x i0) (* [:x f g]))
          (-> (* :x i1) (* :x))))
      (with (-> (* (-1 bool-suspend-suspend) %:a (0 i0 i1))
                (0 (* :a i0) <> (* :a i1) <>))
        (-> (* n2 (1 i01)) (1 n2 refl) ;; (1 n m2 s m2 rev)
            : (0 n2 n2))
        (-> (* s2 (1 i01)) (1 s m2) ;; (1 n m2)
            : (0 n2 s2)))
      (with (-> (* (0 n2 s2) %:b (0 i0 i1) %:i)
                (1 (* (1 :b) i0) <> (* s2 (1 :i)) <>
                   (* (1 :b) i1) <> rev (* n2 (1 :i)) <> rev))
        (-> (* (1 n m2) (1 i01)) (2 #t m m2)
            : (1 (1 n2 refl) (1 s m2)
                 (1 n m2) rev (1 n2 refl) rev)
            = (1 (1 s m2) (1 n m2) rev))
        (-> (* (1 s m2) (1 i01)) (2)
            : (1 (1 n2 refl) (1 s m2)
                 (1 s m2) rev (1 n2 refl) rev)
            = (1 (1 s m2)
                 (1 s m2) rev)
            = (1))      )
      (with (-> (* (1 n m2 s m2 rev) %:c (0 i0 i1) %:i)
                (2 (* (1 n m2) (1 :i)) <>
                   (1 (* n2 (1 i01)) <>
                      (* (1 n m2) i1) <>
                      (* s2 (1 i01)) <> rev
                      (* (1 n m2) i0) <> rev)
                   (* (1 s m2) (1 :i)) <>
                   (1 (* (1 n m2) i1) <>
                      (* (1 n m2) i0) <> rev
                      (* (1 s m2) i1) <> rev
                      (* (1 s m2) i0) <>)
                   (* (2 :c) i0) <>
                   (1 (* (1 n m2) i1) <>
                      (* (1 s m2) i1) <> rev)
                   (* (2 :c) i1) <>
                   (1)))
        (-> (* (2 #f m m2) (1 i01)) (3)
            : (2 (2 #t m m2)
                 (1 (1 n2 refl)
                    (1 n m2)
                    (1 s m2) rev
                    (1 n2 refl) rev)
                 (2)
                 (1 (1 n m2)
                    (1 n2 refl) rev
                    (1 s m2) rev
                    (1 n2 refl) rev)
                 (2 #f m m2 (1 n m2 s m2 rev)
                    #t m m2 (1))
                 (1 (1 n m2)
                    (1 s m2) rev)
                 (2 #f m m2)
                 (1)))
        (-> (* (2 #t m m2) (1 i01)) (3)
            : (2 (2 #t m m2)
                 (2)
                 (2 n2 refl refl)
                 (2 #t m m2))))))
  ```

- x -
  其中
  有如下的縮寫
  ```scheme
  (:> (2)
      (1 (1 n2 refl) (1 s m2)
         (1 s m2) rev (1 n2 refl) rev)
      (1 (1 s m2)
         (1 s m2) rev)
      (1))

  ==>

  (2)
  : (1 (1 n2 refl) (1 s m2)
       (1 s m2) rev (1 n2 refl) rev)
  = (1 (1 s m2)
       (1 s m2) rev)
  = (1)
  ```

# infix notation

```scheme
(note
  (2 surf)
  g =>
  (2 #f m m2 (1 n m2 s m2 rev)
     #t m m2 (1))
  f =>
  (2 (2 surf) (1 (1 b2 refl) (1 b2 refl) rev)
     (2 b2 refl refl) (1))
  ==
  (2 surf))

(note
  The above rewriting, if to be expressed in prefix notation,
  would be ::
  (= [(2 surf) g]
     (2 #f m m2 (1 n m2 s m2 rev)
        #t m m2 (1)))
  (= [(2 #f m m2 (1 n m2 s m2 rev)
         #t m m2 (1)) f]
     (2 (2 surf) (1 (1 b2 refl) (1 b2 refl) rev)
        (2 b2 refl refl) (1)))
  (= (2 (2 surf) (1 (1 b2 refl) (1 b2 refl) rev)
        (2 b2 refl refl) (1))
     (2 surf))
  I think this might be where we really need infix notation.)
```

# should we free the use of '[]' or using ',' as clojure?

- x -
  首先我們總結一下現象

  ```scheme
  (: [n m2] (0 n2 s2))
  (: [s m2] (0 n2 s2))
  (: [#f m m2] (1 n m2 s m2 rev))
  (: [#t m m2] (1 n m2 s m2 rev))
  ```

  這裏 [] 單純是爲了讓人知道 [] 內的是一個單一的元素
  也就是說在語義上 這是可以省略的
  因爲 按照出現在 [] 中的各個元素的定義
  我們就能知道它們將 compose 得一個單一的元素
  而這種多餘的 [] 可以讓人免於這種計算
  並且把這些期望明顯得表達在語法中 使得機器可以檢查之

  ```scheme
  (lambda  (-> (* bool-suspend (-1 I)) bool-suspend)
    (-> (* :x i0) [:x f g])
    (-> (* :x i1) :x))
  ```

  這個被認爲是下面的縮寫
  ```scheme
  (lambda  (-> (* bool-suspend (-1 I)) bool-suspend)
    (-> [(* :x i0)] [:x f g])
    (-> [(* :x i1)] [:x]))
  ```

  這裏
  (-> ...) 內 [] 使得多個參數和多個返回值成爲可能
  [雖然 這個例子中 單參數 與 單返回值]
  也就是說
  [] 內可能是多個元素
  比如

  ```scheme
  (lambda  (-> [(* bool-suspend (-1 I)) (* bool-suspend (-1 I))]
               [bool-suspend bool-suspend])
    (-> [(* :x0 i0) (* :x1 i0)] [:x0 f g :x1 f g])
    (-> [(* :x0 i1) (* :x1 i1)] [:x0 :x1]))
  ```

  是否允許如下的 '明確化' 呢
  ```scheme
  (lambda  (-> [(* bool-suspend (-1 I)) (* bool-suspend (-1 I))]
               [bool-suspend bool-suspend])
    (-> [(* :x0 i0) (* :x1 i0)] [[:x0 f g] [:x1 f g]])
    (-> [(* :x0 i1) (* :x1 i1)] [[:x0] [:x1]]))
  ```
  或者
  ```scheme
  (lambda  (-> [(* bool-suspend (-1 I)) (* bool-suspend (-1 I))]
               [bool-suspend bool-suspend])
    (-> [(* :x0 i0) (* :x1 i0)] [:x0 f g, :x1 f g])
    (-> [(* :x0 i1) (* :x1 i1)] [:x0, :x1]))
  ```

- k -
  我想前者嵌套的 [[]] 肯定是不可取的

- x -
  但是如果使用後者的話
  比如
  ```scheme
  (def bool-suspend
    (type space
      (: n s (-1 <>))
      (: m (-> bool (0 n s)))))
  ```
  就應該寫成
  ```scheme
  (def bool-suspend
    (type space
      (: [n, s] (-1 <>))
      (: m (-> bool (0 n s)))))
  ```
  然而對於 (: ...) 來說 這其實是沒有必要的

- k -
  但是我們可以要求這一點
  以使得 對 [] 與 ',' 的使用變得一致

- x -
  那我們就追求這種一致性吧
  也許之後對 (: ...) 這個語法關鍵詞的擴展
  可能使得 (: ...) 類似於 (-> ...)
  從而讓 這種用法變得有必要

- k -
  這樣我們就要以 clojure 的方式使用 ',' 了

- x -
  沒錯

------

- x -
  我們來明確一下各個規則
  [] 可能出現在 (: ...) (-> ...) (* ...) 中
  而不允許出現在 (0 ...) (1 ...) (2 ...) (3 ...) 等等 中
  而 ',' 可以出現在如上所有語法關鍵詞中

- k -
  從 [:x0 f g, :x1 f g]
  可以看出 ',' 是說
  在某個函數作用之後
  另起一個新元素加入到棧中

- x -
  之所以保留 [] 的濫用[即 爲了可讀性而冗餘]
  是想要保留它做別的語義
  但是這可能是不對的
  應該允許 [] 的濫用

- k -
  也就是說
  (1)
  在某些地方[某些語法關鍵詞中]和某些時候[某些語境下]
  我們必須要利用 [] 與 ',' 才能使得語法無歧義
  (2)
  但是在其他地方
  我們使用這兩種標點只是爲了
  使得讀者能夠更快地理解我們的語義
  而不用另行語法分析

- x -
  沒錯
  比如 (-> ...) 中
  當有多個輸入值或者多個返回值時
  (-> [... ... ...] ...) 就是有必要的了
  然而是否加入 ',' 確是作者可以選擇的
  (-> [..., ..., ...] ...)
  而在 (: [n, s] (-1 <>)) 中
  甚至 [] 都是沒必要的
  可以直接寫成 (: n s (-1 <>)) 而沒有歧義
  這也是作者可以選擇的
  但是我常常選擇前者 以維持一點閱讀時的一致感

- k -
  我們是否有必要舉出當前所有的可能用到這兩種標點的地方呢
  比如 (~ ...) (~~ ...) (= ...)

- x -
  我想對於等價關係
  我們最好讓多個元素之出現有相互等價之意
  而 (:> ...) (=> ...) 都被當作是等價關係

- k -
  如此看來當前的語法問題就又解決了

- x -
  沒錯
  並且我覺得 我們還發展出了一些關於語法的一般理論
  或者不能說是 '理論'
  因爲我們並不遵循生成語法的傳統
  我們是否接機給生成語法一個批判呢?

- k -
  啊 還是算了
  我們用我們自己的設計本身來批判它吧
  不要再專門另行討論了

- x -
  但是
  如果能總結出一些批判的觀點就更好
  哦
  我想一個批判就夠了
  我們把語法之無歧義
  定義爲能夠寫一個程序來以無歧義的方式分歧出來其語義
  並且這種分析可能是依賴語境的

# a syntax for power [for repeating]

- to help (: c (1 r r r r r r))
  and (-> [n-cell boundary] [x0 refl {n-1}])

- x -
  我發現我們不能把這個語法單純地設計爲一個 syntax macro
  這樣的設計很容易想到
  但是如此一來我們就沒法以一致的方式使用它了
  那麼我們需要把它設計爲一個函數
  或者說一個高階函數 即一個組合子

- k -
  如果是這樣 我們就需要語法來 quote 函數

- x -
  沒錯
  我們不應該避開這個語義

- k -
  最自然的 quote 函數時用的 [] 已經被用掉了
  那麼就剩下 {} 了
  [x0 refl {n-1}]
  應該寫成
  [x0 {refl} ...]
  應該寫成
  (1 {r} ...)
  因爲 'refl' 與 'r' 都是函數

- x -
  [x0 {refl} n 1 sub times]
  (1 {r} 6 times)
