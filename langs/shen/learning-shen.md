---
title: learning shen
---

# note

## bar-ket

- () 代碼
- [] list
  其中的 () 是會被計算的
- {} 類型聲明
- <> vector
  但是因爲要作大於號和小於號
  所以 reader 不用這種括號
  只能是 writer 用

## >< hash-table

## >< type

## pattern-match

- 在蟬語中
  在提供了基本的函數語義和語法擴展機制之後
  再在其內實現模式匹配

# function application

- 函數作用 完全地一元化 即 curry
- "作用" 是
  以 "()" 爲邊綴符號的
  多元函數
  邊界所包圍的值中
  有一個是特殊的 被視爲函數
  而其餘的 被視爲其參數
- 在邊綴表達式 所界定的邊界內
  1. 參數個數不夠時
     會形成 curry
  2. 參數個數多出時
     也可能還有默認的處理方式
     尤其是二元函數
     尤其是具有結合性的二元函數
- 只要注意
  在邊綴表達式 所界定的邊界內
  元素個數的不同會引起不同的處理方式就可以了
  正式邊綴表達式的使用 使得同一個函數能夠承載多種語義的
- 注意這與蟬語的處理方式不同
  蟬語的函數作用是
  以 "()" 爲邊綴符號的
  [即 小圈]
  一元函數
  其參數之取用由各個函數本身決定
  小圈之外還有大圈可用以定界
  以幫助優化完全 curry 的語法

```shen
\* all curry *\
\* eager-eval *\

(+ (* 7 8) 2)
(* 7)
```

# lambda abstraction

- λ xyz : N  =
  (/. x y z N)
```shen
(/. X X)
((/. X X) 9)
((/. X Y Y) 6 7)
((/. X Y (* X Y)) 6 7)
```

# data type

## boolean

true and false are booleans
The basic boolean operators are:
- if
- and
- or
- not
- cases (like cond in lisp)
```shen
(if (= 6 (+ 4 2))
    yes
    no)

(and (number? 6)
     (string? "3"))

(or (= 8 8)
    (= 8 9))

\\ 9 is NOT a boolean
\\ so the following get you error

(or 9 (= 8 8))

(cases
 (= 8 9) kkk
 (= 8 7) aaa
 true error!
 )
```

## symbol

### intern

- 即 string->symbol
  這是用一個 散列表 來實現的

```shen
(intern "kkk")
```

### hash

- 這裏所給出的是一般用途的散列函數

```shen
\\ (hash a n)
\\ where a is any expression and n is a natural number
\\ will give the hash value of a within the interval 0 to n
(hash abc 10)
(hash abc 100)
(hash abc 1000)
(hash abc 10000)
```

### property list

- 這種語義
  可用以統一 函數名 變元名 等各種 名
  的實現方式
  當然
  伴隨着這種統一而來的是靈活的使用方式
- 可以看出 鏈表處理 對這個語義有很大的影響
  因爲
  如果想要以靈活的方式使用
  字符串 到 一個大的向量的索引 的 散列函數
  那麼 散列表 對衝突的處理就勢必使用鏈表
  想要把這裏的設計決策做好 所需要考慮的因素是非常多的
- 術語上
  散列函數 進行計算
  散列表 處理衝突
```shen
(put table 1 a)
(put table 2 [kkk])
(put table 3 c)

(get table 1)
(get table 2)
(get table 3)
```

## string

```shen
(str 123)
(str "123")
(str abc)

(cn "1" "2")
(@s "1" "2")
(@s "111" "222" "333")

(pos "12345" 3)

(tlstr "12345")
```

## list

```shen
(= [1 (+ 1 1) 3]
   [1 2 3])

(head [1])
(tail [1])
(cons 1 [])
(cons 1 2)
[1 2 | [3]]
```

## vector

```shen
\\ vector creates a vector
\\ with n element numbered from 1 to n
(vector 0) \\ = <>
(vector 3)

(set *myvector* (@v 1 <>))
(vector? (value *myvector*))

\\ and the index 0 holds the size of the vector
(limit (value *myvector*))
\\ should equal to
(<-vector (value *myvector*) 0)
\\ but "cannot access 0th element of a vector"

(<-vector (value *myvector*) 1)

(@v 0 (value *myvector*)) \\ NOT side effect but copy elements
(limit (value *myvector*))
\\ so
\\ use @v to create vector is waste of GC

(set *myvector*
      (@v 0 (value *myvector*))) \\ side effect
(limit (value *myvector*))

(<-vector (value *myvector*) 2)
(vector-> (value *myvector*) 2 a) \\ side effect

(value *myvector*)
```

## pair

```shen
(@p (@p 1 2) 3)

(@p 1 2 3 4)
\\ equals to
(@p 1 (@p 2 (@p 3 4)))

(fst (@p 1 2))
(snd (@p 1 2))

(tuple? (@p 1 (@p 2 3)))
(tuple? (@p (@p 2 3) 1))

\\ pair 是一個獨立的數據型
\\ list is not implemented by pair
(tuple? [1 | 2])

\\ pair is implemented by vector [1 level lower vector]
\\ normal vector is as (<size> <value> <value> ...)
\\ while pair is as (tuple <value> <value> ...)
\\ thus the predicate tuple? get implemented well
```

## @s @p @v

- 這三個都不是副作用
  都以相同的方式展開
  (@x _ _ _ _)
  =>
  (@x _ (@x _ (@x _ _)

# side effect

## assignment

- 由於簡化了 symbol 的使用方式
  所以
  當要把一個 symbol 作爲 var 時
  就需要明顯地使用 value
- 函數的命名空間和變量的命名空間是分離的

```shen
\\ global
(set dozen 6)
(value dozen)
(bound? dozen)

\\ local
\\   in the body
\\   one can not reset X to other value
\\   but one can use another let to block X
(let X 6
     Y 5
  (* X Y))
```

## io

### open & close

```shen
\\ open is relative to *home-directory*
\\ one can use cd to change *home-directory*

\\ stream is buffer with

\\ The basic functions for streams are
\\ open
\\ close
\\ stinput
\\ stoutput

\\ on open
\\   in  for read
\\   out for write
\\ so
\\ the file is viewed as be in the outside

(set *mystream* (open "learning-shen.org" in))
(close (value *mystream*))
```

### read-byte

```shen
\\ read-byte
\\ write-byte
(set *mystream* (open "learning-shen.org" in))
(read-byte (value *mystream*))
(close (value *mystream*))
```

### write-byte

```shen
(write-byte 1 (stoutput))
(write-byte 2 (stoutput))
(write-byte 3 (stoutput))

\\ write will create and overwrite file when needed

(set *mystream* (open "kkk.org" out))

(write-byte 1 (value *mystream*))
(write-byte 2 (value *mystream*))
(write-byte 3 (value *mystream*))

(close (value *mystream*))
```

### nl [newline]

```shen
(nl)
\\ print a new line and return 0
(nl 10)
\\ print 10 new line and return 0
```

### pr [write-string]

```shen
(pr "abc")
(pr "abc" (stoutput))
\\ * denotes print
\\ * receiving a string and printing it to the terminal
\\   and returning it as a value
\\ * second optional argument
\\   is where you wish to print the string to
\\   the default place is (stoutput)
```

### print [write-sexp,after-eval]

```shen
(print (@v 1 2 <>))

(print [(@v 1 2 <>)
        (@v 3 4 <>)
        5])
```

### output [write-string,with-slot]

```shen
\\ * ~% print a new line
(output "a string~%")
(output "a string~%" (stoutput))

\\ return what output return
\\ but do not print
\\ output and print are implemented by make-string and pr
(make-string "a string~%")

\\ * ~A A denotes a thing
(output "~A says, hello world~%" "Fred")
(output "~A say, hello world~%" [Bill and Ben (+ 1 1)])

\\ * ~S with string's double quote
(output "~S says, hello world~%" "Fred")
(output "~S say, hello world~%" [Bill and Ben (+ 1 1)])

\\ * ~R with round bra-ket
(output "~R say, hello world~%" [Bill and Ben (+ 1 1)])
```

### about read

- a shen token is a sexp
- lineread
  reads in a line of Shen tokens terminated by a new line.
- read
  reads the first available Shen token
- input
  reads the first available Shen token and evaluates it
  returning a normal form
- input+
  receives a type T
  and a stream S
  and reads the first token off S
  evaluates it
  and returns the normal form
  if that token is of type T
  If the token is not of type T
  then an error is returned
- all these functions return an error empty stream
  if the stream is empty

### lineread [read-line,as-list]

```shen
\\ read one line of input to a list
(lineread)

\\ type:    1 2 3
\\ return: [1 2 3]

\\ type:    1 2 (+ 1 2)
\\ return: [1 2 [+ 1 2]]

\\ type:    1 2 [+ 1 2]
\\ return: [1 2 [cons + [cons 1 [cons 2 []]]]]
```

### read [read-sexp]

```shen
(read)

\\ type:   (+ 1 2)
\\ return: [+ 1 2]

\\ type:   [+ 1 2]
\\ return: [cons + [cons 1 [cons 2 []]]]
```

### input [read-sexp,and-eval]

```shen
(input)

\\ type:   (+ 1 2)
\\ return: 3

\\ type:   [+ 1 2]
\\ return: [+ 1 2]
```

### path

```shen
\\ (DEFUN make-pathname (Name) (MAKE-PATHNAME :NAME Name))
(LOAD "test.lsp")
(make-pathname "abc")
```

### file

```shen
\\ as string
(write-to-file "factorial.shen"
"
(define factorial
  0 -> 1
  X -> (* X (factorial (- X 1))))
"
)

(read-file "factorial.shen") \\ as sexp
(read-file-as-bytelist "factorial.shen")

(load "factorial.shen") \\ eval the file
(factorial 3)
```

### hush

*hush* is set by default to false
If set to true
then all messages printed from output and print are disabled
through messages using pr will still be printed to the target stream
Effectively this disables system reports from Shen
and all printing is then driven by the user
This very useful feature was suggested by Ramil Farkshatov
as an aid to diagnostics
```shen
(set *hush* true)

(pr "abc")
```

# name

## define

- 所有的函數都必須用 pattern-match 來定義
  模式匹配在於綁定約束變元
  而
  簡單的函數參數作爲約束變元的的綁定
  只不過是一般的模式匹配所能形成的約束變元的綁定的特殊情況
- 模式匹配是處理結構化數據的良好方式
  因爲此時結構化數據的結構
  能得以最直觀的方式被展現給讀者
- 模式匹配只有和數據構造子一起使用才好
  因爲豐富的數據的構造子是產生結構化數據的主要方式

## zero arg

```shen
(define kkk
  -> 1)
(kkk)
```

## two name-space

- symbol is a symbol
  (value symbol) is the value bound to symbol
  (function symbol) is the function bound to symbol

```shen
(define kkk
  X -> 1)
(kkk 90)
((function kkk) 90)

(set kkk (lambda X X))
((value kkk) 90)
```

## simple ^-^

```shen
(define factorial
  0 -> 1
  X -> (* X (factorial (- X 1))))
(factorial 1)
(factorial 3)
(factorial 6)

\\ 在 @s 這個構造子所形成的模式中
\\ 從前向後匹配子字符串
\\ 每個約束變元匹配一個長度爲1的字符串
\\ 尾部約束變元特殊處理
(define kkk
  (@s A B C) -> C)
(kkk "123456")

(define kkk
  (@s A "456") -> A)
(kkk "1456")
(kkk "123456")

(define kkk
  (@s "___" A "___" String-tail) -> A)
(kkk "___k___123")

(define kkk->aaa
  "" -> ""
  (@s "kkk" Tail) -> (@s "aaa" (kkk->aaa Tail))
  (@s S Tail) -> (@s S (kkk->aaa Tail)))
(kkk->aaa "kkk xxx kkk xxx")

(define total
  [] -> 0
  [X | Y] -> (+ X (total Y)))
(total [1 2 3])
```

## where and <-

```shen
\\ 用 where 來做條件匹配
(define mmm
  X Y -> X where (> X Y)
  _ Y -> Y)
(mmm 1 2)

\\ backtracking is invoked by using <- in place of ->
\\ 匹配到一個值了 然後拿來用了
\\ 然後發現不對勁的時候 只要返回 (fail)
\\ 就退出這個此比配 而進入下一個匹配
\\ 這比 where 更靈活
(define mmm
  X Y <- (if (> X Y)
             X
             (fail))
  _ Y -> Y)
(mmm 1 2)
```

# eval

- 這裏是 [] 的劣勢
  因爲爲了寫一個以鏈表方式表示的函數
  需要改變很多括號
  一個好

```shen
(eval [+ 1 2])

(eval
 [define factorial
   0 -> 1
   X -> [* X [factorial [- X 1]]]])
(factorial 6)

(eval
 [define factorial
   0 -> 1
   (hd (cons X [Y])) -> [* X [factorial [- X 1]]]])

(eval
 [define rev
   [] -> []
   [cons X Y] -> [append [rev Y] [cons X []]]])
(rev [1 2 3])
```

# native call to sbcl

```shen
\\ a native common lisp function is uppercase

((protect REVERSE) [1 2 3])

(define my-reverse
  X -> ((protect REVERSE) X))
(my-reverse [1 2 3])

\\ In order to load a CL file,
\\ the readtable must be reset
\\ and re-reset after loading
\\ to conform to the differences between CL and Shen.
\\ The function load-lisp in the following program will do that;
\\ (load-lisp "foo.lisp")
\\ will load the CL file foo.lsp.

(define load-lisp
  File -> (trap-error
           (let LispReadTable (readtable upcase)
                Load ((protect LOAD) File)
                ShenReadTable (readtable preserve)
                loaded)
           (/. Error
               (do (readtable preserve)
                   (error (error-to-string Error))))))

(define readtable
  Case -> (let String
            (make-string
             "(SETF (READTABLE-CASE *READTABLE*) ~A)"
             (cases (= Case upcase) ":UPCASE"
                    (= Case downcase) ":DOWNCASE"
                    (= Case preserve) ":PRESERVE"
                    (= Case invert) ":INVERT"
                    true (error "case ~A not recognised~%" Case)))
            ((protect EVAL) ((protect READ-FROM-STRING) String))))


\\ Note that the CL functions loaded from a CL file in this way
\\ will revert to uppercase after load-lisp is finished.
\\ Hence a function 'foo' in the CL file
\\ will be invoked by 'FOO' from within Shen.
```

# lazy-eval

- 在 shen 中沒有零元的 lambda-abstraction
  所以不能直接用 lambda-abstraction 來形成惰性求值

```shen
(freeze (+ 8 9))
(thaw (freeze (+ 8 9)))
```

# exceptions

```shen
(simple-error "ererer")

(set kkk 999)

\\ 不會有新的賦值
\\ 當 simple-error 作用的時候 就會做一個全局的跳出
(set kkk (simple-error "ererer"))


\\ "error" has the same formating features as "output"
(error "this is an error message, followed by a new line~%")


\*

(trap-error)
  exp:
  call-with-error:
1. if meet error(exception) in exp
   fun will be called with the error as an argument
2. if not meet error in exp
   the whole will be as exp

,*\

(trap-error
 (error "this is an error message, followed by a new line~%")
 (/. E "I trapped the error."))

(trap-error
 (simple-error "ererer")
 (/. E (+ E E)))

(trap-error
 (error "this is an error message, followed by a new line~%")
 (/. E (error-to-string E)))

(trap-error
  1
  (/. E (error-to-string E)))
```

# type

## type check value

```shen
\\ type check
(tc +)

\\ no type check
(tc -)


kkk
\\ kkk : symbol

"kkk"
\\ "kkk" : string

(@s "10" " green" " bottles")
\\ "10 green bottles" : string

true
\\ true : boolean
(= 4 5)
\\ false : boolean

666
\\ 666 : number
(* 2.3 2)
\\ 4.6 : number

[1 a]
\\ type error

[1 2 3]
\\ [1 2 3] : (list number)

(@p 1 2 a)
\\ (@p 1 (@p 2 a)) : (number * (number * symbol))

(@v 1 2 3 <>)
\\ <1 2 3> : (vector number)

(freeze (* 7 8))
\\ #<FUNCTION (LAMBDA ()) {100442ED6B}> : (lazy number)

(/. X X)
\\ #<FUNCTION (LAMBDA (X)) {100461DCCB}> : (A --> A)

(freeze (/. X X))
\\ #<FUNCTION (LAMBDA ()) {10046B71BB}> : (lazy (A --> A))
```

## define typed function

```shen
\\ typed version of the following function is hard
(tc -)
(define tuple->list
  (@p X Y) -> [X | (tuple->list Y)]
  X -> [X])
(tuple->list (@p 1 2 3))


(tc +)

(define factorial
  {number --> number}
  0 -> 1
  X -> (* X (factorial (- X 1))))
(factorial 1)
(factorial 3)
(factorial 6)

(define kkk->aaa
  {string --> string}
  "" -> ""
  (@s "kkk" Tail) -> (@s "aaa" (kkk->aaa Tail))
  (@s S Tail) -> (@s S (kkk->aaa Tail)))
(kkk->aaa "kkk xxx kkk xxx")


(define unit-vector?
  {(vector A) --> boolean}
  (@v _ <>) -> true
  _ -> false)
(unit-vector? (@v 1 <>))
(unit-vector? (@v 1 2 <>))

(define unit-string?
  {string --> boolean}
  (@s X "") -> true
  _ -> false)
(unit-string? "a")
(unit-string? "abc")


\\ NO side effect
(define vector-double
  {(vector number) --> (vector number)}
  <> -> <>
  (@v X V) -> (@v (+ X X) (vector-double V)))
(vector-double (@v 1 2 3 <>))

(define remove-duplicates
  {(list A) --> (list A)}
  [] -> []
  [X | Y] -> (remove-duplicates Y) where (element? X Y)
  [X | Y] -> [X | (remove-duplicates Y)])
(remove-duplicates [2 3 3 3 3  2 3 3 3 3 3])

(define total
  {(list number) --> number}
  [] -> 0
  [X | Y] -> (+ X (total Y)))
(total [1 2 3])

(define member
  {A --> (list A) --> boolean}
  _ [] -> false
  X [X | _] -> true
  X [_ | Y] -> (member X Y))
(member 1 [1 2 3])
(member 4 [1 2 3])
(member a [1 2 3])


\\ 下面是二元函數的迭代
(define foldl
  {(A --> A --> A) --> A --> (list A) --> A}
  F Z [] -> Z
  F Z [X | Xs] -> (foldl F (F Z X) Xs))
(foldl (function +) 0 [1 2 3])
```

## nick name of type

```shen
(tc +)

(synonyms coordinate (number * number))

\\ type:
(@p 1 2) : coordinate
\\ echo:
(@p 1 2) : (number * number)
```

## sequent calculus

- 模仿邏輯學中的古怪語法來定義新的數據類型
  是設計上的敗筆

```shen
(tc +)

(datatype color

  ____________
  yellow : color;

  __________
  red : color;

  ___________
  green : color;)

(datatype color

  if (element? X [red yellow green blue])
  __________________________________
  X : color;)

(define kkk
  {color --> string}
  X -> (str X))

(kkk red)
(kkk 1)



(datatype rank

  if (element? X [ace 2 3 4 5 6 7 8 9 10 jack queen king])
  __________________________________
  X : rank;)

(datatype suit

  if (element? Suit [spades hearts diamonds clubs])
  __________________________________
  Suit : suit;)

(datatype card

  Rank : rank;
  Suit : suit;
  __________________________________
  [Rank Suit] : card;

  Rank : rank, Suit : suit >> P;
  __________________________________
  [Rank Suit] : card >> P;
  )

(datatype card

  Rank : rank;
  Suit : suit;
  ==================
  [Rank Suit] : card;)

[5 spades]
[king hearts]
[king hearts] : card

(define get-suit
  {card --> suit}
  [Rank Suit] -> Suit)
(get-suit [5 spades])
(get-suit [king hearts])
```

# macro

```shen
(defmacro macro-add1
  [add1 N] -> [+ N 1]
  [sub1 N] -> [- N 1])

(value *macros*)
(function macroexpand)

(add1 100)
(sub1 100)

(macroexpand [sub1 100])

(defmacro exec-macro
  [exec Expr]
  -> [trap-error [time Expr] [/. (protect E) failed]])

(exec
 (value *macros*))

(macroexpand
 [exec
  [value *macros*]])
```

# compiler-compiler

## 用 defcc 來定義作用於鏈表的謂詞

```shen
\\ 句子 == 名詞詞組 動詞詞組
(defcc <sent>
  <np> <vp>;)
\\ 名詞詞組 == 冠詞 名詞 | 專有名詞
(defcc <np>
  <det> <n>;
  <name>;)
\\ 冠詞 == 定冠詞 | 不定冠詞
(defcc <det>
  the;
  a;)
\\ 一些名詞
(defcc <n>
  cat;
  dog;)
\\ 一些專有名詞
(defcc <name>
  bill;
  ben;)
\\ 動詞詞組 == 謂語動詞 名詞詞組
(defcc <vp>
  <vtrans> <np>;)
\\ 一些謂語動詞
(defcc <vtrans>
  likes;
  chases;)

(compile (function <det>)
         [the])
(compile (function <det>)
         [a the])
(compile (function <vp>)
         [chases the cat])
(compile (function <sent>)
         [the cat likes the dog])
(compile (function <sent>)
         [the cat likes the canary])


(defcc <bcs>
  [<bs>] [<cs>];)

(defcc <bs>
  b <bs>;
  b;)

(defcc <cs>
  c <cs>;
  c;)

(compile (function <bcs>)
         [[b b b] [c c]])
(compile (function <bcs>)
         [[b b b] [c c]
          kkk])
(compile (function <bcs>)
         [kkk
          [b b b] [c c]])
```

## semantic actions in yacc

```shen
(defcc <as>
  a <as>;
  a;)

(defcc <as>
  a <as> := [b | <as>];
  a := [b];)

(compile (function <as>)
         [a a a a a])


(define question
  NP VP -> (append
            [(protect Is) it true that]
            NP VP
            [?]))

(question (compile (function <sent>)
                   [the cat likes the dog])
          [kkk])
```

## reserved non-terminals, pattern matching

- <e> always succeeds consuming
  none of the input and under semantic completion
  returns the empty list
- <!> always succeeds and consumes
  all of the input and under semantic completion
  returns that remaining input
- variables and wildcards are allowed to pattern match
  under shen-yacc as in shen
  and lists can be embedded in the input.
  the | notation is not used in the parsing
  (to the left of :=)
  but can occur to the right

# prolog

```shen
(defprolog member
  X [X | _] <--;
  X [_ | Y] <-- (member X Y);)

(defprolog rev
  [] [] <--;
  [X | Y] Z <-- (rev Y W) (conc W [X] Z);)

(defprolog conc
  [] X X <--;
  [X | Y] Z [X | W] <-- (conc Y Z W);)

(prolog? (member 1 [1 2]))
(prolog? (member 0 [1 2]))
(prolog? (member X [1 2]))
(prolog? (member X [1 2]) (return X))
(prolog? (rev [1 2] X) (return X))
```

# package

```shen
(package aaa. [bbb] ccc)
```
