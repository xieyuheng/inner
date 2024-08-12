---
title: lisp-tutorial
---

# function application

- 作爲一個相對古典的語言
  沒有隱式的 curry
  多元函數的作用是嚴格的
- "作用" 是
  以 "()" 爲邊綴符號的
  多元函數
  邊界所包圍的值中
  有一個是特殊的 被視爲函數
  而其餘的 被視爲其參數
- 在邊綴表達式 所界定的邊界內
  1. 參數個數不夠時
     直接報告錯誤
  2. 參數個數多出時
     也可能還有默認的處理方式
     尤其是二元函數
     尤其是具有結合性的二元函數
- 只要注意
  在邊綴表達式 所界定的邊界內
  元素個數的不同會引起不同的處理方式就可以了
  正是邊綴表達式的使用 使得同一個函數能夠承載多種語義的
- 或者用 apply 和 funcall
```lisp
(+ 1 2)
```

# apply & funcall

- function as argument

```lisp
(+ 1 2 3)

;; on named function
(apply (function +) '(1 2 3))
(apply #'+ '(1 2 3))
(apply '+ '(1 2 3))

;; funcall 是必要的
;; 因爲 邊綴表達式 () 的頭上的 symbol
;; 會被解釋爲一個函數名 而不會被當作局部變元處理
(funcall (function +) 1 2 3)
(funcall #'+ 1 2 3)
(funcall '+ 1 2 3)

;; I will use (function +) always
(sort (list 4 2 3 1) (function <))

;; on lambda function
(funcall #'(lambda (x) (+ x 100))
         1)
(funcall (function (lambda (x) (+ x 100)))
         1)
(funcall (lambda (x) (+ x 100))
         1)
(apply (lambda (x) (+ x 100))
       '(1))

;; I will use (lambda (x) (+ x 100)) always
(mapcar (lambda (x) (+ x 10))
        '(1 2 3 4 5))
```

# lambda abstraction

- λ xyz : N  =

  (lambda (x y z) N)

```lisp
(lambda (a b) (+ a b))

((lambda (a b) (+ a b))
 1 2)


;; &rest

((lambda (x1 x2 &rest x333)
   (list x1 x2 x333))
 1 2 3 3 3 3 3 3)


;; &optional

((lambda (x1 x2 &optional x3)
   (list x1 x2 x3))
 1 2)

((lambda (x1 x2 &optional x3)
   (list x1 x2 x3))
 1 2 9)

((lambda (x1 x2 &optional (x3 3))
   (list x1 x2 x3))
 1 2)

((lambda (x1 x2 &optional (x3 3))
   (list x1 x2 x3))
 1 2 9)


;; &key

((lambda (x1 x2 &key x3)
   (list x1 x2 x3))
 1 2 :x3 9)

((lambda (x1 x2 &key (x3 3))
   (list x1 x2 x3))
 1 2)
```

# data type

## boolean

```lisp
;; t as true
t

;; nil as false
()
'()
nil
'nil
```

## symbol

```lisp
;; case-insensitive
(eq 'kkk 'KKK)

;; value name space
(defvar *kkk* 5)
(symbol-value '*kkk*)
(boundp '*kkk*)

(defun kkk () 'k)
(symbol-function 'kkk)

(list '|ci ci ci|
      '|ca,ca,ca|
      '|(da da da)|
      '|   |
      '|abc|
      '|ABC|
      '|\|\|\||
      '|\\\\\\|)


;; symbol->string
;; i.e. hash-back
(symbol-name 'kkk)
(mapcar (lambda (symbol)
          (list (symbol-name symbol)
                (length (symbol-name symbol))))
        (list '|ci ci ci|
              '|ca,ca,ca|
              '|(da da da)|
              '|   |
              '|abc|
              '|ABC|
              '|\|\|\||
              '|\\\\\\|))


;; 每一個 key 就等價於所有 symbol 所組成的一個新的命名空間
;; 不過這種全局的性質 一般只有語言的核心部分才會使用到
;; 否則不同的人所寫的程序就相互衝突了
;; 而 一般的程序都只使用一般性質的 hash-table
(setf (get 'kkk 'color1) 'red
      (get 'kkk 'color2) 'yellow
      (get 'kkk 'color3) 'blue)
(get 'kkk 'color1)
(symbol-plist 'kkk)

;; 其實 (function symbol) 的更清晰的語義是
;;   (get 'symbol 'function)
;; 而 symbol 的更清晰的語義是
;;   (get 'symbol 'value)
;; 這兩個命名空間並沒有什麼特殊性
;; 尤其是當能以如此的方式爲某個命名空間提供特殊的語法時
;; 就能形成對多個命名空間的良好利用
;; 比如 package[module] 和 type

;; 返回兩個值
;; 第二個值是一個對函數運行狀態的報告
;; intern 以 package name 爲額外參數
;; 這說明不同的 package
;; 有不同的 hash-function 和 hash-table 邪
(intern "RANDOM-SYMBOL")
(eq (intern "RANDOM-SYMBOL")
    'random-symbol)
(eq (intern "random-symbol")
    'random-symbol)
```

## number

```lisp
(expt (expt (expt (expt 10 10) 10) 10) 10)
(+ 5/9 3/4)
(* 2 (+ #c(10 5) 4))
```

## list

## array

- matrix
  is called array
- one dimension matrix
  is called vector
- 然而其實
  就實現方式而言
  高維的結構是用一維的結構實現的
  而不是相反
  這是由計算機之內存的線性的[相對線性的]尋址方式決定的
- 此處的設計其實還是相當優良的
  common-lisp 之不好之處幾乎就只在於
  因爲 由一個成員構成複雜的委員會而領導
  所以 其某些設計決策爲了兼顧各方而混亂
- 當然其不好之處還在於對函數的晦澀命名
  然而這幾乎是所有需要定義術語的領域的通病
  common-lisp 做的其實已經很好了
- 其實這裏的 '() 與 #()
  也都可以稱作是邊綴表達式
  只不過其形態不對稱而已
```lisp
;; 在 sbcl 中 默認的是
(make-array '(2 3) :initial-element 0)
(make-array '(2 3))


(setf a3
      (make-array '(3) :initial-element nil))
(setf v3
      (make-array 3 :initial-element nil))
;; 這裏可以看 出語法設計的不規則性
;; 儘管這裏的 不規則性 其實是情有可原的


(setf a23
      (make-array '(2 3) :initial-element nil))

(setf a234
      (make-array '(2 3 4) :initial-element nil))



;; 很直觀地
;;   長度爲 2 的向量中 包含 長度爲 3 的向量
;;   長度爲 3 的向量中 包含 長度爲 4 的向量

;; literal array as the following
#3a(((nil nil nil nil) (nil nil nil nil) (nil nil nil nil))
    ((nil nil nil nil) (nil nil nil nil) (nil nil nil nil)))

(setf *print-array* t)
#3a(((nil nil nil nil) (nil nil nil nil) (nil nil nil nil))
    ((nil nil nil nil) (nil nil nil nil) (nil nil nil nil)))

(setf *print-array* nil)
#3a(((nil nil nil nil) (nil nil nil nil) (nil nil nil nil))
    ((nil nil nil nil) (nil nil nil nil) (nil nil nil nil)))


;; aref denotes array-reference
(aref a23 0 0)
(setf (aref a23 0 0) 1)


(make-array '(3) :initial-element nil)
(make-array 3 :initial-element nil)
(vector nil nil nil)

(vector "a" 'b 3)
(setf v (vector "a" 'b 3))
(aref v 0)

(svref v 0)
;; svref denotes simple-vector
;; simple as
;;   not adjustable
;;   not displaced
;;   not has a fill-pointer


(arrayp #3a(((nil nil nil nil) (nil nil nil nil) (nil nil nil nil))
            ((nil nil nil nil) (nil nil nil nil) (nil nil nil nil))))
(vectorp #3a(((nil nil nil nil) (nil nil nil nil) (nil nil nil nil))
             ((nil nil nil nil) (nil nil nil nil) (nil nil nil nil))))
```

## char & string

- string is char-vector

```lisp
(char-code #\@) ;; 64
(code-char 64)
(char-code #\中)
(code-char 20013) ;; #\U4E2D

(char< #\A #\a)

(sort "elbow" (function char<))

(aref "abc" 0)
(char "abc" 0)

;; destructive function (setf aref)
;; called on constant data
(let ((str "Merlin"))
  (setf (char str 3) #\k)
  str)

(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)

(copy-seq #(1 2 3))

(equal "lisp" "lisp")
(equal "lisp" "LISP")

(equalp "lisp" "lisp")
(equalp "lisp" "LISP")


(string-equal "lisp" "LISP")

(equal #(1) #(1))
(equalp #(1) #(1))


;; nil means do not print at all
;; it makes format become make-string
(format nil "~A or ~A" "truth" "dare")
(concatenate 'string "not " "to worry")


(princ '("kkk" "kkk" "kkk"))
(princ "he yelled \"stop that thief!\" from the busy street.")
```

## sequence

- 儘管在 common-lisp 中
  並沒有一個機制來以一致的方式
  表示數學結構之間的複雜關係
  但還是嘗試使用 sequence
  來綜合 lisp 和 vector 這兩個數學結構
- ><><><
  同樣是試圖捕捉數學結構間的關係
  以避免對處理函數的重複定義
  [正如 以避免對類似命題的重複證明]
  但是 common-lisp 與 haskell 對此的處理方式非常不同
  這種處理方式直接的差別
  以及其各自所達到的效果
  值得仔細分析
- 另外在神語中
  key-word argument 完全可以用模式匹配來實現
```lisp
(elt '(a b c) 0)
(elt #(a b c) 0)
(elt "abc" 0)


:key
:test
:from-end
:start
:end

(position #\a "fantasia")
(position #\a "fantasia" :start 3 :end 5)
(position #\a "fantasia" :start 5 :end nil)

(position #\a "fantasia" :from-end t)

(position 'a '((c d) (a b)) :key (function car))
(position 'a `(,(car '(c d)) ,(car '(a b))))
(position 'a (list (car '(c d)) (car '(a b))))

(position '(a b) '((a b) (c d)))
(position '(a b) '((a b) (c d)) :test (function eql))
(position '(a b) '((a b) (c d)) :test (function equal))

(position 3 '(1 0 7 5) :test (function <))


(defun second-word (string)
  (let* ((sqace (code-char 32))
         (position-after-sqace
          (+ (position sqace string) 1)))
    (subseq string position-after-sqace
            (position sqace string
                      :start position-after-sqace))))
(second-word "Form follows function.")


(position-if (function oddp) '(2 2 2 3 3))
(position-if (function oddp) '(2 2 2))

(find #\a "cat")
(find-if (lambda (char) (char= #\a char))
         "cat")

(find 'a '(c a t))
(find-if (lambda (symbol) (eq 'a symbol))
         '(c a t))

(member 'a '(c a t))
(member-if (lambda (symbol) (eq 'a symbol))
           '(c a t))


(find-if (lambda (x)
           (eql (car x) 'a))
         '((c c) (a a) (t t)))
(find 'a
      '((c c) (a a) (t t))
      :key (function car))


;; like foldl
(reduce (function intersection)
        '((b r a d s) (b a d) (c a t)))
(intersection (intersection '(b r a d s)
                            '(b a d))
              '(c a t))
```

## structure

- 這種用來定義一系列函數的函數
  在蟬語中也將常用與將某些些代碼的模式結構化
  然而
  我將設計良好的命名規則
  以讓相關的操作都變得瞭然

```lisp
(defstruct point
  x
  y)


(make-point)

;; make-point
;; point-p
;; copy-point
;; point-x
;; point-y

(setf p (make-point :x 0 :y 0))

(point-p p)
(typep p 'point)

(point-x p)
(point-y p)
(setf (point-y p) 2)



(defstruct polemic
  (type (progn
          (format t "What kind of polemic was it? ")
          (read)))
  (effect nil))
(setf kkk (make-polemic))

(defstruct (point (:conc-name p)
                  ;; change "point-" to "p"
                  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p stream depth)
  (format stream "#<~A,~A>" (px p) (py p)))

(setf p (make-point :x 0 :y 0))

(point-p p)
(typep p 'point)

(px p)
(py p)
(setf (py p) 2)
```

## hash table

```lisp
(setf color-table (make-hash-table))

;; gethash 返回兩個值
;; 其中第二個值是一個 type-tag
;; type-tag 被用以表明這個位置的是否有值存入
;; nil 不能作爲信號來表明沒有值存入
;; 因爲 nil 本身就可能是被存入的值
;; >< 這種信號性的返回值在蟬語中應該如何設計呢
;; 有更好的處理方式嗎?
;; 如果只是使用多返回值的話 那麼在蟬語中是很簡單的
(gethash 'color1 color-table)

;; 萬能的 setf
(setf (gethash 'color1 color-table) 'red
      (gethash 'color2 color-table) 'yellow
      (gethash 'color3 color-table) 'blue)

;; remhash 的返回值 只有一個
;; 並且是一個信號性的值 代表需要進行 remove
(remhash 'color1 color-table)

(maphash (lambda (key value)
           (format t "~A = ~A ~%" key value))
         color-table)



;; 注意這列的 hash-table 是一般性的
;;   hash-function 可以作用於的值可以是任何類型的
;;   而其 可以存儲的值也可以是任何類型的
;; 與蟬語中的並不同類

(defun kkk (x) x)

(setf bug-table (make-hash-table))
(setf (gethash (function kkk) bug-table)
      "kkk took my baby away")
;; (push "kkk took my baby away"
;;       (gethash (function kkk) bug-table))
(gethash (function kkk) bug-table)

;; 重新定義之後就不被認爲是相同的值了
(defun kkk (x) (+ x x))


;; 關於實現方式
;; 說 hash-table 的大小會在需要的時候自動增加
;; 難道 hash-function 能夠以漸進的方式被改寫?
;; 也許
;; 確實
;; 巧妙的數論函數可以完成很多讓人意想不到的任務
;; ><><><
;; 值得好好研究一下數論函數在 hash-function 中的應用

;; 因爲需要查找 所以又涉及到對不同的謂詞[等詞]的處理
;; 這又是實現上的一個難點
(setf writers (make-hash-table :test (function equal)))
(setf (gethash '(ralph waldo emerson) writers) t)
```

# side effect

## assignment

- defvar 定义全局变量
  defparameter 定义全局变量
  并且所作的绑定不会被 defvar 修改
  let 定义局部变量
- flet 定义局部非递归函数
  labels 定义局部递归函数
- 注意 其二類分屬兩個命名空間

```lisp
;; global
;; 全局的名 是值與 symbol 的綁定
;; 而局部的名 的實現方式各異

;; setf 和 setq 是在全局和局部都通用的
;; 不知道他們又什麼區別

(defparameter kkk 1)
(boundp 'kkk)
kkk
(defvar kkk 2)
;; 如果 kkk 之前已經被定義過了
;; 那麼 defvar 不會重新綁定其值 但是也不會報錯
kkk

(defvar xxx 1)
xxx
(defvar xxx 2)
xxx

(setf xxx1 1)
xxx1
(defvar xxx1 2)
xxx1

(setq xxx2 1)
xxx2
(defvar xxx2 2)
xxx2

;; 只要是對這個命名空間的綁定都是如此



(defparameter *kkk* 10)

(defconstant LIMIT (+ *kkk* 1))

(boundp '*kkk*)
(boundp 'limit)

(setf a 'b
      c 'd
      e 'f)

;; generalized references
(setf x (list 'a 'b 'c))
(setf (car x) 'k) ;; (set-car! x 'n)
(setf (car (cdr x)) 'k)
(setf (car (cdr (cdr x))) 'k)

;; 只要把 () 視爲一個邊綴表達式
;; 那麼下面的語義的實現方式就容易理解了
(defparameter *colours* (list 'red 'green 'blue))
(setf (car *colours*) 'yellow)
(push 'red (cdr *colours*))

;; 但是 common-lisp 中 相關的語法還是顯得混亂
;; 在蟬語中 我要以一致的方式來解決這些問題


;; local
(let ((x 6)
      (y 5))
  (* x y))

(let ((x6 6)
      (y 5))
  (boundp 'x6))

;; 局部的 名 與值的綁定也是可以更改的
(let ((x 6)
      (y 5))
  (setf x 2)
  (* x y))

(let ((x 6)
      (y 5))
  (setq x 2)
  (* x y))

;; 局部的 counter 在 add-counter 內部
(let ((counter 10))
  (defun add-counter (x)
    (prog1
        (+ counter x)
      (incf counter))))

(setf counter 10)

(boundp 'counter)

(mapcar (function add-counter) '(1 1 1 1))
(add-counter 50)


;; 簡陋的模式匹配
(destructuring-bind (w (x y) . z) '(a (b c) d e)
  (list w x y z))
```

## io

### read [read-sexp]

```lisp
(defun ask-for-list ()
  (format t " ^-^ please enter a list: ")
  (let ((val (read)))
    (if (listp val)
        val
        (ask-for-list))))
```

### format

```lisp
(defun format-names (list)
  (format nil "~{~:(~a~)~#[.~; and ~:;, ~]~}" list))

(format-names '(doc grumpy happy sleepy bashful
                sneezy dopey))
"Doc, Grumpy, Happy, Sleepy, Bashful, Sneezy and Dopey."
(format-names '(fry laurie))
"Fry and Laurie."
(format-names '(bluebeard))
"Bluebeard."
```

### format & do & dolist

- 在考慮各種語法糖的時候
  注意它們是如何引入約束變元的
  對約束變元的使用 是不同編程風格的特徵

```lisp
(defun show (start end)
  (do ((i start (+ i 1)))
      ((> i end)
       (format t "~% finish ^-^"))
    (format t "~% ~A ~A ~A ~A" i (* i i) (* i i i) (* i i i i))))
(defun show (i end)
  (if (> i end)
      'done
      (progn
        (format t "~% ~A ~A ~A ~A" i (* i i) (* i i i) (* i i i i))
        (show-squares (+ i 1) end))))
(show 3 9)


(defun how-long? (lst)
  (let ((len 0))
    (dolist (_ lst)
      (setf len (+ len 1)))
    len))
(defun how-long? (lst)
  (if (null lst)
      0
      (+ (how-long? (cdr lst)) 1)))
(how-long? '(1 2 3))
```

### path

```lisp
(user-homedir-pathname)

(setf *default-pathname-defaults* (user-homedir-pathname))

(make-pathname :name "kkk~")

;; literal
#P"kkk~"
```

### file & read & format

- stream 作爲輸入輸出的一種抽象
- ><><><
  輸入輸出的形式多種多樣
  仔細想來其類別相當複雜
  值得仔細分析一下
- 最簡單的有兩種
- 文件的讀寫
  其樣貌類似與文本編輯器的 buffer
  把文件從硬盤讀到一塊內存中
  然後修改 然後保存回硬盤
  此時在 buffer 中
  可以有[一個或多個]類似光標的指針
- 基本輸入輸出

```lisp
;; form a (file . buffer) pair
(setf stream (open (make-pathname :name "kkk~")
                   :direction ':output
                   :if-exists ':supersede))
;; edit the buffer
(format stream "kkk took my baby away ~%")
;; save-buffer-to-file
(close stream)

(with-open-file (stream (make-pathname :name "kkk~")
                        :direction ':output
                        :if-exists ':supersede)
  (format stream "with-open-file~%")
  (format stream "kkk took my baby away~%"))


(setf stream (open (make-pathname :name "kkk~")
                   :direction ':input))
(read-line stream)



;; 在讀文件的時候也有一個類似光標的隱含的指針
;; 我應該把這個指針明顯化
;; 只不過在從命令行中讀字符時
;; 這個指針是不能隨便亂動的 因爲後面的字符還沒輸入呢
(with-open-file (stream (make-pathname :name "kkk~")
                        :direction ':input)
  (list (read-line stream)
        (read-line stream)))

(with-open-file (stream (make-pathname :name "kkk~")
                        :direction ':input)
  (list (read-line stream)
        (read-line stream)
        (read-line stream)))

(with-open-file (stream (make-pathname :name "kkk~")
                        :direction ':input)
  (list (read-line stream nil)
        (read-line stream nil)
        (read-line stream nil)
        (read-line stream nil)))

(with-open-file (stream (make-pathname :name "kkk~")
                        :direction ':input)
  (list (read-line stream nil 'eof)
        (read-line stream nil 'eof)
        (read-line stream nil 'eof)
        (read-line stream nil 'eof)))

(defun read#line (&key
                    (from *standard-input*)
                    (eof-as-error? t)
                    (read-eof-as 'eof))
  (read-line from eof-as-error? read-eof-as))

(with-open-file (stream (make-pathname :name "kkk~")
                        :direction ':input)
  (list (read#char :from stream :eof-as-error? nil)
        (read#char :from stream :eof-as-error? nil)
        (read#char :from stream :eof-as-error? nil)
        ))


;; 可選擇的參數都應該用 &key 來定義
;; 應該在省略 :key 的時候遵從默認的順序
;; 而不應該使用 &optional
;; 這是函數調用語法接口的設計失誤

;; read-line 又是需要返回一些信號的例子
;; 因此 它返回兩個值
;; 第一個是所讀入的字符串
;; 第二個
;;   以 nil 表 正常讀入
;;   以 t 表 沒有遇到 newline 字符 被讀的東西就結束了

(defun pseudo-cat (file)
  (with-open-file (str file rdirection :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))

;; read as read-sexp
;; 應該把用以實現 read-line 和 read-sexp 的函數暴露出來
;; 使用戶能夠自己定義閱讀器


;; 同 read 也是 read-sexp
;; 這種閱讀中 因爲沒有 eval
;; 所以 symbol 是不用加引號的
(read-from-string "aaa bbb ccc")
;; 返回兩個參數
;; 其二是 光標的位置
;; 在上面的例子中
;; 光標在 bbb 的第一個 b
;; 因爲只有讀到 bbb 前面的空格時
;; 才能判定出一個 symbol
;; 並且猶豫 光標不能回退 所以就停在了 b

;; 在光標可以回退的時候也沒有回退
;; 這是爲了統一處理兩種不同類型的輸入而設計的
;; 但是這並不是良好的設計

;; 所以在使用時
;; 爲了靈活性 可以總是把文件讀入到字符串中來處理
;; 這樣 就能夠以明顯的方式實現一個或多個光標了

;; 另有 read-char
;; 而 peek-char 是 read-char 的 不移動光標的版本



(princ "Hello")
(prin1 "Hello")  ;; with quote
(terpri) ;; newline
;; 這些函數傻逼名字就足以讓我不使用他們了
;; 所以只使用 format

;; 而 format 的設計失誤在於
;; 不應該用 t 和 nil 來做默認的參數
;;   因爲語義不清晰
;; 不應該用 format-string (or control-string)
;;   而應該用可以以更靈活的方式排版的語法
;;   以增加可讀性和靈活性
```

## dynamic scoped global variables

*standard-input*
*package*
*readtable*
*print-readably*
*print-circle*

```lisp
;; dynamically rebinding
;; the built-in special variable *standard-output*
(with-open-file (file-stream #p"kkk~"
                             :direction :output)
  (let ((*standard-output* file-stream))
    (print "This prints to the file, not stdout."))
  (print "And this prints to stdout, not the file."))
```

# name

## defun

```lisp
(defun explode (string &optional (delimiter #\Space))
  (let ((pos (position delimiter string)))
    (if (null pos)
        (list string)
        (cons (subseq string 0 pos)
              (explode (subseq string (1+ pos))
                       delimiter)))))

(explode "foo,     bar, baz" #\,)
(explode "foo, bar,     baz")
```

## two name-space

- 'symbol is a symbol
  symbol is the value bound to symbol
  (function symbol) is the function bound to the symbol

```lisp
(setf (symbol-function 'kkk)
      (lambda () 'function-name-space))
(defun kkk ()
  'function-name-space)

(funcall (symbol-function 'kkk))
(funcall (function kkk))
;; the function bounded to a name in function-name-space
;; will be fetched
;; when the name is at the head of
;; the function application borderfix notation
(kkk)


(setf (symbol-value 'kkk)
      (lambda () 'value-name-space))
(defparameter kkk
  (lambda () 'value-name-space))

(funcall (symbol-value 'kkk))
;; the value bounded to a name in value-name-space
;; will be fetched
;; when the name is at the body of
;; the function application borderfix notation
(funcall kkk)
```

# macro

## [note]

- 在蟬語中
  因爲語法解析的過程與生成代碼的過程是結合在一起的
  所以 macro 可以直接用語法解析器來實現
  所以 macro 和函數一樣 是一等公民的
  而在 lisp 中情況並非如此

## defmacro

- 其實在寫 macro 的時候
  lisp 中對 symbol 的處理 完全可以換成是 shen 的語義
  否則這種底層的 macro 太難看了

```lisp
(defmacro nil! (x)
  `(setf ,x nil))
(nil! x1)

;; 返回的第二個值是信號
(macroexpand-1 '(nil! x2))
(macroexpand-1 '(kkk x))

(nil! a1)

((lambda (expr)
   (apply (lambda (x) `(setf ,x nil))
          (cdr expr)))
 '(nil! a2))

;; 不同的是
;; 實際上
;; 上面返回的鏈表 會作爲代碼被 編譯器處理
;; 而下面是用解釋器在處理所返回的代碼
(eval ((lambda (expr)
         (apply (lambda (x) `(setf ,x nil))
                (cdr expr)))
       '(nil! a3)))


(setf list '(a b c))
`(list is ,list)
`(its elements are ,@list)
```

## loop

```lisp
(defvar *list*
  (loop
     :for x := (random 1000)
     :repeat 10
     :collect x))

;; 下面的循环找出最大的偶数
(loop
   :for elt :in *list*
   :when (evenp elt)
   :maximizing elt)


(loop
   :for elt :in *list*
   :collect (log elt))

(loop
   :for elt :in *list*
   :collect (log elt) :into logs
   :finally
   (return logs))

(let ((*list* (loop
                 :for x := (random 1000)
                 :repeat 10
                 :collect x)))
 (loop
    :for elt :in *list*
    :collect (log elt) :into logs
    :finally
    (return
      (loop
         ;; 下面的(values ms ns)可以用来返回多值
         :for l :in logs
         :if (> l 6) :collect l :into ms
         :else :collect l :into ns
         :finally (return (values ms ns))))))

;; log是自然对数:(log 2.72828)
;; (expt 2.72828 6)
;; 所以上面返回的两个列表大概是6,4开
```

# reader macro

## build-in

```lisp
(read-from-string "(400 500 600)")
;; ==> (400 500 600)
;; ==> 13

(type-of (read-from-string "t"))
;; ==> BOOLEAN

#'+        ;; for functions
(type-of #'+)

#\\ ;; for literal characters
(type-of #\\)

#c(4 3)    ;; for complex numbers
(type-of #c(4 3))

#p"/path/" ;; for filesystem paths
(type-of #p"/path/")
```

## example

```lisp
(set-macro-character
 #\`
 (lambda (stream char)
   (list (quote quote)
         (read stream t nil t))))

(set-dispatch-macro-character
 #\# #\?
 (lambda (stream charl char2)
   (list 'quote
         (let ((1st nil))
           (dotimes (i (+ (read stream t nil t) 1))
             (push i 1st))
           (nreverse 1st)))))

(set-macro-character
 #\[
 (lambda (stream char)
   (list 'quote
         (let ((1st nil))
           (dotimes (i (+ (read stream t nil t) 1))
             (push i 1st))
           (nreverse 1st)))))

;;[10
#?10


(set-macro-character #\}
                     (get-macro-character #\)))

(set-macro-character
 #\{
 (lambda (stream char)
   (read-delimited-list #\} stream t)))

{null nil}
```

## json-reader

```lisp
;; (cl:defpackage #:json-reader
;;   (:use #:cl)
;;   (:export #:enable-json-syntax
;;            #:disable-json-syntax))

;; (cl:in-package #:json-reader)

(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +comma+ #\,)
(defconstant +colon+ #\:)

(defun transform-primitive (value)
  (if (symbolp value)
      (cond
        ((string-equal (symbol-name value) "true") t)
        ((string-equal (symbol-name value) "false") nil)
        ((string-equal (symbol-name value) "null") nil)
        (t value))
      value))

(defun create-json-hash-table (&rest pairs)
  (let ((hash-table (make-hash-table :test #'equal)))
    (loop for (key . value) in pairs
       do (setf (gethash key hash-table) value))
    hash-table))

(defun read-next-object (separator delimiter
                         &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
        (let* ((object (read input-stream t nil t))
               (next-char (peek-next-char)))
          (cond
            ((char= next-char separator) (discard-next-char))
            ((and delimiter (char= next-char delimiter)) nil)
            (t (error "Unexpected next char: ~S" next-char)))
          object))))

(defun read-separator (stream char)
  (declare (ignore stream))
  (error "Separator ~S shouldn't be read alone" char))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +comma+ 'read-separator)
    (loop
       for object = (read-next-object +comma+ +right-bracket+ stream)
       while object
       collect (transform-primitive object) into objects
       finally (return `(vector ,@objects)))))

(defun stringify-key (key)
  (etypecase key
    (symbol (string-downcase (string key)))
    (string key)))

(defun read-left-brace (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +comma+ 'read-separator)
    (set-macro-character +colon+ 'read-separator)
    (loop
       for key = (read-next-object +colon+ +right-brace+ stream)
       while key
       for value = (read-next-object +comma+ +right-brace+ stream)
       collect `(cons ,(stringify-key key) ,(transform-primitive value)) into pairs
       finally (return `(create-json-hash-table ,@pairs)))))


(defvar *previous-readtables* nil)

(defmacro enable-json-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setf *readtable* (copy-readtable))
    (set-macro-character +left-bracket+ (function read-left-bracket))
    (set-macro-character +right-bracket+ (function read-delimiter))
    (set-macro-character +left-brace+ (function read-left-brace))
    (set-macro-character +right-brace+ (function read-delimiter))))

(defmacro disable-json-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* (pop *previous-readtables*))))



(enable-json-syntax)





;; vector-empty
(let ((x []))
  (assert (vectorp x))
  (assert (zerop (length x))))

;; vector-single-element
(let ((x [1]))
  (assert (vectorp x))
  (assert (= (length x) 1))
  (assert (= (elt x 0) 1)))

;; vector-true-false
(let ((x [true, false]))
  (assert (vectorp x))
  (assert (= (length x) 2))
  (assert (eql (elt x 0) t))
  (assert (eql (elt x 1) nil)))

;; vector-strings
(let ((x ["foo", "bar", "baz"]))
  (assert (vectorp x))
  (assert (= (length x) 3))
  (assert (every #'string-equal x '("foo" "bar" "baz"))))

;; vector-lisp-forms
(let* ((w "blah")
       (x [ "foo", 1, (+ 3 4), w ]))
  (assert (vectorp x))
  (assert (= (length x) 4))
  (assert (every #'equalp x (list "foo" 1 7 w))))

;; hash-table-empty
(let ((x {}))
  (assert (hash-table-p x))
  (assert (zerop (hash-table-count x))))

;; hash-table-single-entry
(let ((x {"foo": 1}))
  (assert (hash-table-p x))
  (assert (= (hash-table-count x) 1))
  (assert (eql (gethash "foo" x) 1)))

;; hash-table-table-single-null-entry
(let ((x {"foo": null}))
  (assert (hash-table-p x))
  (assert (= (hash-table-count x) 1))
  (assert (eql (gethash "foo" x) nil)))

;; hash-table-multiple-entries
(let ((x {
        "foo": 1,
        "bar": 2,
        "baz": 3
        }))
  (assert (hash-table-p x))
  (assert (= (hash-table-count x) 3))
  (assert (eql (gethash "foo" x) 1))
  (assert (eql (gethash "bar" x) 2))
  (assert (eql (gethash "baz" x) 3)))

;; hash-table-lisp-forms
(let* ((w "blah")
       (x {
         "foo": 1,
         "bar": (+ 3 4),
         "baz": w
         }))
  (assert (hash-table-p x))
  (assert (= (hash-table-count x) 3))
  (assert (eql (gethash "foo" x) 1))
  (assert (eql (gethash "bar" x) 7))
  (assert (eql (gethash "baz" x) w)))

;; hash-table-key-literals
(let ((x { foo: 1, bar: 2 }))
  (assert (hash-table-p x))
  (assert (= (hash-table-count x) 2))
  (assert (eql (gethash "foo" x) 1))
  (assert (eql (gethash "bar" x) 2)))

;; vector-includes-hash-table
(let ((x [ {  foo: 1 } ]))
  (assert (vectorp x))
  (assert (= (length x) 1))
  (let ((hash-table (elt x 0)))
    (assert (hash-table-p hash-table))
    (assert (eql (gethash "foo" hash-table) 1))))
```

## test json-reader

```lisp
;; To run these tests,
;;
;; 1. (LOAD "json-reader.lisp")   ;; load json reader
;; 2. (LOAD "test.lisp")          ;; load this file
;; 3. (run-tests :json-test)      ;; run the tests

(cl:in-package #:cl-user)

(defpackage #:json-test)

(json-reader:enable-json-syntax)

(defun random-number ()
  (random (expt 2 32)))

(defun random-string ()
  (with-output-to-string (out)
    (loop repeat (random 10)
       do (format out "~A " (random (expt 2 32))))))

(defun run-tests (package)
  (do-symbols (s package)
    (when (fboundp s)
      (format t "~&~A: ~A" (symbol-name s)
              (handler-case (progn (funcall s) t)
                (error (c) c))))))

(defun json-test::vector-empty ()
  (let ((x []))
    (assert (vectorp x))
    (assert (zerop (length x)))))

(defun json-test::vector-single-element ()
  (let ((x [1]))
    (assert (vectorp x))
    (assert (= (length x) 1))
    (assert (= (elt x 0) 1))))

(defun json-test::vector-true-false ()
  (let ((x [true, false]))
    (assert (vectorp x))
    (assert (= (length x) 2))
    (assert (eql (elt x 0) t))
    (assert (eql (elt x 1) nil))))

(defun json-test::vector-strings ()
  (let ((x ["foo", "bar", "baz"]))
    (assert (vectorp x))
    (assert (= (length x) 3))
    (assert (every #'string-equal x '("foo" "bar" "baz")))))

(defun json-test::vector-lisp-forms ()
  (let* ((w "blah")
         (x [ "foo", 1, (+ 3 4), w ]))
    (assert (vectorp x))
    (assert (= (length x) 4))
    (assert (every #'equalp x (list "foo" 1 7 w)))))

(defun json-test::hash-table-empty ()
  (let ((x {}))
    (assert (hash-table-p x))
    (assert (zerop (hash-table-count x)))))

(defun json-test::hash-table-single-entry ()
  (let ((x {"foo": 1}))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 1))
    (assert (eql (gethash "foo" x) 1))))

(defun json-test::hash-table-table-single-null-entry ()
  (let ((x {"foo": null}))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 1))
    (assert (eql (gethash "foo" x) nil))))

(defun json-test::hash-table-multiple-entries ()
  (let ((x {
             "foo": 1,
             "bar": 2,
             "baz": 3
            }))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 3))
    (assert (eql (gethash "foo" x) 1))
    (assert (eql (gethash "bar" x) 2))
    (assert (eql (gethash "baz" x) 3))))

(defun json-test::hash-table-lisp-forms ()
  (let* ((w "blah")
         (x {
              "foo": 1,
              "bar": (+ 3 4),
              "baz": w
            }))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 3))
    (assert (eql (gethash "foo" x) 1))
    (assert (eql (gethash "bar" x) 7))
    (assert (eql (gethash "baz" x) w))))

(defun json-test::hash-table-key-literals ()
  (let ((x { foo: 1, bar: 2 }))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 2))
    (assert (eql (gethash "foo" x) 1))
    (assert (eql (gethash "bar" x) 2))))

(defun json-test::vector-includes-hash-table ()
  (let ((x [ {  foo: 1 } ]))
    (assert (vectorp x))
    (assert (= (length x) 1))
    (let ((hash-table (elt x 0)))
      (assert (hash-table-p hash-table))
      (assert (eql (gethash "foo" hash-table) 1)))))

(json-reader:disable-json-syntax)
```

# format

```lisp
(let ((k (make-string-output-stream)))
  (format k "~s" 123)
  (format k "~s" 456)
  (format k "~s" 789)
  (get-output-stream-string k))


(format t "~%")

(format t "~%~&")
(format t "1~&")
(format t "1~|")
```

# eval & coerce & compile

- note that
  a expression will be evaluated with no lexical context

```lisp
(defun read-eval-print-loop ()
  (do ()
      (nil)
    (format t "~%> ")
    (print (eval (read)))))

(coerce '(lambda (x) x) 'function)
(coerce '(lambda (x) x) 'list)

(compile nil '(lambda (x) (+ x 2)))
(compile 'read-eval-print-loop)
```

# multiple values

```lisp
;; 默認返回一個值
(+ (floor pi) 2)

;; 返回給某些特殊的函數作爲參數時
;; 會返回兩個值
(floor pi)

;; 一種可能的實現方式是
;; 让返回值能夠[在運行時]查詢自己所处的环境
;; 即 是哪個函數在調用這個值
;; 然后再根据环境返回不同的值

(multiple-value-bind (integral fractional)
    (floor pi)
  (+ integral fractional))



;; to create

(values 'a nil (+ 2 4))

((lambda ()
   ((lambda ()
      (values 1 2)))))

;; default to one value
(let ((x (values 666)))
  x)

((lambda (x)
   x)
 (values 666))


(values)

;; default to one value
(let ((x (values)))
  x)



;; to receive
(multiple-value-bind (x y z) (values 1 2 3)
  (list x y z))
(multiple-value-bind (x y z) (values 1)
  (list x y z))

(multiple-value-bind (s m h) (get-decoded-time)
  (format nil "~A:~A:~A" h m s))



(multiple-value-list (values 1 2 3))
(multiple-value-call (function list) (values 1 2 3))


(multiple-value-call (function +) (values 1 2 3))

(apply (function +)
       (multiple-value-list (values 1 2 3)))

(apply (function +)
       (list 1 2 3))
```

# type

```lisp
;; 類型是有層次的
(typep 27 'integer)
(typep 27 'real)
(typep 27 t)

(declaim (type fixnum *count*))
```

# control

## block

- 這些語法在進行複雜的輸入輸出時可能有用
  而在一般的計算中 我儘量使用函數範式

```lisp
(progn
  '<body>)

;; 可以用以在循環中非局部退出
;; 但是在函數範式下 用遞歸函數來實現循環時
;; 使用並不方便
;; 即 必須定義局部的遞歸函數
;; 尤其是當想要把內部的函數因子化之時
(block name
  '<body>
  ;; ...
  (return-from name 'value)
  ;; ...
  '<body>
  )

(defun ak ()
  (return-from ak 47))

(block nil
  '<body>
  ;; ...
  (return 'value)
  ;; ...
  )

(tagbody
   (setf x 0)
 top
   (setf x (+ x 1))
   (format t "~A " x)
   (if (< x 10) (go top)))
```

## named block

```lisp
(block early
  'aaa
  (return-from early 'kkk)
  'bbb)
```

## catch & throw

```lisp
(defun super ()
  (catch 'abort
    (sub)
    (format t "We'll never see this.")))

(defun sub ()
  (throw 'abort 99))

(super)
```

## unwind-protect

- whenever certain actions have to be followed by
  some kind of cleanup or reset
  unwind-protect may be useful

```lisp
(setf x 1)

(catch 'abort
  (unwind-protect
       (throw 'abort 99)
    (setf x 2)))
```

# error handling

- 在 sbcl 中
  打印自定義的報錯信息
  跟着是出錯類型
  然後是 restart 和 backtrace

```lisp
;; error
(progn
  (error "Oops!")
  (format t "After the error."))

(error "Your report uses ~A as a verb." 'status)


;; check-type
(let ((x '(a b c)))
  (check-type (car x) integer "an integer")
  x)

;; assert
(let ((sandwich '(ham on rye)))
  (assert (eql (car sandwich) 'chicken)
          ((car sandwich))
          "I wanted a ~A sandwich." 'chicken)
  sandwich)
```

# handler-case

```lisp
(defun assess-condition (condition)
  (handler-case (signal condition)
    (warning () "Lots of smoke, but no fire.")
    ((or arithmetic-error control-error cell-error stream-error)
        (condition)
      (format nil "~S looks especially bad." condition))
    (serious-condition (condition)
      (format nil "~S looks serious." condition))
    (condition () "Hardly worth mentioning.")))
;; =>  ASSESS-CONDITION

(assess-condition (make-condition 'stream-error :stream *terminal-io*))
;; =>  "#<STREAM-ERROR 12352256> looks especially bad."

(define-condition random-condition (condition) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (princ "Yow" stream))))
;; =>  RANDOM-CONDITION

(assess-condition (make-condition 'random-condition))
;; =>  "Hardly worth mentioning."





(handler-case form
  (type1 (var1) . body1)
  (type2 (var2) . body2) ...)

;; is approximately equivalent to:

(block #1=#:g0001
  (let ((#2=#:g0002 nil))
    (tagbody
       (handler-bind ((type1 #'(lambda (temp)
                                 (setq #1# temp)
                                 (go #3=#:g0003)))
                      (type2 #'(lambda (temp)
                                 (setq #2# temp)
                                 (go #4=#:g0004))) ...)
         (return-from #1# form))
       #3# (return-from #1# (let ((var1 #2#)) . body1))
       #4# (return-from #1# (let ((var2 #2#)) . body2)) ...)))




(handler-case form
  (type1 (var1) . body1)
  ...
  (:no-error (varN-1 varN-2 ...) . bodyN))

;; is approximately equivalent to:

(block #1=#:error-return
  (multiple-value-call #'(lambda (varN-1 varN-2 ...) . bodyN)
    (block #2=#:normal-return
      (return-from #1#
        (handler-case (return-from #2# form)
          (type1 (var1) . body1) ...)))))


```

# common lisp object system

## generic function

- the common lisp object system (clos)
  does not associate methods with classes
  but rather under generic functions

```lisp
(defclass kkk ()
  (k1 k2 k3))

(setf k (make-instance 'kkk))
(setf (slot-value k 'k1) 1)

(defclass kkk ()
  ((k1 :accessor k1)
   (k2 :accessor k2)
   (k3 :accessor k3)))

(setf k (make-instance 'kkk))
(setf (k1 k) 1)
(setf (k2 k) 2)
(setf (k3 k) 3)


(defclass aaa ()
  ((a1 :accessor a1)
   (a2 :accessor a2)
   (a3 :accessor a3)))

(defmethod kkk (&key
                  x1
                  x2)
  `(,x1 ,x2))
(kkk :x1 1 :x2 2)

(defmethod kkk (&key
                  x1
                  (x2 aaa))
  ;; {var | ({var | (keywordvar)} [initform [supplied-p-parameter] ])}
  `(,x1 (,x2 aaa)))
(kkk (make-instance 'kkk)
     (make-instance 'aaa))
;; 如果新定義了下面的函數
;; 那麼對上面的函數的調用結果就改變了
(defmethod kkk ((x1 kkk) x2)
  `((,x1 kkk) ,x2))



(defgeneric key-input (key-name))

(defmethod key-input (key-name)
  (format nil "No keybinding for ~a" key-name))
(key-input 1)

(defmethod key-input ((key-name (eql :space)))
  (format nil "Space key pressed"))
(key-input :space)

(defmethod key-input ((key-name (eql :return)))
  (format nil "Return key pressed"))
(key-input :return)

(function  key-input)
```

# package

- 所有的函數都在某個 package 中
  就算核心函數也不例外
- always be in the package that you are developing
- in-package 是就某個文件而言的
  在一個文件中對 in-package 的調用並不影響之後被 load 的文件

```lisp
*package*

(intern "ARBITRARY"
        (make-package :foo
                      :use '(:cl)))


;; lisp maintains a special variable called *package*
;; which is bound to the current package

(in-package :cl)
(defvar xxx 'x)
(in-package :foo)


(package-name *package*)
(package-name :foo)

(find-package :foo)
(find-package *package*)
(find-package "FOO")

(symbol-package 'sym)

;; 原來 :kkk 是 keyword:kkk 的縮寫
;; 語義上有特殊性質的東西
;; 又發現它可以被劃歸到某種一致的處理方式中
;; 就涉及到一些設計決策了
;; 可是這裏的 :kkk 用特殊的處理方式處理的話
;; 也將是合理的
```