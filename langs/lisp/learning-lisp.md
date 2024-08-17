---
title: learning lisp
---

# function application

与 scheme 的差异在于，common-lisp 有多个命名空间（namespace），
可以想像为 symbol 到 properties 的列表，
在不同的情况下引用一个 symbol 时，取不同的属性，
每个属性就代表了一个命名空间。

比如 `(f x y)`：

- `f` 在函数位置，就取 `:function`；
- `x` 和 `y` 在参数位置，就取 `:value`。

这种多个命名空间的设计的特点在于，
可以用 string 来命名一个 type，保存在 :type 的命名空间中，
然后再用 string 来命名一个 value，保存在 :value 的命名空间中，
这样就不需要用 PascalCase 的 String 和 lisp-case 的 string
来区分类型与保存这个类型的值的变量了。

但是，我认为这种设让 common-lisp 相较于 scheme 显得非常复杂。
并且，在 dependent type 时代，因为 type 就是 value，
所以看来 :type 与 :value 都保存在同一个命名空间中才是合理的。

但是，也许在 dependent type 时代，这种设计是正确的，
因为在 scheme 中，没法出合理的 naming convention 来区分 type 与 value。
这样，引用同一个 symbol 的时候，
不是用 naming convention，
而是用更结构化的 namespace 来区分 type 与 value，
也许就是合理的。

毕竟，在一个 symbol 下同时保存 value 与 type，
正好也是实现类型检查器时 context 的实现方式。

# apply & funcall

想要取 `:function` 时，用 `(function f)` 或者 `#'f`。
有些函数拿到 symbol 时，也会直接取 `:function`。

```lisp
(+ 1 2 3)

;; apply 有两个参数，第一个是函数，第二个是 args。

(apply (function +) '(1 2 3))
(apply #'+ '(1 2 3))
(apply '+ '(1 2 3))

;; function 有可变个参数，第一个是函数，其余的是 args。

;; funcall 和是必要的
;; 因为 边缀表达式 () 的头上的 symbol
;; 会被解释为一个函数名 而不会被当作局部变元处理
(funcall (function +) 1 2 3)
(funcall #'+ 1 2 3)
(funcall '+ 1 2 3)

;; 我将主要用 `(function +)`

(sort (list 4 2 3 1) (function <))

;; 对于 lambda function 来说，没有歧义，
;; 此时使用 (function ...) 和 #'f... 是多余的。

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

```lisp
(lambda (a b) (+ a b))
((lambda (a b) (+ a b)) 1 2)

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

(type-of t)

;; nil as false
()
'()
nil
'nil

(type-of ())
(type-of '())
(type-of nil)
(type-of 'nil)
```

## symbol

```lisp
;; case-insensitive
(eq 'kkk 'KKK)

;; value name space
(defvar *kkk* 5)
(symbol-value '*kkk*)
(boundp '*kkk*)
(boundp '*vvv*)

(defun kkk () 'k)
(symbol-function 'kkk)
(funcall (symbol-function 'kkk))
(apply (symbol-function 'kkk) '())

(list '|ci ci ci|
      '|ca,ca,ca|
      '|(da da da)|
      '|   |
      '|abc|
      '|ABC|
      '|\|\|\||
      '|\\\\\\|)
```

symbol 与 string 之间的转化：

```list
;; symbol->string
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

;; string->symbol
(intern "kkk")
(eq (intern "kkk") 'kkk)
```

每个 symbol 可以有多个属性，这个功能可以用来实现命名空间。

其实 `(function f)` 类似于 `(get 'f :function)`，
其实 `f` 类似于 `(get 'f :value)`，
但是其实并没有这样实现，
因为语言内部的命名空间最好不要和用户能定义的命名空间相冲突。

- 实际上用不是用 `get`，
  是用 `symbol-value` 和 `symbol-function` 这两个接口函数。

这种全局的性质，一般只有语言的核心部分才会使用到，
否则不同的人所写的程序就相互冲突了。
而一般的程序都只使用一般性质的 hash-table。

```lisp
(setf (get 'kkk 'color1) 'red
      (get 'kkk 'color2) 'yellow
      (get 'kkk 'color3) 'blue)

(get 'kkk 'color1)
(get 'kkk 'color2)

(symbol-plist 'kkk)

;; 用 keyword symbol 也可以：

(setf (get :kkk :color1) :red
      (get :kkk :color2) :yellow
      (get :kkk :color3) :blue)

(get :kkk :color1)
(get :kkk :color2)

(symbol-plist :kkk)
```

## number

```lisp
(expt (expt (expt (expt 10 10) 10) 10) 10)
(+ 5/9 3/4)
(* 2 (+ #c(10 5) 4))
```

## list

这个数据类型无需多言了。

## array

array 其实是 tensor，
`make-array` 的第一个参数是 tensor 的 shape。

```lisp
;; 在 sbcl 中 默认的是
(make-array '(2 3) :initial-element 0)
(make-array '(2 3))

(setf a3 (make-array '(3) :initial-element nil))
(setf v3 (make-array 3 :initial-element nil))
(setf a23 (make-array '(2 3) :initial-element nil))
(setf a234 (make-array '(2 3 4) :initial-element nil))

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

(equal "lisp" "lisp") ;; T
(equal "lisp" "LISP") ;; NIL

(equalp "lisp" "lisp") ;; T
(equalp "lisp" "LISP") ;; T

(string-equal "lisp" "LISP")

(equal #(1) #(1)) ;; NIL
(equalp #(1) #(1)) ;; T

(concatenate 'string "not " "to worry")

;; nil 代表不要 print，只是构造 string
(format nil "~A or ~A" "truth" "dare")
;; t 代表 print to stdout，这打破函数复合的 API 设计真是不好
(format t "~A or ~A" "truth" "dare")

;; for human

(princ '("kkk" "kkk" "kkk"))
(princ "he yelled \"stop that thief!\" from the busy street.")

;; for machine

(print '("kkk" "kkk" "kkk"))
(print "he yelled \"stop that thief!\" from the busy street.")
```

## sequence

```lisp
(elt '(a b c) 0)
(elt #(a b c) 0)
(elt "abc" 0)

;; position 的可选参数

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

用一个定义生成一些列函数。

当有 dot 语法可以用来表示消息传递时，
这样的设计是完全没必要的。

有 generic 机制之后，
也可设计和 keyword 相关的 generic，
来避免生成一系列函数。

也许生成一系列函数也有优点，
就是简单，语言可以没有任何 generic 机制。

但是何必呢？
最基本的 dot 语法的 generic 机制一般的程序员都非常熟悉了，
而 common-lisp 的 `defgeneric` lisper 也很熟悉了。

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

构造 hash-table 用 `make-hash-table`。

```lisp
(setf color-table (make-hash-table))
```

`gethash` 返回两个值
其中第二个值是一个 type-tag
type-tag 被用以表明这个位置的是否有值存入
nil 不能作为信号来表明没有值存入
因为 nil 本身就可能是被存入的值

这种 API 感觉是很脱离实际的。
在 JavaScript 里没有这种 API，也没什么问题。

```lisp
(gethash 'color1 color-table)

;; 万能的 setf
(setf (gethash 'color1 color-table) 'red
      (gethash 'color2 color-table) 'yellow
      (gethash 'color3 color-table) 'blue)

;; remhash (hash-table-remove) 的返回值 只有一个
;; 并且是一个信号性的值 代表需要进行 remove
(remhash 'color1 color-table)

(maphash (lambda (key value)
           (format t "~A = ~A ~%" key value))
         color-table)

;; 函数也可以作为 hash-table 的 key：

(defun kkk (x) x)

(setf bug-table (make-hash-table))
(setf (gethash (function kkk) bug-table)
      "kkk took my baby away")
;; (push "kkk took my baby away"
;;       (gethash (function kkk) bug-table))
(gethash (function kkk) bug-table)

;; 重新定义之后就不被认为是相同的值了
(defun kkk (x) (+ x x))
```

# side effect

## assignment

- defvar 定义全局变量
  defparameter 定义全局变量
  并且所作的绑定不会被 defvar 修改
  let 定义局部变量

- flet 定义局部非递归函数
  labels 定义局部递归函数

- 注意 其二类分属两个命名空间

```lisp
;; global
;; 全局的名 是值与 symbol 的绑定
;; 而局部的名 的实现方式各异

;; setf 和 setq 是在全局和局部都通用的
;; 不知道他们又什么区别

(defparameter kkk 1)
(boundp 'kkk)
kkk
(defvar kkk 2)
;; 如果 kkk 之前已经被定义过了
;; 那么 defvar 不会重新绑定其值 但是也不会报错
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

;; 只要是对这个命名空间的绑定都是如此



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

;; 只要把 () 视为一个边缀表达式
;; 那么下面的语义的实现方式就容易理解了
(defparameter *colours* (list 'red 'green 'blue))
(setf (car *colours*) 'yellow)
(push 'red (cdr *colours*))

;; 但是 common-lisp 中 相关的语法还是显得混乱
;; 在蝉语中 我要以一致的方式来解决这些问题


;; local
(let ((x 6)
      (y 5))
  (* x y))

(let ((x6 6)
      (y 5))
  (boundp 'x6))

;; 局部的 名 与值的绑定也是可以更改的
(let ((x 6)
      (y 5))
  (setf x 2)
  (* x y))

(let ((x 6)
      (y 5))
  (setq x 2)
  (* x y))

;; 局部的 counter 在 add-counter 内部
(let ((counter 10))
  (defun add-counter (x)
    (prog1
        (+ counter x)
      (incf counter))))

(setf counter 10)

(boundp 'counter)

(mapcar (function add-counter) '(1 1 1 1))
(add-counter 50)


;; 简陋的模式匹配
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

- 在考虑各种语法糖的时候
  注意它们是如何引入约束变元的
  对约束变元的使用 是不同编程风格的特征

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

- stream 作为输入输出的一种抽象
- ><><><
  输入输出的形式多种多样
  仔细想来其类别相当复杂
  值得仔细分析一下
- 最简单的有两种
- 文件的读写
  其样貌类似与文本编辑器的 buffer
  把文件从硬盘读到一块内存中
  然后修改 然后保存回硬盘
  此时在 buffer 中
  可以有[一个或多个]类似光标的指针
- 基本输入输出

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



;; 在读文件的时候也有一个类似光标的隐含的指针
;; 我应该把这个指针明显化
;; 只不过在从命令行中读字符时
;; 这个指针是不能随便乱动的 因为后面的字符还没输入呢
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


;; 可选择的参数都应该用 &key 来定义
;; 应该在省略 :key 的时候遵从默认的顺序
;; 而不应该使用 &optional
;; 这是函数调用语法接口的设计失误

;; read-line 又是需要返回一些信号的例子
;; 因此 它返回两个值
;; 第一个是所读入的字符串
;; 第二个
;;   以 nil 表 正常读入
;;   以 t 表 没有遇到 newline 字符 被读的东西就结束了

(defun pseudo-cat (file)
  (with-open-file (str file rdirection :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))

;; read as read-sexp
;; 应该把用以实现 read-line 和 read-sexp 的函数暴露出来
;; 使用户能够自己定义阅读器


;; 同 read 也是 read-sexp
;; 这种阅读中 因为没有 eval
;; 所以 symbol 是不用加引号的
(read-from-string "aaa bbb ccc")
;; 返回两个参数
;; 其二是 光标的位置
;; 在上面的例子中
;; 光标在 bbb 的第一个 b
;; 因为只有读到 bbb 前面的空格时
;; 才能判定出一个 symbol
;; 并且犹豫 光标不能回退 所以就停在了 b

;; 在光标可以回退的时候也没有回退
;; 这是为了统一处理两种不同类型的输入而设计的
;; 但是这并不是良好的设计

;; 所以在使用时
;; 为了灵活性 可以总是把文件读入到字符串中来处理
;; 这样 就能够以明显的方式实现一个或多个光标了

;; 另有 read-char
;; 而 peek-char 是 read-char 的 不移动光标的版本



(princ "Hello")
(prin1 "Hello")  ;; with quote
(terpri) ;; newline
;; 这些函数傻逼名字就足以让我不使用他们了
;; 所以只使用 format

;; 而 format 的设计失误在于
;; 不应该用 t 和 nil 来做默认的参数
;;   因为语义不清晰
;; 不应该用 format-string (or control-string)
;;   而应该用可以以更灵活的方式排版的语法
;;   以增加可读性和灵活性
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

- 在蝉语中
  因为语法解析的过程与生成代码的过程是结合在一起的
  所以 macro 可以直接用语法解析器来实现
  所以 macro 和函数一样 是一等公民的
  而在 lisp 中情况并非如此

## defmacro

- 其实在写 macro 的时候
  lisp 中对 symbol 的处理 完全可以换成是 shen 的语义
  否则这种底层的 macro 太难看了

```lisp
(defmacro nil! (x)
  `(setf ,x nil))
(nil! x1)

;; 返回的第二个值是信号
(macroexpand-1 '(nil! x2))
(macroexpand-1 '(kkk x))

(nil! a1)

((lambda (expr)
   (apply (lambda (x) `(setf ,x nil))
          (cdr expr)))
 '(nil! a2))

;; 不同的是
;; 实际上
;; 上面返回的链表 会作为代码被 编译器处理
;; 而下面是用解释器在处理所返回的代码
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
;; 默认返回一个值
(+ (floor pi) 2)

;; 返回给某些特殊的函数作为参数时
;; 会返回两个值
(floor pi)

;; 一种可能的实现方式是
;; 让返回值能够[在运行时]查询自己所处的环境
;; 即 是哪个函数在调用这个值
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
;; 类型是有层次的
(typep 27 'integer)
(typep 27 'real)
(typep 27 t)

(declaim (type fixnum *count*))
```

# control

## block

- 这些语法在进行复杂的输入输出时可能有用
  而在一般的计算中 我尽量使用函数范式

```lisp
(progn
  '<body>)

;; 可以用以在循环中非局部退出
;; 但是在函数范式下 用递归函数来实现循环时
;; 使用并不方便
;; 即 必须定义局部的递归函数
;; 尤其是当想要把内部的函数因子化之时
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
  打印自定义的报错信息
  跟着是出错类型
  然后是 restart 和 backtrace

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
;; 如果新定义了下面的函数
;; 那么对上面的函数的调用结果就改变了
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

- 所有的函数都在某个 package 中
  就算核心函数也不例外
- always be in the package that you are developing
- in-package 是就某个文件而言的
  在一个文件中对 in-package 的调用并不影响之后被 load 的文件

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

;; 原来 :kkk 是 keyword:kkk 的缩写
;; 语义上有特殊性质的东西
;; 又发现它可以被划归到某种一致的处理方式中
;; 就涉及到一些设计决策了
;; 可是这里的 :kkk 用特殊的处理方式处理的话
;; 也将是合理的
```
