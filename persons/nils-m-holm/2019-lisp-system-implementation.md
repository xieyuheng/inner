---
title: lisp system implementation
author: nils m holm
year: 2019
---

# My Motive

[2026-05-22] 在设计 meta-lisp 的过程中，发现我用 c 写的 stack-lisp 速度很慢。
并且 stack-lisp 目前只有 sexp 语法，没有二进制 byte code。
因此来学一下更快的 byte code 设计，以及编译到 native code 的可能。

# 1. Preface

本书介绍 LISP9 的实现细节。
LISP9 作为 LISP 与 scheme 的差异在于 naming convention，
即倾向于使用简单的名字。

> The LISP9 implementation follows an idiomatic coding style that
> may need some getting used to. Here is a summary:
>
> - function names are verbs, only sometimes nouns
> - function names are short, but expressive
> - global variable names are short nouns
> - local variable names are single-character placeholders
>
> At the lexical level in C code:
>
> - CONSTANTS are all-upper-case
> - Global variables are capitalized
> - everything else is all-lower-case
>
> In LISP code global variables wear *earmuffs*.

关于「单字符的变量名」，作者给出的例子是：

```lisp
;; compare

(defun (reverse-and-concatenate
        base-list
        appended-list)
  (if (null base-list)
    appended-list
    (reverse-and-concatenate
     (cdr base-list)
     (cons (car base-list)
           appended-list))))

;; with

(defun (reconc a b)
  (if (null a)
    b
    (reconc (cdr a)
            (cons (car a) b))))
```

感觉有点道理。

但是这个例子是为了证明「单字符的变量名」好，而故意走极端的设计。
不用单字符的变量名，也可以写清晰：

```scheme
(define (list-reverse-append list tail)
  (if (null? list)
    list
    (list-reverse-append
     (cdr list)
     (cons (car list) tail))))
```

> In the concrete example, `reconc` ("reverse and concatenate") is a
> well-known LISP idiom, just like `strcmp` ("string-compare") is a
> well-known C idiom. Languages thrive on the use of idioms. They make
> the language brief and clear at the same time. Their only flip side
> is that they require some accommodation by those who are not used to
> them. But then you are already using terms like "loading a program"
> or "doing a function call" without thinking about them too much. You
> will get used to new idioms as easily as you got used to those.

我认为 reconc 和 strcmp 这些缩写的名字，
其用途主要是在命名过程中可以突破英语语法的限制。

> All concepts are introduced in a strict bottom-up order, as far as
> possible, so everything builds on top of previously introduced
> topics when going through the text in the intended order.

感觉这些风格可以当做反例来学。

# 2. Prelude

TODO
