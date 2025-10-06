---
title: a prettier printer
author: philip wadler
year: 1997
---

# My Motive

[2025-10-06] 在实现 x-lisp，好久没读论文了。
需要给 x-lisp 实现 `pretty-print` 函数，
随便搜索了一下，竟然有相关的论文。

看来在 CS 领域，能想到的 worth solving problem 都有论文。

# Intro

> Joyce Kilmer and most computer scientists agree: there is no poem as
> lovely as a tree. In our love affair with the tree it is parsed,
> pattern matched, pruned — and printed. A pretty printer is a tool,
> often a library of routines, that aids in converting a tree into
> text. The text should occupy a minimal number of lines while
> retaining indentation that reflects the underlying tree. A good
> pretty printer must strike a balance between ease of use,
> flexibility of format, and optimality of output.

> Over the years, Richard Bird and others have developed the algebra
> of programming to a fine art. John Hughes (1995) describes the
> evolution of a pretty printer library, where both the design and
> implementation have been honed by an appealing application of
> algebra.

Richard Bird 有一本书就叫做 the algebra of programming，
另外他还和 philip wadler 一起写过 haskell 的书，
都值得一读。

John Hughes (1995) 的引用是：

- John Hughes.
  The design of a pretty-printer library.
  In J. Jeuring and E. Meijer, editors,
  Advanced Functional Programming,
  Springer Verlag LNCS 925, 1995.

> A widely used imperative pretty-printer is
> described by Derek Oppen (1980).

这里的引用是：

- Derek Oppen.
  Pretty-printing.
  ACM Transactions on Programming Languages and Systems, 2(4): 1980.

> The pretty printer presented here uses an algorithm equivalent to
> Oppen’s, but presented in a functional rather than an imperative
> style.  Further comparison with Hughes’s and Oppen’s work appears
> in the conclusions.

能够把 imperative code 变成 algebric code，
到底是什么意思？
读 Richard Bird 那本书应该可以系统学习到。

# 1 A simple pretty printer

> To begin, we consider the simple case where each document has only
> one possible layout -- that is, no attempt is made to compress
> structure onto a single line.  There are six operators for this
> purpose.

```haskell
(<>)   :: Doc -> Doc -> Doc
nil    :: Doc
text   :: String -> Doc
line   :: Doc
nest   :: Int -> Doc -> Doc
layout :: Doc -> String
```

> ... `nest i` as the function that adds `i` spaces
> after each newline (to increase indentation) ...

indentation 不是针对整个字符串的，
而是针对 newline 字符的。

> Every document can be reduced to a normal form of text alternating
> with line breaks nested to a given indentation,
>
>    text s0 <> nest i1 line <> text s1 <> · · · <> nest ik line <> text sk

有 normal form 的概念了，有点 algebra 的感觉了。
我想这就是 imperative code 变成 algebric code 的意义，
可以通过对 algebric code 的等价变换，获得等价的 side-effect。

注意，上面不是在定义数据类型 `Doc`，
而是 interface（类似 monad 的 bind 和 return interface），
之后还要给出这些 inference function 需要满足的等式（也是类似 monad）。

在 haskell 中，这需要用 type class 实现，
在 x-lisp 中，可以用 expression（ADT） + 解释器实现。

具体对 doc 的 algebra 来说，能够在不同的宽度限制下解释 expression，
就解决了我们面临的问题。

之前听说过写解释器的时候，
有对 expression 的 initial encoding 和 final encoding，
其实 initial encoding 就是正常地，
用 ADT 表达 expression，然后实现 eval 函数，
而 final encoding 是用 interface 来表达 expression，
然后用 interface 的不同 instance，来代表不同的 eval 方式。

其实是说，解释器和 interface 之间可以相互转换。
注意，抽象的数学结构比如 group 都是用 interface 定义的，
那么，转换过来就是说抽象的 group 可以定义为 ADT（polymorphic），
具体的 group 可以定义为 eval 函数，也就是解释器。

比如，monad 的 type class，
是用 interface + implicit argument 机制，
来表示 monad 这个抽象的数学结构。
按照这里的推理，就是说也可以用具体的 ADT 来表示 monad，
然后用不同的解释器来代表具体的 moand。

```scheme
(claim list-lift (-> (-> A (list? B)) (-> (list? A) (list? B))))
(claim list-unit (-> A (list? A)))

>> (list-lift string-chars ["abc" "123"])
=> ["a" "b" "c" "1" "2" "3"]
```

可以转化成：

```scheme
(define-data (list-monad? A B)
  (lift (f (-> A (list? B))) (m (list? A)))
  (unit (x A)))

(claim eval-list-monad
  (polymorphic (A B)
    (-> (list-monad? A B) (list? B))))

(define (eval-list-monad list-monad)
  (match list-monad
    ((lift f list) (list-append-many (list-map f list)))
    ((unit x) [x])))

>> (eval-list-monad (lift string-chars ["abc" "123"]))
=> ["a" "b" "c" "1" "2" "3"]
```

问题是，如何把上面的 `list-monad?` 进一步抽象为 `monad?`？

一般的 `define-data` 带有的 polymorphic 参数，其实是：

```scheme
(define-data (list-monad? (A pred?) (B pred?))
  (lift (f (-> A (list? B))) (m (list? A)))
  (unit (x A)))
```

在这种具体表示下，可以定义 `monad?`：

```scheme
(define-data (monad? (M (-> pred? pred?)))
  (claim lift
    (polymorphic (A B)
      (pi ((f (-> A (M B)))
           (m (M A)))
        (M B))))
  (claim unit
    (polymorphic (A)
      (pi ((x A))
        (M A)))))
```

这不太行得通。

对于 type class，也就是 interface 而言，需要增加高阶类型参数。
但是对解释器而言，可能反而应该减少类型参数：

```scheme
(define-data (monad? M)
  (lift (f (-> anything? M)) (m M))
  (unit (x anything?)))
```

`eval-list-monad` 可以进一步改为：

```scheme
(claim eval-list-monad
  (-> (monad? (list? anything?))
      (list? anything?)))

(define (eval-list-monad list-monad)
  (match list-monad
    ((lift f list) (list-append-many (list-map f list)))
    ((unit x) [x])))

>> (eval-list-monad (lift string-chars ["abc" "123"]))
=> ["a" "b" "c" "1" "2" "3"]
```

但是也不太对。

还是需要更强的类型系统才能表达出来。

[2025-10-07] 也许需要使用 `define-generic` 才能表达出来，
正如实现 propagator 时所用的 monad。

# 2 A pretty printer with alternative layouts

> We now consider documents with multiple possible layouts. Whereas
> before we might view a document as equivalent to a string, now we
> will view it as equivalent to a set of strings, each corresponding
> to a different layout of the same document.

> This extension is achieved by adding a single function.
>
>     group :: Doc -> Doc
>
> Given a document, representing a set of layouts, group returns the
> set with one new element added, representing the layout in which
> everything is compressed on one line. This is achieved by replacing
> each newline (and the corresponding indentation) with text
> consisting of a single space. (Variants might be considered where
> each newline carries with it the alternate text it should be
> replaced by. For instance, some newlines might be replaced by the
> empty text, others with a single space.)

> The function layout is replaced by one that chooses the prettiest
> among a set of layouts. It takes as an additional parameter the
> preferred maximum line width of the chosen layout.
>
>     pretty :: Int -> Doc -> String
>
> (Variants might be considered with additional parameters, for
> instance a ‘ribbon width’ indicating the maximum number of
> non-indentation characters that should appear on a line.)

# 5 Related work and conclusion

> *Bird’s influence* Richard Bird put algebraic design on the map (so
> to speak). John Hughes made pretty printer libraries a prime example
> of the algebraic approach.  The greatest homage is imitation, and
> here I have paid as much homage as I can to Hughes and Bird.

有点难懂，但是感觉很神奇。
可以看看 Bird 的书。

另外，这篇论文还有一个好懂的 ocaml版本：

- christian-lindig/2000-stricty-pretty
