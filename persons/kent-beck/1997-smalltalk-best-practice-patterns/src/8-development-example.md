---
title: 8. Development Example
---

> In this chapter, I will pretend to develop a piece of software
> guided by the Best Practice Patterns. It will likely seem incredibly
> slow to you, since I won’t write more than a couple of words of
> code without pointing out another pattern. Not to worry, once you
> get used to the patterns you don’t really think of them
> individually. You build higher levels of structure in your mind that
> let you just write clean code quickly and without thinking
> consciously of the smaller steps, much as you can speak without
> consulting a grammar book.

# PROBLEM

> The problem we will solve is representing
> multi-currency monetary values.

解决方案不涉及到汇率之类的问题，
只是把所有类型的钱放在一起。

下面我只记录两种风格的代码 -- 可以看作是一个习题，
让读者（之后的我）重新发现代码用到了哪些 pattern。

# START

```smalltalk
Class: Money
  superclass: Object
  instance variables: amount currency
```

```scheme
(define-class money ()
  :amount number
  :currency symbol)
```

```smalltalk
Money class>>amount: aNumber currency: aSymbol
  ^self new
    setAmount: aNumber
    currency: aSymbol

Money>>setAmount: aNumber currency: aSymbol
  amount := aNumber.
  currency := aSymbol
```

在 cicada-lisp 中可以直接用 `create`。

```scheme
(create money
  :amount a-number
  :currency a-symbol)
```

```smalltalk
Money>>printOn: aStream
  aStream
    print: amount;
    space;
    nextPutAll: currency
```

```scheme
(define (print-on (self money) (a-stream stream))
  (print a-stream (self :amount))
  (space a-stream)
  (next-put-all (self :currency)))
```

```smalltalk
Money
  amount: 5
  currency: #USD
"5 USD"
```

```scheme
(create money
  :amount 5
  :currency 'USD)
;; 5 USD
```

# ARITHMETIC
# INTEGRATION
# SUMMARY
