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
(new money
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
  (print a-stream self:amount)
  (space a-stream)
  (next-put-all a-stream self:currency))
```

```smalltalk
Money
  amount: 5
  currency: #USD
"5 USD"
```

```scheme
(new money
  :amount 5
  :currency 'USD)
;; 5 USD
```

# ARITHMETIC

```smalltalk
Money>>+ aMoney
  ^self species
    amount: amount + aMoney amount
    currency: currency

Money>>amount
  ^amount

| m1 m2 |
m1 := Money
        amount: 5
        currency: #USD.
m2 := Money
        amount: 7
        currency: #USD.
m1 + m2 "12 USD"
```

```scheme
(define (add (x money) (y money))
  (new money
    :amount (add x:amount y:money)
    :currency x:currency))

(let ((m1 (new money :amount 5 :currency 'USD))
      (m2 (new money :amount 7 :currency 'USD)))
  (add m1 m2)) ;; 12 USD
```

```smalltalk
Class: MoneySum
  superclass: Object
  instance variables: monies

MoneySum class>>monies: aCollection
  ^self new setMonies: aCollection

MoneySum>>setMonies: aCollection
  monies := aCollection
```

```scheme
(define-class money-sum ()
  :monies (list money))

(new money-sum :money a-collection)
```

```smalltalk
MoneySum>>printOn: aStream
  monies do:
    [:each |
    aStream
      print: each;
      nextPutAll: ‘ + ‘].
  aStream skip: -3
```

```scheme
(define (print-on (self money-sum) (a-stream stream))
  (foreach money-sum:monies
    (lambda (a-money)
      (print a-stream a-money)
      (next-put-all a-stream " + ")))
  (skip a-stream -3)
```

```smalltalk
Money>>+ aMoney
  ^currency = aMoney currency
    ifTrue:
      [self species
        amount: amount + aMoney amount
        currency: currency]
    ifFalse:
      [MoneySum monies: (Array
        with: self
        with: aMoney)]

Money>>currency
  ^currency

| m1 m2 |
m1 := Money
        amount: 5
        currency: #USD.
m2 := Money
        amount: 7
        currency: #GBP.
m1 + m2 "5 USD + 7 GBP"
```

```scheme
(define (add (x money) (y money))
  (if (eq? x:currency y:currency)
    (new money :amount x:amount y:amount)
    (new money-sum :monies [x y])))

(let ((m1 (new money :amount 5 :currency 'USD))
      (m2 (new money :amount 7 :currency 'GBP)))
  (add m1 m2)) ;; 5 USD + 7 GBP
```

# INTEGRATION

```smalltalk
Money>> + aMoney
  ^aMoney addMoney: self

Money>>addMoney: aMoney
  ^currency = aMoney currency
    ifTrue:
      [self species
        amount: amount + aMoney amount
        currency: currency]
    ifFalse:
      [MoneySum monies: (Array
        with: self
        with: aMoney)]

MoneySum>> + aMoney
  ^aMoney addMoneySum: self

MoneySum>>addMoney: aMoney
  ^self species monies: (monies copyWith: aMoney)

| m1 m2 |
m1 := Money
        amount: 5
        currency: #USD.
m2 := Money
        amount: 7
        currency: #GBP.
m1 + (m2 + m1) "5 USD + 7 GBP + 5 USD"
```

generic function 的优势一下就体现出来了，
看来不实现 generic function 是不行了。

```scheme
(define (add (x money-sum) (y money))
  (new money-sum
    :monies (cons x:monies y)))

(let ((m1 (new money :amount 5 :currency 'USD))
      (m2 (new money :amount 7 :currency 'USD)))
  (add m1 (add m1 m2))) ;; 5 USD + 7 GBP + 5 USD
```

```smalltalk
Money>> addMoneySum: aMoneySum
  ^aMoneySum addMoney: self

MoneySum>>addMoneySum: aMoneySum
  ^MoneySum monies: monies , aMoneySum monies

| m1 m2 |
m1 := Money
        amount: 5
        currency: #USD.
m2 := Money
        amount: 7
        currency: #GBP.
(m1 + m2) + (m1 + m2) "7 GBP + 5 USD + 7 GBP + 5 USD"
```

```scheme
(define (add (x money) (y money-sum)) (add y x))

(define (add (x money-sum) (y money-sum))
  (new money-sum
    :monies (append x:monies y:monies)))

(let ((m1 (new money :amount 5 :currency 'USD))
      (m2 (new money :amount 7 :currency 'USD)))
  (add (add m1 m2) (add m1 m2))) ;; 7 GBP + 5 USD + 7 GBP + 5 USD
```

作为最终例子，感觉 Kent 的这个 money 差点意思。

相比之下，
Sussman 和他的学生们，
直接 lisp/scheme 例子来写论文更为有趣。

# SUMMARY

作为练习。
