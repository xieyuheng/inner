---
title: learning lojban
---

# web

- http://www.tlg.uci.edu/~opoudjis/lojbanbrochure/lessons/book1.html
- http://www.lojban.org/tiki/CLL+Beginner%27s+Order
- http://dag.github.io/cll/
- http://www.lojban.org/tiki/CLL%2C+aka+Reference+Grammar%2C+Errata

# grammar

bridi (predicate)
selbri (关系 谓词)
sumti (argument)

sumti的顺序被用来区分词所属于的语义类型
对于每个selbri 都需要记住其指定的sumti数和顺序

>< what is gismu ??

tavla:
x1 talks to x2 about x3 in language x4

```lojban
mi tavla do zo'e zo'e
 == mi tavla do
```

I talk to you about something in some language

一个陈述句就是一个被判断为真的命题
一个原子性的命题由一个谓词作用于几个参数构成

vecnu:
seller-x1 sells goods-sold-x2 buyer-x3 price-x4

```lojban
mi vecnu ti ta zo’e
mi ti vecnu ta
 mi ti ta vecnu
```

I sell this to that for some price.
I sell this-thing/these-things to that-buyer/those-buyers.

selbri必须差在一行几个sumti的空隙中
这使得形成语法树的算法变得简单
因此:
selbri sumti sumti ...
就是省略了第一个sumti
selbri后面的sumti算是第二个
由此可见对于每个selbri而言 记住它用什么样的语序形成语义是重要的

melbi:
x1 (object/idea) is beautiful to x2 (observer) by standard x3

# cmavo

即语法词
或语法助词
as separators a heavily needed in this language

## cu

## se

x1 <-> x2

```lojban
mi tavla do ti
do se tavla mi ti
```

## ta

x1 <-> x3

```lojban
mi tavla do ti
ti te tavla do mi
```

## ve

x1 <-> x4

## xe

x1 <-> x5
