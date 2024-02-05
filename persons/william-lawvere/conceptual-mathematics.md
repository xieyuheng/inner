---
title: Conceptual Mathematics
subtitle: A first introduction to categories
authors: [William Lawvere, Stephen Schanuel]
year: 1991
---

## SESSION 1 Galileo and multiplication of objects

# Part I The category of sets

> A _map_ of sets is a process for getting from one set to another. We
> investigate the _composition_ of maps (following one processby a
> second process), and find that the algebra of composition ofmaps
> resembles the algebra of multiplication of numbers, but its
> interpretation is much richer.

## ARTICLE I Sets, maps, composition

用 `A -g-> A -f-> B`
这样的的后缀表达式描述的函数复合，
类似英文的 -s 所属格；
用 `f(g(x))` 和 `f°g(x)`
这样的前缀表达式描述的函数复合，
类似英文的 of 介词。

当我们设计 sexp 的 OOP 语法时，
可以同时支持这两种表达方式。

也许我们应该把语法设计成这样：

```scheme
(define User
  (class
    (claim name String)
    (claim age Number)))

(define xyh
  (object
    (define name "Xie Yuheng")
    (define age 32)))

;; use syntax sugar:

(define xyh
  {.name "Xie Yuheng"
   .age 32})

(.name xyh) ;; name of xyh
(xyh .name) ;; xyh's name
xyh.name    ;; xyh's name
```

# Part II The algebra of composition

## ARTICLE II Isomorphisms

# Part III Categories of structured sets

# Part IV Elementary universal mapping properties

# Part V Higher universal mapping properties
