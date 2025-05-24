---
title: simply typed lisp
date: 2025-05-24
---

# Dynamicly typed

I was thinking about dynamicly typed lisp,
because I want to learn about generic dispatching.

keywords:

- `define-generic`
- `define-handler`
- `define-data`
- `match-data`

```scheme
(define-data exp?
  (exp-var [name string?])
  (exp-fn [name string?] [body exp?])
  (exp-ap [target exp?] [arg exp?]))

(match-data exp? exp
  [(exp-var name) ...]
  [(exp-fn name body) ...]
  [(exp-ap target arg) ...])
```

```scheme
(declare add (-> nat? nat? nat?))
(define (add x y) ...)
```

# Simply typed

It seems simply typed version makes more sense,
But is it still possible to support generic dispatching?

```scheme
(define-datatype exp-t
  (exp-var [name string-t])
  (exp-fn [name string-t] [body exp-t])
  (exp-ap [target exp-t] [arg exp-t]))

(check exp-var (-> string-t exp-t))
(check exp-fn (-> string-t exp-t exp-t))
(check exp-ap (-> exp-t exp-t exp-t))

(match exp
  [(exp-var name) ...]
  [(exp-fn name body) ...]
  [(exp-ap target arg) ...])
```

Suppose data need to be constructed with `()`,
even zero arity `(null)` and `(zero)`,
so that it is clear what is a pattern in pattern matching.

```scheme
(claim add (-> nat-t nat-t nat-t))
(define (add x y)
  (match x
    [(zero) y]
    [(add1 prev)
     (add1 (add prev y))]))

(define-case (add (zero) y) y)
(define-case (add (add1 prev) y) (add1 (add prev y)))
```

```scheme
(claim list-map
  (fresh (A B)
    (-> (list-t A) (-> A B) (list-t B))))
(define (list-map l f)
  (match l
    [(null) (null)]
    [(cons head tail)
     (cons (f head) (list-map tail f))]))
```
