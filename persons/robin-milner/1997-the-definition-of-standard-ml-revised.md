---
title: The definition of standard ML revised
authors: [Robin Milner, Mads Tofte, Robert Harper, David MacQueen]
year: 1997
---

# My Motive

[2025-05-27] 尝试实现一种 simply typed lisp。

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

```scheme
(claim list-map
  (fresh-type (A B)
    (-> (list-t A) (-> A B) (list-t B))))

(define (list-map l f)
  (match l
    [(null) (null)]
    [(cons head tail)
     (cons (f head) (list-map tail f))]))
```

# My Summary

[2025-05-27] 太枯燥了，根本看不下去。
难怪会有 "Commentary on standard ML"。

# Preface

## Language Definition

> The job of a language-definer is twofold. First - as we have already
> suggested - he must create a world of meanings appropriate for the
> language, and must find a way of saying what these meanings
> precisely are.

> The second part of the definer's job is to define _evaluation_
> precisely. This means that he must define at least what meaning, M,
> results from evaluating any phrase P of his language.

> We shall now explain the keystone of our semantic method. First, we
> need a slight but important refinement. A phrase P is never
> evaluated in vacuo to a meaning M, but always against a background;
> this background - call it B - is itself a semantic object, being a
> distillation of the meanings preserved from evaluation of earlier
> phrases (typically variable declarations, procedure declarations,
> etc). In fact evaluation is background-dependent - M depends upon B
> as well as upon P.

> The keystone of the method, then , is a certain kind of assertion
> about evaluation; it takes the form
>
>    B |- P => M
>
> and may be pronounced: 'Against the background B, the phrase P
> evaluates to the meaning M'. The formal purpose of this Definition
> is no more, and no less, than to decree exactly which assertions of
> this form are true.

> This could be achieved in many ways. We have chosen to do it in a
> str uctu red way, as others have , by giving rules which allow
> assertions about a compound phrase P to be inferred from assertions
> about its constituent phrases P1, ..., Pn.
