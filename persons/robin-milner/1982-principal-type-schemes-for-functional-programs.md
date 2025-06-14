---
title: principal type schemes for functional programs
authors: [luis damas, robin milner]
year: 1982
---

# My Motive

[2025-06-14] 在学习 Hindley-Milner type system，
与 Milner 的原始论文相比，这篇后期的论文可能更简单。
读来试试看。

# 1 Introduction

> This paper is concerned with the polymorphic type discipline of ML,
> which is a general purpose functional programming language, although
> it was first introduced as a metalanguage (whence its name) for
> constructing proofs in the LCF proof system.[4] The type discipline
> was studied in [5] where it was shown to be semantically sound, in a
> sense made precise below, but where one important question was left
> open: does the type-checking algorithm -- or more precisely the type
> assignment algorithm (since types are assigned by the compiler, and
> need not be mentioned by the programmer) -- find the most general
> type possible for every expression and declaration?

> Here we answer the question in the affirmative,
> for the purely applicative part of ML.

> It follows immediately that it is decidable whether a program is
> well-typed, in contrast with the elegant and slightly more
> permissive type discipline of Coppo. [1]

也许值得一读：

- [1] "An extended polymorphic type system for applicative languages",
  M. Coppo，1980.

> The discipline can be well illustrated by a small example.

```scheme
(define (map f s)
  (if (null? s)
    null
    (cons (f (car s)) (map f (cdr s)))))
```

> The type checker will deduce a type-scheme for map from existing
> type-schemes for `null?`, `null`, `cons`, `car` and `cdr`; the term
> type-scheme is appropriate since all these objects are
> polymorphic. In fact from

```scheme
(claim null? (nu (A) (-> (list-t A) bool-t)))
(claim null  (nu (A) (list-t A)))
(claim cons  (nu (A) (-> A (-> (list-t A) (list-t A)))))
(claim car   (nu (A) (-> (list-t A) A)))
(claim cdr   (nu (A) (-> (list-t A) (list-t A))))
```

> will be deduced

```scheme
(claim map (nu (A B) (-> (-> A B) (-> (list-t A) (list-t B)))))
```

> Thus, the main result of this paper is that the type-scheme deduced
> for such a declaration (and more generally, for any ML expression)
> is a principal type-scheme, i.e.  that any other type-scheme for the
> declaration is a generic instance of it. This is a generalisation of
> Hindley’s result for Combinatory Logic [3].

值得一读：

- [3] "The principal type-scheme of an object in combinatory logic",
  R. Hindley, 1969.

TODO

# 2 The language

TODO

# 3 Type instantiation
# 4 Semantics
# 5 Type inference
# 6 The type assignment algorithm W
# 7 Completeness of W
