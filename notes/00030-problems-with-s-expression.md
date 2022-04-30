---
title: Problems with S-expression
date: 2022-04-30
---

Some problems, or say features, of S-expression (sexp).

# Rigid body indentation

Idiomatic S-expression use "rigid body indentation",
where the whole expression is moved like a rigid body,
as the following:

```scheme
(record
  (t type-tt)
  (car t)
  (cdr (list-t t)))

(define cons-t (record
                 (t type-tt)
                 (car t)
                 (cdr (list-t t))))

(define cons-t
  (record
    (t type-tt)
    (car t)
    (cdr (list-t t))))
```

Moving expression non-rigidly is not idomtic:

```scheme
(define cons-t (record
  (t type-tt)
  (car t)
  (cdr (list-t t))))
```

**This might be viewed as a desired feature.**

- It gives us one more assumption about the shape of our code,
  thus reduce cognitive load for recognizing patterns of code shapes.

But it wastes indentation, for example:

```scheme
(claim nat-add-commutative
  (Pi ((x y nat-t))
    (eqv-t
     (nat-add x y)
     (nat-add y x))))

(define (nat-add-commutative x y)
  (match (x)
    (zero-t (nat-add-zero-commutative y))
    (succ-t (eqv-compose
             (eqv-apply succ-c (nat-add-commutative x.prev y))
             (nat-add-succ-commutative y x.prev)))))
```

In C-like `{ ... }`, If we want to save indentation,
I can write the above example as:

```
nat-add-commutative : (x y : nat-t) ->
  eqv-t (
    nat-add (x y)
    nat-add (y x))

nat-add-commutative (x y) = case (x) {
  zero-t => nat-add-zero-commutative (y)
  succ-t => eqv-compose (
    eqv-apply (succ-c nat-add-commutative (x.prev y))
    nat-add-succ-commutative (y x.prev))
}
```

And the example above the above example as:

```
record {
  t : type-tt
  car : t
  cdr : list-t (t)
}

cons-t = record {
  t : type-tt
  car : t
  cdr : list-t (t)
}
```

# Too many parenthese indeed

If we want to make the structure of key-value pair explicit,
sexp will use too many parenthese indeed.

Take object for example,

```scheme
(record
  (x 1)
  (y 2))
```

```js
{
  x: 1,
  y: 2,
}
```

For one pair of key-value, the number of delimiters are both two.

|         | delimiters |
|---------|------------|
| `(x 1)` | `(` `)`    |
| `x: 1,` | `:` `,`    |

But sexp always uses parenthese.

The same is true for variable bindings,

```scheme
(Pi ((a A) (b B)) (C a b))
(Sigma ((a A) (b B)) (C a b))
```

```cicada expressions
forall (a: A, b: B) C(a, b)
exists (a: A, b: B) C(a, b)
```

Sexp uses less delimiters in general (because of less commas),
but it always uses parenthese as delimiter,
thus it uses more parenthese.

**This might be viewed as a desired feature.**

- It uses less delimiters in general.
- Only using parenthese as delimiter
  might be viewed as consistency in syntax design.

Clojure has one why of reducing parenthese by depending on the position.

```clojure
{:x 1
 :y 2}
```

An item is viewed as key or value, depends on
whether it occurs in the even or odd position (indexing from zero),
