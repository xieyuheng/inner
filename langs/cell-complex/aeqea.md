---
title: aeqea -- a language for equivalence
---

# [note]

## semantic and examples first

- this time, semantic and examples first.

  we imagine semantic of the language
  than try to express as many examples as possible by new syntax.

## syntax

- (n X) for n level element of X
  for example, we have subspace relation (< (= x0 x0) (1 X))

- (map) for partial-map and total-map

- (%) as reversed (:)

## semantic

- save each other's pointers as glue.
  this encoding of space,
  helps us to check equivalence by semantic instead of syntax.

- the type of a function limits how to apply it or compose it.
  the type of a chain limits how to glue it.

- the type of a chain is its boundary,
  boundary of boundary is empty.

# cylinder

```scheme
(define cylinder
  (type space
    (0 (: a0 a1 (0 <>)))
    (1 (: b0 (= a0 a1))
       (: b1 (= a1 a1))
       (: b2 (= a0 a0)))
    (2 (: c0 (= (+ b0 b1) (+ b2 b0))))))
```

# mobius-band

# tetrahedron

```scheme
(define tetrahedron
  (type space
    (0 (: p0 p1 p2 p3 (0 <>)))
    (1 (: a01 (= p0 p1))
       (: a02 (= p0 p2))
       (: a03 (= p0 p3))
       (: a12 (= p1 p2))
       (: a13 (= p1 p3))
       (: a23 (= p2 p3)))
    (2 (: b012 (= (+ a01 a12) a02))
       (: b123 (= (+ a12 a23) a13))
       (: b013 (= (+ a01 a13) a03))
       (: b023 (= (+ a02 a23) a03)))))

(note basis normal forms)

(define S2
  (type space
    (0 (: p0 p1 (0 <>)))
    (1 (: a0 (= p0 p1)))
    (2 (: b0 (= a0 a0)))))

(define P2
  (type space
    (0 (: p0 (0 <>)))
    (1 (: a0 (= p0 p0)))
    (2 (: b0 (= a0 (- a0))))))
```
