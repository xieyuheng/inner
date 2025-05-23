---
title: par-lisp-untyped
source: "https://github.com/faiface/par-lang/blob/50d85d6913d57b71cb2d29476191461ce55cf441/OLD_README.md"
---

# Program structure

```scheme
(define <name> <expression>)
(define (<name>) <expression> ...)
```

# Channel spawning

```scheme
(channel (<name>) <process>)
```

# Closing channels

```scheme
(define program
  (channel (user) (user !)))
```

```scheme
(define program
  (channel (user)
    (let ([child (channel (parent) (parent !))])
      (@ child ?)
      (user !))))
```

# Linearity of channels

```scheme
(define program
  (channel (user)
    (let ((child1 (channel (parent) (parent !)))
          (child2 (channel (parent)
                    (@ child1 ?) ;; captured `child1` here
                    (parent !))))
      (@ child1 ?) ;; comment this line to avoid crash
      (@ child2 ?)
      (user !))))
```

# Signaling

```scheme
(define program
  (channel (user)
    (user :hello)
    (user :world)
    (user !)))

(define program
  (channel (user)
    (user :hello :world !)))
```

```scheme
(define program
  (channel (user)
    (choice
      [(@ user :first) (user :wrong) (user !)]
      [(@ user :second) (user :correct) (user !)]
      [(@ user :third) (user :wrong) (user !)])))

(define program
  (channel (user)
    (match user
      [:first => (user :wrong) (user !)]
      [:second => (user :correct) (user !)]
      [:third => (user :wrong) (user !)])))
```

# Exchanging values

```scheme
(define true (channel (result) (result :true !)))
(define false (channel (result) (result :false !)))
```

```scheme
(define program
  (channel (user)
    (user true !)))
```

```scheme
(define program
  (channel (user)
    (user true)
    (user false)
    (user true false !)))
```

```scheme
(define program
  (channel (user)
    (let ([child (channel (parent)
                   (parent true)
                   (parent false !))])
      (@ child value1)  ;; creates a new variable `value1`
      (@ child value2)  ;; and another variable, `value2`
      (@ child ?)
      (user value1 value2 !))))
```

# Implementing functions

```scheme
(define not
  (channel (caller)
    (@ caller bool)
    (match bool
      [:true ! => (caller false !)]
      [:false ! => (caller true !)])))

(define program
  (channel (user)
    (let ([function not])
      (@ (function true) result ?)
      (user result !))))
```

```scheme
(define not
  (channel (caller)
    (@ caller bool)
    (match bool
      [:true ! => (caller :false !)]
      [:false ! => (caller :true !)])))

(define program
  (channel (user)
    (let ([negation not])
      (negation true)
      (user negation !))))

(define program
  (channel (user)
    (user (not true) !)))
```

# Linking

```scheme
```

# Recursion, the usual way

```scheme
```

# Recursion, a better way

```scheme
```
