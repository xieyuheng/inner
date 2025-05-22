---
title: par-lisp
repo: "https://github.com/faiface/par-lang"
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
    (choice-input user
      [:first (user :wrong) (user !)]
      [:second (user :correct) (user !)]
      [:third (user :wrong) (user !)])))
```

# Exchanging values

TODO
