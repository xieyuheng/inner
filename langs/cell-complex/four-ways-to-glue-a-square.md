---
title: 4 ways to glue a square
---

# note

- (n ...) 記法用以記錄 n 階元素的類型[即 邊界]
  (n ...) 不單可以被用作類型 還可以被用作 glue 來形成複合元素
  n-1 階元素之 glue 需要寫在 (n ...) 內 [(0) (1) 除外]

# sphere-2

```scheme
(def sphere-2
  (type space
    v w z (0 <>)
    a (1 v w)
    b (1 w z)
    m (2 a b b rev a rev)))

(def sphere-2
  (type space
    v w (0 <>)
    a (1 v w)
    m (2 a a rev)))
```

# torus-2

```scheme
(def torus-2
  (type space
    v (0 <>)
    a b (1 v v)
    m (2 a b a rev b rev)))

(def I
  (type space
    a b (0 <>)
    i (1 a b)))
```

```cicada
space torus_2_t (
  v : torus_2_t
  a b : v == v
  m : a b == b a
)
```

# k-2

```scheme
(def k-2
  (type space
    v (0 <>)
    a b (1 v v)
    m (2 a b a b rev)))
```

# p-2

```scheme
(def p-2
  (type space
    v w (0 <>)
    a (1 v w)
    b (1 w v)
    m (2 a b a b)))

(def p-2
  (type space
    v (0 <>)
    a (1 v v)
    m (2 a a)))
```
