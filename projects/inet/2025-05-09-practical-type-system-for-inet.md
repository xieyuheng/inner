---
title: practical type system for inet
date: 2025-05-09
---

可以先只考虑增加什么样的类型信息可以解决：

- primitive type 的 match 问题。
- 这个变量重复出现时自动 dup。
- 这个变量没有被使用时自动 drop。

```scheme
(define-node tak x! y! z result)

(define-type int-t
  :is int?
  :dup atom-dup
  :drop atom-drop)

(define-rule (tak (is x int-t) (is y int-t) z result)
  (if (int-lt? y x)
    (tak (tak (isub x 1) y z)
         (tak (isub y 1) z x)
         (tak (isub z 1) x y)
         result)
    (connect result z)))
```

# assembly like syntax design

```scheme
(define-rule <pattern>
  (assign-prop x :primitive-predicate int?)
  (assign-prop x :auto-dup int-dup)
  (assign-prop x :auto-drop int-drop)
  ...)
```
