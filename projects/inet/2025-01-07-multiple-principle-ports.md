---
title: multiple principle ports
date: 2025-01-07
---

我感觉只有支持了 multiple principle ports，inet 才算是能实用。
比如 primitive int 之间的 add，就是要等待两个参数都到齐才能作用。

考虑之前对图的分析，感觉有多个 principle ports 也不影响并行运行：

```
     \   |   /
  .-------------.
  |    \ | /    |
  |   (.....)   |
  |      |      |
  |   (.....)   |
  |    / | \    |
  `-------------`
     /   |   \
```

为了实现 multiple principle ports，
在 connect 的时候，就需要判断所连接的局部 net 是否可以 interact，
然后找到对应的 rule，构造一个 activity 到 vm 的 activity list 中。

如果每个 node 只有 single principle port，
对于局部 net 是否可以 interact 的判断就非常简单。
想要支持 multiple principle ports 就是要推广这个判断。

在类 lisp 的语法中可以模仿 prolog 的 logic variable，
用 pattern matching 中重复出现的 variable 来标记 connection 的位置。

```scheme
(define-rule*
  [(add target! addend! result)
   (int x target!)
   (int y addend!)]
  (connect result (int-add x y)))
```

也许 primitive add 并不是一个好例子，
试试 nat-max 的例子。

# lisp-like single principle port

```scheme
(define-node nat-max
  first! second result)

(define-node nat-max-nadd1
  first second! result)

(define-rule (nat-max (nzero) second result)
  (connect second result))

(define-rule (nat-max (nadd1 prev) second result)
  (nat-max-nadd1 prev second result))

(define-rule (nat-max-nadd1 first (nzero) result)
  (nadd1 first result))

(define-rule (nat-max-nadd1 first (nadd1 prev) result)
  (nadd1 (nat-max first prev) result))
```

# lisp-like multiple principle ports

```scheme
(define-node nat-max
  first! second result)

(define-rule (nat-max (nzero) second result)
  (connect second result))

(define-rule (nat-max first (nzero) result)
  (connect first result))

(define-rule (nat-max (nadd1 first-prev) (nadd1 second-prev) result)
  (nadd1 (nat-max first-prev second-prev) result))
```

# forth-like single principle port

```forth
define-node nat-max first! second -- result end
define-node nat-max-nadd1 first second! -- result end

define-rule nzero nat-max
  ( second result )
  second result connect
end

define-rule nadd1 nat-max
  ( second result ) ( prev )
  prev second nat-max-nadd1 result connect
end

define-rule nzero nat-max-nadd1
  ( first result )
  first nadd1 result connect
end

define-rule nadd1 nat-max-nadd1
  ( first result ) ( prev )
  first prev nat-max
  nadd1 result connect
end
```

# forth-like multiple principle ports

假设 define-rule 有两部分，
前一部分是对 connection 的判读，
后一部分是 rule。
判断 connection 就是拆出来 principle ports，
但是不删除 node。

```forth
define-node nat-max first! second! -- result end

define-rule
  nat-max ( first second )
  nzero ( value )
  first value is-connected
do
  // 这里设计是错误的，不能约定在 pattern matching 的时候，
  // 取出所有 principle ports，然后在描述 rule 的 body 的时候，
  // 取出所有 non principle ports，比如这个函数就没法实现。
  // 也许可以约定：没用完的局部变量会在 rule body 中用到。
  ( result )
  second result connect
end

define-rule
  nat-max ( first second )
  nzero ( value )
  second value is-connected
do
  ( result )
  first result connect
end

define-rule
  nat-max nadd1 nadd1
  ( second-value )
  ( first-value )
  ( first second )
  first first-value is-connected
  second second-value is-connected and
do
  ( second-prev )
  ( first-prev )
  ( result )
  first-prev second-prev nat-max nadd1
  result connect
end
```

在上面的约定下，也可以囊括之前的 define-rule 语义：

```forth
define-rule
  nzero nadd is-connected
do
  ( addend result )
  addend result connect
end

define-rule
  nadd1 nadd is-connected
do
  ( addend result ) ( prev )
  prev addend nadd
  nadd1 result connect
end
```
