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
然后找到对应的 rule，构造一个 task 到 vm 的 task queue list 中。

如果每个 node 只有 single principle port，
对于局部 net 是否可以 interact 的判断就非常简单。
想要支持 multiple principle ports 就是要推广这个判断。

在类 lisp 的语法中可以模仿 prolog 的 logic variable，
用 pattern matching 中重复出现的 variable 来标记 connection 的位置。

```scheme
(define-rule*
    ((iadd target! addend! result)
     (int x target!)
     (int y addend!))
  (connect result (int-add x y)))
```

也许 primitive add 并不是一个好例子，
因为这里的 `(int x target!)` 是不合理的，
试试 nat-max 的例子。

# lisp-like single principle port

```scheme
(define-node nat-max
  first! second result)

(define-node nat-max-add1
  first second! result)

(define-rule (nat-max (zero) second result)
  (connect second result))

(define-rule (nat-max (add1 prev) second result)
  (nat-max-add1 prev second result))

(define-rule (nat-max-add1 first (zero) result)
  (add1 first result))

(define-rule (nat-max-add1 first (add1 prev) result)
  (add1 (nat-max first prev) result))
```

用更一般的 `define-rule*`：

```scheme
(define-node nat-max
  first! second result)

(define-node nat-max-add1
  first second! result)

(define-rule* ((nat-max first second result)
               (zero first))
  (connect second result))

(define-rule* ((nat-max first second result)
               (add1 prev first))
  (nat-max-add1 prev second result))

(define-rule* ((nat-max-add1 first second result)
               (zero second))
  (add1 first result))

(define-rule* ((nat-max-add1 first second result)
               (add1 prev second))
  (add1 (nat-max first prev) result))
```

# lisp-like multiple principle ports

```scheme
(define-node nat-max
  first! second result)

(define-rule (nat-max (zero) second result)
  (connect second result))

(define-rule (nat-max first (zero) result)
  (connect first result))

(define-rule (nat-max (add1 first-prev) (add1 second-prev) result)
  (add1 (nat-max first-prev second-prev) result))
```

用更一般的 `define-rule*`：

```scheme
(define-node nat-max
  first! second! result)

(define-rule* ((nat-max first second result)
               (zero first))
  (connect second result))

(define-rule* ((nat-max first second result)
               (zero second))
  (connect first result))

(define-rule*
    ((nat-max first second result)
     (add1 first-prev first)
     (add1 second-prev second))
  (add1 (nat-max first-prev second-prev) result))
```

# forth-like single principle port

```forth
define-node nat-max first! second -- result end
define-node nat-max-add1 first second! -- result end

define-rule zero nat-max
  ( second result )
  second result connect
end

define-rule add1 nat-max
  ( second result ) ( prev )
  prev second nat-max-add1 result connect
end

define-rule zero nat-max-add1
  ( first result )
  first add1 result connect
end

define-rule add1 nat-max-add1
  ( first result ) ( prev )
  first prev nat-max
  add1 result connect
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
  zero ( value )
  first value is-connected
do
  ( result )
  second result connect
end

define-rule
  nat-max ( first second )
  zero ( value )
  second value is-connected
do
  ( result )
  first result connect
end

define-rule
  nat-max add1 add1
  ( second-value )
  ( first-value )
  ( first second )
  first first-value is-connected
  second second-value is-connected and
do
  ( second-prev )
  ( first-prev )
  ( result )
  first-prev second-prev nat-max add1
  result connect
end
```

注意，不能简单地约定在 pattern matching 的时候，
取出所有 principle ports，然后在描述 rule 的 body 的时候，
取出所有 non principle ports，否则 nat-max zero 就没法实现。

也许可以约定：没用完的局部变量会在 rule body 中用到。
上面的定义用到的就是这种约定。

这样的约定的一个缺点是：有太多的约定了。

- 也许如果能够按照名字取出 field 会更好。

注意，lisp-like 的语法设计并不受这种问题的困扰。

在上面的约定下，也可以囊括之前的 define-rule 语义：

```forth
define-rule
  zero add is-connected
do
  ( addend result )
  addend result connect
end

define-rule
  add1 add is-connected
do
  ( addend result ) ( prev )
  prev addend add
  add1 result connect
end
```
