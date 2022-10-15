---
title: Lisp uses too much parenthesis only because of key-value maps
date: 2022-10-16
---

Lisp uses too much parenthesis only because of key-value map.

When expressing other things,
Lisp uses the same amount of parenthesis
comparing to c-family languages.

For example:

```c
f(a, b, c)
```

```scheme
(f a b c)
```

But when expressing key-value maps,
the `[]` are the "too much" parenthesis:

```scheme
(define mark
  (object
    ["Alex" (object
              ["biology" 73]
              ["history" 85])]
    ["Jim" (object
             ["biology" 86]
             ["history" 92])]))
```

Comparing to PHP:

```php
$mark = [
  "Alex" => [
    "biology" => 73,
    "history" => 85,
  ],
  "Jim" => [
    "biology" => 86,
    "history" => 92,
  ],
];
```

One way of solving is to view `<key> => <value>` as a sugar of `[<key> <value>]`:

```scheme
(define mark
  (object
    "Alex" => (object
                "biology" => 73
                "history" => 85)
    "Jim" => (object
               "biology" => 86
               "history" => 92)))
```

Another example:

```scheme
(fn ife (Pi ((A Set)) (-> Boolean A A A))
  [(ife A true a b) a]
  [(ife A false a b) b])

;; with => sugar:

(fn ife (Pi ((A Set)) (-> Boolean A A A))
  (ife A true a b) => a
  (ife A false a b) => b)
```
