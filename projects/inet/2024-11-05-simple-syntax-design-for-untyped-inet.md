---
title: simple syntax design for untyped inet
date: 2024-11-05
---

How to design inet in the spirit of uxn and modal and fractran?
Remove type and simplify the syntax.

Design 1:

```
* zero -- value!
* add1 prev -- value!
* add target! addend -- result

! zero add
  (add)-addend result-(add)

! add1 add
  (add)-addend (add1)-prev add
  add1 result-(add)

= one zero add1
= two one one add
= three two one add
= four two two add

. two two add two two add
```

Design 2:

define node by `(name)`, and rule by `(name)-(name)`.

```
(zero) -- value!
(add1) prev -- value!
(add) target! addend -- result

(zero)-(add)
  (add)-addend result-(add)

(add1)-(add)
  (add)-addend (add1)-prev add
  add1 result-(add)

= one zero add1
= two one one add
= three two one add
= four two two add

. two two add two two add
```

Design 3:

```
* (zero) -- value!
* (add1) prev -- value!
* (add) target! addend -- result

! (zero)-(add)
  (add)-addend result-(add)

! (add1)-(add)
  (add)-addend (add1)-prev add
  add1 result-(add)

= one zero add1
= two one one add
= three two one add
= four two two add

. two two add two two add
```
