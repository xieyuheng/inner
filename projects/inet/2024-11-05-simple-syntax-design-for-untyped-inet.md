---
title: simple syntax design for untyped inet
date: 2024-11-05
---

How to design inet in the spirit of uxn and modal and fractran?

有类型的：

```
type Nat -- @Type end

node zero
  ------------
  Nat :value!
end

node add1
  Nat :prev
  ------------
  Nat :value!
end

node add
  Nat :target!
  Nat :addend
  ------------
  Nat :result
end

rule zero add
  (add)-addend
  result-(add)
end

rule add1 add
  (add)-addend
  (add1)-prev add
  add1 result-(add)
end

claim one -- Nat end
define one zero add1 end

claim two -- Nat end
define two one one add end

claim three -- Nat end
define three two one add end

claim four -- Nat end
define four two two add end

two two add
two two add @run
```

去掉类型的：

```
node zero -- value! end
node add1 prev -- value! end
node add target! addend -- result end

rule zero add
  (add)-addend
  result-(add)
end

rule add1 add
  (add)-addend
  (add1)-prev add
  add1 result-(add)
end

define one zero add1 end
define two one one add end
define three two one add end
define four two two add end

two two add two two add @run
```

简化语法的：

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

一些简化语法的方案。
假设上面的是方案（1）。

那么方案（2）可能是，
定义 node 的语法可以用 `(name)` 来代表，
为了整齐用 `(name)-(name)` 来代表规则：

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

也许可以冗余一些，方案（3）：

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
