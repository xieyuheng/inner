---
title: Why Pure type system?
---

# Why Pure type system?

------
- Author: Xie Yuheng
- Date: 2019-12-05
- Keywords: Dependent types.
------

``` js
(s1, s2) in axioms
---------------------- (axiom)
ctx_empty |- s1 : s2

exists s1, s2 in sorts
(s1, s2, s3) in rules
ctx |- A: s1
ctx, x: A |- B: s2
------------------------ (pi abstraction)
ctx |- (x: A) -> B : s3

exists A
ctx |- f: (x: A) -> B
ctx |- a: A
------------------------ (lambda application)
ctx |- f(a) : subst(B, x, a)

exists s1, s2 in sorts
ctx |- A: s1
ctx, x: A |- b: B
ctx, x: A |- B: s2
---------------------------------- (lambda abstraction)
ctx |- (x: A) => b : (x: A) -> B

exists B2
ctx |- A: B2
beta_reduction(B2, B1)
exists s in sorts
ctx |- B1: s
---------------------- (conversion)
ctx |- A: B1
```

## automath-like

let's try a dependent system

``` js
<exp> = <var> | <fn> | <ap>
<fn> = [<var>: <exp>] <exp>
<ap> = { <exp> } <exp>
```

- note that, in this `<var> <var>` is not a `<exp>`
  which is the only different from jojo calculus

we can use `<fn>` as type of `<fn>`

``` js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : [x: A] C
```

we can use `(- ...)` as type of `<fn>`

``` js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : (- A) C
```

we can use pi type as type of `<fn>`

``` js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : pi[x: A] C
```

### pure type system

if we use pi type, we can get something like the pure type system

suppose we have the following judgments,

``` js
<symbel> in sorts
(<symbel>, <symbel>) in axioms
(<symbel>, <symbel>, <symbel>) in rules
```

then we can formulate the following rules,

``` js
(s1, s2) in axioms
---------------------- (axiom)
ctx_empty |- s1 : s2

exists s1, s2 in sorts
(s1, s2, s3) in rules
ctx |- A: s1
ctx, x: A |- B: s2
------------------------ (pi abstraction)
ctx |- pi[x: A] B : s3

exists A
ctx |- f: pi[x: A] B
ctx |- a: A
------------------------ (lambda application)
ctx |- { a } f : subst(B, x, a)

exists s1, s2 in sorts
ctx |- A: s1
ctx, x: A |- b: B
ctx, x: A |- B: s2
---------------------------------- (lambda abstraction)
ctx |- [x: A] b : pi[x: A] B

exists B2
ctx |- A: B2
beta_reduction(B2, B1)
exists s in sorts
ctx |- B1: s
---------------------- (conversion)
ctx |- A: B1
```

### lambda as type

let's only write application and abstraction for simplicity

``` js
exists A
ctx |- f: [x: A] B
ctx |- a: A
------------------------ (lambda application)
ctx |- { a } f : subst(B, x, a)

ctx, x: A |- b: B
---------------------------------- (lambda abstraction)
ctx |- [x: A] b : [x: A] B
```

what is the implication of this
simple structural lambda abstraction rule?

**TODO**
we need to implement this.
and we need to implement one `exp_t` for two concrete syntaxes.
