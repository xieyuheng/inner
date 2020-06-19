# Dependently Typed JoJo Calculus

------
- Author: Xie Yuheng
- Date: **WORK IN PROGRESS**
- Keywords: Type system.
------

## Abstract

## The algebraic structure of the space of dependent type

What is the algebraic structure of the space of dependent types?

## forth type system

``` js
ctx_lookup(ctx, f) == [A] -> [B]
---------------------------------- (variable lookup)
ctx |- f : [A] -> [B]

ctx |- f : [A] -> [B, X]
ctx |- g : [X] -> [C]
----------------------------- (cut)
ctx |- f g : [A] -> [B, C]

ctx |- f : [A] -> [X]
ctx |- g : [B, X] -> [C]
----------------------------- (cut-deep)
ctx |- f g : [B, A] -> [C]

ctx, x: A |- f : [B] -> [C]
----------------------------------- (let)
ctx |- (x: A) f : [B, A] -> [C]

ctx |- f : [A] -> [B]
------------------------------------- (quote)
ctx |- { f } : [] -> [[A] -> [B]]
```

## about exe (nullary-apply)

``` js
ctx |- f : [] -> [[A] -> [B]]
------------------------------- (exe)
ctx |- f exe : [A] -> [B]

ctx |- f : [] -> [[A] -> [B]]
------------------------------- (exe by let)
ctx |- f (x: [A] -> [B]) x : [A] -> [B]

------------------------------- (exe alone)
ctx |- exe : [A, [A] -> [B]] -> [B]

------------------------------- (exe alone by let)
ctx |- (x: [A] -> [B]) x : [A, [A] -> [B]] -> [B]
```

## jojo

type level computation

``` js
ctx_lookup(ctx, f) == { (x: A) B }
---------------------------------- (variable lookup)
ctx |- f : (x: A) B

ctx |- f : (a: A) B X
ctx |- g : (x: X) C
----------------------------- (cut)
ctx |- f g : (a: A) B C

ctx |- f : (a: A) X
ctx |- g : (x: X) (b: B) C
----------------------------- (cut-deep)
ctx |- f g : (a: A) (b: B) C

ctx, x: A |- f : (b: B) C
----------------------------------- (let)
ctx |- (x: A) f : (x: A) (b: B) C

ctx, x: A |- f : C
----------------------------------- (let-simple)
ctx |- (x: A) f : (x: A) C

ctx |- f : (a: A) B
------------------------------------- (quote)
ctx |- { f } : { (a: A) B }
```

## disambiguate (x: A)

`-` reads `match`

``` js
ctx |- f : (- A) B X
ctx |- g : (- X) C
----------------------------- (cut)
ctx |- f g : (- A) B X (- X) C
============ (- A) B C

ctx |- f : (- A) X
ctx |- g : (- X) (- B) C
----------------------------- (cut-deep)
ctx |- f g : (- A) X (- X) (- B) C
============ (- A) (- B) C

ctx, x: A |- f : (- B) C
----------------------------------- (let)
ctx |- (let x: A) f : (- A) (- B) C

ctx, x: A |- f : C
----------------------------------- (let-simple)
ctx |- (let x: A) f : (- A) C
```

so we have `(let x: A) : (- A)`

how about `(- A) : ???`

maybe

``` js
ctx |- A : A2
------------------------------
ctx |- (- A) : (- A2)
```

## dependent type

we need two new jo in exe: `dep` and `lit`

``` js
(dep x: A)
cut ==> (let x: A)

{lit A}
cut ==> A
```

## [todo] semantic of eval

operational semantic
by using a stack, we have call-by-value operational semantic

can we also have call-by-name
and call-by-need operational semantic?

what is a redex in jojo?

``` js
{ X (- X) } => { }
{ a (let x: X) f } => { [x = a] f }
```

## [todo] encoding simple type system in jojo type system

encoding application by composition

## [todo] jojo type system framework

like pure type system framework

## norm-by-eval

can we use norm-by-eval to check equivalence of jojo?

maybe not

because think of the definition of neu_t and val_t
the definition can be viewed as a classification of exp_t

but if we try to define neu_t for jojo

we must be able to know arity and co-arity
of neu_var_t from its type

- we need to handle multi-return values

- for a name
  its cut and exe must have the same arity and co-arity

and the definition will not be classification of jo_t

- but it seems we have no choice

## [todo] term-rewriting

how to check equivalence of jojo?

try term-rewriting?

maybe not

because in lambda calculus
`val_t` and `neu_t` can be viewed as special `exp_t`
but in jojo `val_t` and `jo_t` are totally different

## jojo simple

if no let is allowed to occur in cut
then equivalence between jojo will be simple
