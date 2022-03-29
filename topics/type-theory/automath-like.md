---
title: Automath-like
---

Let's try to define a Automath-like language
with the knowledge about Forth and Joy.

```js
<exp> = <var> | <fn> | <ap>
<fn> = [<var>: <exp>] <exp>
<ap> = { <exp> } <exp>
```

Note that, in this grammar `<var> <var>` is not `<exp>`
which is the only different from jojo calculus.

we can use `<fn>` as type of `<fn>`

```js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : [x: A] C
```

we can use `(- ...)` as type of `<fn>`

```js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : (- A) C
```

we can use pi type as type of `<fn>`

```js
ctx, x: A |- f : C
-------------------------- (let)
ctx |- [x: A] f : pi[x: A] C
```
