---
title: Syntax for writing natural deduction style proofs
date: 2022-05-07
---

In [00017](00017-reversed-inference-rule-style-function-application-syntax.md),
we use changed function application syntax,
aiming to get good syntax for writing proofs.

Maybe we do not need them, and all we need are

- To use assignment in function body.
- Be able to write annotation when needed.

Example proof:

- Take from [do-it-yourself-type-theory.md](../persons/roland-backhouse/do-it-yourself-type-theory.md)

```cicada pseudocode
example : ((Either(A, (A) -> B)) -> B) -> B
example = (f) => f(inr((x) => f(inl(x))))
```

Example proof, with more detailed deduction steps:

- We use `----` as `=>`.
- The arguments of function can be annotated.
- Then, we can simply use `claim` and `define` in function body,
  to break down long a proof,
  meanwhile explicit show intermediate steps of our reasoning.

```cicada pseudocode
example : { { Either(A, { A -> B }) -> B } -> B }
example = {
  f : { Either(A, { A -> B }) -> B }
  --------
  g : { A -> B }
  g = {
    x : A
    ------------
    inl(x) : Either(A, { A -> B })
    f(inl(x)) : B
  }
  inr(g) : Either(A, { A -> B })
  f(inr(g)) : B
}
```

Another example:

```cicada pseudocode
list_append : {
  l, m : List(A)
  ---------
  List(A)
}
list_append = {
  l : List(A)
  m : List(A)
  -----------------------
  g : { A; List(A); List(A); -> List(A) }
  g = {
    x : A
    _ : List(A)
    h : List(A)
    ------------------
    cons(x, h) : List(A)
  }
  list_elim(l, m, g) : List(A)
}
```
