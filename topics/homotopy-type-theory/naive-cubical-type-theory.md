---
title: Naive cubical type theory
author: Bruno Bentzen
year: 2022
---

# 2 The cubical point of view

## 2.1 The type of paths

If we have `p: Path(A, a, b)` in the type context,
without knowing the value of `p`,
we can apply `p` to `I::0` and `I::1`,
to get `p(I::0) = a` and `p(I::1) = b`.

But we can not use `i` in a path abstraction `(i: I) => ...`.

## 2.2 How we should think of paths

Use the topological intuition.

## 2.3 How can we use paths?

```cicada
function refl(
  implicit A: Type,
  implicit a: A,
): Path(A, a, a) {
  return (i: I) => a
}

function ap(
  implicit A: Type,
  implicit B: Type,
  f: (A) -> B,
  implicit a0: A,
  implicit a1: A,
  path: Path(A, a0, a1),
): Path(B, f(a0), f(a1)) {
  return (i: I) => f(path(i))
}

function funext(
  implicit A: Type,
  implicit B: (A) -> Type,
  f: (A) -> B,
  g: (A) -> B,
  pathEverywhere: (x: A) -> Path(B, f(x), g(x)),
): Path((A) -> B, f, g) {
  return (i: I) => (x: A) => pathEverywhere(x, i)
}
```

# 3 There are enough paths

## 3.1 Transportation along paths

- **Xie:** I do not understand transportation yet.

  TODO

```cicada
function transport(
  A: (I) -> Type,
  i: I,
  a: A(i),
  j: I,
): A(j) {
  ...
}

function transport(
  F: Path(Type, A, B),
  a: A,
  j: I,
): B {
  ...
}

function *(
  C: (A) -> Type,
  a: A,
  b: A,
  p: Path(A, a, b),
): (C(a)) -> C(B) {
  // let motive: (I) -> Type = (i) => C(p(i))
  let motive: Path(Type, C(a), C(b)) = (i) => C(p(i))
  return (c: C(a)) => transport(motive, p, c)
}
```

## 3.2 Composition of paths
