---
title: A tutorial on implementing de morgan cubical type theory
author: Tesla Zhang
year: 2022
---

## 2.2. Partial elements

```cicada
let x: I
let y: I
let z: I

let A: Type
let u: (I) -> (I) -> A
let v: (I) -> (I) -> A

element {
  [z = 1] => u(x, y)
  [y = 0] => v(x, z)
}: Partial A {
  [z = 1]
  [y = 0]
}
```

## 3.1. Generalized paths

```cicada
Path(A, a, b)
PathOver((x) ~> A, a, b)

(x) ~> u: PathOver((x) ~> A, u[0/x], u[i/x])

path A {
  [x = 0] => a
  [x = 1] => b
}

path (x) ~> A {
  [x = 0] => a
  [x = 1] => b
}

function concat(
  implicit A: Type,
  implicit a: A,
  implicit b: A,
  implicit c: A,
  p: Path(A, a, b),
  q: Path(A, b, c),
): Path(A, a, c) {
  //
}

function concat(
  implicit A: Type,
  p: path A {},
  q: path A {
    [x = 0] => p[1]
  },
): path A {
    [x = 0] => p[0]
    [x = 1] => q[1]
} {
  //
}

let o: A
let p1: Path(A, o)
let p2: Path(A, o)
let p3: Path(A, o)
let p4: Path(A, o)

// TODO Try to hand type check the following apply of `PathOver`.

PathOver((y) ~> Path(A, p2[y], p3[y]), p0, p1)

path (x, y) ~> A {
  [x = 0] => p2[y]
  [x = 1] => p3[y]
  [y = 0] => p0[x]
  [y = 1] => p1[x]
}
```

## 3.2. Cubicalsub types

To view a partial element as a type?

Not really?

```cicada
sub elements A {
  [...] => ...
  ...
}
```

## 4.3. The coercion operator

```cicada
function fill(implicit A: (I) => Type): path A {
  [x = 0] => u
  [x = 1] => coerce (A, u) {
    ...
  }
} {
  return (x) ~> coerce ((y) ~> A(join(x, y)), u) {
    [x = 0]
    ...
  }
}
```
