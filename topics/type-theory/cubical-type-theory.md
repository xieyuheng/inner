---
title: Cubical type theory
---

# Using elements of Equal as function

So called path type.

A term of type `Equal(A, a, b)` can be used
as a function `p: (Interval) -> A`,
such that `p(Interval.start) = a` and `p(Interval.end) = b`,
i.e. a path in space `A`,

```cicada
function equalPair(
  start: Pair(A, B),
  end: Pair(A, B),
  carPath: Equal(A, car(start), car(end)),
  cdrPath: Equal(B, cdr(start), cdr(end)),
): Equal(Pair(A, B), start, end) {
  // NOTE First we can eta-expend the pairs `start`, `end` to cons:
  //    Equal(Pair(A, B), start, end)
  // => Equal(Pair(A, B), cons(car(start), cdr(start)), cons(car(end), cdr(end)))

  equal carPath(Interval.start) = car(start)
  equal carPath(Interval.end) = car(end)

  equal cdrPath(Interval.start) = cdr(start)
  equal cdrPath(Interval.end) = cdr(end)

  let resultPath: Equal(Pair(A, B), start, end) =
    (i: Interval) => cons(carPath(i), cdrPath(i))

  equal resultPath(Interval.start) = cons(car(start), cdr(start))
  equal resultPath(Interval.end) = cons(car(end), cdr(end))

  // Thus by the definition of `Equal`, we have
  //   resultPath: Equal(Pair(A, B), cons(car(start), cdr(start)), cons(car(end), cdr(end)))

  return resultPath
}
```

```cicada
function equalCompose(
  implicit A: Type,
  implicit x: A,
  implicit y: A,
  implicit z: A,
  xyPath: Equal(A, x, y),
  yzPath: Equal(A, y, z),
): Equal(A, x, z) {

  equal xyPath(Interval.start) = x
  equal xyPath(Interval.end) = y

  equal yzPath(Interval.start) = y
  equal yzPath(Interval.end) = z

  let xzPath: Equal(A, x, z) =
    // NOTE But we can not
    (i: Interval) => match (i) {
      case (Interval.start) => xyPath(i)
      case (Interval.end) => yzPath(i)
    }

  equal xzPath(Interval.start) = x
  equal xzPath(Interval.end) = z

  return xzPath
}
```

# Lecture: Cartesian cubical type theory, by Favonia

[ [YOUTUBE](https://www.youtube.com/watch?v=VbBDxVEu_bA&list=PLtIZ5qxwSNnzpNqfXzJjlHI9yCAzRzKtx&index=73) ]

```cicada
datatype Interval {
  start: Interval
  end: Interval
  path: endpoints { start -end }
}

function aPath(i: Interval): A {
  // TODO How case on `Interval.path`.
  return ...
}

function aSurface(i: Interval, j: Interval): A {
  // TODO How case on `Interval.path`.
  return ...
}
```
