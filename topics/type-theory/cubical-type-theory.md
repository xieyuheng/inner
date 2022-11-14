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

  check carPath(Interval.start): car(start)
  check carPath(Interval.end): car(end)

  check cdrPath(Interval.start): cdr(start)
  check cdrPath(Interval.end): cdr(end)

  let resultPath: Equal(Pair(A, B), start, end) =
    (i: Interval) => cons(carPath(i), cdrPath(i))

  check resultPath(Interval.start): cons(car(start), cdr(start))
  check resultPath(Interval.end): cons(car(end), cdr(end))

  // Thus by the definition of `Equal`, we have
  //   resultPath: Equal(Pair(A, B), cons(car(start), cdr(start)), cons(car(end), cdr(end)))

  return resultPath
}


```
