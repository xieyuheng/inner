---
title: Diagonalization and self reference
author: Raymond Smullyan
year: 1994
---

# 1 Introduction to self-reference

## Part I Quotation and self-reference

### Use and mention

### Self-reference using diagonalization

Use jojo to understand the examples here.

We use Polish notation instead of Forth's reversed Polish notation,
thus execution goes from right to left.

```
D (s) := subst [x] [s] [s];

// substitute [x] by [s] in [s]
```

Note that, identity function is defined as:

```
id (x) := [x];
```

And apply can be define as:

```
apply (x) := x;

[J D x]
D [J D x] = [J D [J D x]]
J D [J D x] = J [J D [J D x]]

X = J [X]
X = J D [J D x] = J [J D [J D x]]

D [D x] = [D [D x]]
```

### Normalization

```
N (s) := [s [s]];

[J N]
N [J N] = [J N [J N]]
J N [J N] = J [J N [J N]]

X = J [X]
X = J N [J N] = J [J N [J N]]
```

// 假设用函数作用语法 f(x) 中的 () 来理解 quote，
// 并且把空格理解为函数复合的中缀表达式
// N(s) := s(s)
// X = J(X)
// X = J N(J N) = J(J N(J N))
// - quote 不能单独出现只能在函数作用语法中出现
// - 不用 quote 好像语义也成立，不用写 [J N] 只用写 J N
// - 这种理解在之后的章节提到过

```
N [N] = [N [N]]
```

- 为什么被称作 "normalization”？
  这与 lambda term 的 normalization 有关吗？

Exercise 1: X = J N [X]

```
X = J N N [J N N] = J N [J N N [J N N]]
J N N [J N N] = J N [J N N [J N N]] = J [J N N [J N N] [J N N [J N N]]]
```

Exercise 2: X = N [X]

```
X = N N [N N] = N [N N [N N]]
N N [N N] = N [N N [N N]] = [N N [N N] [N N [N N]]]
```

Exercise 3: X = Q [X]

```
X = Q N [Q N] = Q [Q N [Q N]]
```

Exercise 4: X = N Q [X]; Y = Q N [Y]

```
X = N Q N [N Q N] = N Q [N Q N [N Q N]]

Y = Q N N [Q N N] -- like Exercise 1.
```

Exercise 5: X = J Q [X]

```
X = J Q N [J Q N] = J Q [J Q N [J Q N]]
```

Exercise 6: X = J N Q [X]; Y = J Q N [Y]

```
X = J N Q N [J N Q N] = J N Q [J N Q N [J N Q N]]
Y = J Q N N [J Q N N] = J Q N [J Q N N [J Q N N]]
```

Exercise 7: X = Q Y; Y = [X]

```
X = Q N [Q N] = Q [Q N [Q N]]
Y = [Q N [Q N]]
```

### One-side quotation

When using jojo as semantic, "One-side quotation" can be understood as
language with first class quotation keyword. (like LISP)

To execute a jojo, the machine read from left,
and break the jojo by the first occur of "*",
push the right part of "*" as quoted program on data stack,
and execute the left part of "*"
(starting from the left end of the left part).

```
A (s) := * s * s;

A * J A = * J A * J A
J A * J A = J * J A * J A

A * A = * A * A

R (s) := * s s;

R * J R * = * J R * J R *
J R * J R * = J * J R * J R *

R * R * = * R * R *
```

## Part II Self-reference in a more general setting

Using a language with general application syntax,
to explain (to generalize) the instance languages above.

```
H : Predicate
X : Sentence
---------------
H(X) : Sentence
```

This can explain H [X] and H * X by viewing them as special H(X).

Note that, we should not view J N [X] as J(N(X)), because that will be J [N [X]].
To explain J N [X], we must introduce the composition syntax (denoted by concatenation).

```
H : Predicate
K : Predicate
-------------
H K : Predicate
```

To explain J N [J N], we must also be able to view predicate J N as sentence.
To explain J R * J R *, we must also be able to view the quotation mark "*" as sentence.

Note that, even after introducing composition syntax,
the above language still can not explain quotation alone, like [X].

This problem can be solved by introducing syntax for quotation,
and view application as quotation plus composition.

This will lead to a concatenative language like jojo.

# 2 Some classical fixed point arguments compared

## Part I Five fixed point arguments

## Part II A unification

## Part III Quasi-diagonalization

# 12 Sequential systems

## Part I Definitions and purpose

We can explain "sequential systems" by a concatenative system real beautifully.
And this will help us understand why concatenative system is better than applicative system.
