---
title: Hoare Logic
---

# [note]

## mainly for imperative programming language

hoare logic is an extrinsic formal system
to proof correctness
of code written in imperative programming language.

## ignore curry–howard correspondence

hoare logic is designed without respect
to curry–howard correspondence.

# rules

## empty statement

```
-------------
{P} skip {P}
```

## assignment

```
-------------
{P[x=>E]} x := E {P}
```


## composition

```
{P} S {Q}
{Q} T {R}
-------------
{P} S;T {R}
```

## conditional

```
{(and B P)} S {Q}
{(and (not B) P)} T {Q}
------------------------
{P} (if B then S else T) {Q}
```

## consequence -- weakening

```
P1 -> P2
{P2} S {Q2}
Q2 -> Q1
--------------
{P1} S {Q1}
```

## while loop

```
{(and P B)} S {P}
------------------------
{P} (while B do S) {(and (not) B) P)}
```

# partial correctness

- {P} C {Q} means:

  1. Whenever P holds of the state before the execution of C,
     then Q will hold afterwards,

  2. or C does not terminate.

- termination needs to be proved separately.
  because In the latter case, there is no "after",
  so Q can be any statement at all.

  - Indeed, one can choose Q to be false
    to express that C does not terminate.

# separation logic

- about the naming:
  virtual separation (modular reasoning) between concurrent modules.

- applications:

  1. automated program verification
  2. automated parallelization

- an extension of hoare logic for ><><><
