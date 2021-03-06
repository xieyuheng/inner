# 4. Easy as Pie

2020-10-31

## Functions as makers

A function of type (A) -> (D) -> X is called
"a maker for X based on an A and a D".

## Typing Pair_elim

In this chapter, we first understand `Pair_elim` by examples.
and try to type it.

We will find that, we can not type it by arrow type,
thus we introduce pi type.

## Carefully explain

When a wrong guess is given,
we carefully explain why it is wrong
by well understood principles.

## One concept at a time

This chapter only meant to introduce one concept -- pi type.

We should introduce concepts one at a time.

## Explaining pi type is different from arrow type

The difference between pi type and arrow type is in
the type of an expression in which a function is applied to arguments.

`flip(Nat)(String)`'s type is `(Pair(Nat)(String)) -> Pair(String)(Nat)`

This is because when an expression described by a pi-expression is applied,
the argument expressions replace the argument names in the body of the pi-expression.
