---
title: par note
repo: "https://github.com/faiface/par-lang"
---

The types are quite literally linear logic propositions:

```
(A) B == A times B
[A] B == not A par B

either { .left A, .right B }
==
A plus B

{ .left => A, .right => B }
==
A with B

! == one
? == bottom

either {} == zero
{} == top
```

# FAQs

## 1

**xyh**: What is the difference between pi-calculus and untyped par-lang?

**Michal Štrba**: The main difference being that
in pi calculus you can freely define channels and spawn processes (using `|`).

In Par, there is more structure to it.
You can’t just freely create channels and processes you:

- Can only create a channel and a process together, using `chan`
- Can only end a process while closing closing a channel, either via `!` or linking

This was also already enforced in the untyped version.

If you notice, this means the number of channels and processes is always the same.

## 2

**xyh**: when reading the paper, i see:

> unlike pi-calculus, both output and input names are bound.
> ... x[y].P in our calculus behave like νy.x<y>.P in pi-calculus.

is it right that par-lang is using pi-calculus's `x<y>.P` instead of CP's `x[y].P`?

**Michal Štrba**: Par has the same receive operation as CP,
except in CP it’s `x(y).P` and in Par it’s `x[y] P`.

What’s different is sending. In CP, you send like this:
```
x[y] (P | Q)
```
where `P` handles `y` and `Q` handles `x`.

In Par, we instead send an expression:
```
x(E) Q
```
And to get the same meaning as in CP’s `x[y] (P | Q)`, we write
```
x(chan y { P }) Q
```
Using the `chan` expression to spawn the `P` process.

Note that variable scoping is clearer here in Par than it is in CP.
But the meaning is the same here.

## 3

**xyh:** why create a new channel `u` and send `u` to `x` and call `put-name(u)`,
instead of just send "tea" to `x`?

**Michal Štrba**: That’s because CP, as described in the paper
doesn’t have expressions at all!
So there’s no piece of code that says “this is a tea”.
All you have is sequences of commands on channels.

You can’t even directly send a channel.
Say if I want to send a channel `a` over channel `x`,
in CP, I have to write:
```
x[y] (y <-> a | …)
```
I can do the same in Par actually!
```
x(chan y { y <> a }) …
```
But I don’t have to since I can just write
```
x(a) …
```

# 4

**xyh**: Any story about how you find the design of expression syntax (and compiling it to process syntax)?

**Michal Štrba**: Just a boring story, but purely by exploration. I got a strong sense that Wadler’s CP was the right way to go when it comes to a language for linear logic. The sense was due to it handling the hard cases of linear logic elegantly.

But I wasn’t happy with CP’s overuse of auxiliary variables and a lack of expressions overall.

So I went through a several iterations of redesigning it, eventually figuring out the `chan c { … }` syntax. I was happy with that.

I implemented it and was playing with it, and noticed some patterns of usage. For example, if I had a function definition, I could always call it like this:
```
// inside a process
let f = function
f(argument)
let result = f
```
I also noticed a similar pattern for other destructive operations.

Then I figured out I could actually separate it into a chan expression:
```
let result = chan return {
  let f = function
  f(argument)
  return <> f
}
```
For constructions, I noticed I can construct pairs like this:
```
let pair = chan return {
  return(first)
  return <> second
}
```
And then it all just clicked.

I noticed a general pattern for constructions:
```
let x = <operation> rest
// can mean
let x = chan result {
  result <operation>
  result <> rest
}
```
and for applications
```
let x = value <operation>
// could be
let x = chan result {
  let object = value
  object <operation>
  result <> object
}
```
And it just all came together perfectly after that.
