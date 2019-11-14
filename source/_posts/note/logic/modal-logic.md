---
title: Modal Logic
---

# Modal Logic

## Links

- [Modal logic, by Kane B, (video lectures)](https://www.youtube.com/playlist?list=PLXKKIUdnOESGk43pUg3NTkRWjglvKXKi7)

## As extension of propositional logic

We add `# P` (`#` as box) and `$ P` (`$` as diamond).
We have `~ # P == $ ~ P` and `~ $ P == # ~ P` (`~` as `not`).

## Kinds of modal logics

Different interpretation of the modal operators `#` and `$`.

| Logic     | Modality                           |
|-----------|------------------------------------|
| alethic   | necessary, possible, impossible    |
| deontic   | obligatory, permissible, forbidden |
| temporal  | always, sometimes, never           |
| epistemic | known                              |
| doxastic  | believed                           |

## Introduction modal logic

Alethic modal logic is the main modal logic.

``` js
necessity      --  # P                 --  necessary P
               --  ~ $ ~ P             --  not possible not P
possibility    --  $ P                 --  possible P
               --  ~ # ~ P             --  not necessary not P
impossibility  --  ~ $ P               --  not possible P
               --  # ~ P               --  necessary not P
                                       --  impossible P
analyticity    --  (# P) | (# ~ P)     --  necessary P or necessary not P
               --  ~(($ P) & ($ ~ P))  --  not (possible P and possible not P)
                                       --  analytic P
contingency    --  ($ P) & ($ ~ P)     --  possible P and possible not P
               --  ~((# P) | (# ~ P))  --  not (necessary P or necessary not P)
                                       --  contingent P
```

We also have,

``` js
necessity -> possibility
// this does not hold because of propositional logic,
//   but because of intuition of meaning the modal operators.

necessity -> analyticity

possibility -> necessity or contingency
contingency -> possibility

analyticity -> impossibility or necessity
impossibility -> analyticity

not possibility == impossibility
not analyticity == contingency
```

But necessity does not contradict with impossibility,

``` js
not impossible P == not necessary not P == possible P
not impossible P == not necessary not P != necessary P
                        necessary not P != not necessary P
```

The semantics of alethic modal logic **possible worlds**,
`possible P` means "there exists a world in which P is true",
`necessary P` means "in all worlds, P is true".

- **[Xie]** When talking about "semantics", think about intuition and implementation.

Kinds of possibility,
- Logical, fixed given the laws of logic, rules out contradictions.
- Nomological, fixed given the laws of nature.
- Temporal, fixed given the actual history of the world.

## System K

The `K` from [Saul Kripke](https://en.wikipedia.org/wiki/Saul_Kripke).

Formation rules (for well-formed formula).

``` js
<exp> = <var> // propositional variable
  | not <exp>
  | <exp> and <exp>
  | <exp> or <exp>
  | # <exp> // necessary <exp>
  | $ <exp> // possible <exp>
```

Interpretation is defined as a function that assign truth value to each propositional variable.

We use model theory to provide semantics for logic.

``` js
model_t = {
  world : world_t
  accessible : (w0: world_t, w1: world_t) -> bool_t // w1 is accessible from w0
  assign : (world_t, var_t) -> bool_t
}
```

Different kinds of possibility corresponding to definitions of relation `accessible`.
Note that, the relation is not necessarily transitive.

## Truth trees of system K

We are going to use the tree method in "Modal Logic for Philosophers", by James W. Garson,
which captures the concepts of modal logic in a clear and common sense way.

- **[Xie]**
  I use judgment and inference rules instead.
  Also I use intuitionistic logic intead of classical logic,
  specially `A -> B` can not be reduced to `(not A) or B`.

The basic judgment in the logic is `<proposition> true in <world>`.

``` js
exists (w1: world_t) {
  accessible(w0, w1)
  P true in w1
}
------------------------ // possible intro
possible P true in w0
```

``` js
forall (w1: world_t) {
  accessible(w0, w1)
  P true in w1
}
------------------------- // necessary intro
necessary P true in w0
```

Example proof,

``` js
distribution : (w : world_t) -> (necessary (P -> Q)) -> (necessary P -> necessary Q) in w
distribution = {
  w : world_t
  |------------------
  return : (necessary (P -> Q)) -> (necessary P -> necessary Q) in w
  return = {
    g : necessary (P -> Q) in w
    g : (w1 : world_t, accessible(w, w1)) -> (P -> Q) in w
    |---------------
    return : necessary P -> necessary Q in w
    return = {
      x : necessary P in w
      x : (w1 : world_t, accessible(w, w1)) -> P in w
      |----------------
      y : (w1 : world_t, accessible(w, w1)) -> Q in w
      y = {
        w1 : world_t
        ac : accessible(w, w1)
        |-------------------------
        g(w1, ac) : (P -> Q) in w
        x(w1, ac) : P in w
        g(w1, ac)(x(w1, ac)) : Q in w
      }
      y : necessary Q
    }
  }
}
```

## A strange property of K

According to the lecturer, the following propositions are can not be proven.

``` js
necessary P -> P
necessary P -> possible P
```

Let's try our inference rules.

``` js
// necessary P -> P
h1 : (w: world_t) -> necessary P -> P in w
h1 = {
  w : world_t
  x : necessary P in w
  x : (w1 : world_t, accessible(w, w1)) -> P in w
  |----------------
  x(w, access_self(w)) : P in w
}

// necessary P -> possible P
h2 : (w: world_t) -> necessary P -> possible P in w
h2 = {
  w : world_t
  x : necessary P in w
  x : (w1 : world_t, accessible(w, w1)) -> P in w
  |----------------
  x(w, access_self(w)) : P in w
  return : exists(w: world_t, accessible(w, w), P) in w
  return = tuple(w, access_self(w), x(w, access_self(w)))
  return : possible P in w
}
```

It seems we can prove them, as lone as we have the following primitive function,
making accessibility relation reflexive.

``` js
access_self : (w: world_t) -> accessible(w, w)
```

## Truth trees for invalid arguments in K

## Logical consequence in K

In the History, modal logic is developed to avoid irrelevant `A -> B` like,
"If pigs can fly, then the moon is made of cheese."

Semantic / model theoretic consequence (Tarski) `premises |= conclusion` means that,
for all interpretation that makes premises true,
the interpretation also makes the conclusion true.

We can also say, `premises |= conclusion` is true,
when there is no counter example.

Syntactic / proof theoretic consequence (Gentzen) `premises |- conclusion` means that,
conclusion can be derived from premises by inference rules.

- **[Xie]**
  Terminology used by other authors,
  - tautology consequence -- propositional logic
  - logical consequence -- predicate logic
  - analytical consequences -- with a specific model

## Soundness & Completeness

A group of inference rules specifies a deduction system.

We can say a deduction system is sound (or complete)
respect to a specific model theory.

Soundness,

``` js
premises |- conclusion
----------------------
premises |= conclusion
```

Completeness,

``` js
premises |= conclusion
----------------------
premises |- conclusion
```

## The systems M, B, S4 & S5

System K is one memmber of a familiy of systems called "normal modal logic".

Accessibility relation can be enhanced,

``` js
M = K + reflexive {
  # P -> P
}

B = K + reflexive + symmetric {
  P -> # $ P
}

S4 = K + reflexive + transitive {
  # P -> # # P
}

S5 = K + reflexive + symmetric + transitive {
  $ P -> # $ P
}
```

## More on the accessibility relation

## Two important truth tree rules, and a space saving method

## A trick for trees in S5

## The modal scope fallacy

Example of scope fallacy is "All that glitter is not gold."
which should be "Not all that glitter is gold."

The modal scope fallacy,
"If P is true, then it cannot be false."
"If P cannot be false, then it is necessarily true."
Thus "If P is true, then it is necessarily true."

``` js
P -> ~ $ ~ P
~ $ ~ P -> # P
--------------
P -> # P
```

or

``` js
P -> # ~ ~ P
# ~ ~ P -> # P
--------------
P -> # P
```

The fallacy is that `P -> # ~ ~ P` is wrong,
only `# (P -> ~ ~ P)` is right.

"If P is true, then it cannot be false."
which is equivalent to
"If P is true, then it necessarily not be false."
should be
"Necessarily, if P is true, then it not be false."

- **[Xie]** More about fallacy,
  https://en.wikipedia.org/wiki/Modal_fallacy
  https://en.wikipedia.org/wiki/Formal_fallacy

## [Note] Relational semantics (a.k.a. frame semantics)

## [Note] Intuitionistic logic as a modal logic
