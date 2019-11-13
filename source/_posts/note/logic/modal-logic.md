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

| Logic     | Modality                |
|-----------|-------------------------|
| alethic   | necessary, possible     |
| deontic   | obligatory, permissible |
| temporal  | always, eventually      |
| epistemic | known                   |
| doxastic  | believed                |

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

The basic judgment in the logic is `<proposition> true in <world>`.

``` js
exists (w1: world_t) {
  accessible(w0, w1)
  P true in w1
}
--------------------
possible P true in w0
```

``` js
forall (w1: world_t) {
  accessible(w0, w1)
  P true in w1
}
--------------------
necessary P true in w0
```

## Truth trees of system K
