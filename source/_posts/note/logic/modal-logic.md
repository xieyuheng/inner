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
| temporal  | always                  |
| epistemic | known                   |
| doxastic  | believed                |

## Alethic modal logic

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
necessity -> analyticity

possibility -> necessity or contingency
contingency -> possibility

analyticity -> impossible or necessity
impossible -> analyticity
```
