---
title: microkanren
subtitle: In a Forth-like Lisp that I implemented long ago
---

# TODO

recover minikanren

design some macros

note about different implementations

play with the reasoned schemer

play with prolog examples

# prolog

## dummy :

```lisp
(+fun :)
```

## dummy (+alias)

```lisp
(+macro +alias note)
```

## (->) to (let)

```lisp
(+macro -> (let body)
  body {'-- eq-p} list-ante
  {', eq-p not} list-filter
  sexp-filter-colon (let new-body)
  `(let (@ new-body list-spread)))
```

## sexp-filter-colon

```lisp
(+fun sexp-filter-colon (let ante)
  (case ante
    (null-t null-c)
    (cons-t
      (case ante.cdr
        (null-t null-c)
        (cons-t
          (if [ante.cdr.car ': eq-p]
            [ante.car ante.cdr.cdr.cdr recur cons-c]
            [ante.cdr recur]))))))
```

## (+type) to (+data)

```lisp
(+macro +type (let body)
   body.car (let name)
   body.cdr (let rest)
  `(+data (@ name) (@ rest sexp-filter-colon list-spread)))
```

# term

## var-t

```lisp
(+type var-t
  id : number-t)
```

## term-u

```lisp
(+alias term-u
  (| string-t
     var-t
     term-u list-u))
```

# unify

## substitution-t

```lisp
(+alias substitution-t [var-t term-u dict-t])
```

## empty-substitution

```lisp
(+fun empty-substitution
  : (-> -- substitution-t)
  new-dict)
```

## s-ext

```lisp
(+fun s-ext
  : (-> substitution-t
        var-t
        term-u
     -- substitution-t)
  dict-insert)
```

## walk

```lisp
(+fun walk
  : (-> term : term-u
        substitution : substitution-t
     -- term-u)
  (case term
    (var-t
      (if [substitution term dict-find]
        [substitution recur]
        [term]))
    (else term)))
```

## unify

```lisp
(+fun unify
  : (-> s : substitution-t
        u : term-u
        v : term-u
     -- (| substitution-t
           false-t))
  u s walk (let u)
  v s walk (let v)
  (cond
    (and [u var-p] [v var-p] [u v eq-p]) [s]
    [u var-p] [s u v s-ext]
    [v var-p] [s v u s-ext]
    (and [u cons-p] [v cons-p])
    [s u.car v.car recur
     dup false-p (bool-when-not u.cdr v.cdr recur)]
    else (if [u v eq-p]
           s
           false-c)))
```

# state

## state-t

```lisp
(+type state-t
  substitution : substitution-t
  id-counter : number-t)
```

## empty-state

```lisp
(+fun empty-state
  : (-> -- state-t)
  empty-substitution
  0
  state-c)
```

# stream

## stream-u

```lisp
(+alias stream-u list-u)
```

## unit

```lisp
(+fun unit
  : (-> state-t -- state-t stream-u)
  null-c cons-c)
```

## mzero

```lisp
(+fun mzero
  : (-> -- state-t stream-u)
  null-c)
```

# goal

## goal-t

- a microkanren program proceeds through
  the application of a goal to a state.

- the result of a microkanren program
  is a stream of satisfying states,
  finite or infinite many.

```lisp
(+alias goal-t (-> state-t -- state-t stream-u))
```

## ==

- == creates primitive goal-t
  which can only return mzero or unit state-t stream-u

- to build longer state-t stream-u
  disj and conj must be used

```lisp
(+fun ==
  : (-> u : term-u
        v : term-u
     -- goal-t)
  {(let state)
   state.substitution u v unify (let substitution)
   (if [substitution false-p]
     mzero
     [substitution
      (. substitution)
      state clone
      unit])})
```

## call/fresh

- the aim of this is to create var

```lisp
(+fun call/fresh
  : (-> fun : (-> var-t -- goal-t) -- goal-t)
  {(let state)
   state.id-counter (let id)
   id inc (. id-counter) state clone
   id var-c fun
   apply})
```

## disj

- a goal constructed from disj returns a non-empty stream
  if either of its two arguments are successful,

- a goal constructed from conj returns a non-empty stream
  if the second argument can be achieved
  in the stream generated by the first.

```lisp
(+fun disj
  : (-> goal1 : goal-t
        goal2 : goal-t
     -- goal-t)
  {(let state)  state goal1  state goal2  mplus})
```

## conj

```lisp
(+fun conj
  : (-> goal1 : goal-t
        goal2 : goal-t
     -- goal-t)
  {goal1 {goal2} bind})
```

## mplus -- merging streams

```lisp
;; just like append of list

;; append is an implementation for finite relations
;;   if the invocation of either of the two goals
;;   on the state results in an infinite stream,
;;   the invocation of disj will not complete.

(+fun mplus
  : (-> stream1 : [state-t stream-u]
        stream2 : [state-t stream-u]
     -- state-t stream-u)
  (cond [stream1 null-p] stream2
        else [stream1.car
              stream1.cdr stream2 recur
              cons-c]))
```

## bind

```lisp
;; just like append-map of list
;;   though with its arguments reversed.

(+fun bind
  : (-> stream : [state-t stream-u]
        goal : goal-t
     -- state-t stream-u)
  (cond [stream null-p] mzero
        else [stream.car goal
              stream.cdr {goal} recur
              mplus]))
```

# test

## unify

```lisp
(begin
  empty-substitution
  '(a b c)
  '(a b c)
  unify
  empty-substitution
  eq-p bool-assert)

(begin
  empty-substitution
  '((a b c) (a b c) (a b c))
  '((a b c) (a b c) (a b c))
  unify
  empty-substitution
  eq-p bool-assert)

(begin
  empty-substitution
  (lit-list 'a 'b 0 var-c)
  (lit-list 'a 'b 'c)
  unify
  empty-substitution 0 var-c 'c s-ext
  eq-p bool-assert)

(begin
  empty-substitution
  `((a b c) (a b c) (a b (@ 0 var-c)))
  `((a b c) (a b c) (a b c))
  unify
  empty-substitution 0 var-c 'c s-ext
  eq-p bool-assert)

(begin
  empty-substitution
  `(a b (@ 0 var-c))
  `(a b c)
  unify
  empty-substitution 0 var-c 'c s-ext
  eq-p bool-assert)
```

## goal

### call/fresh

```lisp
(assert
  empty-state
  {5 ==} call/fresh
  apply
  (lit-list
   (lit-dict 0 var-c 5) 1 state-c)
  eq-p)

(assert
  empty-state
  '(5 5 5) '(5 5 5) ==
  apply
  (lit-list
   (lit-dict) 0 state-c)
  eq-p)

(assert
  empty-state
  6 5 ==
  apply
  (lit-list)
  eq-p)
```

### a-and-b

```lisp
(+fun a-and-b
  {7 ==} call/fresh
  {(let b)  b 5 ==  b 6 ==  disj} call/fresh
  conj)

(assert
  empty-state
  a-and-b
  apply
  (lit-list
   (lit-dict 1 var-c 5, 0 var-c 7) 2 state-c
   (lit-dict 1 var-c 6, 0 var-c 7) 2 state-c)
  eq-p)
```