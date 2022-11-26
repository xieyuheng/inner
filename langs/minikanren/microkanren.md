---
title: microkanren
subtitle: In a LISP I implemented long ago
---

# TODO

recover minikanren

design some macros

note about different implementations

play with the reasoned schemer

- x -
  語法爲函數複合而優化的語言
  可以減少命名

  然而 邏輯式編程中的 unnest 與 unification
  引入了大量的 functional 中不需要的名字

  函數複合 對這種 命名激增 有何影響呢?

play with prolog examples

# [note]

## meta programming vs strict ffi

- x -
  because of the use of (+data)
  maybe we can not embed another language in this languagecicada-script
  as well as in scheme

- k -
  why?

- x -
  because it need meta programming

- k -
  what's wrong about meta programming?

- x -
  because meta programming is hard to understand

- k -
  what should we do then?

- x -
  we use strict ffi

## stack-lize the use of closure

- a microkanren program proceeds through
  the application of a goal to a state.

  the result of a microkanren program
  is a stream of satisfying states,
  finite or infinite many.

  goal : (-> state -- state stream)

- x -
  thus a goal is a closure
  in which hide all the controls

- k -
  and you want to make them explicit
  by using stack?
  why?

- x -
  because the reason of using native closure
  is about embedding

  and we already decided to use strict ffi
  instead of meta programming

- k -
  but why native closure is not good for strict ffi?
  what's really wrong about native closure?
  what is the interface you want?
  how will things really be used?

------

- x -
  we stack-lize
  because we want to avoid native closure tricks

## the meaning of type class abstraction

- x -
  當嘗試解釋 disj 和 conj
  並且描述如何用 mplus 和 bind 來實現它們時
  就感覺 type class 所提供的抽象視角是很重要的
  爲什麼會有這種感覺呢?

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

## [note] stack-lization

- x -
  we must quit using closure to implement goal-t
  we can use goal-apply instead of apply

## ==

- == creates primitive goal-t
  which can only return mzero or unit state-t stream-u

- to build longer state-t stream-u
  disj and conj must be used

- x -
  with dependent type
  maybe we can express all this

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

- the aim of this is to create var [hypo]

- in our language
  hypo can even go without a name

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

## [note] disj & conj 與代數結構

- x -
  說這裏的 disj & conj 類似 bool 代數中的 or 和 and
  其實二者是代數結構
  [goal-t = (-> state-t -- state-t stream-u)]
  中的運算

- k -
  運算律是什麼?
  或者說此代數結構的公理是什麼?

- x -
  我們先來看 forgetful functor 作用於這個代數結構的效果
  如果 考慮 goal-t 所返回的 state-t stream-u 的長度
  那麼 disj 如 add 而 conj 如 mul
  再次遺忘 而考慮 state-t stream-u 的長度是否爲 0
  那麼 disj 如 or 而 conj 如 and

- k -
  但是注意 只給出 goal-t 的話
  並不能得到一個 state-t stream-u
  還需要給出 goal-t 的參數 state-t
  當參數不同時 state-t stream-u 的長度是不同的
  並且 state-t stream-u 的長度還可能是無窮的

- x -
  只有當給出了某個固定的參數的時候
  才能作出所說的遺忘

  並且 當考慮到 call/fresh 之類的算子的時候
  就知道所處理的空間是很豐富而複雜的
  也許在範疇論裏 這些算子都有所對應吧

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
