---
title: seven concurrency models in seven weeks
author: paul butcher
year: 2014
---

# intro

- shared-memory (clojure) vs distributed-memory (erlang)

# threads and locks

## 1: mutual exclusion and memory models

## 2: beyond intrinsic locks

## 3: on the shoulders of giants

# functional programming

## [note]

- 之前說過 一個常識是
  在電子計算機裏
  所有的計算都是由 改變計算機內存的狀態 來完成的

- 現在看來
  如果想要 concurrency 與 parallelism
  就要抽象掉個常識

## future and promise

- x -
  promise 類似於 sequent 中的不定元

# the clojure way

## atom and STM -- software transactional memory

# actors

## intro

- functional programming, avoids mutable state and share states
  actor programming, retains mutable state but avoids sharing states

## [note]

- x -
  傳遞信息的時候 symbol 這個數據結構如何處理?
  注意我們的 module system 是依賴這個數據結構的

## 1: messages and mailboxes

- a station handle messages sequentially.

- when receiving messages,
  processes act asynchronously.

- The video of
  Erik Meijer and Clemens Szyperski
  talking to Carl Hewitt
  about the actor model
  at Lang.NEXT 2012

## 2: error handling and resilience

- separating error handling out
  into a separate supervisor process.

- linking processes

# communicating sequential processes

## process algebra

- Primitives
  - Events
  - Primitive processes
- Algebraic operators
  - Prefix
  - Deterministic Choice
  - Nondeterministic Choice
  - Interleaving
  - Interface Parallel
  - Hiding

# data parallelism

# the lambda architecture
