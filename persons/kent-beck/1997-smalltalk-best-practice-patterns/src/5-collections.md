---
title: 5. Collections
---

# CLASSES

> Our discussion of collections is divided into three sections:
>
> - Classes -- When you want to use a collection, the first thing you
>   have to decide is “which one.” This section describes what
>   problem or problems each of the major collection classes solve.
>
> - Protocol -- Programming habits carried over from other languages
>   can reduce the effectiveness of code when you use collections.
>   This section describes the major messages you can send to
>   collections and what problem each solves.
>
> - Idioms -- Because collections are so powerful, there are a small
>   set of standard tricks that experienced Smalltalkers know to play
>   with them.  If you are reading code that uses one of these idioms,
>   you may be puzzled at first. This section introduces the problems
>   you can solve using collections in unusual ways.

## Collection

<question>
  How do you represent a one-to-many relationship?

  <answer>
    Collection

    Use a Collection.
  </answer>
</question>

## OrderedCollection

<question>
  How do you code Collections whose size can’t be
  determined when they are created?

  <answer>
    OrderedCollection

    Use an OrderedCollection as your default dynamically sized Collection.
  </answer>
</question>

```smalltalk
Class: Document
  superclass: Object
  instance variables: paragraphs

Document>>addParagraph: aParagraph
  self paragraphs add: aParagraph

Document>>typesetOn: aPrinter
  self paragraphs do: [:each | each typesetOn: aPrinter]
```

`ordered-collection` 在 lisp 的语境下就是 `list`。

```scheme
(define-class document ()
  paragraphs (list paragraph))

(define (add-paragraph (self document) (a-paragraph paragraph))
  (cons (self :paragraphs) a-paragraph))

(define (typeset-on (self document) (a-printer printer))
  (foreach (self :paragraphs)
    (lambda (a-paragraph)
      (typeset-on a-paragraph a-printer))))
```

## RunArray

这是我第一次见这种数据结构。
这应该是受压缩算法启发的一个 collection class。

- RunArray 可以被实现为和 OrderedCollection 有完全相同接口的 class。

<question>
  How do you compactly code an OrderedCollection or Array
  where you have the same element many times in a row?

  <answer>
    RunArray

    Use a RunArray to compress long runs of the same element.
  </answer>
</question>

## Set

<question>
  TODO

  <answer>
    Set

    TODO
  </answer>
</question>

## Equality Method

<question>
  TODO

  <answer>
    Equality Method

    TODO
  </answer>
</question>

## Hashing Method

<question>
  TODO

  <answer>
    Hashing Method

    TODO
  </answer>
</question>

## Dictionary

<question>
  TODO

  <answer>
    Dictionary

    TODO
  </answer>
</question>

## SortedCollection

<question>
  TODO

  <answer>
    SortedCollection

    TODO
  </answer>
</question>

## Array

<question>
  TODO

  <answer>
    Array

    TODO
  </answer>
</question>

## ByteArray

<question>
  TODO

  <answer>
    ByteArray

    TODO
  </answer>
</question>

## Interval

<question>
  TODO

  <answer>
    Interval

    TODO
  </answer>
</question>

# COLLECTION PROTOCOL

## IsEmpty

<question>
  TODO

  <answer>
    IsEmpty

    TODO
  </answer>
</question>

## Includes:

<question>
  TODO

  <answer>
    Includes:

    TODO
  </answer>
</question>

## Concatentation

<question>
  TODO

  <answer>
    Concatentation

    TODO
  </answer>
</question>

## Enumeration

<question>
  TODO

  <answer>
    Enumeration

    TODO
  </answer>
</question>

## Do

<question>
  TODO

  <answer>
    Do

    TODO
  </answer>
</question>

## Collect

<question>
  TODO

  <answer>
    Collect

    TODO
  </answer>
</question>

## Select/Reject

<question>
  TODO

  <answer>
    Select/Reject

    TODO
  </answer>
</question>

## Detect

<question>
  TODO

  <answer>
    Detect

    TODO
  </answer>
</question>

## Inject:into:

<question>
  TODO

  <answer>
    Inject:into:

    TODO
  </answer>
</question>

# COLLECTION IDIOMS

## Duplicate Removing Set

<question>
  TODO

  <answer>
    Duplicate Removing Set

    TODO
  </answer>
</question>

## Temporarily Sorted Collection

<question>
  TODO

  <answer>
    Temporarily Sorted Collection

    TODO
  </answer>
</question>

## Stack

<question>
  TODO

  <answer>
    Stack

    TODO
  </answer>
</question>

## Queue

<question>
  TODO

  <answer>
    Queue

    TODO
  </answer>
</question>

## Searching Literal

<question>
  TODO

  <answer>
    Searching Literal

    TODO
  </answer>
</question>

## Lookup Cache

<question>
  TODO

  <answer>
    Lookup Cache

    TODO
  </answer>
</question>

## Parsing Stream

<question>
  TODO

  <answer>
    Parsing Stream

    TODO
  </answer>
</question>

## Concatenating Stream

<question>
  TODO

  <answer>
    Concatenating Stream

    TODO
  </answer>
</question>
