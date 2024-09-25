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
  (cons self:paragraphs a-paragraph))

(define (typeset-on (self document) (a-printer printer))
  (foreach self:paragraphs
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
  How do you code a Collection whose elements are unique?

  <answer>
    Set

    Use a Set.
  </answer>
</question>

```smalltalk
owners
  | results |
  results := OrderedCollection new.
  self accounts do:
    [:each || owner |
    owner := each owner.
    (results includes: owner) ifFalse: [results add: owner]].
  ^results

owners
  | results |
  results := Set new.
  self accounts do: [:each | results add: each owner].
  ^results
```

```scheme
(define (owners (self app))
  (let ((results (create list)))
    (foreach self:accounts
      (lambda (an-account)
        (let ((an-owner an-account:owner))
          (if (not (includes? results an-owner))
            (list-push! results an-owner))))
    results)))

(define (owners (self app))
  (let ((results (create set)))
    (foreach self:accounts
       (lambda (an-account) (set-add! results an-owner)))
  results))
```

## Equality Method

如果能限制 value 都为 record + array，
就可以用结构化的 equal? 来统一处理所有 class 的等价问题。

- 但是也许不能这么做，因为 object 中可能带有函数；
  但是也许可以，因为函数也有 normalization 意义上的等价。

<question>
  How do you code equality for new objects?

  <answer>
    Equality Method

    If you will be putting objects in a Set, using them as Dictionary keys,
    define a method called "=". Protect the implementation of "=" so
    only objects of compatible classes will be fully tested for equality.
    or otherwise using them with other objects that define equality,
  </answer>
</question>

```smalltalk
Point>>= aPoint
  (aPoint isMemberOf: self class) ifFalse: [^false].
  ^x = aPoint x & (y = aPoint y)


Book>>= aBook
  (aBook isMemberOf: self class) ifFalse: [^false].
  ^self author = aBook author & (self title = aBook title)

Library>>hasAuthor: authorString title: titleString
  | book |
  book := Book
    author: authorString
    title: titleString.
  ^self books includes: book
```

如果对 A 和 B 定义了 equal?，
看来对 A B 的 union 也定义 equal?，
如何表达这种结构化的扩张呢？

- 在 generic function 的机制下，
  是可以用增加 handler 的方式来实现的。

```scheme
(define (equal? (x any) (y any)) (eq? x y))

(define (equal? (p point) (q point))
  (and (equal? p:x q:x)
       (equal? p:y q:y)))


(define (equal? (x book) (y book))
  (and (equal? x:title y:title)
       (equal? x:author x:author)))

(define (has-author? (self library) (author string) (title string))
  (let ((a-book (create book :author author :title title)))
    (includes? self:books a-book)))
```

## Hashing Method

这里提到想要放入 set 的元素，需要实现 hash。

- 其实也可以有结构化的自动实现。
- 就算没有 hash 也可以实现 set，只不过效率更低。

<question>
  How do you ensure that new objects
  work correctly with hashed Collections?

  <answer>
    Hashing Method

    If you override "=" and use the object with a hashed Collection,
    override "hash" so that two objects that are equal
    return the same hash value.
  </answer>
</question>

其实下面的实现并不能保证 hash 的唯一性，
真的想要结构化的唯一性其实是很难的。

- 哥德尔配数法就是一个例子，
  但是所得到的数可能太大了而不实用。

```smalltalk
Book>>hash
  ^self title hash bitXor: self author hash
```

```scheme
(define (hash (self book))
  (bit-xor (hash self:title)
           (hash self:author)))
```

## Dictionary

<question>
  How do you map one kind of object to another?

  <answer>
    Dictionary

    Use a Dictionary.
  </answer>
</question>

```smalltalk
Widget>>defaultColors
  | results |
  results := Dictionary new.
  results
    at: ‘foreground’
    put: Color black.
  results
    at: ‘background’
    put: Color mauve.
  ^results
```

```scheme
(define (default-colors (self widget))
  {:foreground color-black
   :background color-mauve})
```

## SortedCollection

<question>
  How do you sort a collection?

  <answer>
    SortedCollection

    Use a SortedCollection.
    Set its sort block if you want to
    sort by some criteria other than "<=".
  </answer>
</question>

```smalltalk
childrenByAge
  ^self children asSortedCollection: [:a :b | a age < b age]
```

cicada-lisp 可以使用更简单的 API，
可能没必要生成一个新的 SortedCollection。

## Array

<question>
  How do you code a collection with a fixed number of elements?

  <answer>
    Array

    Use an Array.
    Create it with "new: anInteger" so that
    it has space for the number of elements you know it needs.
  </answer>
</question>

## ByteArray

<question>
  How do you code an Array of numbers
  in the range 0..255 or -128..127?

  <answer>
    ByteArray

    Use a ByteArray.
  </answer>
</question>

## Interval

在 cicada-lisp 中，这个类型可能会叫做 `range` 而不是 `interval`。

<question>
  How do you code a collection of numbers in sequence?

  <answer>
    Interval

    Use an Interval with start, stop,
    and an optional step value.
    The Shortcut Constructor Methods
    Number>>to: and to:by:
    build Intervals for you.
  </answer>
</question>

```smalltalk
(1 to: 20) do: [:each | ...]
```

```scheme
(foreach (range 0 20) (lambda (each) ...))
```

# COLLECTION PROTOCOL

> The uniform protocol of the collection classes are one of their
> greatest strengths. Client code is effectively decoupled from
> decisions about how to store a collection by using a common set of
> messages.

> Many Smalltalk programmers take a long time to learn all the
> collection protocol. They begin with a couple of messages, like do:,
> add:, and size, and then stick with them for a long time.

Collection library 在每个语言中都是这个体验，这个现象很有趣。

> When I started programming in Smalltalk, I can remember being
> overwhelmed by it. There is a lot to learn just to achieve basic
> competence.  However, the collection protocol is one of the highest
> leverage pieces of the sys- tem. The better you can use it, the
> faster you can code and the faster you can read other folks’ code.

> This section highlights the most important messages in the
> collection repertoire. It is by no means exhaustive. I selected
> these messages to pattern because they are the ones I see most often
> missed or misused.

## IsEmpty

<question>
  How do you test if a collection is empty?

  <answer>
    IsEmpty

    Send isEmpty to test whether a collection is empty (has no elements).
    Use notEmpty to test whether a collection has elements.
  </answer>
</question>

```smalltalk
aCollection size = 0

aCollection isEmpty

| answer |
answer := Dialog request: ‘Number, please?’.
answer isEmpty ifTrue: [^self].
...
```

```scheme
(equal? (size a-collection) 0)
(empty? a-collection)

(let ((an-answer (dialog-request "Number, please?")))
  (if (empty? an-answer) (return))
  ...)
```

## Includes:

<question>
  How do you search for a particular element in a collection?

  <answer>
    Includes:

    Send includes: and pass the object to be searched for.
  </answer>
</question>

```smalltalk
aCollection includes: anObject

employs: aPerson
  ^employees includes aPerson

collection1 select: [:each | collection2 includes: each]
```

```scheme
(includes? a-collection an-object)

(define (employs (a-person person))
  (includes? employees a-person))

(define (intersection (x collection) (y collection))
  (filter x (lambda (each) (includes? y each))))
```

## Concatentation

<question>
  How do you put two collections together?

  <answer>
    Concatentation

    Concatenate two collections by sending ","
    to the first with the second as an argument.
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
