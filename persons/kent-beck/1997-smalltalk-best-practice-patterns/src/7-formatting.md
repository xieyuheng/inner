---
title: 7. Formatting
---

> No other topic generates more heat and less light than code
> formatting. Everybody has their own style and attempts to impose
> another style are met with ferocious resistance. So, why am I
> willingly sticking my head into this buzz saw?

> The first reason is "because it is there." I want to push patterns
> to their limit, to see how well they apply to a detail-oriented,
> exception-ridden, and emotion-filled topic. I wrote these patterns
> over the course of a couple of months. As new special cases came up,
> I either had to modify the patterns, add a new pattern, or format my
> code according to the existing patterns. Before long, I no longer
> was finding cases where I had to change the patterns. I am quite
> pleased that all of formatting in Smalltalk fits into ten patterns.

> The second reason is "because it is important." Not necessarily
> formatting code according to these patterns but formatting them
> according to some set of consistent rules gives a team a smoothness
> to their interaction. If everybody formats the same way, then
> reviews and code transfer are never delayed while someone "cleans
> up" the code. Done right, formatting can convey a lot of information
> about the structure of code at a glance.

> The third reason is to advance the discussion of formatting.
> Stating the rules as patterns makes my goals and tradeoffs explicit.
> If you have a different style, you can use these patterns as an
> example to cast your own rules as patterns. Then you can compare,
> explicitly, what problems each set of patterns solves and what
> problems each ignores.

> The priorities of these patterns are:
>
> 1. To make the gross structure of the method apparent at a glance.
>    Complex messages and blocks, in particular, should jump out at
>    the reader.
>
> 2. To preserve vertical space. There is a huge difference between
>    reading a method that fits into the text pane of a browser and
>    reading one that forces you to scroll. Keeping methods compact
>    vertically lets you have smaller browsers and still be able to
>    read methods without scrolling. This reduces window management
>    overhead and leaves more screen space for other programming
>    tools.
>
> 3. To be easy to remember. I have seen style guides that have 50–100
>    rules for formatting. Formatting is important but it shouldn’t
>    take that much brain power.

这里提到的 "To preserve vertical space"
和 smalltalk 中编辑器的设计有关，
我觉得 scroll 很方便，更希望能节省横向的空间，
虽好代码很瘦，可以在手机上看。

# Inline Message Pattern

<question>
  How do you format the message pattern?

  <answer>
    Inline Message Pattern

    Write the message pattern without explicit line breaks.
  </answer>
</question>

```smalltalk
from: fromInteger to: toInteger with: aCollection startingAt: startInteger

from: fromInteger
to: toInteger
with: aCollection
startingAt: startInteger
```

```scheme
(define (... (:from from integer) (:to to integer) (:with a-collection collection) (:starting-at start integer))
  ...)

(define (...
         (:from from integer)
         (:to to integer)
         (:with a-collection collection)
         (:starting-at start integer))
  ...)
```

# Type Suggesting Parameter Name

> There are two important pieces of information associated with every
> variable -- what messages it receives (its type) and what role it
> plays in the computation. Understanding the type and role of
> variables is important for understanding a piece of code.

第三次重复强调 role v.s. type 这个观点了。

> Keywords communicate their associated parameter’s role. Since the
> keywords and parameters are together at the head of every method,
> the reader can easily understand a parameter’s role without any
> help from the name.

<question>
  What do you call a method parameter?

  <answer>
    Type Suggesting Parameter Name

    Name parameters according to their most general expected class,
    preceded by "a" or "an". If there is more than one parameter
    with the same expected class, precede the class with a descriptive word.
  </answer>
</question>

```smalltalk
at: anInteger put: anObject
at: keyObject put: valueObject
```

```scheme
:at an-integer :put an-object
:at key-object :put value-object
```

# Indented Control Flow

> Arguments do not need to be aligned, unlike keywords, because
> readers seldom scan all the arguments. Arguments are only
> interesting in the context of their keyword.

反对 align arguments。
这在某些语言里 align arguments 甚至是 convention。

<question>
  How do you indent messages?

  <answer>
    Indented Control Flow

    Put zero or one argument messages on the same lines as their receiver.
    For messages with two or more keywords put each
    keyword/argument pair on its own line, indented two spaces.
  </answer>
</question>

```smalltalk
aCollection
  copyFrom: 1
  to: aString size
  with: aString
  startingAt: 1
```

```scheme
(copy a-collection
  :from 1
  :to (size a-string)
  :with a-string
  :starting-at 1)
```

从上面的例子可以看出，
把 `copy` 这个动词放到一个 keyword 里，
是不对称的。

# Rectangular Block

> ... the tendency of the eye to distinguish and interpolate vertical
> and horizontal lines. The square brackets used to signify blocks
> lead the eye to create the illusion of a whole rectangle even though
> one isn’t there.

这里的套路，直接支持了 lisp 中的「刚性 block」convention。

<question>
  How do you format blocks?

  <answer>
    Rectangular Block

    Make blocks rectangular. Use the square brackets as the upper left
    and bottom right corners of the rectangle. If the statement in the
    block is simple, the block can fit on one line. If the statement
    is compound, bring the block onto its own line and indent.
  </answer>
</question>

# Guard Clause

<question>
  How do you format code that shouldn’t execute if a condition holds?

  <answer>
    Guard Clause

    Format the one-branch conditional with an explicit return.
  </answer>
</question>

```smalltalk
connect
  self isConnected
    ifFalse: [self connectConnection]

connect
  self isConnected ifTrue: [^self].
  self connectConnection
```

```scheme
(define (connect (self connection))
  (if (not (is-connected self))
    (connect-connection self)))

(define (connect (self connection))
  (if (is-connected self) (return))
  (connect-connection self))
```

# Conditional Expression

<question>
  How do you format conditional expressions
  where both branches assign or return a value?

  <answer>
    Conditional Expression

    Format conditionals so their value is used
    where it clearly expresses the intent of the method.
  </answer>
</question>

```smalltalk
self isInitialized
  ifTrue: [cost := self calculateCost]
  ifFalse: [cost := 0]

cost := self isInitialized
  ifTrue: [self calculateCost]
  ifFalse: [0]
```

```smalltalk
cost
  self isInitialized
    ifTrue: [^self calculateCost]
    ifFalse: [^0]

cost
  ^self isInitialized
    ifTrue: [self calculateCost]
    ifFalse: [0]
```

```scheme
(define (cost (self ...) )
  (if (is-Initialized self)
    (calculate-cost self)
    0))
```

# Simple Enumeration Parameter

<question>
  What do you call the parameter to an enumeration block?

  <answer>
    Simple Enumeration Parameter

    Call the parameter "each".
    If you have nested enumeration blocks,
    append a descriptive word to all parameter names.
  </answer>
</question>

```smalltalk
self children do: [:each | self processChild: each]

1 to: self width do:
  [:eachX |
    1 to: self height do:
      [:eachY | ...]]
```

```scheme
(foreach self:children
  (lambda (each)
    (process-child self each)))

(foreach self:children
  (lambda (each-child)
    (process-child self each-child)))

(foreach (range 0 self:width)
  (lambda (each-x)
    (foreach (range 0 self:height)
      (lambda (each-y)
        ...))))
```

# Cascade

<question>
  How do you format multiple messages to the same receiver?

  <answer>
    Cascade

    Use a Cascade to send several messages to the same receiver.
    Separate the messages with a semicolon.
    Put each message on its own line and indent one tab.
    Only use Cascades for messages with zero or one argument.
  </answer>
</question>

```smalltalk
self listPane parent color: Color black.
self listPane parent height: 17.
self listPane parent width: 11.

self listPane parent
  color: Color black;
  height: 17;
  width: 11
```

lisp 的前缀表达式中，没有这个机制，
除非引入需要破坏前缀表达式的语法糖，
我认为这得不偿失。

而且这条 pattern 里面有
"Only use Cascades for messages with zero or one argument."
这种更限定条件，
所以也不是很好用。

但是，如果我们不把 `(:keyword object)` 理解为 `(object :keyword)`，
而是认为 `(:keyword object)` 是构造 message 的方式。
那么，外加一个 `send` 函数，cascades 也许就是可行的：

```scheme
(self:list-pane:parent :color color-black)
(self:list-pane:parent :height 17)
(self:list-pane:parent :width 11)

(send self:list-pane:parent
  (:color color-black)
  (:height 17)
  (:width 11))
```

但是其实也不行，
因为我们常用的是 generic function，
而不是 method。

# Yourself

与其有这么多使用 Cascades 时遇到的复杂情况，
与相应的特殊规则，
不如不用 Cascades。

<question>
  How can you use the value of a Cascade
  if the last message doesn’t return the receiver of the message?

  <answer>
    Yourself

    When you need the value of a Cascade
    and the last message does not return the receiver,
    append the message "yourself" to the Cascade.
  </answer>
</question>

```smalltalk
all := OrderedCollection new
  add: 5;
  add: 7;
  yourself
```

# Interesting Return Value

<question>
  TODO

  <answer>
    Interesting Return Value

    TODO
  </answer>
</question>
