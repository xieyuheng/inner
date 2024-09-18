---
title: 3. Behavior
---

> Objects model the world through behavior and state. Behavior is the
> dynamic, active, computational part of the model. State is what is
> left after behavior is done, how the model is represented before,
> after, and during a computation.

> Of the two, behavior is the more important to get right. The primacy
> of behavior is one of the odd truths of objects; odd because it
> flies in the face of so much accumulated experience. Back in the bad
> old days, you wanted to get the representation right as quickly as
> possible because every change to the representation bred changes in
> many different computations.

> Objects (done right) change all that. No longer is your system a
> slave of its representation. Because objects can hide their
> representation behind a wall of messages, you are free to change
> representation and only affect one object.

其实在用 record + functions 的时候，
我就是在先确定 representation
-- 通常是一个 record type，
然后以这个为基础来写 functions。

也许，我的这种方式应该被归类为 data-oriented programming，
可能不该说与 OOP 相比谁对谁错，而是开发方式的演变。

> I saw a great comment at OOPSLA (the Object Oriented Programming
> Languages, Systems and Applications conference). It said, "This
> seems an awful fuss for a fancy procedure call." Well, separating
> computation into messages and methods and binding the message to the
> method at run time, based on the class of the receiver, may seem
> like a small change from an ordinary procedure call, but it is a
> small change that makes a big difference.

这点对于 lisp/scheme 的 generic + handler 也适用。

# METHODS

## Composed Method

<question>
  How do you divide a program into methods?

  <answer>
    Composed Method

    Divide your program into methods that perform one identifiable
    task. Keep all of the operations in a method at the same level of
    abstraction. This will naturally result in programs with many
    small methods, each a few lines long.
  </answer>
</question>

```smalltalk
Controller>>controlActivity
  self controlInitialize.
  self controlLoop.
  self controlTerminate
```

```scheme
(define (control-activity (a-controller controller))
  (control-initialize a-controller)
  (control-loop a-controller)
  (control-terminate a-controller))
```

## Constructor Method

<question>
  How do you represent instance creation?

  <answer>
    Constructor Method

    Provide methods that create well-formed instances.
    Pass all required parameters to them.
  </answer>
</question>

```smalltalk
Point new x: 0; y: 0

Point class>>r: radiusNumber theta: thetaNumber
  ^self
    x: radiusNumber * thetaNumber cos
    y: radiusNumber * thetaNumber sin
```

```scheme
(create point :x 0 :y 0)

(define (polar-point (:radius radius number) (:theta theta number))
  (create point
    :x (cos (* radius theta))
    :y (sin (* radius theta))))
```

## Constructor Parameter Method

<question>
  How do you set instance variables
  from the parameters to a Constructor Method?

  <answer>
    Constructor Parameter Method

    Code a single method that sets all the variables.
    Preface its name with "set," then the names of the variables.
  </answer>
</question>

```smalltalk
Point class>>x: xNumber y: yNumber
  ^self new
    x: xNumber;
    y: yNumber;
    yourself

Point class>>x: xNumber y: yNumber
  ^self new
    setX: xNumber
    y: yNumber

Point>>setX: xNumber y: yNumber
  x := xNumber.
  y := yNumber.
  ^self
```

```scheme
(define a-point (create point :x x :y y))

(define (point-set-x! (a-point point) (x number))
  (set! a-point :x x))

(point-set-x! a-point x)

;; Every keyword can be viewed as a generic function.

(define (:set-x! (a-point point) (x number))
  (set! a-point :x x))

(:set-x! a-point x)
(a-point :set-x! x)
```

## Shortcut Constructor Method

<question>
  What is the external interface for creating a new object
  when a Constructor Method is too wordy?

  <answer>
    Shortcut Constructor Method

    Represent object creation as a message to one of the arguments to
    the Constructor Method. Add no more than three of these Shortcut
    Constructor Methods per system you develop.
  </answer>
</question>

```smalltalk
Point class>>x: xNumber y: yNumber
  ^self new
    setX: xNumber
    y: yNumber

Number>>@ aNumber
  ^Point
    x: self
    y: aNumber
```

```scheme
(define (p (x number) (y number))
  (create point :x x :y y))

(create point :x x :y y)
(p x y)
```

## Conversion

<question>
  How do you convert information
  from one object’s format to another’s?

  <answer>
    Conversion

    Convert from one object to another
    rather than overwhelm any one object’s protocol.
  </answer>
</question>

## Converter Method

<question>
  How do you represent simple conversion of an object to another
  object with the same protocol but different format?

  <answer>
    Converter Method

    Provide a method in the object to be converted that converts to
    the new object. Name the method by prepending "as" to the class
    of the object returned.
  </answer>
</question>

```smalltalk
Collection>>asSet
Number>>asFloat
```

```scheme
(claim as-set (-> collection set))
(define (as-set (a-collection collection))
  ...)

(claim as-float (-> number float))
(define (as-float (n number))
  ...)
```

## Converter Constructor Method

<question>
  How do you represent the conversion of an object to another with
  different protocol?

  <answer>
    Converter Constructor Method

    Make a Constructor Method that takes the object to be converted as
    an argument.
  </answer>
</question>

## Query Method

<question>
  How do you represent testing a property of an object?

  <answer>
    Query Method

    Provide a method that returns a Boolean. Name it by prefacing
    the property name with a form of "be" -- is, was, will, etc.
  </answer>
</question>

```smalltalk
Switch>>makeOn
  status := #on
Switch>>makeOff
  status := #off
Switch>>status
  ^status

WallPlate>>update
  self switch status = #on ifTrue: [self light makeOn].
  self switch status = #off ifTrue: [self light makeOff]

Switch>>on
  "Return true if the receiver is on, otherwise return false."

isNil
isControlWanted
isEmpty
```

```scheme
(define (:make-on (a-switch switch))
  (set! a-switch :status 'on))

(define (:make-off (a-switch switch))
  (set! a-switch :status 'off))

(define (:update (a-wall-plate wall-plate))
  (match (a-wall-plate :switch :status)
    ('on (a-wall-plate :light :make-on))
    ('off (a-wall-plate :light :make-off))))

(define (:on (a-switch switch))
  "Return true if the receiver is on, otherwise return false.")

nil?
control-wanted?
empty?
```

## Comparing Method

<question>
  How do you order objects with respect to each other?

  <answer>
    Comparing Method

    Implement "<=" to return true if the receiver
    should be ordered before the argument.
  </answer>
</question>

```smalltalk
Event>><= anEvent
  ^self timestamp <= anEvent timestamp
```

```scheme
(define (:<= (target event) (an-event event))
  (<= (target :timestamp) (an-event :timestamp)))

(target-event :<= an-event)
```

## Reversing Method

<question>
  How do you code a smooth flow of messages?

  <answer>
    Reversing Method

    Code a method on the parameter. Derive its name from the original
    message. Take the original receiver as a parameter to the new
    method. Implement the method by sending the original message to
    the original receiver.
  </answer>
</question>

```smalltalk
Point>>printOn: aStream
  x printOn: aStream.
  aStream nextPutAll: ‘ @ ‘.
  y printOn: aStream

Stream>>print: anObject
  anObject printOn: self
Point>>printOn: aStream
  aStream
    print: x;
    nextPutAll: ‘ @ ‘;
    print: y
```

```scheme
(define (:print-on (a-point point) (a-stream stream))
  (a-point :x :print-on a-stream)
  (a-stream :next-put-all " @ ")
  (a-point :y :print-on a-stream))

(define (:print (a-stream stream) (an-object object))
  (an-object :print-on a-stream))
(define (:print-on (a-point point) (a-stream stream))
  (a-stream :print (a-point :x))
  (a-stream :next-put-all " @ ")
  (a-stream :print (a-point :y)))
```

## Method Object

<question>
  How do you code a method where many lines of code share
  many arguments and temporary variables?

  <answer>
    Method Object

    Create a class named after the method. Give it an instance
    variable for the receiver of the original method, each argument,
    and each temporary variable. Give it a Constructor Method that
    takes the original receiver and the method arguments. Give it one
    instance method, #compute, implemented by copying the body of the
    original method. Replace the method with one that creates an
    instance of the new class and sends it #compute.
  </answer>
</question>

```smalltalk
Obligation>>sendTask: aTask job: aJob
  | notProcessed processed copied executed |
  ...150 lines of heavily commented code...

Class: TaskSender
  superclass: Object
  instance variables: obligation task job notProcessed processed copied executed

TaskSender class>>obligation: anObligation task: aTask job: aJob
  ^self new
    setObligation: anObligation
    task: aTask
    job: aJob

TaskSender>>compute
  ...150 lines of heavily commented code...

Obligation>>sendTask: aTask job: aJob
  (TaskSender
    obligation: self
    task: aTask
    job: aJob) compute
```

```scheme
(define
    (:send (an-obligation obligation)
           (:task a-task task)
           (:job a-job job))
  (let ((not-processed ...)
        (processed ...)
        (copied ...)
        (executed ...))
    ...150 lines of heavily commented code...))

(define-class task-sender (object)
  :obligation
  :task
  :job
  :not-processed
  :processed
  :copied
  :executed)

(define (:compute (a-task-sender task-sender))
  ...150 lines of heavily commented code...)

(define
    (:send (an-obligation obligation)
           (:task a-task task)
           (:job a-job job))
  ((create task-sender
     :obligation: an-obligation
     :task: a-task
     :job: a-job)
   :compute))
```

## Execute Around Method

<question>
  How do you represent pairs of actions
  that have to be taken together?

  <answer>
    Execute Around Method

    Code a method that takes a Block as an argument. Name the method
    by appending "During: aBlock" to the name of the first method
    that needs to be invoked. In the body of the Execute Around
    Method, invoke the first method, evaluate the block, then invoke
    the second method.
  </answer>
</question>

```smalltalk
Cursor>>showWhile: aBlock
  | old |
  old := Cursor currentCursor.
  self show.
  aBlock value.
  old show

File>>openDuring:self open.
  aBlock value.
  self close
  aBlock

File>>openDuring: aBlock
  self open.
  [aBlock value] ensure: [self close]
```

```scheme
(define (:show-while (a-cursor cursor) (a-block block))
  (let ((old (current-cursor)))
    (a-cursor :show)
    (a-block :value)
    (old :show)))

(define (:open-during (a-file file) (a-block block))
  (a-file :open)
  (a-block :value)
  (a-file :close))

(define (:open-during (a-file file) (a-block block))
  (a-file :open)
  ((lambda () (a-block :value))
   :ensure (lambda () (a-file :close))))
```

## Debug Printing Method

<question>
  How do you code the default printing method?

  <answer>
    Debug Printing Method

    Override printOn: to provide information about
    an object’s structure to the programmer.
  </answer>
</question>

```smalltalk
Association>> printOn: aStream
  aStream
    print: self key;
    nextPutAll: ‘->’;
    print: self value
```

```scheme
(define (:print-on (an-association association) (a-stream stream))
  (a-stream :print (an-association :key))
  (a-stream :next-put-all "->")
  (a-stream :print (an-association :value)))
```

## Method Comment

<question>
  How do you comment methods?

  <answer>
    Method Comment

    Communicate important information that is not obvious from the
    code in a comment at the beginning of the method.
  </answer>
</question>

```smalltalk
(self flags bitAnd: 2r1000) = 1 "Am I visible?"
  ifTrue: [...]

isVisible
  ^(self flags bitAnd: 2r1000) = 1

self isVisible
  ifTrue: [...]
```

```scheme
(if (equal? (self :flags :bit-and 2r1000) 1)
  ;; Am I visible?
  ...)

(define (:visible? self)
  (equal? (self :flags :bit-and 2r1000) 1))

(if (self :visible?)
  ...)
```

```smalltalk
Bin>>run
  "Tell my station to process me."
  self station process: self
```

```scheme
(define (run (a-bin bin))
  "Tell my station to process me."
  ((a-bin :station) :process a-bin))

(define (run (a-bin bin))
  "Tell my station to process me."
  (a-bin :station :process a-bin))
```

# MESSAGES

> Messages are the heartbeat of a Smalltalk program. Without messages,
> there would be no program. Deftly managing this heartbeat is the
> first skill of the expert Smalltalk programmer. When you learn to
> see your program in terms of patterns of messages and you learn what
> can be done to that stream of messages to solve problems, then you
> will be able to solve any problem you can imagine in Smalltalk.

> Procedural languages explicitly make choices. When you code up a
> case statement, you say once and for all what all the possibilities
> are. In Smalltalk, you use messages to make choices for you. The
> extra added bonus is that the set of choices is not set in
> concrete. You can come along later and add new choices without
> affecting the existing choices just by defining a new class.

> This section talks about the tactical ways you can use the message
> stream. It gives you a toolbox of techniques for solving problems by
> manipulating the communication between objects.

## Message

<question>
  How do you invoke computation?

  <answer>
    Message

    Send a named message and let the receiving object decide
    what to do with it.
  </answer>
</question>

## Choosing Message

<question>
  How do you execute one of several alternatives?

  <answer>
    Choosing Message

    Send a message to one of several different kinds of objects,
    each of which executes one alternative.
  </answer>
</question>

```smalltalk
responsible := (anEntry isKindOf: Film)
  ifTrue: [anEntry producer]
  ifFalse: [anEntry author]

Film>>responsible
  ^self producer
Entry>>responsible
  ^self author

responsible := anEntry responsible
```

```scheme
(define responsible
  (if (an-entry :is-kind-of film)
    (an-entry :producer)
    (an-entry :author)))

(define (:responsible (a-film film))
  (a-film :producer))

(define (:responsible (an-entry entry))
  (an-entry :author))

(define responsible (an-entry :responsible))
```

## Decomposing Message

<question>
  How do you invoke parts of a computation?

  <answer>
    Decomposing Message

    Send several messages to "self."
  </answer>
</question>

```smalltalk
Controller>>controlActivity
  self
    controlInitialize;
    controlLoop;
    controlTerminate
```

```scheme
(define (:control-activity (a-controller controller))
  (a-controller :control-initialize)
  (a-controller :control-loop)
  (a-controller :control-terminate))
```

## Intention Revealing Message

<question>
  How do you communicate your intent
  when the implementation is simple?

  <answer>
    Intention Revealing Message

    Send a message to "self."
    Name the message so it communicates what is to be done
    rather than how it is to be done.
    Code a simple method for the message.
  </answer>
</question>

```smalltalk
ParagraphEditor>>highlight: aRectangle
  self reverse: aRectangle

Collection>>isEmpty
  ^self size = 0

Number>>reciprocal
  ^1 / self
```

```scheme
(define (:highlight
         (self paragraph-editor)
         (a-rectangle rectangle))
  (self :reverse a-rectangle))

(define (:is-empty (self collection))
  (equal? (self :size) 0))

(define (:reciprocal (self number))
  (div 1 self))
```

## Intention Revealing Selector

<question>
  What do you name a method?

  <answer>
    Intention Revealing Selector

    Name methods after what they accomplish.
  </answer>
</question>

```smalltalk
Array>>linearSearchFor:
Set>>hashedSearchFor:
BTree>>treeSearchFor:

Collection>>searchFor:

Collection>>includes:
```

```scheme
(define (:linear-search-for (self array)))
(define (:hashed-search-for (self set)))
(define (:tree-search-for (self btree)))

(define (:search-for (self collection)))

(define (:includes (self collection)))
```

## Dispatched Interpretation

<question>
  How can two objects cooperate
  when one wishes to conceal its representation?

  <answer>
    Dispatched Interpretation

    Have the client send a message to the encoded object.
    Pass a parameter to which the encoded object
    will send decoded messages.
  </answer>
</question>

可以理解为 method level 的 dependency injection。

- 将：从一个对象中取出所需要的数据，然后进行操作。

- 改成：把操作写成一个 lambda，参数是所需要的最简的数据，
  然后把这个 lambda 给对象，
  对象自己构造所需要的数据，
  然后调用这个 lambda。

```smalltalk
True>>ifTrue: trueBlock ifFalse: falseBlock
  ^trueBlock value
False>>ifTrue: trueBlock ifFalse: falseBlock
  ^falseBlock value

String>>at: anInteger
  ^Character asciiValue: (self basicAt: anInteger)
```

```scheme
(define (:if (self true)
             (true-block block)
             (false-block block))
  (true-block :value))

(define (:if (self false)
             (true-block block)
             (false-block block))
  (false-block :value))

(define (:at (self string) (an-integer integer))
  (character :ascii-value  (self :basic-at an-integer)))
```

```smalltalk
PostScriptShapePrinter>>display: aShape
  1 to: aShape size do:
    [:each || command arguments |
    command := aShape commandAt: each.
    arguments := aShape argumentsAt: each.
    command = #line ifTrue:
      [self
        printPoint: (arguments at: 1);
        space;
        printPoint: (arguments at: 2);
        space;
        nextPutAll: ‘line’].
    command = #curve...
    ...]


PostScriptShapePrinter>>lineFrom: fromPoint to: toPoint
  self
    printPoint: fromPoint;
    space;
    printPoint: toPoint;
    space;
    nextPutAll: ‘line’

PostScriptShapePrinter>>display: aShape
  1 to: aShape size do:
    [:each |
    aShape
      sendCommandAt: each
      to: self]


Shape>>sendCommandsTo: anObject
  1 to: self size do:
    [:each |
    self
      sendCommandAt: each
      to: anObject]

PostScriptShapePrinter>>display: aShape
  aShape sendCommandsTo: self
```

```scheme
(define (:display (self postscript-shape-printer) (a-shape shape))
  (foreach (range 1 (a-shape :size))
    (lambda (i)
      (let ((a-command (a-shape :command-at i))
            (arguments (a-shape :arguments-at i)))
        (if (equal? a-command 'line)
          (self :print-point (arguments :at 1))
          (self :space)
          (self :print-point (arguments :at 2))
          (self :space)
          (self :next-put-all "line"))
        (if (equal? a-command 'curve) ...)
        ...))))


(define (:line (self postscript-shape-printer) (from point) (to point))
  (self :print-point from)
  (self :space)
  (self :print-point to)
  (self :space)
  (self :next-put-all "line"))

(define (:display (self postscript-shape-printer) (a-shape shape))
  (foreach (range 1 (a-shape :size))
    (lambda (i)
      (let ((a-command (a-shape :command-at i))
            (arguments (a-shape :arguments-at i)))
        (if (equal? a-command 'line)
          (self :line (arguments :at 1) (arguments :at 2)))
        (if (equal? a-command 'curve) ...)
        ...))))


(define (:send-command-at (self a-line) (:to an-object object))
  (let ((arguments (self :arguments-at i)))
    (an-object :line (arguments :at 1) (arguments :at 2))))

(define (:display (self postscript-shape-printer) (a-shape shape))
  (foreach (range 1 (a-shape :size))
    (lambda (i)
      (a-shape :send-command-at i :to self))))


(define (:send-commands-to (self shape) (an-object object))
  (foreach (range 1 (self :size))
    (lambda (i)
      (self :send-command-at i :to an-object))))

(define (:display (self postscript-shape-printer) (a-shape shape))
  (a-shape :send-commands-to self))
```

## Double Dispatch

<question>
  How can you code a computation that has many cases,
  the cross product of two families of classes?

  <answer>
    Double Dispatch

    Send a message to the argument.
    Append the class name of the receiver to the selector.
    Pass the receiver as an argument.
  </answer>
</question>

如果语言有 generic 的功能，
解决这个问题就简单很多了。

double dispatch 看起来也是可行的，
但是由于要给中间 method 命名，
所以是不 scalable 的。

```smalltalk
Integer>>+ aNumber
  ^aNumber addInteger: self
Float>>+ aNumber
  ^aNumber addFloat: self

Integer>>addInteger: anInteger
  <primitive: 1>
Float>>addFloat: aFloat
  <primitive: 2>

Integer>>addFloat: aFloat
  ^self asFloat addFloat: aFloat
Float>>addInteger: anInteger
  ^self addFloat: anInteger asFloat
```

```scheme
(define (:add (x integer) (y number))
  (y :add-integer x))
(define (:add (x float) (y number))
  (y :add-float x))

(define (:add-integer (x integer) (y integer))
  <primitive: 1>)
(define (:add-float (x float) (y float))
  <primitive: 2>)

(define (:add-float (x integer) (y float))
  (x :as-float :add-float y))
(define (:add-integer (x float) (y integer))
  (x :add-float (y :as-float)))
```

正常的 generic 实现方式：

```scheme
(define (add (x integer) (y integer)) <primitive: 1>)
(define (add (x float) (y float)) <primitive: 2>)
(define (add (x integer) (y float)) (add (x :as-float) y))
(define (add (x float) (y integer)) (add x (y :as-float)))
```

## Mediating Protocol
## Super
## Extending Super
## Modifying Super
## Delegation
## Simple Delegation
## Self Delegation
## Pluggable Behavior
## Pluggable Selector
## Pluggable Block
## Collecting Parameter
