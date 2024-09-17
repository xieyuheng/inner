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
  Composed Method

  How do you divide a program into methods?

  <answer>
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
  Constructor Method

  How do you represent instance creation?

  <answer>
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
  Constructor Parameter Method

  How do you set instance variables
  from the parameters to a Constructor Method?

  <answer>
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

(define-handler (:set-x! (a-point point) (x number))
  (set! a-point :x x))

(:set-x! a-point x)
(a-point :set-x! x)
```

## Shortcut Constructor Method

<question>
  Shortcut Constructor Method

  What is the external interface for creating a new object
  when a Constructor Method is too wordy?

  <answer>
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
  Conversion

  How do you convert information
  from one object’s format to another’s?

  <answer>
    Convert from one object to another
    rather than overwhelm any one object’s protocol.
  </answer>
</question>

## Converter Method

<question>
  Converter Method

  How do you represent simple conversion of an object to another
  object with the same protocol but different format?

  <answer>
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
  Converter Constructor Method

  How do you represent the conversion of an object to another with
  different protocol?

  <answer>
    Make a Constructor Method that takes the object to be converted as
    an argument.
  </answer>
</question>

## Query Method

<question>
  Query Method

  How do you represent testing a property of an object?

  <answer>
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
(define-handler (:make-on (a-switch switch))
  (set! a-switch :status 'on))

(define-handler (:make-off (a-switch switch))
  (set! a-switch :status 'off))

(define-handler (:update (a-wall-plate wall-plate))
  (match (a-wall-plate :switch :status)
    ('on (a-wall-plate :light :make-on))
    ('off (a-wall-plate :light :make-off))))

(define-handler (:on (a-switch switch))
  "Return true if the receiver is on, otherwise return false.")

nil?
control-wanted?
empty?
```

## Comparing Method

<question>
  Comparing Method

  How do you order objects with respect to each other?

  <answer>
    Implement "<=" to return true if the receiver
    should be ordered before the argument.
  </answer>
</question>

```smalltalk
Event>><= anEvent
  ^self timestamp <= anEvent timestamp
```

```scheme
(define-handler (:<= (target event) (an-event event))
  (<= (target :timestamp) (an-event :timestamp)))

(target-event :<= an-event)
```

## Reversing Method

<question>
  Reversing Method

  How do you code a smooth flow of messages?

  <answer>
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
(define-handler (:print-on (a-point point) (a-stream stream))
  (a-point :x :print-on a-stream)
  (a-stream :next-put-all " @ ")
  (a-point :y :print-on a-stream))

(define-handler (:print (a-stream stream) (an-object object))
  (an-object :print-on a-stream))
(define-handler (:print-on (a-point point) (a-stream stream))
  (a-stream :print (a-point :x))
  (a-stream :next-put-all " @ ")
  (a-stream :print (a-point :y)))
```

## Method Object

<question>
  Method Object

  How do you code a method where many lines of code share
  many arguments and temporary variables?

  <answer>
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
(define-handler
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

(define-handler (:compute (a-task-sender task-sender))
  ...150 lines of heavily commented code...)

(define-handler
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
  Execute Around Method

  How do you represent pairs of actions
  that have to be taken together?

  <answer>
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
(define-handler (:show-while (a-cursor cursor) (a-block block))
  (let ((old (current-cursor)))
    (a-cursor :show)
    (a-block :evaluate)
    (old :show)))

(define-handler (:open-during (a-file file) (a-block block))
  (a-file :open)
  (a-block :evaluate)
  (a-file :close))

(define-handler (:open-during (a-file file) (a-block block))
  (a-file :open)
  ((lambda () (a-block :evaluate))
   :ensure (lambda () (a-file :close))))
```

## Debug Printing Method

<question>
  Debug Printing Method

  How do you code the default printing method?

  <answer>
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
(define-handler (:print-on (an-association association) (a-stream stream))
  (a-stream :print (an-association :key))
  (a-stream :next-put-all "->")
  (a-stream :print (an-association :value)))
```

## Method Comment

<question>
  Method Comment

  How do you comment methods?

  <answer>
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

(define-handler (:visible? self)
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
  Message

  How do you invoke computation?

  <answer>
    Send a named message and let the receiving object decide
    what to do with it.
  </answer>
</question>

## Choosing Message

<question>
  Choosing Message

  How do you execute one of several alternatives?

  <answer>
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
## Intention Revealing Message
## Intention Revealing Selector
## Dispatched Interpretation
## Double Dispatch
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
