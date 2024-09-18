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

- generic 的缺点也许是不能像 OOP 那样被理解为消息传递，
  但是也可以理解为群发消息给所有参数。

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

(define (set-x! (a-point point) (x number))
  (set! a-point :x x))

(set-x! a-point x)
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
(define (make-on (a-switch switch))
  (set! a-switch :status 'on))

(define (make-off (a-switch switch))
  (set! a-switch :status 'off))

(define (update (a-wall-plate wall-plate))
  (match (a-wall-plate :switch :status)
    ('on (make-on (a-wall-plate :light)))
    ('off (make-off (a-wall-plate :light)))))

(define (on (a-switch switch))
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
(define (<= (x event) (y event))
  (<= (x :timestamp) (y :timestamp)))
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
(define (print-on (a-point point) (a-stream stream))
  (print-on (a-point :x) a-stream)
  (next-put-all a-stream " @ ")
  (print-on (a-point :y) a-stream))


(define (print (a-stream stream) (an-object object))
  (print-on an-object a-stream))

(define (print-on (a-point point) (a-stream stream))
  (print a-stream (a-point :x))
  (next-put-all a-stream " @ ")
  (print a-stream (a-point :y)))
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
(define (send (an-obligation obligation)
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

(define (compute (a-task-sender task-sender))
  ...150 lines of heavily commented code...)

(define (send (an-obligation obligation)
              (:task a-task task)
              (:job a-job job))
  (compute
   (create task-sender
     :obligation an-obligation
     :task a-task
     :job a-job)))
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
(define (show-while (a-cursor cursor) (a-block block))
  (let ((old (current-cursor)))
    (show a-cursor)
    (evaluate a-block)
    (show old)))

(define (open-during (a-file file) (a-block block))
  (open a-file)
  (evaluate a-block)
  (close a-file))

(define (open-during (a-file file) (a-block block))
  (open a-file)
  (run-and-ensure (lambda () (evaluate a-block))
    (lambda () (close a-file))))
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
(define (print-on (an-association association) (a-stream stream))
  (print a-stream (an-association :key))
  (next-put-all a-stream "->")
  (print a-stream (an-association :evaluate)))
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
(if (equal? (bit-and (self :flags) 2r1000) 1)
  ;; Am I visible?
  ...)

(define (visible? self)
  (equal? (bit-and (self :flags) 2r1000) 1))

(if (visible? self)
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
  (process (a-bin :station) a-bin))
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
(define (responsible (an-entry entry))
  (if (is-kind-of an-entry film)
    (an-entry :producer)
    (an-entry :author)))


(define (responsible (a-film film))
  (a-film :producer))

(define (responsible (an-entry entry))
  (an-entry :author))
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
(define (control-activity (a-controller controller))
  (control-initialize a-controller)
  (control-loop a-controller)
  (control-terminate a-controller))
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
(define (highlight
         (self paragraph-editor)
         (a-rectangle rectangle))
  (reverse self a-rectangle))

(define (is-empty (self collection))
  (equal? (size self) 0))

(define (reciprocal (self number))
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
(define (linear-search-for (self array)))
(define (hashed-search-for (self set)))
(define (tree-search-for (self btree)))

(define (search-for (self collection)))

(define (includes (self collection)))
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
(define (ifte (self true)
              (true-block block)
              (false-block block))
  (evaluate true-block))

(define (ifte (self false)
              (true-block block)
              (false-block block))
  (evaluate false-block))

(define (at (self string) (an-integer integer))
  (ascii-value (basic-at self an-integer)))
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
(define (display (self postscript-shape-printer) (a-shape shape))
  (foreach (range 1 (size a-shape))
    (lambda (i)
      (let ((a-command (command-at a-shape i))
            (arguments (arguments-at a-shape i)))
        (if (equal? a-command 'line)
          (print-point self (ref arguments 1))
          (space self)
          (print-point self (ref arguments 2))
          (space self)
          (next-put-all self "line"))
        (if (equal? a-command 'curve) ...)
        ...))))


(define (print-line (self postscript-shape-printer) (from point) (to point))
  (print-point self from)
  (space self)
  (print-point self to)
  (space self)
  (next-put-all self "line"))

(define (display (self postscript-shape-printer) (a-shape shape))
  (foreach (range 1 (size a-shape))
    (lambda (i)
      (let ((a-command (command-at a-shape i))
            (arguments (arguments-at a-shape i)))
        (if (equal? a-command 'line)
          (print-line self (ref arguments 1) (ref arguments 2)))
        (if (equal? a-command 'curve) ...)
        ...))))


(define (send-command-at (self a-line) (:to an-object object))
  (let ((arguments (arguments-at self i)))
    (print-line an-object (ref arguments 1) (ref arguments 2))))

(define (display (self postscript-shape-printer) (a-shape shape))
  (foreach (range 1 (size a-shape))
    (lambda (i)
      (send-command-at a-shape i :to self))))


(define (send-commands-to (self shape) (an-object object))
  (foreach (range 1 (size self))
    (lambda (i)
      (send-command-at self i :to an-object))))

(define (display (self postscript-shape-printer) (a-shape shape))
  (send-commands-to a-shape self))
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
(define (add (x integer) (y number))
  (add-integer y x))
(define (add (x float) (y number))
  (add-float y x))

(define (add-integer (x integer) (y integer))
  <primitive: 1>)
(define (add-float (x float) (y float))
  <primitive: 2>)

(define (add-float (x integer) (y float))
  (add-float (as-float x) y))
(define (add-integer (x float) (y integer))
  (add-float x (as-float y)))
```

正常的 generic 实现方式：

```scheme
(define (add (x integer) (y integer)) <primitive: 1>)
(define (add (x float) (y float)) <primitive: 2>)
(define (add (x integer) (y float)) (add (as-float x) y))
(define (add (x float) (y integer)) (add x (as-float y))
```

## Mediating Protocol

<question>
  How do you code the interaction between two objects
  that need to remain independent?

  <answer>
    Mediating Protocol

    Refine the protocol between the objects
    so the words used are consistent.
  </answer>
</question>

对于 generic function 而言也适用。

在 generic function 的情形下，
protocol 体现为一组需要同时实现的 generic functions，
比如 monad 的 fmap 和 flatten。

- 在大量使用 generic function 之前，
  我把 monad 理解为一个带有类型参数的 class，
  如何调和这两种观点？

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
