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
> Languages, Systems and Applications conference). It said, “This
> seems an awful fuss for a fancy procedure call.” Well, separating
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

## Constructor Method

<question>
  Constructor Method

  How do you represent instance creation?

  <answer>
    Provide methods that create well-formed instances.
    Pass all required parameters to them.
  </answer>
</question>

## Constructor Parameter Method

<question>
  Constructor Parameter Method

  How do you set instance variables
  from the parameters to a Constructor Method?

  <answer>
    Code a single method that sets all the variables.
    Preface its name with “set,” then the names of the variables.
  </answer>
</question>

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
    the new object. Name the method by prepending “as” to the class
    of the object returned.
  </answer>
</question>

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
    the property name with a form of “be” -- is, was, will, etc.
  </answer>
</question>

## Comparing Method

<question>
  Comparing Method

  How do you order objects with respect to each other?

  <answer>
    Implement “<=” to return true if the receiver
    should be ordered before the argument.
  </answer>
</question>

## Reversing Method

<question>
  Reversing Method


  <answer>

  </answer>
</question>

## Method Object

<question>
  Method Object


  <answer>

  </answer>
</question>

## Execute Around Method

<question>
  Execute Around Method


  <answer>

  </answer>
</question>

## Debug Printing Method

<question>
  Debug Printing Method


  <answer>

  </answer>
</question>

## Method Comment

<question>
  Method Comment


  <answer>

  </answer>
</question>

# MESSAGES

## Message
## Choosing Message
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
