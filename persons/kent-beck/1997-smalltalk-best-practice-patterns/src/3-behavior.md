# 3. BEHAVIOR

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

## METHODS

### Composed Method
### Constructor Method
### Constructor Parameter Method
### Shortcut Constructor Method
### Conversion
### Converter Method
### Converter Constructor Method
### Query Method
### Comparing Method
### Reversing Method
### Method Object
### Execute Around Method
### Debug Printing Method
### Method Comment

## MESSAGES

### Message
### Choosing Message
### Decomposing Message
### Intention Revealing Message
### Intention Revealing Selector
### Dispatched Interpretation
### Double Dispatch
### Mediating Protocol
### Super
### Extending Super
### Modifying Super
### Delegation
### Simple Delegation
### Self Delegation
### Pluggable Behavior
### Pluggable Selector
### Pluggable Block
### Collecting Parameter
