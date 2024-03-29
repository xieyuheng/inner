---
title: Design Patterns
author: Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides
date: 1994
---

# info

## Categories of patterns

Ralph Johnson - 22 Years Of Design Patterns:
- video: https://www.youtube.com/watch?v=Lf0uHSBTKEw&ab_channel=FASTFoundation

Old categories:
- Structural
- Behavioral
- Creational

New categories:
- Core (important):
  - Composite
  - Strategy
  - Decorator
  - State
  - Iterator
  - Observer
  - Mediator
  - Facade
  - Proxy
  - Command
  - Template Method
  - Adapter
  - Null Object (new)

- Peripheral (less important)
- Compound
- Creational

Iterator: (stream, map, filter, reduce)

Observer: (listener and event, data binding)
- you'd better build debug features into patterns
  - for example, to help checking missing dependencies

Observer + Iterator = Reactive programming

Composite: Component :> Leaf, Composite
- Where to put children side-effect methods? in Component or Composite?
  - We should put them in Composite, because this way is type safe.

# 1 Introduction

## Experienced designers evidently know something inexperienced ones don't. What is it?

One thing expert designers know not to do is solve every problem from first principles.
Rather, they reuse solutions that have worked for them in the past.
When they find a good solution, they use it again and again.
Such experience is part of what makes them experts.

## What is a design pattern?

Christopher Alexander says, "Each pattern describes a problem which occurs over and
over again in our environment, and then describes the core of the solution to that
problem, in such a way that you can use this solution a million times over, without ever
doing it the same way twice" [AIS+77, page x]

- In this sense, the followings are all application domain-specific design pattern,
  where the application domain is programming language implementation.

  - closure (for lexical scope)
  - bidirectional type checking (reading inference rule as type checking algorithm)
  - normalization by evaluation (NbE) (partial evaluation)
  - elaboration during type checking (clear separation of roles)

  Are those design patterns clearly expressed in my OOD code?

## What are the four parts of a design pattern?

1. The pattern name
2. The problem
3. The solution
4. The consequences

## What is the "level of abstraction" of a design pattern?

Point of view affects one's interpretation of what is and isn't a pattern.
One person's pattern can be another person's primitive building block.

- low level:
  such as linked lists, hash tables,
  and Kent Beck's book "implementation patterns"

- high level:
  domain-specific designs for an entire application or subsystem.

- mid level:
  descriptions of communicating objects and classes that are
  customized to solve a general design problem in a particular context.

## What is a good example of design pattern?

The Model/View/Controller (MVC) triad of classes
is used to build user interfaces in Smalltalk-80.
Looking at the design patterns inside MVC
should help you see what we mean by the term "pattern."

MVC is a high level design pattern, which contains
Observer, Composite, and Strategy mid level design patterns.

## What template should we use to describe design patterns?

- Pattern Name and Classification

  The pattern's name conveys the essence of the pattern succinctly.
  A good name is vital, because it will become part of your design vocabulary.

- Intent (problem)

  A short statement that answers the following questions:
  What does the design pattern do?
  What is its rationale and intent?
  What particular design issue or problem does it address?

- Also Known As

  Other well-known names for the pattern, if any.

- Motivation (examples)

  A scenario that illustrates a design problem and how the class
  and object structures in the pattern solve the problem.

- Applicability (when to use)

  What are the situations in which the design pattern can be applied?
  What are examples of poor designs that the pattern can address?
  How can you recognize these situations?

- Structure (class diagram and sequence diagram)

  A graphical representation of the classes in the pattern
  using a notation based on the Object Modeling Technique (OMT) [RBP+91].
  We also use interaction diagrams [JCJO92, Boo94]
  to illustrate sequences of requests and collaborations between objects.

- Participants

  The classes and/or objects participating in the design pattern
  and their responsibilities.

- Collaborations

  How the participants collaborate to carry out their responsibilities.

- Consequences (trade-offs)

  How does the pattern support its objectives?
  What are the trade-offs and results of using the pattern?
  What aspect of system structure does it let you vary independently?

- Implementation

  What pitfalls, hints, or techniques should you be aware of
  when implementing the pattern? Are there language-specific issues?

- Sample Code

  Code fragments that illustrate how you might implement the pattern in C++ or Smalltalk.

- Known Uses (big examples)

  Examples of the pattern found in real systems.
  We include at least two examples from different domains.

- Related Patterns

  What design patterns are closely related to this one?
  What are the important differences? With which other patterns should this one be used?

## What are the ways by which we can classify design patterns?

- Scope:
  - Class:
    Deal with relationships between classes and their subclasses.
    These relationships are established through inheritance.
  - Object:
    Maybe use dependency injection?

- Purpose:
  - Creational:
    In the scope of class creational patterns defer some part of object creation to subclasses,
    in the scope of object creational patterns defer it to another object.
  - Structural:
    Deal with the composition of classes or objects.
  - Behavioral:
    Characterize the ways in which classes
    or objects interact and distribute responsibility.

## Why we want multiple ways of thinking about patterns?

Because having multiple ways of thinking about patterns
will deepen your insight into what they do, how they compare,
and when to apply them.

(Similar is true for a lot of other things.)

## How design patterns solve design problems?

- Finding Appropriate Objects (roles, responsibilities)
- Determining Object Granularity (level of abstractions)
- Specifying Object Interfaces (collaborations)
- Specifying Object Implementations (use class instance and inheritance)
- Putting Reuse Mechanisms to Work
- Relating Run-Time and Compile-Time Structures
- Designing for Change

## When to use inheritance?

We should not use inheritance for code sharing.

We should only use inheritance to describes when
an object can be used in place of another.

When inheritance is used carefully (some will say properly), all classes derived from
an abstract class will share its interface. This implies that a subclass merely adds or
overrides operations and does not hide operations of the parent class. All subclasses
can then respond to the requests in the interface of this abstract class, making them all
subtypes of the abstract class.

## How should we program?

Program to an interface, not an implementation.

## Does parser belong to one of the creational patterns?

TODO I do not know yet.

## Does my use of evaluation and check function belong to one of the patterns?

TODO I do not know yet.

## How to select a design pattern?

Match the pattern to your problem.

# 2 A Case Study: Designing a Document Editor

## 2.2 Document Structure

The main pattern is Composite.

## 2.3 Formatting

## 2.4 Embellishing the User Interface

## 2.5 Supporting Multiple Look-and-Feel Standards

## 2.6 Supporting Multiple Window Systems

## 2.7 User Operations

## 2.8 Spelling Checking and Hyphenation

# 3 Creational Patterns

# 4 Structural Patterns

## Composite (Object, Structural)

### Intent

Compose objects into tree structures to represent part-whole hierarchies.
Composite lets clients treat individual objects and compositions of objects uniformly.

- examples of this pattern:
  - web frontend components
  - parse trees
  - programming language expresses

- the teaching of SICP also said, to know a language:
  - first, we know about its primitive elements
  - then, how to compose old elements to new element
  - finally, how to do abstraction, so that a composite is like primitive again

### Motivation

In simple implementation, code that uses some classes
must treat primitive and container objects differently,
even if most of the time the user treats them identically.

Having to distinguish these objects makes the application more complex.
The Composite pattern describes how to use recursive composition
so that clients don't have to make this distinction.

The key to the Composite pattern is an abstract class
that represents both primitives and their containers.
It also declares operations that all composite objects share.

# 5 Behavioral Patterns
