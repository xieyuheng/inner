---
title: Practical Object-Oriented Design
author: Sandi Metz
year: 2019
---

- What is Object-Oriented Design?

  To understand a program is to understand the roles its functions play.

  OOD provides techniques to organize a program,
  so that the roles its functions play are
  explicitly and clearly expressed in the arrangement of code.

# 1 Object-Oriented Design 1

- What is the world like?

  The world is procedural.

- What is the world also like?

  The world is also object-oriented.

- What is the difference between object-oriented programming and actor model?

  OOP views the world as a series of spontaneous interactions between objects.

  The difference is on "spontaneous interactions".

- Why some OOD failed?

  They are failures of perspective.

  Object-oriented design requires that you shift from
  thinking of the world as a collection of predefined procedures
  to modeling the world as a series of messages that pass between objects.

## 1.1 In Praise of Design 2

### 1.1.1 The Problem Design Solves 2

- What problem does design solve?

  Design makes software easy to change.

- Why software need to change?

  It always does.

  The customers didn't know what they wanted,
  they didn't say what they meant.

  You didn't understand their needs,
  you've learned how to do something better.

  The application was a huge success, now everyone wants more.

  Change is unavoidable.

### 1.1.2 Why Change Is Hard 3

- Why change is hard?

  To answer this question, we must explain the model of object-oriented application first.

  Object-oriented applications are made up of parts that interact
  to produce the behavior of the whole. The parts are objects;
  interactions are embodied in the messages that pass between them.

  Getting the right message to the correct target object
  requires that the sender of the message know things about the receiver.

  This knowledge creates dependencies between the two,
  and these dependencies stand in the way of change.

- Why these dependencies stand in the way of change?

  Because changing one require you to change others.
  Also because highly dependent parts can not be reused in different contexts.

- When can poorly designed application survive?

  When the application is very small.
  As long as it grows, poor design kill the application.

### 1.1.3 A Practical Definition of Design 3

- What is design?

  The design is the arrangement of code.
  Design is thus an art of arranging code.

- What is the primary goal of design?

  The primary goal of design is to reduce the cost of change.

## 1.2 The Tools of Design 4

### 1.2.1 Design Principles 4

- What are the main design principles of OOP?

  The SOLID acronym, coined by Michael Feathers
  and popularized by Robert Martin,
  represents five of the most well-known principles
  of object-oriented design:
  - Single Responsibility,
  - Open-Closed,
  - Liskov Substitution,
  - Interface Segregation,
  - and Dependency Inversion.

  Other principles include Andy Hunt and Dave Thomas's DRY (Don't Repeat Yourself)
  and the Law of Demeter (LoD) from the Demeter project at Northeastern University.

- Where did these design principles come from?

  All of these principles got their start as
  choices someone made while writing code.

  Early OO programmers noticed that
  some code arrangements made their lives easier
  while others made them harder.

  These experiences led them to develop
  opinions about how to write good code.

### 1.2.2 Design Patterns 6

- What's good about design patterns?

  The notion of design patterns is incredibly powerful.
  To name common problems and to solve the problems in common ways brings the fuzzy into focus.
  Design Patterns gave an entire generation of programmers the means to communicate and collaborate.

## 1.3 The Act of Design 6

### 1.3.1 How Design Fails 6

### 1.3.2 When to Design 7

### 1.3.3 Judging Design 9

### 1.4 A Brief Introduction to Object-Oriented Programming

#### 1.4.1 Procedural Languages 11

#### 1.4.2 Object-Oriented Languages 11

## 1.5 Summary 13

# 2 Designing Classes with a Single Responsibility 15

## 2.1 Deciding What Belongs in a Class 16

### 2.1.1 Grouping Methods into Classes 16

### 2.1.2 Organizing Code to Allow for Easy Changes 16

## 2.2 Creating Classes That Have a Single Responsibility 17

### 2.2.1 An Example Application: Bicycles and Gears 17

### 2.2.2 Why Single Responsibility Matters 21

### 2.2.3 Determining If a Class Has a Single Responsibility

### 2.2.4 Determining When to Make Design Decisions 22

## 2.3 Writing Code That Embraces Change 24

### 2.3.1 Depend on Behavior, Not Data 24

### 2.3.2 Enforce Single Responsibility Everywhere 29

- What design techniques can be used here?

  All the same design techniques work
  ask them questions about what they do
  and try to describe their responsibilities in a single sentence.

- Describe the relation between the refactorings and the ultimate design.

  Refactoring reveal design.

  Do these refactorings even when you do not know the ultimate design.
  They are needed, not because the design is clear, but because it isn't.
  You do not have to know where you're going to use good design practices to get there.
  Good practices reveal design.

- What are the benefits of methods that have a single responsibility?

  - Expose previously hidden qualities
  - Avoid the need for comments
  - Encourage reuse
  - Are easy to move to another class

- You found the extra responsibilities,
  but you are not sure about creating a new class for them.
  What should you do?

  We can remove extra responsibilities without creating a new class.

  Your goal is to preserve single responsibility in the old class
  while making the fewest design commitments possible.

  Because you are writing changeable code,
  you are best served by postponing decisions
  until you are absolutely forced to make them.

  Any decision you make in advance of an explicit requirement is just a guess.
  Don't decide; preserve your ability to make a decision later.

  Without creating a new public class, we can create a inner private class.

- What should be our attitude towards a muddled class with too many responsibilities?

  If you have a muddled class with too many responsibilities,
  separate those responsibilities into different classes.

  Concentrate on the primary class.
  Decide on its responsibilities
  and enforce your decision fiercely.

  If you identify extra responsibilities that you cannot yet remove, isolate them.
  Do not allow extraneous responsibilities to leak into your class.

## 2.4 Finally, the Real Wheel 33

## 2.5 Summary

# 3 Managing Dependencies 37

- What are the three ways an object might fulfill a desired behavior?

  For any desired behavior, an object either
  - knows it personally,
  - inherits it,
  - or knows another object who knows it.

  The previous chapter "Enforce Single Responsibility Everywhere",
  is about the first way.

  This chapter "Managing Dependencies",
  is about the third way.

## 3.1 Understanding Dependencies 38

- What is the definition of dependency between objects?

  An object depends on another object if,
  when one object changes,
  the other might be forced to change in turn.

  Dependency must be defined relative to change.

### 3.1.1 Recognizing Dependencies 39

- How can an object depend on another object?

  An object has a dependency when it knows:
  - The name of another class.
  - The name of a message that it intends to send to someone other than self.
  - The arguments that a message requires.
  - The order of those arguments.

### 3.1.2 Coupling Between Objects (CBO) 39

- What is the definition of coupling?

  A and B are coupled if changing one imply changing another.

  Coupling must be defined relative to change.

  coupling(A, B, d) = implication(change(A, d), change(B, d))

- What is the difference between dependency and coupling?

  Based on their definitions, they are the same.

### 3.1.3 Other Dependencies 40

- Beside the four kinds of dependencies listed previously,
  what are other dependency-related issues?

  - message chain
  - tests on code

## 3.2 Writing Loosely Coupled Code 41

### 3.2.1 Inject Dependencies 41

- What kind of dependency does this "Inject Dependencies" coding technique deal with?

  Referring to another class by its name.

- How to use dependency injection?

  Using dependency injection to shape code relies on your ability to recognize that
  the responsibility for knowing the name of a class
  and the responsibility for knowing the name of a message to send to that class
  may belong in different objects.

### 3.2.2 Isolate Dependencies 44

- What should we do if we cannot remove unnecessary dependency?

  If you cannot remove unnecessary dependencies,
  you should isolate them within your class,
  so that they are easy to spot and reduce when circumstances permit.

- What should be our attitude towards dependency?

  Think of every dependency as an alien bacterium that's trying to infect your class.

  Give your class a vigorous immune system; quarantine each dependency.

  Dependencies are foreign invaders that represent vulnerabilities,
  and they should be concise, explicit, and isolated.

- When we can not use dependency injection, what techniques we can use to isolate dependency?

  - isolate instance creation
    - creation in constructor
    - lazy creation in method
  - isolate vulnerable external messages

  - How "isolate instance creation" improves arrangement of code?

    An application whose classes are sprinkled with
    entangled and obscure class name references is unwieldy and inflexible,
    while one whose class name dependencies are concise, explicit, and isolated
    can easily adapt to new requirements.

  - What is the definition of external messages?

    Messages that are "sent to someone other than self."

  - When to use "isolate vulnerable external messages"?

    This technique becomes necessary when a class contains
    embedded references to a message that is likely to change.

    Isolating the reference provides some insurance against being affected by that change.

    Although not every external method is a candidate for this preemptive isolation,
    it's worth examining your code, looking for and wrapping the most vulnerable dependencies.

### 3.2.3 Remove Argument-Order Dependencies

- What are the methods we can use to remove argument-order dependencies?

  - use keyword arguments
  - explicitly define defaults
  - isolate multiparameter initialization

- What is the disadvantage of using keyword arguments?

  - Keyword arguments is verbose.

  - It remove dependency on arguments order,
    but introduce dependency on argument names,
    once the keyword arguments API are published,
    one can not change the names.

- Beside removing dependency on arguments order, what else is good about using keyword arguments?

  The keyword arguments is essentially record type data, which is self-descriptive.

  About "Self-descriptive messages", recall the four uniform interface constraints RESTful:

  - Resource identification in requests -- URI (Uniform Resource Identifier)
  - Resource manipulation through representations -- I am not sure about the meaning of this.
  - Self-descriptive messages -- record type
  - Hypermedia as the engine of application state -- hyperlink -- like dependency injection?

  As Sandi said:
  Using keyword arguments requires the sender
  and the receiver of a message to state the keyword names.
  This results in explicit documentation at both ends of the message.
  Future maintainers will be grateful for this information.

- What is the principle behind "isolate multiparameter initialization"?

  The classes in your application should depend on code that you own;
  use a wrapping method to isolate external dependencies.

  The above technique for replacing positional arguments with keywords
  is perfect for cases where you are forced to depend on external interfaces
  that you cannot change.

  Do not allow these kinds of external dependencies to permeate your code;
  protect yourself by wrapping each in a method that is owned by your own application.

## 3.3 Managing Dependency Direction 53

- What is the most import way of managing dependency?

  Reverse the direction of the dependency.

### 3.3.1 Reversing Dependencies 53

- How to reverse dependency?

  By moving a method from one class to its dependency class.

- What is the difference between reversing dependency and dependency injection?

  dependency injection remove dependency,
  reversing dependency change direction.

### 3.3.2 Choosing Dependency Direction 55

- Based on what simple truths about code, you tell your classes,
  "depend on things that change less often than you do."

  - Some classes are more likely than others to have changes in requirements.
  - Concrete classes are more likely to change than abstract classes.
  - Changing a class that has many dependents will result in widespread consequences.

- Recall the "Likelihood of Requirements Change change versus number of dependents" chart.

  | A | Abstract Zone | have little likelihood of change but contain many dependents        |
  | B | Benign Zone   | rarely change and have few dependents                               |
  | C | Concrete Zone | contains code that is quite likely to change but has few dependents |
  | D | Danger Zone   | guaranteed to change and has many dependents                        |

- How to gradually make an application unmaintainable?

  You can guarantee that any application will gradually become unmaintainable
  by making its Zone D classes more likely to change than their dependents.

- In you own developing experience, do you have any examples about dependency direction design?

  If a type system is implemented by OOP,
  the "check" method should belong to expression instead of type,
  because type is more abstract then expression.

## 3.4 Summary

- Make a table of coding techniques that can be used to managing dependencies.

  |---------------------------------------+------------------+----------+--------------------------|
  | technique                             | dependency       | managing | side effect              |
  |---------------------------------------+------------------+----------+--------------------------|
  | dependency injection                  | class name       | remove   |                          |
  | (remove named class)                  |                  |          |                          |
  |---------------------------------------+------------------+----------+--------------------------|
  | isolate instance creation             | class name       | isolate  |                          |
  |---------------------------------------+------------------+----------+--------------------------|
  | isolate vulnerable external messages  | message name     | isolate  |                          |
  |---------------------------------------+------------------+----------+--------------------------|
  | use keyword arguments                 | argument order   | remove   | depends on argument name |
  | (remove positional arguments)         |                  |          |                          |
  |---------------------------------------+------------------+----------+--------------------------|
  | explicitly define defaults            | message argument | reduce   |                          |
  | (reduce required arguments)           |                  |          |                          |
  |---------------------------------------+------------------+----------+--------------------------|
  | isolate multiparameter initialization | message argument | isolate  |                          |
  |                                       | argument order   | isolate  |                          |
  |---------------------------------------+------------------+----------+--------------------------|

# 4 Creating Flexible Interfaces 61

## 4.1 Understanding Interfaces 61

- Why messages is the core concept of OOP?

  Because messages form the communication pattern between objects,
  which is the dynamic structure of the running application.

- What are the two kinds of interfaces?

  The first kind of interface is public vs. private method, that is,
  methods within a class and how and what to expose to others.

  The second kind of interface is the one that represents a concept
  that is broader than a class and is defined by a set of messages.

  This chapter is about the first kind of interface.

  the next chapter "Reducing Costs with Duck Typing",
  is about the second kind of interface.

- What is the relation between interface (the first kind) and messages?

  Public interfaces constrain the communication pattern formed by the messages.

## 4.2 Defining Interfaces 63

- Why this distinction between public and private exists?

  Because it is the most effective way to do business.

### 4.2.1 Public Interfaces 64

- What are the properties of public Interface?

  - Reveal its primary responsibility.
  - Are expected to be invoked by others.
  - Will not change on a whim.
  - Are safe for others to depend on.
  - Are thoroughly documented in the tests.

### 4.2.2 Private Interfaces 64

- What are the properties of private Interface?

  - Handle implementation details.
  - Are not expected to be sent by other objects.
  - Can change for any reason whatsoever.
  - Are unsafe for others to depend on.
  - May not even be referenced in the tests.

### 4.2.3 Responsibilities, Dependencies, and Interfaces 64

- What is the relation between public methods and responsibilities?

  Public methods should read like a description of responsibilities.

## 4.3 Finding the Public Interface 65

### 4.3.1 An Example Application: Bicycle Touring Company 65

### 4.3.2 Constructing an Intention 65

- The emphasis on messages rather than objects here,
  reminds me of the functional style in "Architecture with Ashi Krishnan".

  - If messages are more important, why should not we speak about function, input and output?

    Because we wish to add new expression type easily. (The open close principle)

### 4.3.3 Using Sequence Diagrams 66

- Compare sequence diagram with function type, what is the difference?

  Function type describe:
  - function name
  - input type
  - output type

  We can do design be composing function types.

  Sequence diagram describe:
  - message name (function name)
  - sender (the module (class) that is calling the function)
  - receiver (the module (class) where the function is implemented)

  In one sequence diagram many threads of message can be described.

  We can see that the information described are both very useful in our design!

  When used in static type language, adding type to each message suffice for our need.
  - If we draw sequence diagram first, we should then attach type to the messages.
  - If we specify function type first, we should then
    organize the functions of the problem domain into a sequence diagram.

- What is the value of sequence diagrams?

  They explicitly specify the messages that pass between objects,
  and because objects should only communicate using public interfaces,
  sequence diagrams are a vehicle for exposing, experimenting with,
  and ultimately defining those interfaces.

- If we want to invert the design conversation,
  to emphasis messages instead of classes, what should we do?

  Draw sequence diagram!

  Suddenly, the conversation has changed!
  It is now revolving around messages.
  Instead of deciding on a class and then figuring out its responsibilities,
  you are now deciding on a message and figuring out where to send it.

- After emphasising messages, the fundamental design question is changed from what to what?

  The fundamental design question is changed
  from "I know I need this class, what should it do?"
  to "I need to send this message, who should respond to it?"

### 4.3.4 Asking for "What" Instead of Telling "How" 70

- What is the effect of switching the conversation from "How" to "What"?

  The size of the public interface in receiver was drastically reduced.

### 4.3.5 Seeking Context Independence 72

- What constitutes an object's context?

  The things that an object knows about other objects make up its context.

- What is the difference between context and dependency?

  The techniques to deal with them are different,
  for dependency, we can use dependency injection,
  for context, it is not proper to use dependency injection,
  we still need to know the receiver class name,
  but can tell the receiver our intention and pass self to it.

### 4.3.6 Trusting Other Objects 74

### 4.3.7 Using Messages to Discover Objects 75

### 4.3.8 Creating a Message-Based Application 77

## 4.4 Writing Code That Puts Its Best (Inter)Face Forward 77

### 4.4.1 Create Explicit Interfaces 77

### 4.4.2 Honor the Public Interfaces of Others 79

### 4.4.3 Exercise Caution When Depending on Private Interfaces

### 4.4.4 Minimize Context 80

## 4.5 The Law of Demeter 80

### 4.5.1 Defining Demeter 81

### 4.5.2 Consequences of Violations 81

### 4.5.3 Avoiding Violations 82

### 4.5.4 Listening to Demeter 83

## 4.6 Summary 84

# 5 Reducing Costs with Duck Typing 85

- What is the purpose of object-oriented design.

  The purpose of object-oriented design is to reduce the cost of change.

## 5.1 Understanding Duck Typing 85

- What is type in normal programmer's view?

  Programming languages use the term type
  to describe the category of the contents of a variable.

- What is type in correspondent programmer's view?

  Type is proposition.

  By the way, programm is proof.

### 5.1.1 Overlooking the Duck 86

### 5.1.2 Compounding the Problem 88

### 5.1.3 Finding the Duck 90

### 5.1.4 Consequences of Duck Typing 94

- What are the consequences of duck typing?

  The use of duck typing (interface) is an act of abstraction.

  This tension between
  the costs of concretion and
  the costs of abstraction
  is fundamental to object-oriented design.

  Concrete code is easy to understand but costly to extend.
  Abstract code may initially seem more obscure
  but, once understood, is far easier to change.

- What is the hallmark of a confident designer?

  The ability to tolerate ambiguity about the class of an object
  is the hallmark of a confident designer.

  Once you begin to treat your objects
  as if they are defined by their behavior
  rather than by their class,
  you enter into a new realm of expressive flexible design.

- What is the definition of "polymorphism" in OOP?

  Polymorphism in OOP refers to the ability of
  many different objects to respond to the same message.

  Senders of the message need not care about the class of the receiver;
  receivers supply their own specific version of the behavior.

  A single message thus has many (poly) forms (morphs).

## 5.2 Writing Code That Relies on Ducks 95

### 5.2.1 Recognizing Hidden Ducks 95

- What are the common coding patterns that indicate the presence of a hidden duck?

  - Case statements that switch on class
  - kind_of? and is_a? (instance of, type of)
  - responds_to? (hasOwnProperty)

- What should you think when you see case statements?

  When you see this pattern, you know that
  all of the cases must share something in common;
  they arrive here because of that common thing.

  Examine the code and ask yourself,
  "What is it that the calling function wants from each of its cases?"

  The answer to that question suggests the message you should send;
  this message begins to define the underlying duck type.

### 5.2.2 Placing Trust in Your Ducks 97

- What should you do when you see the code patterns above?

  When you see these code patterns,
  concentrate on the offending code's expectations
  and use those expectations to find the duck type.

  Once you have a duck type in mind,
  define its interface,
  implement that interface where necessary,
  and then trust those implementers to behave correctly.

  Flexible applications are built on objects that operate on trust;
  it is your job to make your objects trustworthy.

### 5.2.3 Documenting Duck Types 98

- What is the problem of dynamic language?

  In dynamic language,
  the duck type and its public interface
  are a concrete part of the design
  but a virtual part of the code.

  We need to write tests (as documentation) for these interfaces.

### 5.2.4 Sharing Code between Ducks 98

- Where we will learn about sharing code between ducks?

  In Chapter 7, "Sharing Role Behavior with Modules".

### 5.2.5 Choosing Your Ducks Wisely 98

- What is the measuring stick of design?

  The purpose of design is to lower costs (of changing);
  bring this measuring stick to every situation.

## 5.3 Conquering a Fear of Duck Typing 100

### 5.3.1 Subverting Duck Types with Static Typing 100

- What feature will allow a static type language
  to use the technique of duck typing very well?

  Record type and structural typing,
  maybe with intersection type and union type.

### 5.3.2 Static versus Dynamic Typing 101

- Why some people love static typing?

  Because type is proposition, and programm is proof.

### 5.3.3 Embracing Dynamic Typing 102

- Why some people dislike metaprogramming?

  Because they are unnecessary,
  unless you want to use macro system to embed DSL in lisp.

  And if you there is no macro system,
  you can still write interpreter or compiler.

## 5.4 Summary 103

# 6 Acquiring Behavior through Inheritance 105

## 6.1 Understanding Classical Inheritance 105

- What is inheritance?

  Inheritance is, at its core, a mechanism for automatic message delegation.

  It defines a forwarding path for not-understood messages.
  It creates relationships such that,
  if one object cannot respond to a received message,
  it delegates that message to another.

  You don't have to write code to explicitly delegate the message;
  instead you define an inheritance relationship between two objects,
  and the forwarding happens automatically.

- How can we can do inheritance in the "record type + closure" style of OOP?

  By explicitly delegate the messages.

- What are the mathematic fields that study inheritance?

  - Lattice theory -- https://en.wikipedia.org/wiki/Lattice_(order)
  - Formal concept analysis -- https://en.wikipedia.org/wiki/Formal_concept_analysis

## 6.2 Recognizing Where to Use Inheritance 106

### 6.2.1 Starting with a Concrete Class 107

### 6.2.2 Embedding Multiple Types 109

- What is an antipattern?

  An antipattern is a common pattern that appears to be beneficial
  but is actually detrimental, and for which
  there is a well-known alternative.

### 6.2.3 Finding the Embedded Types 111

### 6.2.4 Choosing Inheritance 112

### 6.2.5 Drawing Inheritance Relationships 114

## 6.3 Misapplying Inheritance 114

## 6.4 Finding the Abstraction 116

- What is subclasse?

  Subclasses are specializations of their superclasses.
  A subclasse should be everything a superclass is, plus more.

### 6.4.1 Creating an Abstract Superclass 117

- How to minimize the costs of creating a hierarchy?

  Creating a hierarchy has costs;
  the best way to minimize these costs  is to
  maximize your chance of getting the abstraction right
  before allowing subclasses to depend on it.

  One subclass is not good moment to create a hierarchy,
  Two or three subclasses are better.

- How should we choose between duplication and abstraction?

  To make a choose, we need to be clear about our aim.

  The aim is to reduce costs, thus we need
  to identify the cost in each decision.

  Your choice about whether to wait or to proceed hinges on
  how soon you expect a third case to appear versus
  how much you expect the duplication to cost.

  If a third case is imminent,
  it may be best to duplicate the code
  and wait for better information.

  However, if the duplicated code would need to change every day,
  it may be cheaper to go ahead and create the hierarchy.

  You should wait, if you can,
  but don't fear to move forward
  based on two concrete cases if this seems best.

### 6.4.2 Promoting Abstract Behavior 120

- What refactoring strategy should we use here?

  TODO

- What kinds of costs should be included in every decision you make?

  Every decision you make includes two costs:
  - one to implement it
  - and another to change it when you discover that you were wrong.

  Taking both costs into account when choosing among alternatives
  motivates you to make conservative choices that minimize the cost of change.

### 6.4.3 Separating Abstract from Concrete 123

### 6.4.4 Using the Template Method Pattern 125

### 6.4.5 Implementing Every Template Method 127

## 6.5 Managing Coupling between Superclasses and Subclasses 129

### 6.5.1 Understanding Coupling 129

### 6.5.2 Decoupling Subclasses Using Hook Messages 134

- What is the possible couplings in the relation between superclass and subclass?

  - Subclass depends on subclass's constructor.
    - We can use post constructor hook to remove this in dynamic language.
    - I do not know how to remove this in static language.
  - When overriding a method, subclass can call superclass's method.
    - We can try to minimize this.

## 6.6 Summary 139

- What problem does inheritance solve?

  Inheritance solves the problem of related types
  that share a great deal of common behavior
  but differ across some dimension.

# 7 Sharing Role Behavior with Modules 141

- Can we do this in the "record type + closure" style of OOP?

  Yes just like inheritance.

## 7.1 Understanding Roles 142

### 7.1.1 Finding Roles 142

- What is mixin?

  When an object includes a mixin, the methods defined therein
  become available via automatic delegation.

  - Just like inheritance.

- Which principle might be violated when using mixin?

  Single Responsibility principle.

  Because an object that directly implements few methods
  might still have a very large response set.

- That are the total set of messages to which an object can respond?

  The total set of messages to which an object can respond includes:
  - Those it implements
  - Those implemented in all objects above it in the hierarchy
  - Those implemented in any module that has been added to it
  - Those implemented in all modules added to any object above it in the hierarchy

  If this seems like a frighteningly large and potentially confusing response set,
  you have a clear grasp of the problem. Acquiring an understanding
  of the behavior of a deeply nested hierarchy
  is at best intimidating,
  at worst, impossible.

### 7.1.2 Organizing Responsibilities 143

### 7.1.3 Removing Unnecessary Dependencies 146

### 7.1.4 Writing the Concrete Code 147

### 7.1.5 Extracting the Abstraction 150

- What is the difference between inheritance and mixin?

  | technique   | meaning        |
  |-------------+----------------|
  | inheritance | is-a           |
  | mixin       | behaves-like-a |

  However, the coding techniques for these two things are very similar
  and this similarity exists because both techniques rely on automatic message delegation.

### 7.1.6 Looking Up Methods 153

### 7.1.7 Inheriting Role Behavior 157

## 7.2 Writing Inheritable Code 158

### 7.2.1 Recognize the Antipatterns 158

- What are the two antipatterns that indicate that your code might benefit from inheritance.

  First, an object that uses a variable with a name like type, kind or category
  to determine what message to send to self
  contains two highly related but slightly different types.
  Code like this can be rearranged to use classical inheritance
  by putting the common code in an abstract superclass
  and creating subclasses for the different types.

  Second, when a sending object checks the class of a receiving object
  to determine what message to send, you have overlooked a duck type.
  In this situation, all of the possible receiving objects play a common role.
  This role should be codified as a duck type,
  and receivers should implement the duck type's interface.
  Once they do, the original object can send one single message to every receiver,
  confident that because each receiver plays the role, it will understand the common message.

- How can duck type share behavior?

  In addition to sharing an interface, duck types might also share behavior.
  When they do, place the shared code in a mixin
  and include that mixin in each class or object that plays the role.

### 7.2.2 Insist on the Abstraction 158

### 7.2.3 Honor the Contract 159

### 7.2.4 Use the Template Method Pattern 160

### 7.2.5 Preemptively Decouple Classes 160

### 7.2.6 Create Shallow Hierarchies 160

## 7.3 Summary 161

# 8 Combining Objects with Composition 163

- In composition, what is the relation between larger object and its parts?

  In composition, the larger object is connected to its parts via a has-a relationship.

## 8.1 Composing a Bicycle of Parts 163

### 8.1.1 Updating the Bicycle Class 164

### 8.1.2 Creating a Parts Hierarchy 165

- What is the relation between Bicycle hierarchy and Parts hierarchy?

  The structure is the same,
  changing Bicycle hierarchy to Parts hierarchy,
  is like pushing sum type into property,
  is like the distributive law of elementary algebra.
  x * (y + z) = (x * y) + (x * z)

## 8.2 Composing the Parts Object 168

### 8.2.1 Creating a Part 168

### 8.2.2 Making the Parts Object More Like an Array 172

## 8.3 Manufacturing Parts 176

### 8.3.1 Creating the PartsFactory 177

### 8.3.2 Leveraging the PartsFactory 179

## 8.4 The Composed Bicycle 181

## 8.5 Deciding between Inheritance and Composition 185

### 8.5.1 Accepting the Consequences of Inheritance 186

### 8.5.2 Accepting the Consequences of Composition 188

### 8.5.3 Choosing Relationships 189

## 8.6 Summary 191

# 9 Designing Cost-Effective Tests 193

## 9.1 Intentional Testing 194

- What are the three skills for the art of writing changeable code?

  An understanding of object-oriented design,
  good refactoring skills,
  and the ability to write efficient tests

  form a three-legged stool upon which changeable code rests.

  Well-designed code is easy to change,
  refactoring is how you change from one design to the next,
  and tests free you to refactor with impunity.

### 9.1.1 Knowing Your Intentions 194

- What are the potential benefits of testing?

  - Finding Bugs
  - Supplying Documentation
  - Deferring Design Decisions
  - Supporting Abstractions
  - Exposing Design Flaws

- Why we want to understand the potential benefits of testing?

  Because understanding of these benefits will increase your motivation to achieve them.

- What are the "decision points" in a design?

  The decision points is mentioned
  in the section about "Deferring Design Decisions",
  but it is not limited to testing.

  As your design skills improve you will begin to write applications
  that are sprinkled with places where you know the design needs something
  but you don't yet have enough information to know exactly what.
  These are the places where you are awaiting additional information,
  valiantly resisting the forces that compel you to commit to a specific design.

  These "pending" decision points are often coded as slightly embarrassing,
  extremely concrete hacks hidden behind totally presentable interfaces.
  This situation occurs when you are aware of just one concrete case in the present
  but you fully expect new cases to arrive in the near future.

  You know that at some point you will be better served by code that
  handles these many concrete cases as a single abstraction,
  but right now you don't have enough information to anticipate
  what that abstraction will be.

### 9.1.2 Knowing What to Test 196

- What are the design principles of testing?

  The design principles you are enforcing in your application apply to your tests as well.
  Each test is merely another application object that needs to use an existing class.

  The more the test gets coupled to that class, the more entangled the two become and
  the more vulnerable the test is to unnecessarily being forced to change.

- What to test?

  Not only should you limit couplings, but the few you allow should be to stable things.
  The most stable thing about any object is its public interface;

  it logically follows that the tests you write
  should be for messages that are defined in public interfaces.

  The most costly and least useful tests are those that
  blast holes in an object's containment walls
  by coupling to unstable internal details.

  These overeager tests prove nothing about the overall correctness of an application
  but nonetheless raise costs because they break with every refactoring of underlying class.

- How to test side effects (outgoing command messages)?

  However, many outgoing messages do have side effects
  - a file gets written,
  - a database record is saved,
  - an action is taken by an observer
  upon which your application depends.

  These messages are commands and it is the responsibility
  of the sending object to prove that they are properly sent.

  Proving that a message gets sent is a test of behavior, not state,
  and involves assertions about the number of times,
  and with what arguments, the message is sent.

  Here, then, are the guidelines for what to test:
  - Incoming messages should be tested for the state they return.
  - Outgoing command messages should be tested to ensure they get sent.
  - Outgoing query messages should not be tested.

- How should we test a language implementation, specially its type system?

  We should write expected passing and failing type checkings.
  But whether to depend on concrete syntax, there are costs and tradeoffs.

  - In our design, we choose to depend on concrete syntax to make it easier to write test cases.
    But the parser is not test independently,
    and everytime we change the syntax, we have to change type syntax tests.

### 9.1.3 Knowing When to Test 199

- When to test?

  You should write tests first,
  whenever it makes sense to do so.

### 9.1.4 Knowing How to Test 200

## 9.2 Testing Incoming Messages 202

### 9.2.1 Deleting Unused Interfaces 204

### 9.2.2 Proving the Public Interface 204

### 9.2.3 Isolating the Object under Test 206

### 9.2.4 Injecting Dependencies Using Classes 208

### 9.2.5 Injecting Dependencies as Roles 210

## 9.3 Testing Private Methods 215

### 9.3.1 Ignoring Private Methods during Tests 216

### 9.3.2 Removing Private Methods from the Class under Test 216

### 9.3.3 Choosing to Test a Private Method 216

## 9.4 Testing Outgoing Messages 217

### 9.4.1 Ignoring Query Messages 217

### 9.4.2 Proving Command Messages 218

## 9.5 Testing Duck Types 221

### 9.5.1 Testing Roles 221

### 9.5.2 Using Role Tests to Validate Doubles 227

## 9.6 Testing Inherited Code 233

### 9.6.1 Specifying the Inherited Interface 233

### 9.6.2 Specifying Subclass Responsibilities 236

### 9.6.3 Testing Unique Behavior 240

## 9.7 Summary 244
