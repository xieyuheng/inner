---
title: 99 bottles of OOP
author: Sandi Metz
year: 2021
---

# 1. Rediscovering Simplicity

## What kind of code should we write first?

Write Shameless Green code first.

- TDD will teach us how to do this.
- abstraction should be guided by new requirements, keep it simple when there is no new requirements
- new design with new abstraction should be achieved by refactoring

## How should we measure code quality? is it even possible?

ABC Metric -- Assignments, Branches and Conditions,
is a good quantitative measure of code quality.

## What questions can help us understand the cost and value of code?

Questions about cost and value of code:

1. How difficult was it to write? (past)
2. How hard is it to understand? (now)
3. How expensive will it be to change? (future)

Among the above (now) is the most import question.

Beside the above, domain questions is another important kind of questions.

- Which is about understanding.

# 2. Test Driving Shameless Green

## Why we wish to "getting green first"?

Getting green first, Shameless Green maximum understandability
but is generally unconcerned with changeability.

Because we'd better achieve understandability and changeability in steps.

## How to achieve understandability and changeability

To achieve understandability and changeability,
we, little by little, separate the things that change
from the things that remain the same.

Like reversing the distribution of multiplication over addition:

x _ y + x _ z => x \* (y + z)

## Why we wish to delay abstraction?

Because we want to wait for more information.

Delay abstraction, wait for more information.

It's better to tolerate duplication
than to anticipate the wrong abstraction.

## What questions can help us understand the cost and value of making abstraction?

Ask the following questions, before making an abstraction:

1. Does the change I'm contemplating make the code harder to understand?
2. What is the future cost of doing nothing now?
3. When will the future arrive, or how soon will I get more information?

## During the search for Shameless Green, We should tolerate what kind of duplication?

When it isolates a new independent example,
for which the underlying abstraction is not clear yet.

## And, we should not tolerate what kind of duplication?

Those kind of duplication that blurs the responsibility.

## When should we jump over the small steps of TDD?

Never.

Because you do not always know what is actually right.

## What is Kent Beck's Triangulation? [三角测量法]

Targeting multiple tests in one implementation step.
Because you can not fake multiple tests.

## How should we design public API?

As method provider, we should think about message sender,
and help them to know as less as possible.

Bwtween sender and provider, to know is to depend.

## What is the first step in learning the art of testing?

The first step in learning the art of testing
is to understand how to write tests that
confirm what your code does
without any knowledge of how your code does it.

# 3. Unearthing Concepts

## When a new requirement arrive, what we learned?

The arrival of a new requirement tells you two things:

- Exactly how the code should change
- The code need to be easy to change

## What is your licence to improve some (Shameless Green) code?

Someone has asked for a change.

## What is the "open" principle of SOLID?

O - Open-Closed

Objects should be open for extension, but closed for modification.

"open for extension" means extension not by editing the conditional,
but by merely adding code.

When faced with a new requirement,
first "open" the code for change,
then add the new code.

---

It is important to note that, the sum type of algebra datatype, is about being close.
Being close, we can make sure all cases are covered in a "proof by case" (function is proof).

## If we do not know how to open the code, what should we do?

We should remove the easiest to fix and best understood code smell.

## How to find code smells?

Make a list of the things you dislike about your code.

## How to achieve good abstraction?

Use the "Flocking Rules":

1. Select the things that are most alike.
2. Find the smallest difference between them.
3. Make the simplest change that will remove that difference.

DRYing out sameness has some value, but DRYing out difference has more.

## Why "Flocking"?

Birds flock, fish school, and insects swarm.

A flock's behavior can appear so synchronized and complex
that it gives the impression of being centrally coordinated.
Nothing could be further from the truth.

The group's behavior is the result of a continuous series of
small decisions being made by each participating individual.
These decisions are guided by three simple rules.

1. Alignment - Steer towards the average heading of neighbors
2. Separation - Don't get too close to a neighbor
3. Cohesion - Steer towards the average position of the flock

Thus, complex behavior emerges from the repeated application of simple rules.
In the same way that the rules in this sidebar allow birds to flock,
the "Flocking Rules" for code allow abstractions to appear.

---

I found a correspondence of the "Flocking Rules" in algebra:

The "Flocking Rules":

1. Select the things that are most alike.
2. Find the smallest difference between them.
3. Make the simplest change that will remove that difference.

Is like reversing the distribution of multiplication over addition:

`x * y + x * z => x * (y + z)`

1. `x * y` and `x * z` are alike.
2. The smallest difference is `y` vs. `z`.
3. Extract `(y + z)` and use the distribution law, to get one product.

The metaphor of "a flock of birds" is beautiful :)

And algebra is also beautiful, the "Flocking Rules" in algebra means,
by doing this steps locally we can achieve some kind of normal form in a ring.

- Ring: https://en.wikipedia.org/wiki/Ring_(mathematics)

## What can we do when we are in the struggle for a name?

There are two pieces of information that can help in the struggle for a name.

One is a general rule and the other is the new requirement:

- The general rule is to name a thing one level of abstraction higher than the thing itself.
  (this rule applies more to methods than to classes.)

- While new requirement can provide new instance for the category we are trying to name.
  We should name the category using the language of the domain.

- Avdi Grimm:

  We can also use rows and columns in an imaginary spreadsheet,
  to write things down, to help find names for underlying concepts.

  This naming technique is called "what would the column header be?"

- Tom Stuart:

  To name a concept for which you have only a few examples,
  it can help to imagine other concrete things
  that might also fall into the same category.

# 4. Practicing Horizontal Refactoring

## Should we think far ahead for creating better abstraction?

You can use your common sense, but in general you should not.

When creating an abstraction,
first describe its responsibility as you understand it at this moment,
then choose a name which reflects that responsibility.

The effort you put into selecting good names right now
pays off by making it easier to recognize perfect names later.

You can learn something during these steps, do not jump.

## How to reducing the number of dependencies imposed upon message senders?

By requiring that receivers return trustworthy objects,
which is a generalization of the Liskov Substitution Principle.

## What are the benefits of abstractions?

Abstractions are beneficial in many ways.

They consolidate code into a single place
so that it can be changed with ease.

They name this consolidated code,
allowing the name to be used as a shortcut for an idea,
independent of its current implementation.

These are valuable benefits, but abstractions also help in another, more subtle, way.
In addition to the above, abstractions tell you
where your code relies upon an idea.

But to get this last benefit,
you must refer to an abstraction
in every place where it applies.

## Why the ABC score is worse, but we consider the code quality improved?

Because it revealed and isolated a lot of useful concepts.

Maybe a better score is (ABC / number of domain concepts).

# 5. Separating Responsibilities

## What is the truth about refactoring?

Sometimes some refactoring is wrong, and we need to backtrack.
This is expected because refactoring is an idea
that help us to explore a problem domain safely.

If after a refactoring, the code is still not open to the new requirement.
Don't worry, have faith, iterate, find new code smells to attack.

## What questions can help us find code smells?

The following questions help separating responsibilities.

Look at the class as a whole and expose common qualities of the code:

1. Do any methods have the same shape?
2. Do any methods take an argument of the same name?
3. Do arguments of the same name always mean the same thing?
4. If you were going to break this class into two pieces, where's the dividing line?

Look at the methods:

5. Do the tests in the conditionals have anything in common?
6. How many branches do the conditionals have?
7. Do the methods contain any code other than the conditional?
8. Does each method depend more on the argument that got passed, or on the class as a whole?

## How does naming methods and naming classes different?

The rule about naming can thus be amended:
while you should continue to name methods after what they mean,
classes can be named after what they are.

# 6. Achieving Openness

## What is the "Data Clumps" code smell?

If these two things always appear together,
it's a signal that this pairing represents a deeper concept,
and that concept should be named.

## Why sometimes programmers add blank line in code?

Programmers add blank lines to signify changes of topic.

The presence of multiple topics suggests
the existence of multiple responsibilities,
which makes code harder to understand when reading,
and easier to harm when changing.

## How does skilled programmers choose the best solution?

For example, to fix the "Switch Statement" code smell,
should we use the "Replace Conditional with State/Strategy"
or the "Replace Conditional with Polymorphism"?

It's the result of a lifetime of coding experiments.
Their present-day actions are informed by
a diverse body of knowledge gained piecemeal, over time.

Their deep familiarity with many varieties of code entanglements
allows them to unconsciously map appropriate solutions onto common problems,
often without the necessity of first writing code.

They also know that they don't know everything.
This belief in their own fallibility lends them caution.
Skilled programmers do what's right when they intuit the truth,
but otherwise they engage in careful, precise,
reproducible, and reversible coding experiments.
You are encouraged to do the same.

The best way to figure out what will happen
if you follow competing recipes is to try it,
speculatively, try them all.
Evaluate the results.
Choose one and proceed,
or revert all and try again.

Practice builds intuition.
Do it enough, and you'll seem magical too.

## What is polymorphism in OOP?

In OO, polymorphism refers to the idea of
having many different kinds of objects
that respond to the same message.

## What is a "factory"?

When several classes play a common role,
a factory is a method whose job is to
return the right role-playing object.

- This means that "Replace Conditional with Polymorphism" can not remove all conditionals,
  but can merge all conditionals into one conditional in a "factory".

## With polymorphism in place what do we know about domain question about variation?

Domain question about variation are questions like the following form:

- Which `______` are most alike? In what way?
- Which `______` are most different? In what way?

The subtype hierarchy developed for the polymorphism,
looks like part of a concept lattice in formal concept analysis.

## How to change return type of a polymorphic method in a step by step way?

By temporarily allow functions that use the return value to accept both types.

# 7. Manufacturing Intelligence

## How factory different from other methods that use conditionals?

Factories don't know what to do;
instead, they know how to choose who does.

A conditional that selects an object vs. A conditional that supplies behavior.

## What knowledge do factories capture?

Knowledge of the class names of the variants,
and of the logic necessary to choose the correct one,
can be captured in factories.

## What is a factory's responsibility?

A factory's responsibility is to manufacture the right object for a given role.

## What dimensions factories can vary along?

Factories can vary along these dimensions:

1. The factory can be open to new variants or closed
2. variant own choosing logic
   - for example, by a `can_handle` static method
3. variant own creating logic
   - for example, by a `try_to_create` static method

## What is a example use of factories in language implementation?

Parser is.

In which syntax will change in lockstep with expression class,
Thus maybe factories should not own all the responsibilities.

But maybe we need to support multiple style of syntax,
so the factories should own all the responsibilities.

## What are ways to keep a factory open to new variants?

1. Use meta programming.
2. The factory
   1. holds onto the registry, and
   2. provides a way for candidates to add themselves to it.

For example, web components use (2).

Note that, in (2) variants knows the factory.

## Why experienced programmers are good at writing change-tolerant code?

One reason experienced programmers are good at writing change-tolerant code
is that they've built up a set of internal guidelines
about how to guess well.

They understand that although dependencies can't be avoided,
they can be deliberately chosen with an eye towards stability.

# 8. Developing a Programming Aesthetic

## What we should do about programming aesthetic?

While learning from others, it is more important to develop your own.

## What is intuition?

Judgement is informed by past experience.

Experience accumulates into an intuition
about how best to act in the face of uncertainty.

Intuition is a form of pattern matching
performed by your unconscious mind,
trained throughout your career on scores of code examples.

## How should we deal with our intuition?

Intuition is generated by the big super-computer of your unconscious mind.
Intuition not well expressed by words are just feelings.
Intuition not well expressed by words are not convincing.

It's the job of your conscious brain to figure out how to put words on those feelings.

These words form your programming aesthetic,
or the set of principles that underlie and guide your work.

Intuition drives action,
justified by aesthetics,
and guided by heuristics.

## What is Dependency Inversion Principle (DIP)?

1. High-level modules should not depend upon low-level modules.
   Both should depend upon abstractions.

2. Abstractions should not depend upon details.
   Details should depend upon abstractions.

First, note that the word "module" in the definition above
does not refer to a specific language feature.
In this definition module means an encapsulated,
named unit of functionality in a program.
You can substitute the words "classes" or "objects" for "modules."

## How to create new role and do dependency injection?

Isolate the behavior you want to vary.

One of the most fundamental concepts in OO is to isolate the behavior you want to vary.

## When injecting collaborators, should you inject classes or instances of those classes?

We should inject instances.

Because we do not want to depend on how instances are created.

Injecting classes will violate the Law of Demeter.

The rule for injecting dependencies is that
you should inject the thing you want to talk to.

In other words, the receiver may directly send messages
only to the injected object,
not to it and all of its friends.

The practical effect of this rule is to
prohibit the use of injected objects in message chains
that violate the Law of Demeter.

## What is the Law of Demeter (LoD)?

The Law of Demeter says that from within a method, messages should be sent only to:

1. objects that are passed in as arguments to the method
2. objects that are directly available to this

---

This is define in a worse way in "Object-Oriented Programming: An Objective Sense of Style":

For all classes C and for all methods M attached to C,
all objects to which M sends a message
must be instances of classes associated with the following classes:

1. The argument classes of M (including C).
2. The instance variable classes of C.

(Objects created by M, or by functions or methods which M calls,
and objects in global variables are considered as arguments of M.)

## Why the we should obey the Law of Demeter?

The Law of Demeter effectively restricts the list of other objects
to which an object may send a message.
Its purpose is to reduce the coupling between objects.

From the message-senders point of view,
an object may talk to its neighbors but not to its neighbor's neighbors.

Objects may only send messages to direct collaborators.

## How to cure Demeter violations?

Use message forwarding.

Also to avoid encoding the names of existing objects
into the names of the forwarding messages,
We should think about design from the message senders point of view.

## What is the difference between delegation and message forwarding?

In delegation sender also pass itself to receivers.

## How to get a quick handle on the consequences of a code arrangement?

One way is to attempt to test it.

Testing is the first form of reuse.

It is all about feedback.

## What does OOD teach us about when we want something?

If you want something, just ask for it.
If the receiver doesn't know how to comply, teach it.
Don't be trapped by what's currently true,
but instead, loosen coupling by designing a conversation
that embodies what the message sender wants.

## What does well-designed object-oriented applications consist of?

Well-designed object-oriented applications consist of loosely-coupled objects
that rely on polymorphism to vary behavior.

Injecting dependencies loosens coupling.

Polymorphism isolates variant behavior
into sets of interchangeable objects
that look the same from the outside
but behave differently on the inside.

## When using dependency injection, what should we do about object creation?

Object creation should be pushed more towards the edges.

Applications that use dependency injection evolve,
naturally and of necessity, into systems where
object creation begins to separate from object use.

Object creation gets pushed more towards the edges, towards the outside,
and the objects themselves interact more towards the middle, or the inside.

## What rules experienced programmers know about class name, that make applications most easily adapt to the unknown future?

Experienced object-oriented programmers know that applications most easily adapt to the unknown future if they:

- resist giving instance methods knowledge of concrete class names, and
- seek opportunities to move the object creation towards the edges of the application.

These are guidelines, not hard and fast rules.
This is especially true in cases like this where the hard-coded reference is to a factory,
so the coupling is already loose.

Even so, you should be eternally alert for instance methods that reference class names
and perpetually on the lookout for ways to remove those references,
by pushing object creation towards the edges.

## What is a programming aesthetic?

Well expressed intuition is aesthetic.

A programming aesthetic is the set of internal heuristics
that guide your behavior in times of uncertainty.

Vague feelings about the rightness of code
become part of your aesthetic
once you can eloquently and convincingly
use actual words to explain your concerns and proposed improvements.

A good programming aesthetic focuses attention
on improvements that are likely to prove worthwhile.

## What are some precepts that belong in everyone's object-oriented programming aesthetic?

1. Put domain behavior on instances.
2. Be averse to allowing instance methods to know the names of constants.
3. Seek to depend on injected abstractions rather than hard-coded concretions.
4. Push object creation to the edges, expecting objects to be created in one place and used in another.
5. Avoid Demeter violations, using the temptation to create them as a spur to search for deeper abstractions.

# 9. Reaping the Benefits of Design

## What can tests do?

Tests help us know if something breaks.

It's important to know if something breaks, but tests can do far more.
They give you the opportunity to explain the domain to future readers.

They expose design problems that make code hard to reuse.

Well-designed code is easy to test; when testing is hard,
the code's design needs attention.

It should be easy to create simple, intention-revealing tests.
When it's not, the chief problem is often too much coupling.
In such cases the solution is not to write complicated tests that overcome tight coupling,
but rather to loosen the coupling so that you can write simple tests.

The most cost-effective time to intervene in tightly coupled code is right now,
before new requirements cause you to to start reusing these objects.

Writing tests will uncover every bit of overlooked tight coupling
and immediately reward you for fixing it.

## What should we test?

Every class should have its own unit test, unless doing otherwise saves money.

The allowed-to-skip-tests bar is high, but some code meets it.

## When we are allowed to skip tests?

Creating a new class by following a recipe instead of by doing TDD is allowable,
We are allowed to temporarily skip tests in this case.

Private class should not be tested.

We can also skip tests for the classes that are too simple and too small,
to tell other programmers that they are too simple and too small.

Tests should give you the freedom to improve code,
not glue you to its current implementation.

When they constrain rather than liberate,
ask if they're worthwhile, and consider omitting them.

## How to justify skipped tests?

In the rare case where you decide to forego giving a class its own unit test,
you must be able to defend this decision with a clearly articulated justification.

In addition to size and complexity, visibility is also an important consideration.

Visibility is determined by the context in which the class is known.

## How to test Exp and Core class of a type system implementation?

I do not know yet, I know two choices:

1. depend on the syntax
2. do not depend on the syntax

If Exp class owns responsibilities of syntax and parsing, (1) would be simple.

But (2) seems too expensive no matter how.

## Why unit tests for each class is important?

Because integration test involves many objects in combination,
the code could break quite far from the origin of the problem.
This makes it hard to determine the cause of an error.

## Why integration tests are important?

Integration tests are great at proving the correctness of
the collaboration between groups of objects.

## How should we write unit tests?

Unit tests ought to tell an illuminating story.

They should demonstrate and confirm
the class's direct responsibilities,
and do nothing else.

You should strive to write the fastest tests possible,
in the fewest number necessary,
using the most intention-revealing expectations,
and the least amount of code.

## Why we should write tests first?

It's far better to struggle with a test that you don't understand
than to write code that you don't understand.

Tests force you to clarify your intentions because they make explicit assertions.

Code has no such pressure, and can be left a confusing mess forever.

## What is an object's context?

An object's context is its surrounding environment,
or the interrelated conditions under which it can exist.

## How to reduce a object's (or class') context?

After refactoring, some classes might still reflect the context they come from.

It might be helpful to remove some dependencies,
generalize some methods,
and rename the class after a more general concept.

## How should we name our classes when using design patterns?

We should not include the name of a pattern in the name of a class.
Because pattern names don't generally reflect concepts in your application.

We should search for names that add semantic meaning.

We should not give up too soon on the hard problem of naming.

## How to use interface in dynamicly typed language?

Use test to test class has methods in the interface,
maybe including number of arguments, and type of arguments.

## After coding of refactoring, what should we do at last?

Check the code for one more time,
to obliterating obsolete context.

And peruse the complete listings and glory in your accomplishments.

## What features allow object-oriented to interact with unanticipated variants without having to change?

When designed with the following features,
object-oriented code can interact with new
and unanticipated variants without having to change:

1. Variants are isolated.

   They're usually isolated in some kind of object, often a new class.

2. Variant selection is isolated.

   Selection happens in factories, which may be as simple as isolated conditionals that choose a class.

3. Message senders and receivers are loosely coupled.

   This is commonly accomplished by injecting dependencies.

4. Variants are interchangeable.

   Message senders treat injected objects as equivalent players of identical roles.
