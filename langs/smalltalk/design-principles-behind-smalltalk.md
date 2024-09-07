---
title: design principles behind smalltalk
author: Dan Ingalls
---

# design

Personal Mastery:
If a system is to serve the creative spirit,
it must be entirely comprehensible to a single individual.
Any barrier that exists between the user and some part of the system
will eventually be a barrier to creative expression.
Any part of the system that cannot be changed
or that is not sufficiently general is a likely source of impediment.
If one part of the system works differently from all the rest,
that part will require additional effort to control.
Such an added burden may detract from the final result
and will inhibit future endeavors in that area.
We can thus infer a general principle of design:

Good Design:
A system should be built with a minimum set of unchangeable parts;
those parts should be as general as possible;
and all parts of the system should be held in a uniform framework.

# language

Purpose of Language:
To provide a framework for communication.

The scope of language design:
Communication between two people
(or between one person and a computer)
includes communication on two levels.
Explicit communication includes the information that is transmitted in a given message.
Implicit communication includes the relevant assumptions common to the two beings.
The design of a language for using computers must deal with internal models, external media,
and the interaction between these in both the human and the computer.
- 也就是强调设计并实现一个程序语言并不是单单写一个编译器那么简单
  而是要设计人和机器之间交流的方式

# Communicating Objects

Objects:
A computer language should support the concept of "object"
and provide a uniform means for referring to the objects in its universe.
Objects are created when expressions are evaluated,
and they can be passed around by uniform reference.
When all references to an object have disappeared from the system,
the object itself vanishes, and its storage is reclaimed.

Storage Management:
To be truly "object-oriented",
a computer system must provide automatic storage management.

Messages:
Computing should be viewed as an intrinsic capability of objects
that can be uniformly invoked by sending messages.
for example:
smalltalk sends the name of the desired operation,
along with any arguments, as a message to the number,
with the understanding that the receiver knows best how to carry out the desired operation.
The transmission of messages is the only process
that is carried on outside of objects
and this is as it should be, since messages travel between objects.

Uniform Metaphor:
A language should be designed around a powerful metaphor
that can be uniformly applied in all areas.
large applications should be viewed in the same way
as the fundamental units from which the system is built.
In Smalltalk especially,
the interaction between the most primitive objects
is viewed in the same way as the highest-level interaction
between the computer and its user !!!
Every object in Smalltalk, even a lowly integer,
has a set of messages, a protocol,
that defines the explicit communication to which that object can respond.
Internally, objects may have local storage
and access to other shared information
which comprise the implicit context of all communication.
For instance,
the message + 5 (add five) carries an implicit assumption that
the augend is the present value of the number receiving the message.

# Organization

System complexity:
As the number of components in a system increases,
the chances for unwanted interaction increase rapidly.
Because of this, a computer language should be designed
to minimize the possibilities of such interdependence.

Modularity:
No component in a complex system should depend on the internal details of any other component.

The message-sending metaphor provides modularity
because all access to the internal state of an object is through the same message interface.

Classification:
A language must provide a means for classifying similar objects,
- 即 定义新的数据类型
and for adding new classes of objects on equal footing with the kernel classes of the system.

Polymorphism:
A program should specify only the behavior of objects, not their representation.
- 上面是说接口与实现方式之间的分离

Factoring:
Each independent component in a system would appear in only one place.

Leverage:
When a system is well factored, great leverage is available to users and implementers alike.

Virtual Machine:
A virtual machine specification establishes a framework for the application of technology.

# User Interface

Reactive Principle:
Every component accessible to the user
should be able to present itself in a meaningful way for observation and manipulation.

Operating System:
An operating system is a collection of things that don't fit into a language. There shouldn't be one.

# Future Work

Natural Selection:
Languages and systems that are of sound design will persist,
to be supplanted only by better ones.
